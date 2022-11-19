// tiny language 2
module Named = {
  type rec expr =
    | Cst(int)
    | Add(expr, expr)
    | Mul(expr, expr)
    | Var(string)
    | Let(string, expr, expr)

  // store expr's name and value
  type env = list<(string, int)>
  let assoc = (x: string, env: env) =>
    env->Belt.List.getAssoc(x, (a, b) => a == b)->Belt.Option.getExn
  let eval = (expr: expr) => {
    let rec go = (expr: expr, env: env) => {
      switch expr {
      | Cst(i) => i
      | Add(a, b) => go(a, env) + go(b, env)
      | Mul(a, b) => go(a, env) * go(b, env)
      | Var(x) => assoc(x, env) // find named expr 'x' in env
      | Let(x, e1, e2) => go(e2, list{(x, go(e1, env)), ...env}) // eval e1 and bind the result to 'x', update env and eval e2
      }
    }
    go(expr, list{})
  }
}

module Nameless = {
  type rec expr =
    | Cst(int)
    | Add(expr, expr)
    | Mul(expr, expr)
    | Var(int)
    | Let(expr, expr)

  type env = list<int>
  let eval = (expr: expr) => {
    let rec go = (expr: expr, env: env) => {
      switch expr {
      | Cst(i) => i
      | Add(a, b) => go(a, env) + go(b, env)
      | Mul(a, b) => go(a, env) * go(b, env)
      | Var(n) => List.nth(env, n)
      | Let(e1, e2) => go(e2, list{go(e1, env), ...env})
      }
    }
    go(expr, list{})
  }
}

module StackMachine = {
  type instr = Cst(int) | Add | Mul | Var(int) | Pop | Swap
  type instrs = list<instr>
  type operand = int
  type stack = list<operand>

  // Homework1: write an intepreter for the stack machine with variables
  let eval = (instrs: instrs) => {
    let rec go = (instrs: instrs, stk: stack) => {
      switch (instrs, stk) {
      | (list{Cst(i), ...rest}, _) => go(rest, list{i, ...stk})
      | (list{Add, ...rest}, list{a, b, ...stk}) => go(rest, list{a + b, ...stk})
      | (list{Mul, ...rest}, list{a, b, ...stk}) => go(rest, list{a * b, ...stk})
      | (list{Var(i), ...rest}, stk) => go(rest, list{stk->Belt.List.getExn(i), ...stk})
      | (list{Swap, ...rest}, list{a, b, ...stk}) => go(rest, list{b, a, ...stk})
      | (list{Pop, ...rest}, list{_, ...stk}) => go(rest, stk)
      | (list{}, list{result}) => result
      | _ => assert false
      }
    }
    go(instrs, list{})
  }
}

// compile env
type cenv = list<string>
// find x's index
let index = (cenv, x) => {
  let rec find = (cenv, n) => {
    switch cenv {
    | list{} => raise(Not_found)
    | list{a, ...rest} =>
      if a == x {
        n
      } else {
        find(rest, n + 1)
      }
    }
  }
  find(cenv, 0)
}
// compile Named.expr to Nameless.expr
let compileToNameless = (expr: Named.expr): Nameless.expr => {
  let rec go = (expr: Named.expr, cenv: cenv): Nameless.expr => {
    switch expr {
    | Cst(i) => Cst(i)
    | Add(a, b) => Add(go(a, cenv), go(b, cenv))
    | Mul(a, b) => Mul(go(a, cenv), go(b, cenv))
    | Var(x) => Var(index(cenv, x))
    | Let(x, e1, e2) => Let(go(e1, cenv), go(e2, list{x, ...cenv}))
    }
  }
  go(expr, list{})
}

// Homework2: write a compiler to translate Nameless.expr to stack machine instructions
type sv = Slocal | Stmp
type senv = list<sv>

// calculate var index
let sindex = (senv, i) => {
  let rec go = (senv, i, acc) => {
    switch senv {
    | list{} => raise(Not_found)
    | list{Slocal, ...rest} =>
      if i == 0 {
        acc
      } else {
        go(rest, i - 1, acc + 1)
      }
    | list{Stmp, ...rest} => go(rest, i, acc + 1)
    }
  }
  go(senv, i, 0)
}

// simulate push and pop on abstract stack
let concat = Belt.List.concatMany
let compileNamelessToInstrs = (expr: Nameless.expr): StackMachine.instrs => {
  let rec go = (expr: Nameless.expr, senv: senv): StackMachine.instrs => {
    switch expr {
    | Cst(i) => list{Cst(i)}
    | Var(s) => list{Var(sindex(senv, s))}
    | Add(e1, e2) => concat([go(e1, senv), go(e2, list{Stmp, ...senv}), list{Add}])
    | Mul(e1, e2) => concat([go(e1, senv), go(e2, list{Stmp, ...senv}), list{Mul}])
    | Let(e1, e2) => concat([go(e1, senv), go(e2, list{Slocal, ...senv}), list{Swap, Pop}])
    }
  }
  go(expr, list{})
}

module Tests = {
  let testCase: array<(Named.expr, int)> = [
    (Cst(42), 42),
    (Add(Cst(1), Cst(2)), 3),
    (Mul(Cst(2), Cst(3)), 6),
    (Add(Add(Cst(1), Cst(2)), Cst(3)), 6),
    (Mul(Mul(Cst(1), Cst(2)), Cst(3)), 6),
    (Add(Cst(1), Let("x", Cst(2), Add(Var("x"), Var("x")))), 5)
  ]

  let testEvalNamedExpr = () => {
    Js.log("test eval named expr...")
    testCase->Js.Array2.forEach(test => {
      let (expr, expect) = test
      assert (Named.eval(expr) == expect)
    })
    Js.log("pass")
  }

  let testEvalNamelessExpr = () => {
    Js.log("test eval nameless expr...")
    testCase->Js.Array2.forEach(test => {
      let (expr, expect) = test
      let namelessExpr = compileToNameless(expr)
      assert (Nameless.eval(namelessExpr) == expect)
    })
    Js.log("pass")
  }

  let testCompileAndEvalNameless = () => {
    Js.log("test compile nameless expr to stack machine instructions and eval...")
    testCase->Js.Array2.forEach(test => {
      let (expr, expect) = test
      let namelessExpr = compileToNameless(expr)
      let instrs = compileNamelessToInstrs(namelessExpr)
      assert (StackMachine.eval(instrs) == expect)
    })
    Js.log("pass")
  }
}

Tests.testEvalNamedExpr()
Tests.testEvalNamelessExpr()
Tests.testCompileAndEvalNameless()
