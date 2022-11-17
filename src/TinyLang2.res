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
  let assoc = (x: string, env: env) => env->Belt.List.getAssoc(x, (a, b) => a == b)->Belt.Option.getExn
  let rec eval = (expr: expr, env: env) => {
    switch expr {
    | Cst(i) => i
    | Add(a, b) => eval(a, env) + eval(b, env)
    | Mul(a, b) => eval(a, env) + eval(b, env)
    | Var(x) => assoc(x, env) // find named expr 'x' in env
    | Let(x, e1, e2) => eval(e2, list{(x, eval(e1, env)), ...env}) // eval e1 and bind the result to 'x', update env and eval e2
    }
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
  let rec eval = (expr, env) => {
    switch expr {
      | Cst(i) => i
      | Add(a, b) => eval(a, env) + eval(b, env)
      | Mul(a, b) => eval(a, env) * eval(b, env)
      | Var(n) => List.nth(env, n)
      | Let(e1, e2) => eval(e2, list{eval(e1, env), ...env})
    }
  }
}

module StackMachine = {
  type instr = Cst(int) | Add | Mul | Var(int) | Pop | Swap
  type instrs = list<instr>
  type operand = int
  type stack = list<operand>

  // Homework1: write an intepreter for the stack machine with variables
  let rec eval = (instrs: instrs, stk: stack) => {
    switch (instrs, stk) {
    | (list{ Cst(i), ... rest}, _ ) => eval(rest, list{i, ...stk})
    | (list{ Add, ... rest}, list{a, b, ...stk}) => eval(rest, list{a+b, ...stk})
    | (list{ Mul, ... rest}, list{a, b, ...stk}) => eval(rest, list{a*b, ...stk})
    | (list{ Var(i), ... rest}, stk) => eval(rest, list{stk->Belt.List.getExn(i), ...stk})
    | (list{ Swap, ... rest}, list{a, b, ...stk}) => eval(rest, list{b, a, ...stk})
    | (list{ Pop, ... rest}, list{_, ...stk}) => eval(rest, list{...stk})
    | (list{}, list{result}) => result
    | _ => assert false
    }
  }
}

// compile env
type cenv = list<string>
// find x's index
let index = (cenv, x) => {
  let rec find = (cenv, n) => {
    switch cenv {
      | list{} => raise(Not_found)
      | list{a, ...rest} => if a == x { n } else {find(rest, n+1)}
    }
  }
  find(cenv, 0)
}
// lowering expr to Nameless.expr
let rec lower = (expr: Named.expr, cenv: cenv): Nameless.expr => {
  switch expr {
    | Cst(i) => Cst(i)
    | Add(a, b) => Add(lower(a, cenv), lower(b, cenv))
    | Mul(a, b) => Mul(lower(a, cenv), lower(b, cenv))
    | Var(x) => Var(index(cenv, x))
    | Let(x, e1, e2) => Let(lower(e1, cenv), lower(e2, list{x, ...cenv}))
  }
}

let concat = Belt.List.concatMany
// Homework2: write a compiler to translate Nameless.expr to stack machine instructions
let rec compileNameless = (expr: Nameless.expr): StackMachine.instrs => {
  switch expr {
    | Cst(i) => list{Cst(i)}
    | Add(a, b) => concat([compileNameless(a), compileNameless(b), list{Add}])
    | Mul(a, b) => concat([compileNameless(a), compileNameless(b), list{Mul}])
    | Var(x) => list{Var(x)}
    | Let(e1, e2) => concat([compileNameless(e1), compileNameless(e2), list{Swap, Pop}])
  }
}

module Tests = {
  // TODO
}