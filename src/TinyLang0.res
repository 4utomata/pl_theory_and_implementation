// tiny language 0
type rec expr = 
  | Cst(int)
  | Add(expr, expr)
  | Mul(expr, expr)

// use rescript to evaluate AST (big step semantics)
let rec eval = (expr: expr) => {
  switch expr {
  | Cst(i) => i
  | Add(a, b) => eval(a) + eval(b)
  | Mul(a, b) => eval(a) * eval(b)
  }
}

module StackMachine = {
  // instruction
  type instr = Cst(int) | Add | Mul
  // instruction set
  type instrs = list<instr>
  type operand = int
  type stack = list<operand>

  let concat = Belt.List.concatMany
  // compile AST to instruction set
  let rec compile = (expr: expr): instrs => {
    switch expr {
      | Cst(i) => list{Cst(i)}
      | Add(a, b) => concat([compile(a), compile(b), list{Add}])
      | Mul(a, b) => concat([compile(a), compile(b), list{Mul}])
    }
  }

  // evaluate instructions (small step semantics)
  let rec eval = (instrs: instrs, stk: stack) => {
    switch (instrs, stk) {
    | (list{ Cst(i), ... rest}, _ ) => eval(rest, list{i, ...stk})
    | (list{ Add, ... rest}, list{a, b, ...stk}) => eval(rest, list{a+b, ...stk})
    | (list{ Mul, ... rest}, list{a, b, ...stk}) => eval(rest, list{a*b, ...stk})
    | (list{}, list{result}) => result
    | _ => assert false
    }
  }
}

module Tests = {
  let testCase = [
    (Cst(42), 42),
    (Add(Cst(1), Cst(2)), 3),
    (Mul(Cst(2), Cst(3)), 6),
    (Add(Add(Cst(1), Cst(2)), Cst(3)), 6),
    (Mul(Mul(Cst(1), Cst(2)), Cst(3)), 6),
  ]

  let testEval = () => {
    Js.log("test eval")
    testCase->Js.Array2.forEach(test => {
      let (expr, expect) = test
      assert(eval(expr) == expect)
    })
  }

  let testCompileAndEval = () => {
    Js.log("test compile and eval")
    testCase->Js.Array2.forEach(test => {
      let (expr, expect) = test
      let compiledInstrs = StackMachine.compile(expr)
      assert(StackMachine.eval(compiledInstrs, list{}) == expect)
    })
  }
}

Tests.testEval()
Tests.testCompileAndEval()