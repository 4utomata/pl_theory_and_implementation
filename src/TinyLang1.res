// tiny language 1
type rec expr =
  | Cst(int)
  | Add(expr, expr)
  | Mul(expr, expr)
  | Var(string)
  | Let(string, expr, expr)

// store expr's name and value
type env = list<(string, int)>

let rec eval = (expr: expr, env: env) => {
  switch expr {
  | Cst(i) => i
  | Add(a, b) => eval(a, env) + eval(b, env)
  | Mul(a, b) => eval(a, env) + eval(b, env)
  | Var(x) => List.assoc(x, env) // find named expr 'x' in env
  | Let(x, e1, e2) => eval(e2, list{(x, eval(e1, env)), ...env}) // eval e1 and bind the result to 'x', update env and eval e2
  }
}
