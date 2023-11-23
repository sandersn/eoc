import { assertDefined } from "./core.js"
import { Exp, Prim, Var, Program, Let, Stmt, Assign, Seq, Return } from "./factory.js"
import parse from "./parser.js"
import { AList } from "./structures.js"
import { read } from "./core.js"
let counter = 0
function gensym() {
  return "g" + counter++
}
export function interpExp(e: Exp, env: AList<string, number>): number {
  switch (e.kind) {
    case "int":
      return e.val
    case "bool":
      return e.val ? 1 : 0
    case "var": {
      return assertDefined(env.get(e.name))
    }
    case "let":
      return interpExp(e.body, new AList(e.name, interpExp(e.exp, env), env))
    case "prim":
      if (e.op === "read") return +read()
      if (e.op === "+") return interpExp(e.args[0], env) + interpExp(e.args[1], env)
      if (e.op === "-" && e.args.length === 1) return -interpExp(e.args[0], env)
      if (e.op === "-" && e.args.length === 2) return interpExp(e.args[0], env) - interpExp(e.args[1], env)
    default:
      return NaN
  }
}
export function interpProgram(p: Program): number {
  return interpExp(p.body, new AList("!!!!!!", NaN, undefined))
}
export function parseProgram(sexp: string): Program {
  return Program(new AList("!!!!!!!!!", NaN, undefined), parse(sexp))
}
export function emitProgram(p: Program): string {
  return emitExp(p.body)
}
export function emitExp(e: Exp): string {
  switch (e.kind) {
    case "int":
      return `${e.val}`
    case "bool":
      return e.val ? "#t" : "#f"
    case "prim":
      return `(${e.op} ${e.args.map(a => emitExp(a)).join(" ")})`
    case "var":
      return e.name
    case "let":
      return `(let (${e.name} ${emitExp(e.exp)}) ${emitExp(e.body)})`
  }
}
export function removeComplexOperands(p: Program): Program {
  return Program(p.info, removeComplexOperandsExp(p.body))
}
function removeComplexOperandsExp(e: Exp): Exp {
  switch (e.kind) {
    case "var":
    case "int":
    case "bool":
      return e
    case "prim": {
      if (e.op === "read") return e
      if (e.args.length === 1) {
        const [tmp, tmps] = removeComplexOperandsAtom(e.args[0], false)
        return generateTmpLets(Prim(e.op, tmp), tmps)
      } else if (e.args.length === 2) {
        const [tmp1, tmps1] = removeComplexOperandsAtom(e.args[0], false)
        const [tmp2, tmps2] = removeComplexOperandsAtom(e.args[1], false)
        return generateTmpLets(Prim(e.op, tmp1, tmp2), [...tmps1, ...tmps2])
      } else {
        throw new Error("Unexpected number of arguments")
      }
    }
    case "let": {
      const [tmpE, tmpsE] = removeComplexOperandsAtom(e.exp, false)
      const [tmpBody, tmpsBody] = removeComplexOperandsAtom(e.body, true)
      return generateTmpLets(Let(e.name, tmpE, generateTmpLets(tmpBody, tmpsBody)), tmpsE)
    }
  }
}
function removeComplexOperandsAtom(e: Exp, isTail: boolean): [Exp, Array<[string, Exp]>] {
  switch (e.kind) {
    case "var":
    case "int":
    case "bool":
      return [e, []]
    case "prim": {
      const tmp = gensym()
      if (e.op === "read") {
        return [Var(tmp), [[tmp, e]]]
      }
      if (e.args.length === 1) {
        const [e1, tmps] = removeComplexOperandsAtom(e.args[0], false)
        return [Var(tmp), [[tmp, Prim(e.op, e1)], ...tmps]]
      } else if (e.args.length === 2) {
        const [e1, tmps1] = removeComplexOperandsAtom(e.args[0], false)
        const [e2, tmps2] = removeComplexOperandsAtom(e.args[1], false)
        const tmps = [...tmps1, ...tmps2]
        return [Var(tmp), [[tmp, Prim(e.op, e1, e2)], ...tmps]]
      } else {
        throw new Error("Unexpected number of arguments")
      }
    }
    case "let": {
      const [e1, tmpsE] = removeComplexOperandsAtom(e.exp, false)
      const [body1, tmpsBody] = removeComplexOperandsAtom(e.body, isTail)
      if ((e1.kind === "var" || e1.kind === "int") && isTail) {
        return [generateTmpLets(Let(e.name, e1, generateTmpLets(body1, tmpsBody)), tmpsE), []]
      }
      const tmp = gensym()
      return [Var(tmp), [[tmp, Let(e.name, e1, generateTmpLets(body1, tmpsBody))], ...tmpsE]]
    }
  }
}

export function uniquifyProgram(p: Program): Program {
  return Program(p.info, uniquifyExp(p.body, new AList("!!!!!!", "@@@@@@@@@", undefined)))
}
function uniquifyExp(e: Exp, env: AList<string, string>): Exp {
  switch (e.kind) {
    case "var":
      return Var(assertDefined(env.get(e.name)))
    case "int":
    case "bool":
      return e
    case "prim":
      return Prim(e.op, ...e.args.map(a => uniquifyExp(a, env)))
    case "let": {
      let x = env.get(e.name) ? gensym() : e.name
      env = new AList(e.name, x, env)
      return Let(x, uniquifyExp(e.exp, env), uniquifyExp(e.body, env))
    }
  }
}
function explicateAssign(e: Exp, x: string, k: Stmt[]): Stmt[] {
  switch (e.kind) {
    case "var":
    case "int":
    case "bool":
    case "prim":
      return [Assign(Var(x), e), ...k]
    case "let": {
      return explicateAssign(e.exp, e.name, explicateAssign(e.body, x, k))
    }
  }
}
export function explicateTail(e: Exp): Stmt {
  switch (e.kind) {
    case "var":
    case "int":
    case "bool":
    case "prim":
      return Return(e)
    case "let": {
      const tail = explicateTail(e.body)
      switch (tail.kind) {
        case "seq":
          return Seq(explicateAssign(e.exp, e.name, tail.statements))
        case "return":
          return Seq(explicateAssign(e.exp, e.name, [tail]))
        case "assign":
          throw new Error("Unexpected assign")
      }
    }
  }
}
function generateTmpLets(init: Exp, tmps: Array<[string, Exp]>): Exp {
  return tmps.reduce((e, [name, exp]) => Let(name, exp, e), init)
}
