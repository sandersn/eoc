import { assertDefined, zip, unzip, Box, box, setBox, unbox } from "./core.js"
import { Exp, Atom, Prim, PrimAtom, Var, Program, Let, If, Bool, SetBang, Begin, While, GetBang } from "./factory.js"
import parse from "./parser.js"
import { AList, alist, assoc } from "./structures.js"
import { read } from "./core.js"
let counter = 0
function gensym() {
  return "g" + counter++
}
/* ### Interpreter ### */
export function interpExp(e: Exp, env: AList<string, Box> | undefined): number {
  switch (e.kind) {
    case "int":
      return e.val
    case "bool":
      return e.val ? 1 : 0
    case "get":
      throw new Error("Do not interpret post uncoverGet ASTs.")
    case "var": {
      return unbox(assertDefined(assoc(env, e.name)))
    }
    case "let":
      return interpExp(e.body, alist(e.name, box(interpExp(e.exp, env)), env))
    case "set":
      setBox(assertDefined(assoc(env, e.name)), interpExp(e.exp, env))
      return NaN
    case "begin":
      for (const exp of e.exps) {
        if (!isNaN(interpExp(exp, env))) {
          throw new Error("Expected expression to have type void, but it returned a value.")
        }
      }
      return interpExp(e.body, env)
    case "while":
      while (interpExp(e.cond, env)) {
        interpExp(e.body, env)
      }
      return NaN
    case "void":
      return NaN
    case "prim":
      return interpPrim(e, env)
    case "if":
      return interpExp(e.cond, env) === 1 ? interpExp(e.then, env) : interpExp(e.else, env)
  }
}
function interpPrim(e: Prim, env: AList<string, Box> | undefined): number {
  if (e.op === "read") return +read()
  if (e.op === "+") return interpExp(e.args[0], env) + interpExp(e.args[1], env)
  if (e.op === "-" && e.args.length === 1) return -interpExp(e.args[0], env)
  if (e.op === "-" && e.args.length === 2) return interpExp(e.args[0], env) - interpExp(e.args[1], env)
  if (e.op === "and" && e.args.length === 2) return interpExp(e.args[0], env) && interpExp(e.args[1], env)
  if (e.op === "or" && e.args.length === 2) return interpExp(e.args[0], env) || interpExp(e.args[1], env)
  if (e.op === "not" && e.args.length === 1) return interpExp(e.args[0], env) ? 0 : 1
  if (e.op === "<=" && e.args.length === 2) return interpExp(e.args[0], env) <= interpExp(e.args[1], env) ? 1 : 0
  if (e.op === "<" && e.args.length === 2) return interpExp(e.args[0], env) < interpExp(e.args[1], env) ? 1 : 0
  if (e.op === ">=" && e.args.length === 2) return interpExp(e.args[0], env) >= interpExp(e.args[1], env) ? 1 : 0
  if (e.op === ">" && e.args.length === 2) return interpExp(e.args[0], env) > interpExp(e.args[1], env) ? 1 : 0
  if (e.op === "==" && e.args.length === 2) return interpExp(e.args[0], env) === interpExp(e.args[1], env) ? 1 : 0
  return NaN
}
export function interpProgram(p: Program): number {
  return interpExp(p.body, undefined)
}

export function parseProgram(sexp: string): Program {
  return Program(undefined, parse(sexp))
}
/* ### Type checker ### */
/** It's nominal babyyyyyyyyyyyyy */
type Type = symbol
const intType = Symbol("Integer")
const boolType = Symbol("Boolean")
const voidType = Symbol("Void")
const operatorTypes = new Map<string, [[...Type[]], Type]>([
  ["+", [[intType, intType], intType]],
  ["-", [[intType, intType], intType]],
  ["read", [[], intType]],
  ["and", [[boolType, boolType], boolType]],
  ["or", [[boolType, boolType], boolType]],
  ["not", [[boolType], boolType]],
  ["<=", [[intType, intType], boolType]],
  ["<", [[intType, intType], boolType]],
  [">=", [[intType, intType], boolType]],
  [">", [[intType, intType], boolType]],
])
function isTypeEqual(t1: Type, t2: Type): boolean {
  return t1 === t2
}
function assertTypeEqual(got: Type, expected: Type, e: Exp): void {
  if (!isTypeEqual(got, expected)) {
    // TODO: Stop throwing errors and collect them instead
    throw new Error(`Got ${emitType(got)} but expected ${emitType(expected)} in ${emitExp(e)}`)
  }
}
/** I have NO idea why the type checker is allowed to return a modified tree (but in chapter 4 doesn't even use the capability yet). */
export function typeCheckProgram(p: Program): Program {
  const [body, t] = typeCheckExp(p.body, undefined)
  assertTypeEqual(t, intType, p.body)
  return Program(p.info, body)
}
function typeCheckExp(e: Exp, env: AList<string, Type> | undefined): [Exp, Type] {
  switch (e.kind) {
    case "int":
      return [e, intType]
    case "bool":
      return [e, boolType]
    case "prim": {
      if (e.op === "==") {
        // == is generic
        const [e1, t1] = typeCheckExp(e.args[0], env)
        const [e2, t2] = typeCheckExp(e.args[1], env)
        assertTypeEqual(t1, t2, e)
        return [Prim(e.op, e1, e2), boolType]
      } else if (e.op === "-" && e.args.length === 1) {
        // - is overloaded
        const [e1, t1] = typeCheckExp(e.args[0], env)
        assertTypeEqual(t1, intType, e)
        return [Prim(e.op, e1), intType]
      }
      const [args, ts] = unzip(e.args.map(arg => typeCheckExp(arg, env)))
      return [Prim(e.op, ...args), typeCheckOp(e.op, ts, e)]
    }
    case "get":
      throw new Error("Do no type check post-uncoverGet ASTs.")
    case "var":
      return [e, assertDefined(assoc(env, e.name))]
    case "let": {
      const [exp, te] = typeCheckExp(e.exp, env)
      const [body, tbody] = typeCheckExp(e.body, alist(e.name, te, env))
      return [Let(e.name, exp, body), tbody]
    }
    case "set":
      const [exp, te] = typeCheckExp(e.exp, env)
      assertTypeEqual(te, assertDefined(assoc(env, e.name)), e)
      return [SetBang(e.name, exp), voidType]
    case "begin": {
      const exps = []
      for (const exp of e.exps) {
        const [expp, expt] = typeCheckExp(exp, env)
        assertTypeEqual(expt, voidType, exp)
        exps.push(expp)
      }
      const [body, tbody] = typeCheckExp(e.body, env)
      return [Begin(exps, body), tbody]
    }
    case "while": {
      const [cond, tcond] = typeCheckExp(e.cond, env)
      const [body, tbody] = typeCheckExp(e.body, env)
      assertTypeEqual(tcond, boolType, e.cond)
      assertTypeEqual(tbody, voidType, e.body)
      return [While(cond, body), voidType]
    }
    case "void":
      return [e, voidType]
    case "if": {
      const [cond, t1] = typeCheckExp(e.cond, env)
      const [then, t2] = typeCheckExp(e.then, env)
      const [else_, t3] = typeCheckExp(e.else, env)
      assertTypeEqual(t1, boolType, e.cond)
      assertTypeEqual(t3, t2, e)
      return [If(cond, then, else_), t2]
    }
  }
}
function typeCheckOp(op: string, args: Type[], e: Prim): Type {
  const sig = operatorTypes.get(op)
  if (sig) {
    const [params, ret] = sig
    for (const [arg, param] of zip(args, params)) {
      assertTypeEqual(arg, param, e)
    }
    return ret
  } else {
    throw new Error(`Unknown operator ${op}`)
  }
}
/* ### Emitter ### */
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
    case "get":
      return `[get ${e.name}]`
    case "let":
      return `(let (${e.name} ${emitExp(e.exp)}) ${emitExp(e.body)})`
    case "set":
      return `(set ${e.name} ${emitExp(e.exp)})`
    case "begin":
      return `(begin ${e.exps.map(emitExp).join(" ")} ${emitExp(e.body)})`
    case "while":
      return `(while ${emitExp(e.cond)} ${emitExp(e.body)})`
    case "void":
      return `(void)`
    case "if":
      return `[if ${emitExp(e.cond)} ${emitExp(e.then)} ${emitExp(e.else)}]`
  }
}
export function emitType(t: Type): string {
  switch (t) {
    case intType:
      return "int"
    case boolType:
      return "bool"
    case voidType:
      return "void"
  }
  throw new Error(`Unexpected type ${t.toString()}`)
}
/* ### Frontend passes ###
 * 1. reparsePrimitives
 * 2. uniquify
 * 3. uncoverGet
 * 4. removeComplexOperands
 */
/** Convert primitive nodes to ifs and calls as needed */
export function reparsePrimitives(p: Program): Program {
  return Program(p.info, reparsePrimitivesExp(p.body))
}
function reparsePrimitivesExp(e: Exp): Exp {
  switch (e.kind) {
    case "prim":
      if (e.op === "if")
        return If(reparsePrimitivesExp(e.args[0]), reparsePrimitivesExp(e.args[1]), reparsePrimitivesExp(e.args[2]))
      else if (e.op === "and") {
        return If(reparsePrimitivesExp(e.args[0]), reparsePrimitivesExp(e.args[1]), Bool(false))
      } else if (e.op === "or") {
        return If(reparsePrimitivesExp(e.args[0]), Bool(true), reparsePrimitivesExp(e.args[1]))
      }
      return Prim(e.op, ...e.args.map(reparsePrimitivesExp))
    // TODO: Later, if e.op not in primitive list, then create a Call
    case "var":
    case "get":
    case "int":
    case "bool":
      return e
    case "let":
      return Let(e.name, reparsePrimitivesExp(e.exp), reparsePrimitivesExp(e.body))
    case "set":
      return SetBang(e.name, reparsePrimitivesExp(e.exp))
    case "begin":
      return Begin(e.exps.map(reparsePrimitivesExp), reparsePrimitivesExp(e.body))
    case "while":
      return While(reparsePrimitivesExp(e.cond), reparsePrimitivesExp(e.body))
    case "void":
      return e
    case "if":
      return If(reparsePrimitivesExp(e.cond), reparsePrimitivesExp(e.then), reparsePrimitivesExp(e.else))
  }
}

export function uniquifyProgram(p: Program): Program {
  return Program(p.info, uniquifyExp(p.body, undefined))
}
function uniquifyExp(e: Exp, env: AList<string, string> | undefined): Exp {
  switch (e.kind) {
    case "var":
      return Var(assertDefined(assoc(env, e.name)))
    case "get":
      throw new Error("Run uniquify before uncoverGet")
    case "int":
    case "bool":
      return e
    case "prim":
      return Prim(e.op, ...e.args.map(a => uniquifyExp(a, env)))
    case "if":
      return If(uniquifyExp(e.cond, env), uniquifyExp(e.then, env), uniquifyExp(e.else, env))
    case "let": {
      let x = assoc(env, e.name) ? gensym() : e.name
      env = alist(e.name, x, env)
      return Let(x, uniquifyExp(e.exp, env), uniquifyExp(e.body, env))
    }
    case "set":
      return SetBang(e.name, uniquifyExp(e.exp, env))
    case "begin":
      return Begin(
        e.exps.map(exp => uniquifyExp(exp, env)),
        uniquifyExp(e.body, env)
      )
    case "while":
      return While(uniquifyExp(e.cond, env), uniquifyExp(e.body, env))
    case "void":
      return e
  }
}

export function uncoverGet(p: Program): Program {
  const sets = collectSet(p.body)
  return Program(p.info, uncoverGetExp(p.body))

  function uncoverGetExp(e: Exp): Exp {
    switch (e.kind) {
      case "prim":
        return Prim(e.op, ...e.args.map(uncoverGetExp))
      case "var":
        return sets.has(e.name) ? GetBang(e.name) : e
      case "get":
      case "int":
      case "bool":
      case "void":
        return e
      case "if":
        return If(uncoverGetExp(e.cond), uncoverGetExp(e.then), uncoverGetExp(e.else))
      case "let":
        return Let(e.name, uncoverGetExp(e.exp), uncoverGetExp(e.body))
      case "set":
        return SetBang(e.name, uncoverGetExp(e.exp))
      case "begin":
        return Begin(e.exps.map(uncoverGetExp), uncoverGetExp(e.body))
      case "while":
        return While(uncoverGetExp(e.cond), uncoverGetExp(e.body))
    }
  }
}
function collectSet(e: Exp): Set<string> {
  const sets: Set<string> = new Set()
  collectSetExp(e)
  return sets

  function collectSetExp(e: Exp): void {
    switch (e.kind) {
      case "prim":
        for (const arg of e.args) collectSetExp(arg)
        return
      case "var":
      case "get":
      case "int":
      case "bool":
      case "void":
        return
      case "if":
        collectSetExp(e.cond)
        collectSetExp(e.then)
        collectSetExp(e.else)
        return
      case "let":
        collectSetExp(e.exp)
        collectSetExp(e.body)
        return
      case "set":
        sets.add(e.name)
        collectSetExp(e.exp)
        return
      case "begin":
        for (const exp of e.exps) collectSetExp(exp)
        collectSetExp(e.body)
        return
      case "while":
        collectSetExp(e.cond)
        collectSetExp(e.body)
    }
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
    case "void":
      return e
    case "prim": {
      if (e.op === "read") return e
      if (e.args.length === 1) {
        const [arg, tmps] = removeComplexOperandsAtom(e.args[0])
        return generateLets(tmps, PrimAtom(e.op, arg))
      } else if (e.args.length === 2) {
        const [arg1, tmps1] = removeComplexOperandsAtom(e.args[0])
        const [arg2, tmps2] = removeComplexOperandsAtom(e.args[1])
        return generateLets([...tmps2, ...tmps1], PrimAtom(e.op, arg1, arg2))
      } else {
        throw new Error("Unexpected number of arguments")
      }
    }
    case "if":
      return If(removeComplexOperandsExp(e.cond), removeComplexOperandsExp(e.then), removeComplexOperandsExp(e.else))
    case "let": {
      return Let(e.name, removeComplexOperandsExp(e.exp), removeComplexOperandsExp(e.body))
    }
    case "get":
      return Var(e.name)
    case "set":
      return SetBang(e.name, removeComplexOperandsExp(e.exp))
    case "begin":
      return Begin(e.exps.map(removeComplexOperandsExp), removeComplexOperandsExp(e.body))
    case "while":
      return While(removeComplexOperandsExp(e.cond), removeComplexOperandsExp(e.body))
  }
}
function removeComplexOperandsAtom(e: Exp): [Atom, Array<[string, Exp]>] {
  switch (e.kind) {
    case "var":
    case "int":
    case "bool":
    case "void":
      return [e, []]
    case "prim": {
      if (e.op === "read") {
        return generateTmp(e, [])
      }
      if (e.args.length === 1) {
        const [e1, tmps] = removeComplexOperandsAtom(e.args[0])
        return generateTmp(PrimAtom(e.op, e1), tmps)
      } else if (e.args.length === 2) {
        const [e1, tmps1] = removeComplexOperandsAtom(e.args[0])
        const [e2, tmps2] = removeComplexOperandsAtom(e.args[1])
        return generateTmp(PrimAtom(e.op, e1, e2), [...tmps2, ...tmps1])
      } else {
        throw new Error("Unexpected number of arguments")
      }
    }
    case "if":
      // chapter 4 notes that e.cond should *definitely* be an expression, so I'm going to leave all 3
      // as-is and see whether explicateTail will fix things up
      return generateTmp(
        If(removeComplexOperandsExp(e.cond), removeComplexOperandsExp(e.then), removeComplexOperandsExp(e.else)),
        []
      )
    case "let": {
      const [e1, tmpsE] = removeComplexOperandsAtom(e.exp)
      const [body1, tmpsBody] = removeComplexOperandsAtom(e.body)
      return generateTmp(Let(e.name, e1, generateLets(tmpsBody, body1)), tmpsE)
    }
    case "set":
      return generateTmp(SetBang(e.name, removeComplexOperandsExp(e.exp)), [])
    case "get":
      return generateTmp(Var(e.name), [])
    case "begin": {
      return generateTmp(Begin(e.exps.map(removeComplexOperandsExp), removeComplexOperandsExp(e.body)), [])
    }
    case "while": {
      const [cond1, tmpsCond] = removeComplexOperandsAtom(e.cond)
      const [body1, tmpsBody] = removeComplexOperandsAtom(e.body)
      return generateTmp(While(cond1, body1), [...tmpsCond, ...tmpsBody])
    }
  }
}
function generateTmp(e: Exp, tmps: Array<[string, Exp]>): [Atom, Array<[string, Exp]>] {
  const tmp = gensym()
  return [Var(tmp), [[tmp, e], ...tmps]]
}
function generateLets(tmps: Array<[string, Exp]>, body: Exp): Exp {
  return tmps.reduce((e, [name, exp]) => Let(name, exp, e), body)
}
