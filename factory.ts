import { AList, DirectedGraph } from "./structures.js"
/** for Language */
export type Var = { kind: "var"; name: string }
export type Prim = { kind: "prim"; op: string; args: Exp[] }
export type If = { kind: "if", cond: Exp, then: Exp, else: Exp }
export type Int = { kind: "int"; val: number }
export type Bool = { kind: "bool"; val: boolean }
export type Let = { kind: "let"; name: string; exp: Exp; body: Exp }
export type Exp = Prim | Var | Int | Bool | Let | If
export type Program = {
  kind: "program"
  info: AList<string, number>
  body: Exp
}

export function Program(info: AList<string, number>, body: Exp): Program {
  return { kind: "program", info, body }
}
export function Prim(op: string, ...args: Exp[]): Prim {
  return { kind: "prim", op, args }
}
export function Var(name: string): Var {
  return { kind: "var", name }
}
export function Int(val: number): Int {
  return { kind: "int", val }
}
export function Bool(val: boolean): Bool {
  return { kind: "bool", val }
}
export function Let(name: string, exp: Exp, body: Exp): Let {
  return { kind: "let", name, exp, body }
}
export function If(cond: Exp, then: Exp, else_: Exp): If {
  return { kind: "if", cond, then, else: else_ }
}

/** for C */
export type Stmt =
  | { kind: "assign"; var: Var; exp: Exp }
  | { kind: "seq"; statements: Stmt[] }
  | { kind: "return"; exp: Exp }
export type CProgram = {
  kind: "cprogram"
  locals: Map<string, number>
  body: Map<string, Stmt>
}
export function Assign(v: Var, exp: Exp): Stmt {
  return { kind: "assign", var: v, exp }
}
export function Seq(statements: Stmt[]): Stmt {
  return { kind: "seq", statements }
}
export function Return(exp: Exp): Stmt {
  return { kind: "return", exp }
}
export function CProgram(locals: Map<string, number>, body: Map<string, Stmt>): CProgram {
  return { kind: "cprogram", locals, body }
}

/** for ASM */
export type Reg = { kind: "reg"; reg: string }
export type Deref = { kind: "deref"; reg: string; offset: number }
export type Imm = { kind: "imm"; int: number }
export type Ref = Var | Imm | Reg | Deref
export type Ops = "addq" | "subq" | "negq" | "movq" | "pushq" | "popq"
export type Instr =
  | { kind: "instr"; op: Ops; args: Ref[] }
  | { kind: "callq"; label: string; int: number }
  | { kind: "ret" }
  | { kind: "jmp"; label: string }
export type X86Program = {
  info: Map<string, number>
  blocks: Map<string, Block>
}
export type Block = {
  kind: "block"
  info: {
    homes: Map<string, Reg | Deref>
    references: Array<Set<string>>
    conflicts: DirectedGraph<string>
  }
  instructions: Instr[]
}

export function Reg(reg: string): Reg {
  return { kind: "reg", reg }
}
export function Deref(reg: string, offset: number): Deref {
  return { kind: "deref", reg, offset }
}
export function Imm(int: number): Imm {
  return { kind: "imm", int }
}
export function Instr(op: Ops, ...args: Ref[]): Instr {
  return { kind: "instr", op, args }
}
export function Callq(label: string, int: number): Instr {
  return { kind: "callq", label, int }
}
export function Ret(): Instr {
  return { kind: "ret" }
}
export function Jmp(label: string): Instr {
  return { kind: "jmp", label }
}
export function X86Program(info: Map<string, number>, blocks: Map<string, Block>): X86Program {
  return { info, blocks }
}
export function Block(info: Block["info"], instructions: Instr[]): Block {
  return { kind: "block", info, instructions }
}