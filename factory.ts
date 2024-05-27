import { AList, Graph } from "./structures.js"
/** for Language */
export type Var = { kind: "var"; name: string }
export type Prim = { kind: "prim"; op: string; args: Exp[] }
export type Cmp = { kind: "prim"; op: "==" | ">" | "<" | ">=" | "<="; args: Exp[] }
export type If = { kind: "if"; cond: Exp; then: Exp; else: Exp }
export type Int = { kind: "int"; val: number }
export type Bool = { kind: "bool"; val: boolean }
export type Let = { kind: "let"; name: string; exp: Exp; body: Exp }
export type SetBang = { kind: "set"; name: string; exp: Exp }
export type GetBang = { kind: "get"; name: string; }
export type Begin = { kind: "begin"; exps: Exp[]; body: Exp }
export type While = { kind: "while"; cond: Exp; body: Exp }
export type Void = { kind: "void" }
export type Exp = Prim | Var | Int | Bool | Let | If | SetBang | GetBang | Begin | While | Void
export type Program = {
  kind: "program"
  info: AList<string, number>
  body: Exp
}

export function Program(info: AList<string, number>, body: Exp): Program {
  return { kind: "program", info, body }
}
export function Prim(op: "==" | ">" | "<" | ">=" | "<=", ...args: Exp[]): Cmp
export function Prim(op: string, ...args: Exp[]): Prim
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
export function SetBang(name: string, exp: Exp): SetBang {
  return { kind: "set", name, exp }
}
export function GetBang(name: string): GetBang {
  return { kind: "get", name }
}
export function Begin(exps: Exp[], body: Exp): Begin {
  return { kind: "begin", exps, body }
}
export function While(cond: Exp, body: Exp): While {
  return { kind: "while", cond, body }
}
export function Void(): Void {
  return { kind: "void" }
}
export function If(cond: Exp, then: Exp, else_: Exp): If {
  return { kind: "if", cond, then, else: else_ }
}

/** for C */
export type Goto = { kind: "goto"; label: string }
export type Stmt =
  | { kind: "assign"; var: Var; exp: Exp }
  | { kind: "seq"; head: Stmt; tail: Stmt }
  | { kind: "return"; exp: Exp }
  | Goto
  | { kind: "if"; cond: Cmp; then: Goto; else: Goto }
export type CProgram = {
  kind: "cprogram"
  locals: Map<string, number>
  body: Map<string, Stmt>
}
export function Assign(v: Var, exp: Exp): Stmt {
  return { kind: "assign", var: v, exp }
}
export function Seq(head: Stmt, tail: Stmt): Stmt {
  return { kind: "seq", head, tail }
}
export function Return(exp: Exp): Stmt {
  return { kind: "return", exp }
}
export function Goto(label: string): Goto {
  return { kind: "goto", label }
}
export function IfStmt(cond: Cmp, then: Goto, else_: Goto): Stmt {
  return { kind: "if", cond, then, else: else_ }
}
export function CProgram(locals: Map<string, number>, body: Map<string, Stmt>): CProgram {
  return { kind: "cprogram", locals, body }
}

/** for ASM */
export type Reg = { kind: "reg"; reg: string }
// TODO: Maybe name it reg instead of bytereg since reg is elsewhere an unconstrained string
export type ByteReg = { kind: "bytereg"; bytereg: "ah" | "al" | "bh" | "bl" | "ch" | "cl" | "dh" | "dl" }
export type Deref = { kind: "deref"; reg: string; offset: number }
export type Imm = { kind: "imm"; int: number }
export type Ref = Var | Imm | Reg | ByteReg | Deref
export type Ops = "addq" | "subq" | "negq" | "xorq" | "cmpq" | "movq" | "movzbq" | "pushq" | "popq"
export type Cc = "e" | "l" | "le" | "g" | "ge"
export type Instr =
  | { kind: "instr"; op: Ops; args: Ref[] }
  | { kind: "instr"; op: "set"; args: [Cc, Ref] }
  | { kind: "callq"; label: string; int: number }
  | { kind: "ret" }
  | { kind: "jmp"; label: string }
  | { kind: "jmpif"; cc: Cc; label: string }
export type X86Program = {
  info: {
    homes: Map<string, Reg | Deref>
    conflicts: Graph<string>
  }
  blocks: Map<string, Block>
}
export type Block = {
  kind: "block"
  info: {
    references: Array<Set<string>>
  }
  instructions: Instr[]
}

export function Reg(reg: string): Reg {
  return { kind: "reg", reg }
}
export function ByteReg(bytereg: "ah" | "al" | "bh" | "bl" | "ch" | "cl" | "dh" | "dl"): ByteReg {
  return { kind: "bytereg", bytereg }
}
export function Deref(reg: string, offset: number): Deref {
  return { kind: "deref", reg, offset }
}
export function Imm(int: number): Imm {
  return { kind: "imm", int }
}
export function equalRef(r1: Ref, r2: Ref): boolean {
  if (r1.kind !== r2.kind) return false
  switch (r1.kind) {
    case "imm":
      return r1.int === (r2 as Imm).int
    case "reg":
      return r1.reg === (r2 as Reg).reg
    case "bytereg":
      return r1.bytereg === (r2 as ByteReg).bytereg
    case "var":
      return r1.name === (r2 as Var).name
    case "deref":
      return r1.reg === (r2 as Deref).reg && r1.offset === (r2 as Deref).offset
  }
}
export function Instr(op: "set", cc: Cc, arg: Ref): Instr
export function Instr(op: Ops, ...args: Ref[]): Instr
export function Instr(op: Ops | "set", ...args: Ref[] | [Cc, Ref]): Instr {
  return { kind: "instr", op, args } as Instr
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
export function JmpIf(cc: Cc, label: string): Instr {
  return { kind: "jmpif", cc, label }
}
export function X86Program(
  info: {
    homes: Map<string, Reg | Deref>
    conflicts: Graph<string>
  },
  blocks: Map<string, Block>
): X86Program {
  return { info, blocks }
}
export function Block(info: Block["info"], instructions: Instr[]): Block {
  return { kind: "block", info, instructions }
}
