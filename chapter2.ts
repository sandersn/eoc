import assert from "node:assert"
type Program = { kind: "program"; info: AList<string, number>; body: Exp }
/** for Language */
type Var = { kind: "var"; name: string }
type Prim = { kind: "prim"; op: string; args: Exp[] }
type Exp = Prim | Var | { kind: "int"; val: number } | { kind: "let"; name: string; exp: Exp; body: Exp }
/** for C */
type Stmt = { kind: "assign"; var: Var; exp: Exp } | { kind: "seq"; statements: Stmt[] } | { kind: "return"; exp: Exp }
type CProgram = {
  kind: "cprogram"
  locals: Map<string, number>
  body: Map<string, Stmt>
}
/** for ASM */
type Ref =
  | Var
  | { kind: "imm"; int: number }
  | { kind: "reg"; reg: string }
  | { kind: "deref"; reg: string; offset: number }
type Instr =
  | { kind: "instr"; op: string; args: Ref[] }
  | { kind: "callq"; label: string; int: number }
  | { kind: "ret" }
  | { kind: "jmp"; label: string }
type X86Program = {
  info: Map<string, number>
  blocks: Map<string, Block>
}
type Block = {
  kind: "block"
  info: Map<string, number>
  instructions: Instr[]
}
/** Read a number from stdin
 * use readline something something can't be bothered
 */
function read() {
  return 0
}
let counter = 0
function gensym() {
  return "g" + counter++
}
function assertDefined<T>(e: T | undefined): T {
  assert(e !== undefined)
  return e
}
class AList<K, V> {
  constructor(public key: K, public value: V, public next?: AList<K, V>) {}
  get(key: K): V | undefined {
    if (this.key === key) return this.value
    else if (this.next) return this.next.get(key)
  }
  set(key: K, value: V): void {
    if (this.key === key) this.value = value
    else if (this.next) this.next.set(key, value)
    else throw new Error(`Key ${key} not found`)
  }
  toMap(m: Map<K, V>) {
    m.set(this.key, this.value)
    if (this.next) this.next.toMap(m)
    return m
  }
  static fromMap<K, V>(m: Map<K, V>): AList<K, V> {
    let alist: AList<K, V> | undefined = undefined
    for (const [k, v] of m) {
      alist = new AList(k, v, alist)
    }
    assert(alist)
    return alist
  }
}
enum Token {
  LParen,
  RParen,
  Let,
  Identifier,
  Number,
  Plus,
  Minus,
  EOF,
}
function lex(s: string) {
  let i = 0
  let value: string = ""
  return {
    pos: () => i,
    next: () => {
      while (s[i] === " ") i++
      switch (s[i]) {
        case "(":
          i++
          return Token.LParen
        case ")":
          i++
          return Token.RParen
        case "+":
          i++
          value = "+"
          return Token.Plus
        case "-":
          i++
          value = "-"
          return Token.Minus
        case "l":
          if (i + 2 < s.length && s[i + 1] === "e" && s[i + 2] === "t") {
            i += 3
            return Token.Let
          }
        default:
          if (s[i] >= "0" && s[i] <= "9") {
            let j = i
            while (s[i] >= "0" && s[i] <= "9") i++
            value = s.slice(j, i)
            return Token.Number
          }
          if (s[i] >= "a" && s[i] <= "z") {
            let j = i
            while (s[i] >= "a" && s[i] <= "z") i++
            value = s.slice(j, i)
            return Token.Identifier
          }
      }
      return Token.EOF
    },
    value: () => value,
  }
}
class LInt {
  interpExp(e: Exp, env: AList<string, number>): number {
    switch (e.kind) {
      case "int":
        return e.val
      case "prim":
        if (e.op === "read") return +read()
        if (e.op === "+") return this.interpExp(e.args[0], env) + this.interpExp(e.args[1], env)
        if (e.op === "-" && e.args.length === 1) return -this.interpExp(e.args[0], env)
        if (e.op === "-" && e.args.length === 2) return this.interpExp(e.args[0], env) - this.interpExp(e.args[1], env)
      default:
        return NaN
    }
  }
  interpProgram(p: Program): number {
    return this.interpExp(p.body, new AList("!!!!!!", NaN, undefined))
  }
  parseProgram(sexp: string): Program {
    const lexer = lex(sexp)
    function parseExp(t: Token): Exp {
      switch (t) {
        case Token.Number:
          return { kind: "int", val: +lexer.value() }
        case Token.LParen: {
          const head = lexer.next()
          if (head === Token.Let) {
            return parseLet()
          } else {
            const op = lexer.value()
            const args = []
            t = lexer.next()
            while (t !== Token.RParen) {
              args.push(parseExp(t))
              t = lexer.next()
            }
            return { kind: "prim", op, args }
          }
        }
        case Token.Identifier:
          return { kind: "var", name: lexer.value() }
        default:
          throw new Error(`Unexpected token at ${lexer.pos()}: ${Token[t]} [${sexp.slice(lexer.pos())}]`)
      }
    }
    function parseLet(): Exp {
      assert(lexer.next() === Token.LParen)
      assert(lexer.next() === Token.Identifier)
      const name = lexer.value()
      const exp = parseExp(lexer.next())
      assert(lexer.next() === Token.RParen)
      const body = parseExp(lexer.next())
      assert(lexer.next() === Token.RParen)
      return { kind: "let", name, exp, body }
    }
    return {
      kind: "program",
      info: new AList("!!!!!!!!!", NaN, undefined),
      body: parseExp(lexer.next()),
    }
  }
  emitProgram(p: Program): string {
    return this.emitExp(p.body)
  }
  emitExp(e: Exp): string {
    switch (e.kind) {
      case "int":
        return `${e.val}`
      case "prim":
        return `(${e.op} ${e.args.map(a => this.emitExp(a)).join(" ")})`
      case "var":
        return e.name
      case "let":
        return `(let (${e.name} ${this.emitExp(e.exp)}) ${this.emitExp(e.body)})`
    }
  }
}
class LVar extends LInt {
  removeComplexOperands(p: Program): Program {
    return { ...p, body: this.removeComplexOperandsExp(p.body) }
  }
  removeComplexOperandsExp(e: Exp): Exp {
    switch (e.kind) {
      case "var":
      case "int":
        return e
      case "prim": {
        if (e.op === "read") return e
        if (e.args.length === 1) {
          const [tmp, tmps] = this.removeComplexOperandsAtom(e.args[0], false)
          return generateTmpLets({ ...e, args: [tmp] }, tmps)
        } else if (e.args.length === 2) {
          const [tmp1, tmps1] = this.removeComplexOperandsAtom(e.args[0], false)
          const [tmp2, tmps2] = this.removeComplexOperandsAtom(e.args[1], false)
          return generateTmpLets({ ...e, args: [tmp1, tmp2] }, [...tmps1, ...tmps2])
        } else {
          throw new Error("Unexpected number of arguments")
        }
      }
      case "let": {
        const [tmpE, tmpsE] = this.removeComplexOperandsAtom(e.exp, false)
        const [tmpBody, tmpsBody] = this.removeComplexOperandsAtom(e.body, true)
        return generateTmpLets({ ...e, exp: tmpE, body: generateTmpLets(tmpBody, tmpsBody) }, tmpsE)
      }
    }
  }
  removeComplexOperandsAtom(e: Exp, isTail: boolean): [Exp, Array<[string, Exp]>] {
    switch (e.kind) {
      case "var":
      case "int":
        return [e, []]
      case "prim": {
        const tmp = gensym()
        if (e.op === "read") {
          return [{ kind: "var", name: tmp }, [[tmp, e]]]
        }
        if (e.args.length === 1) {
          const [e1, tmps] = this.removeComplexOperandsAtom(e.args[0], false)
          return [{ kind: "var", name: tmp }, [[tmp, { ...e, args: [e1] }], ...tmps]]
        } else if (e.args.length === 2) {
          const [e1, tmps1] = this.removeComplexOperandsAtom(e.args[0], false)
          const [e2, tmps2] = this.removeComplexOperandsAtom(e.args[1], false)
          const tmps = [...tmps1, ...tmps2]
          return [{ kind: "var", name: tmp }, [[tmp, { ...e, args: [e1, e2] }], ...tmps]]
        } else {
          throw new Error("Unexpected number of arguments")
        }
      }
      case "let": {
        const [e1, tmpsE] = this.removeComplexOperandsAtom(e.exp, false)
        const [body1, tmpsBody] = this.removeComplexOperandsAtom(e.body, isTail)
        if ((e1.kind === "var" || e1.kind === "int") && isTail) {
          return [
            {
              kind: "let",
              name: e.name,
              exp: e1,
              body: generateTmpLets(body1, tmpsBody),
            },
            [],
          ]
        }
        const tmp = gensym()
        return [
          { kind: "var", name: tmp },
          [[tmp, { ...e, exp: e1, body: generateTmpLets(body1, tmpsBody) }], ...tmpsE],
        ]
      }
    }
  }

  override interpExp(e: Exp, env: AList<string, number>): number {
    switch (e.kind) {
      case "var": {
        return assertDefined(env.get(e.name))
      }
      case "let":
        return this.interpExp(e.body, new AList(e.name, this.interpExp(e.exp, env), env))
      default:
        return super.interpExp(e, env)
    }
  }
  uniquifyProgram(p: Program): Program {
    return {
      ...p,
      body: this.uniquifyExp(p.body, new AList("!!!!!!", "@@@@@@@@@", undefined)),
    }
  }
  uniquifyExp(e: Exp, env: AList<string, string>): Exp {
    switch (e.kind) {
      case "var":
        return { ...e, name: assertDefined(env.get(e.name)) }
      case "int":
        return e
      case "prim":
        return { ...e, args: e.args.map(a => this.uniquifyExp(a, env)) }
      case "let": {
        let x = env.get(e.name) ? gensym() : e.name
        env = new AList(e.name, x, env)
        return {
          kind: "let",
          name: x,
          exp: this.uniquifyExp(e.exp, env),
          body: this.uniquifyExp(e.body, env),
        }
      }
    }
  }
  explicateAssign(e: Exp, x: string, k: Stmt[]): Stmt[] {
    switch (e.kind) {
      case "var":
      case "int":
      case "prim":
        return [{ kind: "assign", var: { kind: "var", name: x }, exp: e }, ...k]
      case "let": {
        return this.explicateAssign(e.exp, e.name, this.explicateAssign(e.body, x, k))
      }
    }
  }
  explicateTail(e: Exp): Stmt {
    switch (e.kind) {
      case "var":
      case "int":
      case "prim":
        return { kind: "return", exp: e }
      case "let": {
        const tail = this.explicateTail(e.body)
        switch (tail.kind) {
          case "seq":
            return {
              kind: "seq",
              statements: this.explicateAssign(e.exp, e.name, tail.statements),
            }
          case "return":
            return {
              kind: "seq",
              statements: this.explicateAssign(e.exp, e.name, [tail]),
            }
          case "assign":
            throw new Error("Unexpected assign")
        }
      }
    }
  }
  explicateControl(p: Program): CProgram {
    const c = new CVar(this)
    const start = this.explicateTail(p.body)
    return {
      kind: "cprogram",
      locals: c.typeCheck(start), // for now; the only block is :start; after that, storing ALL locals on the program doesn't make much sense
      body: new Map([["start", start]]),
    }
  }
}
class CVar {
  /** Doesn't actually type check currently, but binds name to index */
  typeCheck(s: Stmt): Map<string, number> {
    const env: Map<string, number> = new Map()
    let i = 1
    function worker(s: Stmt): void {
      switch (s.kind) {
        case "assign":
          env.set(s.var.name, i)
          i++
          break
        case "return":
          break
        case "seq":
          for (const statement of s.statements) worker(statement)
          break
      }
    }
    worker(s)
    return env
  }
  selectInstructions(p: CProgram): X86Program {
    const start = assertDefined(p.body.get("start"))
    return {
      info: new Map(),
      blocks: new Map([
        [
          "start",
          {
            kind: "block",
            info: p.locals,
            instructions: this.selectInstructionsStmt(assertDefined(start)),
          },
        ],
      ]),
    }
  }
  selectInstructionsExp(e: Exp, to: Ref): Instr[] {
    switch (e.kind) {
      case "prim":
        return this.selectInstructionsPrim(e, to)
      case "int":
        return [
          {
            kind: "instr",
            op: "movq",
            args: [{ kind: "imm", int: e.val }, to],
          },
        ]
      case "var":
        return [{ kind: "instr", op: "movq", args: [e, to] }]
      case "let":
        throw new Error("Unexpected let on rhs of assignment.")
    }
  }
  selectInstructionsPrim(e: Prim, to: Ref): Instr[] {
    switch (e.op) {
      case "read":
        return [
          { kind: "callq", label: "read_int", int: 0 },
          {
            kind: "instr",
            op: "movq",
            args: [{ kind: "reg", reg: "rax" }, to],
          },
        ]
      case "+":
        return [
          {
            kind: "instr",
            op: "movq",
            args: [this.selectInstructionsAtom(e.args[0]), to],
          },
          {
            kind: "instr",
            op: "addq",
            args: [this.selectInstructionsAtom(e.args[1]), to],
          },
        ]
      case "-":
        if (e.args.length === 1) {
          return [
            {
              kind: "instr",
              op: "movq",
              args: [this.selectInstructionsAtom(e.args[0]), to],
            },
            {
              kind: "instr",
              op: "negq",
              args: [to],
            },
          ]
        } else {
          return [
            {
              kind: "instr",
              op: "movq",
              args: [this.selectInstructionsAtom(e.args[0]), to],
            },
            {
              kind: "instr",
              op: "subq",
              args: [this.selectInstructionsAtom(e.args[1]), to],
            },
          ]
        }
      default:
        throw new Error("Unexpected primitive")
    }
  }
  selectInstructionsStmt(s: Stmt): Instr[] {
    switch (s.kind) {
      case "assign":
        return this.selectInstructionsExp(s.exp, s.var)
      case "return":
        return this.selectInstructionsExp(s.exp, { kind: "reg", reg: "rax" })
      case "seq":
        return s.statements.flatMap(s => this.selectInstructionsStmt(s))
    }
  }
  selectInstructionsAtom(e: Exp): Ref {
    switch (e.kind) {
      case "int":
        return { kind: "imm", int: e.val }
      case "var":
        return e
      case "prim":
      case "let":
        throw new Error("Unexpected non-atomic expression in selectInstructionsAtom")
    }
  }
  constructor(public readonly lvar: LVar) {}
  interpStatement(e: Stmt, env: Map<string, number>): number {
    switch (e.kind) {
      case "assign": {
        const v = this.lvar.interpExp(e.exp, AList.fromMap(env))
        env.set(e.var.name, v)
        return v
      }
      case "seq":
        return e.statements.reduce((_, s) => this.interpStatement(s, env), 0)
      case "return":
        return this.lvar.interpExp(e.exp, AList.fromMap(env))
    }
  }
  interpProgram(p: CProgram): number {
    return this.interpStatement(assertDefined(p.body.get("start")), p.locals)
  }
  emitStatement(e: Stmt): string {
    switch (e.kind) {
      case "assign":
        return `${e.var.name} = ${this.lvar.emitExp(e.exp)};\n`
      case "return":
        return `return ${this.lvar.emitExp(e.exp)};\n`
      case "seq":
        return e.statements.map(s => this.emitStatement(s)).join("")
    }
  }
  emitProgram(p: CProgram): string {
    return this.emitStatement(assertDefined(p.body.get("start")))
  }
}
class X86Var {
  // TODO: Initialise this to something (or make accesses default to 0)
  stack: number[] = []
  registers: Map<string, number> = new Map()
  block: Block | undefined
  blocks: Map<string, Block> = new Map()
  emitPreludeConclusion(p: X86Program): X86Program {
    p.blocks.set("main", {
      kind: "block",
      info: new Map(),
      instructions: [
        { kind: "instr", op: "pushq", args: [{ kind: "reg", reg: "rbp" }] },
        {
          kind: "instr",
          op: "movq",
          args: [
            { kind: "reg", reg: "rsp" },
            { kind: "reg", reg: "rbp" },
          ],
        },
        {
          kind: "instr",
          op: "subq",
          args: [
            { kind: "imm", int: 16 },
            { kind: "reg", reg: "rsp" },
          ],
        },
        { kind: "jmp", label: "start" },
      ],
    })
    p.blocks.set("conclusion", {
      kind: "block",
      info: new Map(),
      instructions: [
        {
          kind: "instr",
          op: "addq",
          args: [
            { kind: "imm", int: 16 },
            { kind: "reg", reg: "rsp" },
          ],
        },
        { kind: "instr", op: "pushq", args: [{ kind: "reg", reg: "rbp" }] },
        { kind: "ret" },
      ],
    })
    return p
  }
  patchInstructions(p: X86Program): X86Program {
    const start = assertDefined(p.blocks.get("start"))
    return {
      info: p.info,
      blocks: p.blocks.set("start", {
        ...start,
        instructions: start.instructions.flatMap(i => this.patchInstructionsInstr(i)),
      }),
    }
  }
  patchInstructionsInstr(i: Instr): Instr[] {
    switch (i.kind) {
      case "instr":
        if (i.args.length === 2 && i.args[0].kind === "deref" && i.args[1].kind === "deref") {
          return [
            {
              kind: "instr",
              op: "movq",
              args: [i.args[0], { kind: "reg", reg: "rax" }],
            },
            {
              kind: "instr",
              op: i.op,
              args: [{ kind: "reg", reg: "rax" }, i.args[1]],
            },
          ]
        }
      // fall through
      case "callq":
      case "ret":
      case "jmp":
        return [i]
    }
  }
  assignHomes(p: X86Program): X86Program {
    const start = assertDefined(p.blocks.get("start"))
    return {
      info: p.info,
      blocks: p.blocks.set("start", this.assignHomesBlock(start)),
    }
  }
  assignHomesBlock(block: Block): Block {
    return {
      ...block,
      instructions: block.instructions.map(i => this.assignHomesInstr(block.info, i)),
    }
  }
  assignHomesInstr(info: Map<string, number>, i: Instr): Instr {
    switch (i.kind) {
      case "instr":
        return { ...i, args: i.args.map(a => this.assignHomesRef(info, a)) }
      case "callq":
      case "ret":
      case "jmp":
        return i
    }
  }
  assignHomesRef(info: Map<string, number>, a: Ref): Ref {
    switch (a.kind) {
      case "imm":
      case "reg":
      case "deref":
        return a
      case "var":
        return {
          kind: "deref",
          reg: "rbp",
          offset: -8 * assertDefined(info.get(a.name)),
        }
    }
  }
  emitProgram(xp: X86Program): string {
    return Array.from(xp.blocks.entries())
      .map(([name, b]) => this.emitBlock(name, b))
      .join("\n")
  }
  emitBlock(name: string, block: Block) {
    return `\t${name}:\n${block.instructions.map(i => this.emitInstr(i)).join("\n")}`
  }
  emitInstr(e: Instr): string {
    switch (e.kind) {
      case "instr":
        return `${e.op} ${e.args.map(a => this.emitRef(a)).join(", ")}`
      case "callq":
        return `callq ${e.label}`
      case "ret":
        return "ret"
      case "jmp":
        return `jmp ${e.label}`
    }
  }
  emitRef(r: Ref): string {
    switch (r.kind) {
      case "imm":
        return `$${r.int}`
      case "var":
        return r.name
      case "reg":
        return `%${r.reg}`
      case "deref":
        return `${r.offset}(%${r.reg})`
    }
  }
  interpProgram(xp: X86Program) {
    this.blocks = xp.blocks
    this.block = xp.blocks.get("main")
    while (this.block) {
      const result = this.interpBlock(this.block)
      if (result !== "jmp") {
        return
      }
    }
  }
  interpBlock(e: Block): "jmp" | undefined {
    for (const i of e.instructions) {
      const result = this.interpInstr(i, e.info)
      if (result === 'jmp') {
        return result
      }
      else if (result === 'ret') {
        return
      }
    }
  }
  interpInstr(e: Instr, env: Map<string, number>): "ret" | "jmp" | undefined {
    switch (e.kind) {
      case "instr": {
        switch (e.op) {
          case "movq":
            return this.write(e.args[1], this.interpRef(e.args[0], env))
          case "addq":
            return this.write(e.args[1], this.interpRef(e.args[1], env) + this.interpRef(e.args[0], env))
          case "subq":
            return this.write(e.args[1], this.interpRef(e.args[1], env) - this.interpRef(e.args[0], env))
          case "negq":
            return this.write(e.args[0], -this.interpRef(e.args[0], env))
        }
      }
      case "callq":
        this.registers.set("rax", read())
        break
      case "ret":
        return "ret"
      case "jmp":
        this.block = this.blocks.get(e.label)
        return "jmp"
    }
  }
  interpRef(r: Ref, env: Map<string, number>): number {
    switch (r.kind) {
      case "imm":
        return r.int
      case "var":
        return this.registers.get(r.name) ?? 0
      case "reg":
        return this.registers.get(r.reg) ?? 0
      case "deref":
        return assertDefined(this.stack[(this.registers.get(r.reg) ?? 0) + r.offset])
    }
  }
  write(r: Ref, source: number): undefined {
    switch (r.kind) {
      case "var":
        this.registers.set(r.name, source)
        break
      case "reg":
        this.registers.set(r.reg, source)
        break
      case "deref":
        this.stack[(this.registers.get(r.reg) ?? 0) + r.offset] = source
        break
      case "imm":
        throw new Error("not a ref")
    }
  }
}
function generateTmpLets(init: Exp, tmps: Array<[string, Exp]>): Exp {
  return tmps.reduce((e, [name, exp]) => ({ kind: "let", name, exp, body: e }), init)
}
function test(name: string, actual: number, expected: number) {
  if (actual !== expected) {
    console.log(`Test ${name} failed: expected ${expected}, actual ${actual}`)
  }
}
function runLint(sexp: string) {
  const l = new LInt()
  return l.interpProgram(l.parseProgram(sexp))
}
function runLvar(sexp: string) {
  const l = new LVar()
  return l.interpProgram(l.parseProgram(sexp))
}
function runUniquifyLvar(sexp: string) {
  const l = new LVar()
  const p = l.uniquifyProgram(l.parseProgram(sexp))
  console.log(l.emitProgram(p))
  return l.interpProgram(p)
}
function runRemoveComplexLvar(sexp: string) {
  const l = new LVar()
  const p = l.removeComplexOperands(l.uniquifyProgram(l.parseProgram(sexp)))
  console.log(l.emitProgram(p))
  return l.interpProgram(p)
}
function runExplicateControl(sexp: string) {
  const l = new LVar()
  const c = new CVar(l)
  const p = l.explicateControl(l.removeComplexOperands(l.uniquifyProgram(l.parseProgram(sexp))))
  console.log(c.emitProgram(p))
  return c.interpProgram(p)
}
function runAssignHomes(sexp: string) {
  const l = new LVar()
  const c = new CVar(l)
  const x = new X86Var()
  const p = l.explicateControl(l.removeComplexOperands(l.uniquifyProgram(l.parseProgram(sexp))))
  // console.log(c.emitProgram(p));
  const xp = x.emitPreludeConclusion(x.patchInstructions(x.assignHomes(c.selectInstructions(p))))
  console.log(x.emitProgram(xp))
  x.interpProgram(xp)
  return x.registers.get("rax")!
}
function testLvar(name: string, sexp: string) {
  const expected = runLvar(sexp)
  console.log("\t", sexp, "-->", expected)
  test(name, runAssignHomes(sexp), expected)
}
test("test list basic", runLint("(+ 1 2)"), 3)
test("test lint basic", runLint("(+ (+ 3 4) 12))"), 19)
test("test lvar basic", runLvar("(let (y (+ 1 2)) y)"), 3)
test("test lvar basic", runLvar("(let (x 1) (let (y 2) (+ x y)))"), 3)
testLvar("test lvar remove complex operands", "(+ 42 (- 10))")
testLvar("t", "(+ 42 (+ (+ 3 4) 12))")
testLvar("t", "(+ (let (y (+ 3 4)) (+ y 5)) 12)")
testLvar("t", "(let (a 42) (let (b a) b))")
testLvar("t", "(let (y (+ 1 2)) y)")
testLvar("t", "(let (x 1) (let (y 2) (+ x y)))")
testLvar("t", "(let (x 1) (let (x 2) (+ x x)))")
testLvar("t", "(let (x 1) (+ (let (x 2) x) x))")
testLvar("t", "(let (x 1) (+ (let (x 2) x) (let (x 3) x)))")
testLvar("t", "(let (y 1) (+ (let (x 2) x) (let (x 3) (+ x y))))")
