import { LInt, LVar, CVar, X86Var } from "./chapter2.js"
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
  const initial = x.uncoverLive(c.selectInstructions(p))
  x.buildInterference(initial)
  console.log(x.emitProgram(initial))
  const xp = x.emitPreludeConclusion(x.patchInstructions(x.allocateRegisters(initial)))
  console.log(x.emitProgram(xp))
  // console.log(xp.blocks.get("start")!.info.references)
  // console.log(xp.blocks.get("start")!.info.conflicts)
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
testLvar("t", "(let (v 1) (let (w 42) (let (x (+ v 7)) (let (y x) (let (z (+ x w)) (+ z (- y)))))))")
testLvar("boolean", "(let (x #t) (let (y #f) (if x y x)))")
testLvar("boolean", "(let (x #t) (let (y #f) (if x 1 2)))")