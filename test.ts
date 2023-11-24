import * as l from "./language.js"
import { uniquifyProgram, parseProgram, removeComplexOperands, reparsePrimitives } from "./language.js"
import * as c from "./c.js"
import { explicateControl, selectInstructions } from "./c.js"
import * as x from "./x86.js"
import { uncoverLive, buildInterference, emitPreludeConclusion, patchInstructions, allocateRegisters } from "./x86.js"
function test(name: string, actual: number, expected: number) {
  if (actual !== expected) {
    console.log(`Test ${name} failed: expected ${expected}, actual ${actual}`)
  }
}
function runLvar(sexp: string) {
  return l.interpProgram(reparsePrimitives(parseProgram(sexp)))
}
function runUniquifyLvar(sexp: string) {
  const p = uniquifyProgram(parseProgram(sexp))
  console.log(l.emitProgram(p))
  return l.interpProgram(p)
}
function runRemoveComplexLvar(sexp: string) {
  const p = removeComplexOperands(uniquifyProgram(parseProgram(sexp)))
  console.log(l.emitProgram(p))
  return l.interpProgram(p)
}
function runExplicateControl(sexp: string) {
  const p = explicateControl(removeComplexOperands(uniquifyProgram(parseProgram(sexp))))
  console.log(c.emitProgram(p))
  return c.interpProgram(p)
}
function runAssignHomes(sexp: string) {
  const p = explicateControl(removeComplexOperands(uniquifyProgram(reparsePrimitives(parseProgram(sexp)))))
  console.log(c.emitProgram(p));
  return c.interpProgram(p)
  // const initial = uncoverLive(selectInstructions(p))
  // buildInterference(initial)
  // const xp = emitPreludeConclusion(patchInstructions(allocateRegisters(initial)))
  // console.log(x.emitProgram(xp))
  // console.log(xp.blocks.get("start")!.info.references)
  // console.log(xp.blocks.get("start")!.info.conflicts)
  // return x.interpProgram(xp)
}
function testLvar(name: string, sexp: string) {
  const expected = runLvar(sexp)
  console.log("\t", sexp, "-->", expected)
  test(name, runAssignHomes(sexp), expected)
}
test("test list basic", runLvar("(+ 1 2)"), 3)
test("test lint basic", runLvar("(+ (+ 3 4) 12))"), 19)
test("test lvar basic", runLvar("(let (y (+ 1 2)) y)"), 3)
test("test lvar basic", runLvar("(let (x 1) (let (y 2) (+ x y)))"), 3)
testLvar("test lvar remove complex operands", "(+ 42 (- 10))")
testLvar("t", "(+ 42 (+ (+ 3 4) 12))")
testLvar("t", "(+ (let (y (+ 3 4)) (+ y 5)) 12)")
testLvar("t", "(let (a 42) (let (b a) b))")
testLvar("t", "(let (y (+ 1 2)) y)")
testLvar("t1", "(let (x 1) (let (y 2) (+ x y)))")
testLvar("t2", "(let (x 1) (let (x 2) (+ x x)))")
testLvar("t", "(let (x 1) (+ (let (x 2) x) x))")
testLvar("t", "(let (x 1) (+ (let (x 2) x) (let (x 3) x)))")
testLvar("t", "(let (y 1) (+ (let (x 2) x) (let (x 3) (+ x y))))")
testLvar("t", "(let (v 1) (let (w 42) (let (x (+ v 7)) (let (y x) (let (z (+ x w)) (+ z (- y)))))))")
testLvar("boolean", "(let (x #t) (let (y #f) x))")
testLvar("boolean", "(let (x #t) (let (y #f) (if x y x)))")
testLvar("boolean", "(let (x #t) (let (y #f) (if x 1 2)))")
