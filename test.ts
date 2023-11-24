import * as l from "./language.js"
import { uniquifyProgram, parseProgram, removeComplexOperands, reparsePrimitives } from "./language.js"
import * as c from "./c.js"
import { explicateControl, selectInstructions } from "./c.js"
import * as x from "./x86.js"
import { uncoverLive, buildInterference, emitPreludeConclusion, patchInstructions, allocateRegisters } from "./x86.js"
import { parse } from "node:path"
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
    const parsedProgram = l.typeCheckProgram(reparsePrimitives(parseProgram(sexp)))
    const p = explicateControl(removeComplexOperands(uniquifyProgram(parsedProgram)))
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
testLvar("boolean", "(let (x #t) (let (y #f) 1))")
testLvar("if-boolean", "(let (x #t) (let (y #f) (if x 4 3)))")
testLvar("if", "(let (x #t) (let (y #f) (if x 1 2)))")
testLvar("or", "(let (x #t) (let (y #f) (if (or y x) 3 4)))")
testLvar("and", "(let (x #t) (let (y #f) (if (and x y) 1 2)))")
testLvar("not", "(let (x #t) (let (y #f) (if (not x) 1 2)))")
testLvar("not-bad", "(let (x #t) (let (y #f) (if (not x) (not y) 2)))")
testLvar("not-bad2", "(let (x #t) (let (y #f) (if (not x) (not y) (not 2))))")
testLvar(">=", "(let (x 1) (let (y 2) (if (>= x y) 1 0)))")
testLvar("<=", "(let (x 1) (let (y 2) (if (<= x y) 1 0)))")
testLvar("<", "(let (x 1) (let (y 2) (if (< x y) 1 0)))")
testLvar(">", "(let (x 1) (let (y 2) (if (> x y) 1 0)))")
testLvar("==", "(let (x 1) (if (== x 2) 1 0))")
