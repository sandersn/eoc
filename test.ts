import * as l from "./language.js"
import { uniquifyProgram, parseProgram, removeComplexOperands, reparsePrimitives } from "./language.js"
import * as c from "./c.js"
import { explicateControl, selectInstructions } from "./c.js"
import * as x from "./x86.js"
import { uncoverLive, buildInterference, emitPreludeConclusion, patchInstructions, allocateRegisters } from "./x86.js"
import { parse } from "node:path"
import { Program } from "./factory.js"
function test(name: string, actual: number, expected: number) {
  if (actual !== expected) {
    console.log(`Test '${name}' failed: expected ${expected}, actual ${actual}`)
  }
}
function runLvar(p: Program) {
  return l.interpProgram(p)
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
function runAssignHomes(program: Program, verbose = false) {
  const parsedProgram = l.typeCheckProgram(program)
  const p = explicateControl(removeComplexOperands(uniquifyProgram(parsedProgram)))
  if (verbose) console.log(c.emitProgram(p))
  return c.interpProgram(p)
  // const initial = uncoverLive(selectInstructions(p))
  // buildInterference(initial)
  // const xp = emitPreludeConclusion(patchInstructions(allocateRegisters(initial)))
  // console.log(x.emitProgram(xp))
  // console.log(xp.blocks.get("start")!.info.references)
  // console.log(xp.blocks.get("start")!.info.conflicts)
  // return x.interpProgram(xp)
}
function testLvar(name: string, sexp: string, verbose = false) {
  const program = reparsePrimitives(parseProgram(sexp))
  const expected = runLvar(program)
  console.log("\t", l.emitProgram(program), "-->", expected)
  test(name, runAssignHomes(program, verbose), expected)
}
test("test list basic", runLvar(parseProgram("(+ 1 2)")), 3)
test("test lint basic", runLvar(parseProgram("(+ (+ 3 4) 12))")), 19)
test("test lvar basic", runLvar(parseProgram("(let (y (+ 1 2)) y)")), 3)
test("test lvar basic", runLvar(parseProgram("(let (x 1) (let (y 2) (+ x y)))")), 3)
testLvar("let-in-let-exp", "(let (x (let (y (+ 1 1)) (+ y 1))) x)")
testLvar("exp-unary-negate", "(+ 42 (- 10))")
testLvar("exp-nested-+", "(+ 42 (+ (+ 3 4) 12))")
testLvar("exp-nested-let", "(+ (let (y (+ 3 4)) (+ y 5)) 12)")
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
testLvar("if-or", "(let (x #t) (let (y #f) (if (or y x) 3 4)))")
testLvar("if-and", "(let (x #t) (let (y #f) (if (and x y) 1 (let (x 12) (let (y 13) (+ x y))))))")
testLvar("if-and-or", "(let (x #t) (let (y #f) (if (and (or x x) y) 1 (let (z (or x y)) 14))))")
testLvar("andlet", "(let (x #t) (let (y #f) (if (and (let (x #f) (or x #t)) y) 1 (let (z (or x y)) 14))))")
testLvar("not", "(let (x #t) (let (y #f) (if (not x) 1 2)))")
testLvar(">=", "(let (x 1) (let (y 2) (if (>= x y) 1 0)))")
testLvar("<=", "(let (x 1) (let (y 2) (if (<= x y) 1 0)))")
testLvar("<", "(let (x 1) (let (y 2) (if (< x y) 1 0)))")
testLvar(">", "(let (x 1) (let (y 2) (if (> x y) 1 0)))")
testLvar("==", "(let (x 1) (if (== x 2) 1 0))")
// TODO: Function to test type checking failure
// testLvar("not-bad", "(let (x #t) (let (y #f) (if (not x) (not y) 2)))")
// testLvar("not-bad2", "(let (x #t) (let (y #f) (if (not x) (not y) (not 2))))")
