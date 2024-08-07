import * as l from "./language.js"
import { createProgram } from "./fuzz.js"
import { uncoverGet, uniquifyProgram, parseProgram, removeComplexOperands, reparsePrimitives, typeCheckProgram, exposeAllocation } from "./language.js"
import * as c from "./c.js"
import { explicateControl, selectInstructions } from "./c.js"
import * as x from "./x86.js"
import { uncoverLive, buildInterference, emitPreludeConclusion, patchInstructions, allocateRegisters } from "./x86.js"
import { Program } from "./factory.js"
function test(name: string, actual: number, expected: number) {
  if (actual !== expected) {
    console.log(`Test '${name}' failed: expected ${expected}, actual ${actual}`)
  }
}
function runLvar(p: Program) {
  return l.interpProgram(p)
}
type Stage = "l" | "c" | "x"
function runAssignHomes(program: Program, stage: Stage, verbose = false) {
  const parsedProgram = typeCheckProgram(program)
  if (stage === "l") {
    if (verbose) console.log(l.emitProgram(parsedProgram))
    return l.interpProgram(parsedProgram)
  }
  if (verbose) {
    console.log(l.emitProgram(exposeAllocation(uniquifyProgram(parsedProgram))))
  }
  const p = explicateControl(removeComplexOperands(uncoverGet(exposeAllocation(uniquifyProgram(parsedProgram)))))
  if (stage === "c") {
    if (verbose) console.log(c.emitProgram(p))
    return c.interpProgram(p)
  }
  let xp = selectInstructions(p)
  uncoverLive(xp)
  buildInterference(xp)
  allocateRegisters(xp)
  patchInstructions(xp)
  emitPreludeConclusion(xp)
  if (verbose) console.log(x.emitProgram(xp))
  return x.interpProgram(xp)
}
function testLvar(name: string, sexp: string, stage: Stage = "x", verbose = false) {
  const program = reparsePrimitives(parseProgram(sexp))
  const expected = runLvar(program)
  console.log("\t", l.emitProgram(program), "-->", expected)
  test(name, runAssignHomes(program, stage, verbose), expected)
}
function fuzz(i: number) {
  const program = reparsePrimitives(createProgram())
  // TODO: Put an execution step limit on this, and skip the test if it's exceeded
  const expected = runLvar(program)
  console.log("\t", l.emitProgram(program), "-->", expected)
  // TODO: On failure, re-run with verbose printout for all stages
  test("fuzz" + i, runAssignHomes(program, "x", false), expected)
}
testLvar(
  "while-example",
  `(let (sum 0)
    (let (i 5)
      (begin
        (while (> i 0)
          (begin
            (set sum (+ sum i))
            (set i (- i 1))))
        sum)))`)
testLvar(
  "set-let",
  `(let (x 5)
     (let (i 0)
       (begin
         (set i (let (y 7) (+ x y)))
         i)))`)
testLvar("single-begin", "(begin (+ 1 1))")
testLvar("set-order-of-operations", "(let (x 2) (+ x (begin (set x 40) x)))")
testLvar("set-order-of-operations-2", "(let (y 0) (let (x 2) (+ y (+ x (begin (set x 40) x)))))")
testLvar(
  "set-begin-oooperations",
  `(let (x2 10)
     (let (y3 0)
       (+ (+ (begin (set y3 42) x2)
             (begin (set x2 12) y3))
          x2)))`
)
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
testLvar("not", "(let (x #t) (let (y (not x)) 1))")
testLvar("==", "(let (x 1) (let (y (== x 1)) 1))")
testLvar(">", "(let (x 1) (let (y (> x 1)) 1))")
testLvar("<", "(let (x 1) (let (y (< x 1)) 1))")
testLvar(">=", "(let (x 1) (let (y (>= x 1)) 1))")
testLvar("<=", "(let (x 1) (let (y (<= x 1)) 1))")
testLvar("if-boolean", "(let (x #t) (let (y #f) (if x 4 3)))")
testLvar("if", "(let (x #t) (let (y #f) (if x 1 2)))")
testLvar("if-or", "(let (x #t) (let (y #f) (if (or y x) 3 4)))")
testLvar("if-and", "(let (x #t) (let (y #f) (if (and x y) 1 (let (x 12) (let (y 13) (+ x y))))))")
testLvar("if-and-or", "(let (x #t) (let (y #f) (if (and (or x x) y) 1 (let (z (or x y)) 14))))")
testLvar("andlet", "(let (x #t) (let (y #f) (if (and (let (x #f) (or x #t)) y) 1 (let (z (or x y)) 14))))")
testLvar("if-not", "(let (x #t) (let (y #f) (if (not x) 1 2)))")
testLvar("if->=", "(let (x 1) (let (y 2) (if (>= x y) 1 0)))")
testLvar("if-<=", "(let (x 1) (let (y 2) (if (<= x y) 1 0)))")
testLvar("if-<", "(let (x 1) (let (y 2) (if (< x y) 1 0)))")
testLvar("if->", "(let (x 1) (let (y 2) (if (> x y) 1 0)))")
testLvar("if-==", "(let (x 1) (if (== x 2) 1 0))")
testLvar("vector-from-book", 
  `(let (t (vector 40 #t (vector 2)))
    (if (vector-ref t 1)
      (+ (vector-ref t 0)
         (vector-ref (vector-ref t 2) 0))
      44))`, 'c', true)
// TODO: Still need to test vector-set and vector-length
// TODO: Function to test type checking failure
// TODO: Test get/set unbound things
// testLvar("not-bad", "(let (x #t) (let (y #f) (if (not x) (not y) 2)))")
// testLvar("not-bad2", "(let (x #t) (let (y #f) (if (not x) (not y) (not 2))))")
// testLvar("fuzz-guess", `
// (if (if #f 
//       (> 161088 (if #t 94550 67907))
//       #f) 
//   0 1) 
//   `);
// testLvar(
//   "fuzz1",
//   `(let (g36 (+ (if (if 
//   (if #f 
//       (> (- (+ 83132 77956)) (- (if #t 94550 67907)))
//       (> 40248 (begin  63885))) 
//   #f #f) 
//      (if (if (== 20254 (if (>= 97031 (let (g37 61426) 24508)) 86844 (begin  (if #f 63392 56024)))) #t (if (if (== (if #t 82463 36729) (if #f 63682 65948)) (== (if #t 92757 18495) 45535) #f) (<= (let (g38 64036) 64071) 60252) #f)) 67370 30507) 54987) 56244)) 
//      (if (if (if (< g36 40836) #t (if (>= g36 (begin  85923)) #t #f)) (> g36 87130) #f) 98766 19682))`
// )
// for (let i = 0; i < 10; i++) {
//   fuzz(i)
// }
