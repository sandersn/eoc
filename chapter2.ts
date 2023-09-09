import assert from "node:assert"
type Program = 
  | { kind: "program"; info: AList<string, number>; body: Exp }
type Exp =
  | { kind: "prim"; op: string; args: Exp[] }
  | { kind: "var"; name: string }
  | { kind: "int"; val: number }
  | { kind: "let", name: string, exp: Exp, body: Exp }
/** Read a number from stdin
 * use readline something something can't be bothered
 */
function read() {
    return 0;
}
let counter = 0
function gensym() {
    return "g" + counter++
}
function assertDefined<T>(e: T | undefined): T {
    assert(e)
    return e
}
class AList<K, V> {
    constructor(public key: K, public value: V, public next?: AList<K, V>) {
    }
    get(key: K): V | undefined {
        if (this.key === key) return this.value
        else if (this.next) return this.next.get(key)
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
    let i = 0;
    let value: string = "";
    return {
        pos: () => i,
        next: () => {
            while (s[i] === ' ') i++;
            switch (s[i]) {
                case "(": i++; return Token.LParen
                case ")": i++; return Token.RParen
                case "+": i++; value = "+"; return Token.Plus
                case "-": i++; value = "-"; return Token.Minus
                case "l": if (i + 2 < s.length && s[i + 1] === "e" && s[i + 2] === "t") { i += 3; return Token.Let }
                default:
                    if (s[i] >= "0" && s[i] <= "9") {
                        let j = i;
                        while (s[i] >= "0" && s[i] <= "9") i++;
                        value = s.slice(j, i)
                        return Token.Number


                    }
                    if (s[i] >= "a" && s[i] <= "z") {
                        let j = i;
                        while (s[i] >= "a" && s[i] <= "z") i++;
                        value = s.slice(j, i)
                        return Token.Identifier
                    }
            }
            return Token.EOF
        },
        value: () => value
    }
}
class LInt {
    interpExp(e: Exp, env: AList<string, number>): number {
        switch (e.kind) {
            case "int": return e.val;
            case "prim":
                if (e.op === "read") return +read()
                if (e.op === "+") return this.interpExp(e.args[0], env) + this.interpExp(e.args[1], env)
                if (e.op === "-" && e.args.length === 1) return - this.interpExp(e.args[0], env)
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
                case Token.Number: return { kind: "int", val: +lexer.value() }
                case Token.LParen: {
                    const head = lexer.next()
                    if (head === Token.Let) {
                        return parseLet()
                    }
                    else {
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
                case Token.Identifier: return { kind: "var", name: lexer.value() }
                default: throw new Error(`Unexpected token at ${lexer.pos()}: ${Token[t]} [${sexp.slice(lexer.pos())}]`)
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
        return { kind: "program", info: new AList("!!!!!!!!!", NaN, undefined), body: parseExp(lexer.next()) }
    }
    emitProgram(p: Program): string {
        return this.emitExp(p.body)
    }
    emitExp(e: Exp): string {
        switch (e.kind) {
            case "int": return `${e.val}`
            case "prim": return `(${e.op} ${e.args.map(a => this.emitExp(a)).join(" ")})`
            case "var": return e.name
            case "let": return `(let (${e.name} ${this.emitExp(e.exp)}) ${this.emitExp(e.body)})`
        }
    }
}
class LVar extends LInt {
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
            body: this.uniquifyExp(p.body, new AList("!!!!!!", "@@@@@@@@@", undefined)) 
        }
    }
    uniquifyExp(e: Exp, env: AList<string, string>): Exp {
        switch (e.kind) {
            case "var": return { ...e, name: assertDefined(env.get(e.name)) }
            case "int": return e
            case "prim": return { ...e, args: e.args.map(a => this.uniquifyExp(a, env)) }
            case "let": {
                let x = env.get(e.name) ? gensym() : e.name;
                env = new AList(e.name, x, env);
                return {
                  kind: "let",
                  name: x,
                  exp: this.uniquifyExp(e.exp, env),
                  body: this.uniquifyExp(e.body, env),
                };
            }
        }
    }

}
function test(name: string, actual: number, expected: number) {
    if (actual !== expected) {
        console.log(`Test ${name} failed: expected ${expected}, actual ${actual}`)
    }
}
function runLint(sexp: string) {
    const l = new LInt();
    return l.interpProgram(l.parseProgram(sexp))
}
function runLvar(sexp: string) {
    const l = new LVar()
    return l.interpProgram(l.parseProgram(sexp))
}
function runUniquifyLvar(sexp: string) {
    const l = new LVar();
    const p = l.uniquifyProgram(l.parseProgram(sexp));
    console.log(l.emitProgram(p))
    return l.interpProgram(p);
}
function testLvar(name: string, sexp: string) {
    const expected = runLvar(sexp)
    console.log("\t", sexp, "-->", expected)
    test(name, runUniquifyLvar(sexp), expected)
}
test("test list basic", runLint("(+ 1 2)"), 3)
test("test lint basic", runLint('(+ (+ 3 4) 12))'), 19)
test("test lvar basic", runLvar('(let (y (+ 1 2)) y)'), 3)
test("test lvar basic", runLvar('(let (x 1) (let (y 2) (+ x y)))'), 3);
testLvar("test lvar remove complex operands", '(+ 42 (- 10))')
testLvar("t", '(+ 42 (+ (+ 3 4) 12))')
testLvar("t", '(+ (let (y (+ 3 4)) (+ y 5)) 12)')
testLvar("t", '(let (a 42) (let (b a) b))')
testLvar("t", '(let (y (+ 1 2)) y)')
testLvar("t", '(let (x 1) (let (y 2) (+ x y)))')
testLvar("t", '(let (x 1) (let (x 2) (+ x x)))')
testLvar("t", '(let (x 1) (+ (let (x 2) x) x))')
testLvar("t", '(let (x 1) (+ (let (x 2) x) (let (x 3) x)))')
testLvar("t", '(let (y 1) (+ (let (x 2) x) (let (x 3) (+ x y))))')