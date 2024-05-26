import assert from "node:assert"
import { Int, Bool, type Exp, Let, Var, Prim, SetBang, Begin, While, Void } from "./factory.js"
enum Token {
  LParen,
  RParen,
  Let,
  Set,
  Begin,
  While,
  Void, // TODO: Maybe this should be a function call
  Identifier,
  True,
  False,
  Number,
  Plus,
  Minus,
  Lt,
  Gt,
  Gte,
  Lte,
  EqEq,
  EOF,
}
const keywords = {
  let: Token.Let,
  set: Token.Set,
  begin: Token.Begin,
  while: Token.While,
  void: Token.Void,
}
function lex(s: string) {
  let i = 0
  let value: string = ""
  function nextChar(next: string, token: Token, tokenValue: string, backup: Token, backupValue: string): Token {
    assert(next.length === 1)
    if (i + 1 < s.length && s[i + 1] === next) {
      i += 2
      value = tokenValue
      return token
    }
    i++
    value = backupValue
    return backup
  }
  return {
    pos: () => i,
    next: () => {
      while (s[i] === " " || s[i] === "\n") i++
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
        case "<":
          return nextChar("=", Token.Lte, "<=", Token.Lt, "<")
        case ">":
          return nextChar("=", Token.Gte, ">=", Token.Gt, ">")
        case "=":
          // TODO: fallback should be Eq once assignment is supported
          return nextChar("=", Token.EqEq, "==", Token.EOF, "=")
        case "#":
          if (i + 1 < s.length && (s[i + 1] === "t" || s[i + 1] === "f")) {
            const token = s[i + 1] === "t" ? Token.True : Token.False
            i += 2
            return token
          }
        // Fall through:
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
            return keywords[value as keyof typeof keywords] ?? Token.Identifier
          }
      }
      return Token.EOF
    },
    value: () => value,
  }
}
export default function parse(sexp: string) {
  const lexer = lex(sexp)
  function parseExp(t: Token): Exp {
    switch (t) {
      case Token.Number:
        return Int(+lexer.value())
      case Token.True:
      case Token.False:
        return Bool(t === Token.True)
      case Token.LParen: {
        switch (lexer.next()) {
          case Token.Let:
            return parseLet()
          case Token.Set:
            return parseSet()
          case Token.Begin:
            return parseBegin()
          case Token.While:
            return parseWhile()
          case Token.Void:
            return parseVoid()
          default: {
            const op = lexer.value()
            const args: Exp[] = []
            t = lexer.next()
            while (t !== Token.RParen) {
              args.push(parseExp(t))
              t = lexer.next()
            }
            return Prim(op, ...args)
          }
        }
      }
      case Token.Identifier:
        return Var(lexer.value())
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
    return Let(name, exp, body)
  }
  function parseSet(): Exp {
    assert(lexer.next() === Token.Identifier)
    const name = lexer.value()
    const exp = parseExp(lexer.next())
    assert(lexer.next() === Token.RParen)
    return SetBang(name, exp)
  }
  function parseBegin(): Exp {
    const exps = []
    let last = parseExp(lexer.next())
    let t = lexer.next()
    // TODO: Test that this works with a single-entry `begin`
    while (t !== Token.RParen) {
      exps.push(last)
      last = parseExp(t)
      t = lexer.next()
    }
    //assert(lexer.next() === Token.RParen)
    return Begin(exps, last)
  }
  function parseWhile(): Exp {
    const cond = parseExp(lexer.next())
    const exp = parseExp(lexer.next())
    assert(lexer.next() === Token.RParen)
    return While(cond, exp)
  }
  function parseVoid(): Exp {
    assert(lexer.next() === Token.RParen)
    return Void()
  }
  return parseExp(lexer.next())
}
