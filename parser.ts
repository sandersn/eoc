import assert from "node:assert"
import { Int, Bool, type Exp, Let, Var, Prim } from "./factory.js"
enum Token {
  LParen,
  RParen,
  Let,
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
        case "<":
          return nextChar("=", Token.Lte, '<=', Token.Lt, '<')
        case ">":
          return nextChar("=", Token.Gte, '>=', Token.Gt, '>')
        case "=":
          // TODO: fallback should be Eq once assignment is supported
          return nextChar("=", Token.EqEq, '==', Token.EOF, '=')
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
            return Token.Identifier
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
        const head = lexer.next()
        if (head === Token.Let) {
          return parseLet()
        } else {
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
  return parseExp(lexer.next())
}
