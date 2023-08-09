#lang racket
(require racket/fixnum)
(struct Program (info body))
(struct Prim (op args))
(struct Int (value))
(define ast1 (Prim '+ (list (Int 8) (Int 15))))
(define is-exp 
  (match-lambda
    [(Int n) #t]
    [(Prim 'read '()) #t]
    [(Prim '- (list e)) (is-exp e)]
    [(Prim '+ (list e1 e2)) (and (is-exp e1) (is-exp e2))]
    [(Prim '- (list e1 e2)) (and (is-exp e1) (is-exp e2))]
    [else #f]))
(define is-Lint
  (match-lambda
    [(Program '() a ) (is-exp a)]
    [else #f]))
(define interp-exp
  (match-lambda
    [(Int n) n]
    [(Prim 'read '())
     (define r (read))
     (cond [(fixnum? r) r]
           [else (error 'interp-exp "read: expected an integer" r)])]
    [(Prim '- (list e)) (fx- 0 (interp-exp e))]
    [(Prim '+ (list e1 e2)) (fx+ (interp-exp e1) (interp-exp e2))]
    [(Prim '- (list e1 e2)) (fx- (interp-exp e1) (interp-exp e2))]))
(define interp-Lint
  (match-lambda
    [(Program '() a) (interp-exp a)]))
(define pe-neg
  (match-lambda
    [(Int n) (Int (fx- 0 n))]
    [r (Prim '- (list r))]))
(define/match (pe-add r1 r2)
  [((Int n1) (Int n2)) (Int (fx+ n1 n2))]
  [(_ _) (Prim '+ (list r1 r2))])
(define/match (pe-sub r1 r2)
  [((Int n1) (Int n2)) (Int (fx- n1 n2))]
  [(_ _) (Prim '- (list r1 r2))])
(define pe-exp
  (match-lambda
    [(Int n) (Int n)]
    [(Prim 'read '()) (Prim 'read '())]
    [(Prim '- (list e)) (pe-neg (pe-exp e))]
    [(Prim '+ (list e1 e2)) (pe-add (pe-exp e1) (pe-exp e2))]
    [(Prim '- (list e1 e2)) (pe-sub (pe-exp e1) (pe-exp e2))]))
(define pe-Lint
  (match-lambda
    [(Program '() a) (Program '() (pe-exp a))]))
(define (assert msg bool)
  (unless bool (error msg)))
(define parse-exp
  (match-lambda
    [(list '+ e1 e2) (Prim '+ (list (parse-exp e1) (parse-exp e2)))]
    [(list '- e1 e2) (Prim '- (list (parse-exp e1) (parse-exp e2)))]
    [(list '- e1) (Prim '- (list (parse-exp e1)))]
    [(list 'read) (Prim 'read '())]
    [n #:when (number? n) (Int n)]))
(define (parse-program sexp)
  (Program '() (parse-exp sexp)))
(pe-Lint (Program '() ast1))
(pe-Lint (parse-program '(+ (read) (- 8))))
(parse-program '(+ 8 (- (read))))
(assert "testing is-Lint" (is-Lint (parse-program '(+ 8 15))))