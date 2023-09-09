#lang typed/racket
(require racket/fixnum)
(define-type Exp (U Program Int Prim))
(struct Program ([info : (Listof Any)] [body : Exp]))
(struct Prim ([op : Symbol] [args : (Listof Exp)]))
(struct Int ([value : Fixnum]))
(define ast1 (Prim '+ (list (Int 8) (Int 15))))
(: is-exp (-> Exp Boolean))
(define is-exp 
  (match-lambda
    [(Int n) #t]
    [(Prim 'read '()) #t]
    [(Prim '- (list e)) (is-exp e)]
    [(Prim '+ (list e1 e2)) (and (is-exp e1) (is-exp e2))]
    [(Prim '- (list e1 e2)) (and (is-exp e1) (is-exp e2))]
    [else #f]))
(: is-Lint (-> Program Boolean))
(define is-Lint
  (match-lambda
    [(Program '() a) (is-exp a)]
    [else #f]))
(: interp-exp (-> Exp Fixnum))
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
(: interp-Lint (-> Program Fixnum))
(define interp-Lint
  (match-lambda
    [(Program '() a) (interp-exp a)]))
(: pe-neg (-> Exp Exp))
(define pe-neg
  (match-lambda
    [(Int n) (Int (fx- 0 n))]
    [r (Prim '- (list r))]))
(define (pe-add [r1 : Exp] [r2 : Exp]) : Exp
  (match* (r1 r2)
    [((Int n1) (Int n2)) (Int (fx+ n1 n2))]
    [(_ _) (Prim '+ (list r1 r2))]))
(define (pe-sub [r1 : Exp] [r2 : Exp]) : Exp
  (match* (r1 r2)
    [((Int n1) (Int n2)) (Int (fx- n1 n2))]
    [(_ _) (Prim '- (list r1 r2))]))
(: pe-exp (Exp -> Exp))
(define pe-exp
  (match-lambda
    [(Int n) (Int n)]
    [(Prim 'read '()) (Prim 'read '())]
    [(Prim '- (list e)) (pe-neg (pe-exp e))]
    [(Prim '+ (list e1 e2)) (pe-add (pe-exp e1) (pe-exp e2))]
    [(Prim '- (list e1 e2)) (pe-sub (pe-exp e1) (pe-exp e2))]))
(: pe-Lint (Program -> Program))
(define pe-Lint
  (match-lambda
    [(Program '() a) (Program '() (pe-exp a))]))
(: assert (String Boolean -> Any))
(define (assert msg bool)
  (unless bool (error msg)))
(: parse-exp (Any -> Exp))
(define parse-exp
  (match-lambda
    [(list '+ e1 e2) (Prim '+ (list (parse-exp e1) (parse-exp e2)))]
    [(list '- e1 e2) (Prim '- (list (parse-exp e1) (parse-exp e2)))]
    [(list '- e1) (Prim '- (list (parse-exp e1)))]
    [(list 'read) (Prim 'read '())]
    [n #:when (fixnum? n) (Int n)]))
(: parse-program (Any -> Program))
(define (parse-program sexp)
  (Program '() (parse-exp sexp)))
(pe-Lint (Program '() ast1))
(pe-Lint (parse-program '(+ (read) (- 8))))
(parse-program '(+ 8 (- (read))))
(assert "testing is-Lint" (is-Lint (parse-program '(+ 8 15))))