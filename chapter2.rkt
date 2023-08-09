#lang racket
(require racket/fixnum)
(struct Program (info body))
(struct Prim (op args))
(struct Int (value))
(struct Var (var))
(struct Let (var exp body))
(define interp-Lint-class%
  (class object%
    (super-new)
    (define/public ((interp-exp env) e)
      (match e
        [(Int n) n]
        [(Prim 'read '())
         (define r (read))
         (cond [(fixnum? r) r]
               [else (error 'interp-exp "read: expected an integer" r)])]
        [(Prim '- (list e)) (fx- 0 ((interp-exp env) e))]
        [(Prim '+ (list e1 e2)) (fx+ ((interp-exp env) e1) ((interp-exp env) e2))]
        [(Prim '- (list e1 e2)) (fx- ((interp-exp env) e1) ((interp-exp env) e2))]))
    (define/public ((interp-program env) p)
      (match p
        [(Program info body) ((interp-exp '()) body)]))
    (define/public (parse-exp sexp)
      (match sexp
        [(list '+ e1 e2) (Prim '+ (list (parse-exp e1) (parse-exp e2)))]
        [(list '- e1 e2) (Prim '- (list (parse-exp e1) (parse-exp e2)))]
        [(list '- e1) (Prim '- (list (parse-exp e1)))]
        [(list 'read) (Prim 'read '())]
        [n #:when (number? n) (Int n)]))
    (define/public (parse-program sexp)
      (Program '() (parse-exp sexp)))))
(define interp-Lvar-class%
  (class interp-Lint-class%
    (super-new)
    (define/override ((interp-exp env) e)
      (match e
        [(Var x) (dict-ref env x)]
        [(Let x e body) ((interp-exp (dict-set env x ((interp-exp env) e))) body)]
        [else ((super interp-exp env) e)]))
    (define/override (parse-exp sexp)
      (match sexp
        [(list 'let (list x e) body) (Let x (parse-exp e) (parse-exp body))]
        [x #:when (symbol? x) (Var x)]
        [else (super parse-exp sexp)]))))
(define lvar (new interp-Lvar-class%))
((send lvar interp-program '()) (Program '() (Let 'y (Prim '+ (list (Int 1) (Int 2))) (Var 'y))))
(define (run-Lvar sexp)
  (define lvar (new interp-Lvar-class%))
  ((send lvar interp-program '()) (send lvar parse-program sexp)))
(define (assert msg bool)
  (unless bool (error msg)))
(define (test name actual expected)
  (unless (= actual expected)
    (error (format "Test ~a failed: expected ~a, got ~a" name expected actual))))
(run-Lvar '(let (y (+ 1 2)) y))
(run-Lvar '(let (x 1) (let (y 2) (+ x y))))
(test "test lvar basic" (run-Lvar '(let (x 1) (let (y 2) (+ x y)))) 4)