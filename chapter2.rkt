#lang racket
(require racket/fixnum)
(struct Program (info body))
(struct Prim (op args))
(struct Int (value))
(struct Var (var))
(struct Let (var exp body))
;; Cvar
(struct Assign (var exp))
(struct Seq (statements))
(struct Return (exp))
(struct CProgram (info blocks))

(define Lint%
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
      (Program '() (parse-exp sexp)))
    (define/public (emit-program p)
      (match p
        [(Program info body) (emit-exp body)]))
    (define/public (emit-exp e)
      (match e
        [(Int n) n]
        [x #:when (symbol? x) (list 'incorrect 'symbol x)]
        [(Prim op es) (cons op (for/list ([e es]) (emit-exp e)))]))))

(define Lvar%
  (class Lint%
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
        [else (super parse-exp sexp)]))
    (define/override (emit-exp e)
      (match e
        [(Var (Int x)) (list 'incorrect 'int x)]
        [(Var x) x]
        [(Let x e body) (list 'let (list x (emit-exp e)) (emit-exp body))]
        [else (super emit-exp e)]))
    (define/public ((uniquify-exp env) e)
        (match e
          [(Var x) (Var (dict-ref env x))]
          [(Int n) (Int n)]
          [(Prim op es) (Prim op (for/list ([e es]) ((uniquify-exp env) e)))]
          [(Let x e body)
           (let* ([x-shadowed (dict-ref env x #f)]
                  [x* (if x-shadowed (gensym) x)]
                  [env* (dict-set env x x*)])
             (Let x* ((uniquify-exp env*) e) ((uniquify-exp env*) body)))]))
      (define/public (uniquify-program p)
        (match p
          [(Program info body) (Program info ((uniquify-exp '()) body))]))
      (define/public (remove-complex-operands-atom e)
        (match e
          [(Var x) (values (Var x) '())]
          [(Int n) (values (Int n) '())]
          [(Prim 'read '())
           (define tmp (gensym)) 
           (values (Var tmp) (list (cons tmp (Prim 'read '()))))]
          [(Prim '- (list e))
           (define-values (e* tmps) (remove-complex-operands-atom e))
           (define tmp (gensym))
           (values (Var tmp) (cons (cons tmp (Prim '- (list e*))) tmps))]
          [(Prim op (list e1 e2)) 
           (define-values (e1* tmps1) (remove-complex-operands-atom e1))
           (define-values (e2* tmps2) (remove-complex-operands-atom e2))
           (define tmps (append tmps2 tmps1))
           (define tmp (gensym))
           (values (Var tmp) (cons (cons tmp (Prim op (list e1* e2*))) tmps))]
          [(Let x e body)
           (define-values (e* tmps-e) (remove-complex-operands-atom e))
           (define-values (body* tmps-body) (remove-complex-operands-atom body))
           (match e*
             [(Int n) (values (Let x (Int n) (generate-tmp-lets body* tmps-body)) '())]
             [(Var v) (values (Let x (Var v) (generate-tmp-lets body* tmps-body)) tmps-e)]
             [else 
              (define tmp (gensym))
              (values (Var tmp) 
                      (cons (cons tmp (Let x e* (generate-tmp-lets body* tmps-body)))
                            tmps-e))])]))
      (define/public (remove-complex-operands-exp e)
        (match e
          [(Var x) (Var x)]
          [(Int n) (Int n)]
          [(Prim 'read '()) (Prim 'read '())]
          [(Prim op (list e))
           (define-values (tmp tmps) (remove-complex-operands-atom e))
           (generate-tmp-lets (Prim op (list tmp)) tmps)]
          [(Prim op (list e1 e2))
           (define-values (tmp1 tmps1) (remove-complex-operands-atom e1))
           (define-values (tmp2 tmps2) (remove-complex-operands-atom e2))
           (generate-tmp-lets (Prim op (list tmp1 tmp2)) (append tmps2 tmps1))]
          [(Let x e body)
           (define-values (tmp-e tmps-e) (remove-complex-operands-atom e))
           (define-values (tmp-body tmps-body) (remove-complex-operands-atom body))
           (generate-tmp-lets (Let x tmp-e (generate-tmp-lets tmp-body tmps-body))  tmps-e)]))
      (define/public (remove-complex-operands-program p)
        (match p
          [(Program info body) (Program info (remove-complex-operands-exp body))]))))
(define (generate-tmp-lets init tmps)
  (foldr (lambda (tmp e**) (Let (car tmp) (cdr tmp) e**))
         init
         (reverse tmps)))
(define Cvar%
  (class Lvar%
    (super-new)
    (define/override ((interp-exp env) e)
      (match e
        [(Assign var exp) (dict-set! env var ((interp-exp env) exp))]
        [(Seq statements) (for/last ([s statements]) ((interp-exp env) s))]
        [(Return exp) ((interp-exp env) exp)]
        [else ((super interp-exp env) e)]))
    (define/override (interp-program p)
      (match p
        [(CProgram locals blocks) ((interp-exp locals) (dict-ref blocks 'start))]))))
; (let ((h (make-hash)))
;   (dict-set! h 'x 'running-c-here)
;   (dict-set! h 'y 0)
;   (dict-set! h 'z 0)
;   (send (new Cvar%) interp-program
;         (CProgram
;          h
;          (list (cons 'start (Seq (list (Assign 'x (Int 1)) (Var 'x) (Return (Var 'x)))))))))

(define (run-Lvar sexp)
  (define lvar (new Lvar%))
  ((send lvar interp-program '()) (send lvar parse-program sexp)))
(define (uniq-Lvar sexp)
  (define lvar (new Lvar%))
  (send lvar emit-program 
    (send lvar uniquify-program 
      (send lvar parse-program sexp))))
(define (remove-Lvar sexp)
  (define lvar (new Lvar%))
  (send lvar emit-program 
    (send lvar remove-complex-operands-program 
      (send lvar uniquify-program 
        (send lvar parse-program sexp)))))
(define (assert msg bool)
  (unless bool (error msg)))
(define (test name actual expected)
  (unless (= actual expected)
    (error (format "Test ~a failed: expected ~a, got ~a" name expected actual))))
(remove-Lvar '(+ 42 (- 10)))
(remove-Lvar '(let (a 42) (let (b a) b)))
(remove-Lvar '(let (y (+ 1 2)) y))
(remove-Lvar '(let (x 1) (let (y 2) (+ x y))))
(remove-Lvar '(let (x 1) (let (x 2) (+ x x))))
(remove-Lvar '(let (x 1) (+ (let (x 2) x) x)))
(remove-Lvar '(let (x 1) (+ (let (x 2) x) (let (x 3) x))))
(remove-Lvar '(let (y 1) (+ (let (x 2) x) (let (x 3) (+ x y)))))
(define test-Lvar
  (lambda (name sexp)
    (test name (run-Lvar sexp) (run-Lvar (remove-Lvar sexp)))))
(test "test lvar basic" (run-Lvar '(let (y (+ 1 2)) y)) 3)
(test "test lvar basic" (run-Lvar '(let (x 1) (let (y 2) (+ x y)))) 3)
(test-Lvar "test lvar remove complex operands" '(+ 42 (- 10)))
(test-Lvar "t" '(let (a 42) (let (b a) b)))
(test-Lvar "t" '(let (y (+ 1 2)) y))
(test-Lvar "t" '(let (x 1) (let (y 2) (+ x y))))
(test-Lvar "t" '(let (x 1) (let (x 2) (+ x x))))
(test-Lvar "t" '(let (x 1) (+ (let (x 2) x) x)))
(test-Lvar "t" '(let (x 1) (+ (let (x 2) x) (let (x 3) x))))
(test-Lvar "t" '(let (y 1) (+ (let (x 2) x) (let (x 3) (+ x y)))))
