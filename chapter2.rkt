#lang typed/racket
(require typed/racket/class)
(require racket/fixnum)
;; Lvar
(define-type Exp (U Program Prim Int Var Let))
(struct Program ([info : (HashTable Symbol Exp)] [body : Exp]))
(struct Prim ([op : Symbol] [args : (Listof Exp)]))
(struct Int ([value : Fixnum]))
(struct Var ([var : Symbol]))
(struct Let ([var : Symbol] [exp : Exp] [body : Exp]))
;; Cvar
; (struct Assign ([var : Var] [exp : Exp]))
; (struct Seq ([statements : (Listof Exp)]))
; (struct Return ([exp : Exp]))
; (struct CProgram ([info : Any] [blocks : (Listof Any)]))
;; X86var
; (struct Instr ([op : Symbol] [args : (list Ref)]))
; (struct Callq ([label : Symbol] [int : Fixnum]))
; (struct Retq ())
; (struct Jmp ([label : Symbol]))
; (struct Imm ([int : Fixnum]))
; (struct Reg ([reg : Symbol]))
; (struct Deref ([reg : Symbol] [int : Fixnum]))
; (struct Block ([info : Any] [instructions : (Listof Instruction)]))
; (struct X86Program ([info : Any] [blocks : (Listof (Pair Symbol Block))]))

(define Lint%
  (class object%
    (super-new)
    (: interp-exp ((HashTable Symbol Fixnum) -> (Exp -> Fixnum)))
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
    (: interp-program (Any -> (Program -> Fixnum)))
    (define/public ((interp-program env) p)
      (match p
        [(Program info body) ((interp-exp (make-immutable-hash)) body)]))
    (: parse-exp (Any -> Exp))
    (define/public (parse-exp sexp)
      (match sexp
        [(list '+ e1 e2) (Prim '+ (list (parse-exp e1) (parse-exp e2)))]
        [(list '- e1 e2) (Prim '- (list (parse-exp e1) (parse-exp e2)))]
        [(list '- e1) (Prim '- (list (parse-exp e1)))]
        [(list 'read) (Prim 'read '())]
        [n #:when (fixnum? n) (Int n)]))
    (define/public (parse-program sexp)
      (Program (make-immutable-hash) (parse-exp sexp)))
    (define/public (emit-program p)
      (match p
        [(Program info body) (emit-exp body)]
        ; [(CProgram info blocks) (emit-exp (hash-ref blocks 'start))]
        ))
    (: emit-exp (Exp -> Any))
    (define/public (emit-exp e)
      (match e
        [(Int n) n]
        [(Prim op es) (cons op (map (lambda ([e : Exp]) (emit-exp e)) es))]))))

(define Lvar%
  (class Lint%
    (super-new)
    (: interp-exp ((HashTable Symbol Fixnum) -> (Exp -> Fixnum)))
    (define/override ((interp-exp env) e)
      (match e
        [(Var x) (hash-ref env x)]
        [(Let x e body) ((interp-exp (hash-set env x ((interp-exp env) e))) body)]
        [else ((super interp-exp env) e)]))
    (: parse-exp (Any -> Exp))
    (define/override (parse-exp sexp)
      (match sexp
        [(list 'let (list x e) body) #:when (symbol? x) (Let x (parse-exp e) (parse-exp body))]
        [x #:when (symbol? x) (Var x)]
        [else (super parse-exp sexp)]))
    (: emit-exp (Exp -> Any))
    (define/override (emit-exp e)
      (match e
        [(Var x) x]
        [(Let x e body) (list 'let (list x (emit-exp e)) (emit-exp body))]
        [else (super emit-exp e)]))
    (: uniquify-exp ((HashTable Symbol Symbol) -> (Exp -> Exp)))
    (define/public ((uniquify-exp env) e)
        (match e
          [(Var x) (Var (hash-ref env x))]
          [(Int n) (Int n)]
          [(Prim op es) (Prim op (for/list ([e es]) ((uniquify-exp env) e)))]
          [(Let x e body)
           (let* ([x-shadowed (hash-ref env x #f)]
                  [x* (if x-shadowed (gensym) x)]
                  [env* (hash-set env x x*)])
             (Let x* ((uniquify-exp env*) e) ((uniquify-exp env*) body)))]))
    (define/public (uniquify-program p)
      (match p
        [(Program info body) (Program info ((uniquify-exp (make-immutable-hash)) body))]))
  ))
;     ;   (define/public (remove-complex-operands-atom e tail?)
;     ;     (match e
;     ;       [(Var x) (values (Var x) '())]
;     ;       [(Int n) (values (Int n) '())]
;     ;       [(Prim 'read '())
;     ;        (define tmp (gensym)) 
;     ;        (values (Var tmp) (list (cons tmp (Prim 'read '()))))]
;     ;       [(Prim '- (list e))
;     ;        (define-values (e* tmps) (remove-complex-operands-atom e #f))
;     ;        (define tmp (gensym))
;     ;        (values (Var tmp) (cons (cons tmp (Prim '- (list e*))) tmps))]
;     ;       [(Prim op (list e1 e2)) 
;     ;        (define-values (e1* tmps1) (remove-complex-operands-atom e1 #f))
;     ;        (define-values (e2* tmps2) (remove-complex-operands-atom e2 #f))
;     ;        (define tmps (append tmps2 tmps1))
;     ;        (define tmp (gensym))
;     ;        (values (Var tmp) (cons (cons tmp (Prim op (list e1* e2*))) tmps))]
;     ;       [(Let x e body)
;     ;        (define-values (e* tmps-e) (remove-complex-operands-atom e #f))
;     ;        (define-values (body* tmps-body) (remove-complex-operands-atom body tail?))
;     ;        (match e*
;     ;          [(Int n) #:when tail? (values (Let x (Int n) (generate-tmp-lets body* tmps-body)) '())]
;     ;          [(Var v) #:when tail? (values (Let x (Var v) (generate-tmp-lets body* tmps-body)) '())]
;     ;          [else 
;     ;           (define tmp (gensym))
;     ;           (values (Var tmp) 
;     ;                   (cons (cons tmp (Let x e* (generate-tmp-lets body* tmps-body)))
;     ;                         tmps-e))])]))
;     ;   (define/public (remove-complex-operands-exp e)
;     ;     (match e
;     ;       [(Var x) (Var x)]
;     ;       [(Int n) (Int n)]
;     ;       [(Prim 'read '()) (Prim 'read '())]
;     ;       [(Prim op (list e))
;     ;        (define-values (tmp tmps) (remove-complex-operands-atom e #f))
;     ;        (generate-tmp-lets (Prim op (list tmp)) tmps)]
;     ;       [(Prim op (list e1 e2))
;     ;        (define-values (tmp1 tmps1) (remove-complex-operands-atom e1 #f))
;     ;        (define-values (tmp2 tmps2) (remove-complex-operands-atom e2 #f))
;     ;        (generate-tmp-lets (Prim op (list tmp1 tmp2)) (append tmps2 tmps1))]
;     ;       [(Let x e body)
;     ;        (define-values (tmp-e tmps-e) (remove-complex-operands-atom e #f))
;     ;        (define-values (tmp-body tmps-body) (remove-complex-operands-atom body #t))
;     ;        (generate-tmp-lets (Let x tmp-e (generate-tmp-lets tmp-body tmps-body))  tmps-e)]))
;     ;   (define/public (remove-complex-operands-program p)
;     ;     (match p
;     ;       [(Program info body) (Program info (remove-complex-operands-exp body))]))
;     ;   (define/public (explicate-assign e x k)
;     ;     (match e
;     ;       [(Var y) (cons (Assign (Var x) (Var y)) k)]
;     ;       [(Int n) (cons (Assign (Var x) (Int n)) k)]
;     ;       [(Prim op es) (cons (Assign (Var x) (Prim op es)) k)]
;     ;       [(Let y rhs body)
;     ;        (explicate-assign rhs y (explicate-assign body x k))]))
;     ;   (define/public (explicate-tail e)
;     ;     (match e
;     ;       [(Var x) (Return (Var x))]
;     ;       [(Int n) (Return (Int n))]
;     ;       [(Prim op es) (Return (Prim op es))]
;     ;       [(Let x rhs body)
;     ;        (match (explicate-tail body)
;     ;          [(Seq statements) (Seq (explicate-assign rhs x statements))]
;     ;          [(Return e)  (Seq (explicate-assign rhs x (list (Return e))))])]))
;       ; (define/public (explicate-control p)
;       ;   (match p
;       ;     [(Program info body) 
;       ;      (CProgram (make-hash info) (make-hash (list (cons 'start (explicate-tail body)))))]))
;             ))
; (define (generate-tmp-lets init tmps)
;   (display tmps)
;   (foldr (lambda (tmp e**) (Let (car tmp) (cdr tmp) e**))
;          init
;          (reverse tmps)))
; (define Cvar%
;   (class Lvar%
;     (super-new)
;     (define/override ((interp-exp env) e)
;       (match e
;         [(Assign (Var var) exp) (hash-set! env var ((interp-exp env) exp))]
;         [(Seq statements) (for/last ([s statements]) ((interp-exp env) s))]
;         [(Return exp) ((interp-exp env) exp)]
;         [else ((super interp-exp env) e)]))
;     (define/override (interp-program p)
;       (match p
;         [(CProgram locals blocks) ((interp-exp locals) (hash-ref blocks 'start))]))
;     (define/override (emit-exp e)
;       (match e
;         [(Assign var exp) (list (emit-exp var) '= (emit-exp exp))]
;         [(Seq statements) (for/list ([s statements]) (emit-exp s))]
;         [(Return exp) (list 'return (emit-exp exp))]
;         [else (super emit-exp e)]))))
; (define X86var%
;   (class object%
;     (super-new)
;     (define/public ((interp-exp env) e)
;       (match e
;         [(Instr 'movq (list from to)) 
;          (display 'movq) (display env) (newline)
;          (define to- (interp-ref env to))
;          (define from- (interp-ref env from))
;          (hash-set! env (interp-symbol to) from-)]
;         [(Instr 'addq (list from to)) 
;          (display 'addq) (display env) (newline)
;          (define to- (interp-ref env to))
;          (define from- (interp-ref env from))
;          (hash-set! env (interp-symbol to) (+ to- from-))]
;         [(Instr 'subq (list from to)) 
;          (define to- (interp-ref env to))
;          (define from- (interp-ref env from))
;          (hash-set! env (interp-symbol to) (- to- from-))]
;         [(Instr 'negq (list to)) 
;          (define to- (interp-ref env to))
;          (hash-set! env (interp-symbol to) (- to-))]
;         [(Callq label int) (hash-set! env 'rax (read))]
;         [(Retq) '()] ; jmp to previous thing on stack, stop processing instructions in block
;         [(Jmp label) (error 'interp-exp "jmp not implemented")]
;         ;; TODO: What is for/each called? How to enable retq to short-circuit?
;         ;; TODO: Block probably shouldn't even be here. Maybe.
;         [(Block info instructions) 
;          (for/last ([instr instructions]) ((interp-exp (make-hash)) instr))])) ; info SHOULD have the env in it
;     ; TODO: This derefs too much; the target of a hash-set! needs a symbol, not a value
;     ; (but only Var and Reg have symbols)
;     (define/public (interp-symbol ref)
;       (match ref
;         [(Var x) x] ; assumes no clash with reg names
;         [(Reg reg) reg]))
;     (define/public (interp-ref env ref)
;       (match ref
;         [(Var x) (hash-ref env x 0)] ; assumes no clash with reg names
;         [(Deref reg int) (hash-ref env (+ (hash-ref env reg) int))]
;         [(Imm int) int]
;         [(Int int) int] ; ???? shouldn't be in this language
;         [(Reg reg) (hash-ref env reg 0)]))
;     (define/public (interp-program p)
;       (match p
;         [(X86Program info blocks) 
;          (define env (make-hash))
;          (for ([instr (hash-ref blocks 'start)]) ((interp-exp env) instr))
;          (hash-ref env 'rax)]))
;     ; TODO: The emit is extremely loose. Improve the output here to figure out
;     ; where the looseness is and how incorrect it is.
;     (define/public (emit-instr instr) 
;       (match instr
;         [(Instr op args) (display args) (cons op (for/list ([arg args]) (emit-ref arg)))]
;         [(Callq label int) (list 'call label int)]
;         [(Retq) 'retq] ; jmp to previous thing on stack, stop processing instructions in block
;         [(Jmp label) (list 'jmp label)]))
;     (define/public (emit-ref ref)
;       (match ref
;         [(Var x) x]
;         [(Deref reg int) (list reg '% int)]
;         [(Imm int) int]
;         [(Int int) int] ; shouldn't be in this language, but select-instructions allows vars and ints still
;         [(Reg reg) reg]))
;     (define/public (emit-program p)
;       (match p
;         [(X86Program info blocks) 
;          (for/list ([instr (hash-ref blocks 'start)]) (emit-instr instr))]))
;     (define/public (select-instructions-statement s)
;       (match s
;         [(Seq statements) 
;          (append-map (lambda (s) (select-instructions-statement s)) statements)]
;         [(Assign var (Prim '+ (list arg1 arg2))) 
;          (list (Instr 'movq (list arg1 var)) (Instr 'addq (list arg2 var)))]
;         [(Assign var (Prim '- (list arg1 arg2))) 
;          (list (Instr 'movq (list arg1 var)) (Instr 'subq (list arg2 var)))]
;         [(Assign var (Prim '- (list arg1))) 
;          (list (Instr 'movq (list arg1 var)) (Instr 'negq (list var)))]
;         [(Assign var (Prim 'read '())) 
;          (list (Callq 'read_int 0) (Instr 'movq (list (Reg 'rax) var)))]
;         [(Assign var (Var arg1)) 
;          (list (Instr 'movq (list arg1 var)))]
;         [(Assign var (Int int)) 
;          (list (Instr 'movq (list var (Imm int))))]
;         [(Assign var args)
;          (error 'select-instructions-statement "Assign: ~a" args)]
;         [(Return (Var arg1)) (list (Instr 'movq (list arg1 (Reg 'rax))) (Retq))] 
;         [(Return (Prim op args)) 
;          (append 
;            (select-instructions-statement (Assign (Reg 'rax) (Prim op args)))
;            (list (Retq)))]))
;     (define/public (select-instructions p) 
;         (match p
;           [(CProgram locals blocks) 
;            (X86Program locals (list (cons 'start (select-instructions-statement (hash-ref blocks 'start)))))]))))
; (define (interp-x86 p)
;   (define block (match p ; :: (Listof instruction)
;    [(X86Program info blocks) (hash-ref blocks 'start)]))
;   (define instr (car block))
;   instr
; )
; ; (struct Instr (op args))
; ; (struct Callq (label int))
; ; (struct Retq ())
; ; (struct Jmp (label))
; ; (struct Imm (int))
; ; (struct Reg (reg))
; ; (struct Deref (reg int))
; ; (struct Block (info instructions))
; ; (struct X86Program (info blocks)) ; where blocks :: (Listof (cons label block))

; ; (let ((h (make-hash)))
; ;   (hash-set! h 'x 'running-c-here)
; ;   (hash-set! h 'y 0)
; ;   (hash-set! h 'z 0)
; ;   (send (new Cvar%) interp-program
; ;         (CProgram
; ;          h
; ;          (list (cons 'start (Seq (list (Assign 'x (Int 1)) (Var 'x) (Return (Var 'x)))))))))

(: run-Lint (Any -> Fixnum))
(define (run-Lint sexp)
  (define lint (new Lint%))
  ((send lint interp-program '()) (send lint parse-program sexp)))
(define (run-uniquify-Lvar sexp)
  (define lvar (new Lvar%))
  ((send lvar interp-program '()) (send lvar parse-program sexp)))
(define (run-Lvar sexp)
  (define lvar (new Lvar%))
  ((send lvar interp-program '()) (send lvar parse-program sexp)))
; (define (run-Cvar sexp)
;   (define lvar (new Lvar%))
;   (define cvar (new Cvar%))
;   (define output (send lvar explicate-control
;                        (send lvar remove-complex-operands-program
;                              (send lvar uniquify-program
;                                    (send lvar parse-program sexp)))))
;   (display (send cvar emit-program output))
;   (send cvar interp-program output))
; (define (run-X86var sexp)
;   (define lvar (new Lvar%))
;   (define cvar (new Cvar%))
;   (define x86var (new X86var%))
;   (define output (send lvar explicate-control
;                        (send lvar remove-complex-operands-program
;                              (send lvar uniquify-program
;                                    (send lvar parse-program sexp)))))
;   (display (send cvar emit-program output))
;   (define instrs (send x86var select-instructions output))
;   (display (send x86var emit-program instrs))
;   (newline) (newline) (newline)
;   (send x86var interp-program instrs))
(define (uniq-Lvar sexp)
  (define lvar (new Lvar%))
  (send lvar emit-program 
    (send lvar uniquify-program 
      (send lvar parse-program sexp))))
; (define (remove-Lvar sexp)
;   (define lvar (new Lvar%))
;   (send lvar emit-program 
;     (send lvar remove-complex-operands-program 
;       (send lvar uniquify-program 
;         (send lvar parse-program sexp)))))
; (define (explicate-Lvar sexp)
;   (define lvar (new Lvar%))
;   (define cvar (new Cvar%))
;   (send cvar emit-program
;         (send lvar explicate-control
;               (send lvar remove-complex-operands-program
;                     (send lvar uniquify-program
;                           (send lvar parse-program sexp))))))
; (define (select-Lvar sexp)
;   (define lvar (new Lvar%))
;   (define x86var (new X86var%))
;   (send x86var emit-program
;         (send x86var select-instructions
;               (send lvar explicate-control
;                     (send lvar remove-complex-operands-program
;                           (send lvar uniquify-program
;                                 (send lvar parse-program sexp)))))))
(: assert (String Boolean -> Any))
(define (assert msg bool)
  (unless bool (error msg)))
(: test (String Number Number -> Any))
(define (test name actual expected)
  (unless (= actual expected)
    (error (format "Test ~a failed: expected ~a, got ~a" name expected actual))))
(: test-Lvar (String Any -> Any))
(define test-Lvar
  (lambda (name sexp)
    (display sexp)
    (display " --> ")
    (define expected (run-Lvar sexp))
    (display expected)
    (newline)
    (test name expected (run-uniquify-Lvar sexp))))
    ; (test name expected (run-X86var sexp))))
(test "test lint basic" (run-Lint '(+ 1 2)) 3)
(test "test lint basic" (run-Lint '(+ (+ 3 4) 12)) 19)
(test "test lvar basic" (run-Lvar '(let (y (+ 1 2)) y)) 3)
(test "test lvar basic" (run-Lvar '(let (x 1) (let (y 2) (+ x y)))) 3)
(test-Lvar "test lvar remove complex operands" '(+ 42 (- 10)))
(test-Lvar "t" '(+ 42 (+ (+ 3 4) 12)))
(test-Lvar "t" '(+ (let (y (+ 3 4)) (+ y 5)) 12))
(test-Lvar "t" '(let (a 42) (let (b a) b)))
(test-Lvar "t" '(let (y (+ 1 2)) y))
(test-Lvar "t" '(let (x 1) (let (y 2) (+ x y))))
(test-Lvar "t" '(let (x 1) (let (x 2) (+ x x))))
(test-Lvar "t" '(let (x 1) (+ (let (x 2) x) x)))
(test-Lvar "t" '(let (x 1) (+ (let (x 2) x) (let (x 3) x))))
(test-Lvar "t" '(let (y 1) (+ (let (x 2) x) (let (x 3) (+ x y)))))