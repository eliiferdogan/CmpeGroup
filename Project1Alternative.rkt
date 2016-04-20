#lang plai-typed

;Grammar for operations
;;; S1 = {+, -, *, **}
;;; S2 = [0-9]
;;; S3 = {gtz}
;;; S4 = Functions
;;; S5 = (λ v S)
;;; S = S1 S S | S2 | - S | S3 S2 S S | S4 S | S5 



;; Definitions for myExpr
(define-type myExpr
  [numE (n : number)] ; Number
  [idE (s : symbol)] ; Identifier
  [appE (func : symbol) (arg : (listof myExpr))] ; Function and arguments
  [addE (lhs : myExpr) (rhs : myExpr)] ; Adds rhs and lhs
  [mulE (lhs : myExpr) (rhs : myExpr)] ; Multiplies rhs and lhs
  [powE (lhs : myExpr) (rhs : myExpr)] ; Takes power of lhs to rhs
  [negE (lhs : myExpr)] ; Takes negative of a number
  [gtzE (n : number) (caseOne : myExpr) (caseTwo : myExpr)]) ; caseOne if n>0, caseTwo otherwise

;; Definitions for function definition
(define-type funcDef
  [fdef (name : symbol) (arg : (listof symbol)) (body : myExpr)] ; Name, argument and body for a function
  )


;; Definitions for Binding
(define-type Binding
  [bind (name : symbol) (val : number)])
 
(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)

;; Test Functions
(define addFunc (fdef 'addFunc (list 'x 'y) (addE (idE 'x) (idE 'y))))
(define fa (fdef 'fa (list 'x 'y) (addE (idE 'x) (mulE (numE 2) (idE 'y)))))

(define funcList (list addFunc fa))
; getFuncDef : symbol listOfFuncDef -> funcDef
; Returns the full function definition of n if exists in fds
; Examples
; 'double -> (fdef 'double 'x (addE (idE 'x) (idE 'x)))
; 'triple -> (fdef 'triple 'x (addE (appE 'double (idE 'x)) (idE 'x)))
(define (getFuncDef [n : symbol] [fds : (listof funcDef)]) : funcDef
  (cond
    [(empty? fds) (error 'getFuncDef "Undefined function")]
    [(cons? fds) (cond
                   [(equal? n (fdef-name (first fds))) (first fds)]
                   [else (getFuncDef n (rest fds))])]))

(test (getFuncDef 'addFunc funcList) (fdef 'addFunc (list 'x 'y) (addE (idE 'x) (idE 'y))))
(test (getFuncDef 'fa funcList) (fdef 'fa (list 'x 'y) (addE (idE 'x) (mulE (numE 2) (idE 'y)))))


;; Lookup
(define (lookup [for : symbol] [env : Env]) : number
  (cond
    [(empty? env) (error 'lookup "name not found")]
    [else (cond
            [(symbol=? for (bind-name (first env)))
             (bind-val (first env))]
            [else (lookup for (rest env))])]))


;; Extend Environment Helper
;; extendEnvHelper : (listof symbol) Env funcDef -> Env
;; Template
(define (appEHelper func arg [env : Env] [fds : (listof funcDef)])
  (local ([define fd (getFuncDef func fds)])
              (interp (fdef-body fd)
                      (extendEnvHelper fd arg env fds)
                      fds)))

(define (extendEnvHelper fd arg env fds)
  (cond
    [(= (length arg) 1) (extend-env (bind (first (fdef-arg fd))
                                  (interp (first arg) env fds))
                                  mt-env)]
    [else (append (extendEnvHelper
                   (fdef (fdef-name fd) (rest (fdef-arg fd)) (fdef-body fd))
                   (rest arg) env fds) (extend-env (bind (first (fdef-arg fd))
                                  (interp (first arg) env fds))
                                  mt-env))]))

;; takePower : number number -> number
;; Takes base to the power of top using accumulator approach.
;; Examples
;; 5 3 -> 125
;; 3 2 -> 9
;; -2 4 -> 16
;; -2 5 -> -32
;; 0 4 -> 0
;; 4 -1 -> Illegal Expression
(define (takePower [base : number] [top : number]) : number
  (takePowerHelper base top 1))

;; takePowerHelper : number number number -> number
;; Accumulator approach implementation to takePower function
;; Examples are the same as takePower
;; Template
;; (define (takePowerHelper [b : number] [p : number] [cur : number]) : number
;;   (cond
;;     [(= p 0) ...]
;;     [(> p 0) (takePowerHelper ... ... ...)]
;;     [else ...]))
(define (takePowerHelper [b : number] [p : number] [cur : number]) : number
  (cond
    [(= p 0) cur]
    [(> p 0) (takePowerHelper b (- p 1) (* b cur))]
    [else (error 'takePower "Illegal Expression")]))


; interp : myExpr listOfFuncDefs -> number
; Returns the result of myExpr
; Examples
; (numE 5) funcList) 5
; (numE -5) funcList) -5
; (appE 'double (numE 5)) funcList) -> 10
; (appE 'triple (numE 3)) funcList) -> 9
; (addE (numE 5) (numE 6)) funcList) -> 11
; (addE (numE -3) (numE 2)) funcList) -> -1
; (negE (numE 5)) funcList) -> -5
; (negE (numE -3)) funcList) -> 3
; (negE (numE 20)) funcList) -> -20
; (mulE (numE 5) (numE 6)) funcList) -> 30
; (mulE (numE -3) (numE 2)) funcList) -> -6
; (powE (numE 4) (numE 2)) funcList) -> 16
; (powE (numE -3) (numE 3)) funcList) -> -27
; (gtzE 1 (powE (numE 4) (numE 2)) (mulE (numE 5) (numE 6))) funcList) -> 16
; (gtzE -1 (powE (numE 4) (numE 2)) (mulE (numE 5) (numE 6))) funcList) -> 30
(define (interp [e : myExpr] [env : Env] fds) : number
  (type-case myExpr e
    [numE (n) n]
    [appE (func arg) (appEHelper func arg env fds)]
    [idE (n) (lookup n env)]
    [addE (lhs rhs) (+ (interp lhs env fds) (interp rhs env fds))]
    [mulE (lhs rhs) (* (interp lhs env fds) (interp rhs env fds))]
    [powE (lhs rhs) (takePower (interp lhs env fds) (interp rhs env fds))]
    [negE (lhs) (* (interp lhs env fds) -1)]
    [gtzE (n caseOne caseTwo) (interp (greaterThanZeroHelper (numE n) caseOne caseTwo) env fds)]))

;; greaterThanZeroHelper : myExpr myExpr myExpr -> myExpr
;; Helper function for greater than zero case.
;; Returns caseOne if n>0, caseTwo otherwise.
;; Examples
;; (numE 1) (numE 5) (numE 7) -> (numE 5)
;; (numE 1) (addE (numE 5) (numE 6)) (numE 7) -> (addE (numE 5) (numE 6))
;; (numE 0) (addE (numE 5) (numE 6)) (numE 7) -> (numE 7)
;; (numE -1) (numE 5) (numE 8) -> (numE 8)
;; (addE (numE 3) (numE 4)) (numE 1) (numE -1) -> (numE 1)
;; (mulE (numE 0) (numE 23)) (numE 1) (numE -1) -> (numE -1)
;; Template
;; (define (greaterThanZeroHelper [n : myExpr] [caseOne : myExpr] [caseTwo : myExpr]) : myExpr
;;   (cond
;;     [(> ...n 0) ...caseOne]
;;     [else ...caseTwo]))
(define (greaterThanZeroHelper [n : myExpr] [caseOne : myExpr] [caseTwo : myExpr]) : myExpr
  (cond
    [(> (interp n (list ) (list )) 0) caseOne]
    [else caseTwo]))


; subst : myExpr myExpr myExpr -> myExpr
; Substitutes identifiers in functions
; Examples
; (numE 5) 'x (powE (numE 9) (numE 2)) -> (powE (numE 9) (numE 2))
; (numE 5) 'x (addE (idE 'x) (idE 'x)) -> (addE (numE 5) (numE 5))
; (numE 5) 'x (mulE (numE 8) (idE 'x))) -> (mulE (numE 8) (numE 4)))
(define (subst [what : myExpr] [for : symbol] [in : myExpr]) : myExpr
  (type-case myExpr in
    [numE (n) in]
    [idE (s) (cond
               [(symbol=? s for) what]
               [else in])]
    [appE (func arg) (appE func (list (subst what for (first arg))))]
    [addE (lhs rhs) (addE (subst what for lhs)
                        (subst what for rhs))]
    [mulE (lhs rhs) (mulE (subst what for lhs)
                        (subst what for rhs))]
    [powE (lhs rhs) (powE (subst what for lhs)
                        (subst what for rhs))]
    [negE (lhs) (negE (subst what for lhs))]
    [gtzE (n caseOne caseTwo) (greaterThanZeroHelper (subst what for (numE n)) (subst what for caseOne) (subst what for caseTwo))]))

; parse : s-expression -> myExpr
; Parses an s-expression and outputs as a myExpr
; Examples
; '5 -> (numE 5)
; '-1 -> (numE -1)
; '(+ 5 3) -> (addE (numE 5) (numE 3))
; '(+ 2 6) -> (addE (numE 2) (numE 6))
; '(- 5 3) -> (addE (numE 5) (negE (numE 3)))
; '(- 5) -> (negE (numE 5))
; '(* 5 3) -> (mulE (numE 5) (numE 3))
; '(* 2 6) -> (mulE (numE 2) (numE 6))
; '(** 5 3) -> (powE (numE 5) (numE 3))
; '(** 2 6) -> (powE (numE 2) (numE 6))
; '(gtz 1 (** 5 3) (** 1 4)) -> (gtzE 1 (powE (numE 5) (numE 3)) (powE (numE 1) (numE 4)))
; '(gtz -1 (** 5 3) (** 1 4)) -> (gtzE 1 (powE (numE 5) (numE 3)) (powE (numE 1) (numE 4)))
(define (parse [s : s-expression]) : myExpr
  (cond
    [(s-exp-number? s) (numE (s-exp->number s))]
    [(s-exp-symbol? s) (idE (s-exp->symbol s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (addE (parse (second sl)) (parse (third sl)))]
         [(*) (mulE (parse (second sl)) (parse (third sl)))]
         [(**) (powE (parse (second sl)) (parse (third sl)))]
         [(-) (negHelperMyExpr sl)]
         [(gtz) (gtzE (s-exp->number (second sl)) (parse (third sl)) (parse (fourth sl)))]
         [else (error 'parse "invalid list input")]))]
    [else (error 'parseSug "invalid input")]))

; negHelperMyExpr : (listof s-expression) -> myExpr
; Handles negative case for myExpr
; Examples
; '(- 5 3) -> (addE (numE 5) (negE (numE 3)))
; '(- 5) -> (negE (numE 5))
; Template
; (define (negHelperMyExpr [sl : (listof s-expression)]) : myExpr
;   (cond
;     [(< (length sl) 3) (negE ...(second sl))
;     [else (addE ...(second sl) (negE ...(third sl)))]))
(define (negHelperMyExpr [sl : (listof s-expression)]) : myExpr
  (cond
    [(< (length sl) 3) (negE (parse (second sl)))]
    [else (addE (parse (second sl)) (negE (parse (third sl))))]))


(define factorialHelper (λ (n acc) (cond
                           [(> n 1) (factorialHelper (- n 1) (* n acc))]
                           [else acc])))
(define factorial (λ (n) (gtzE n (numE (factorialHelper n 1)) (numE 1))))

;;;;;;;;;; TESTS BELOW ;;;;;;;;;;
;; NegHelperMyExpr
(test (negHelperMyExpr (s-exp->list '(- 5 3))) (addE (numE 5) (negE (numE 3))))
(test (negHelperMyExpr (s-exp->list '(- 5))) (negE (numE 5)))




;; parse Tests
(test (parse '5) (numE 5))
(test (parse '-1) (numE -1))
(test (parse '(+ 5 3)) (addE (numE 5) (numE 3)))
(test (parse '(- 5 3)) (addE (numE 5) (negE (numE 3))))
(test (parse '(- 5)) (negE (numE 5)))
(test (parse '(+ 2 6)) (addE (numE 2) (numE 6)))
(test (parse '(* 5 3)) (mulE (numE 5) (numE 3)))
(test (parse '(* 2 6)) (mulE (numE 2) (numE 6)))
(test (parse '(** 5 3)) (powE (numE 5) (numE 3)))
(test (parse '(** 2 6)) (powE (numE 2) (numE 6)))
(test (parse '(gtz 1 (** 5 3) (** 1 4))) (gtzE 1 (powE (numE 5) (numE 3)) (powE (numE 1) (numE 4))))
(test (parse '(gtz -1 (** 5 3) (** 1 4))) (gtzE -1 (powE (numE 5) (numE 3)) (powE (numE 1) (numE 4))))



