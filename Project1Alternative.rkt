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

