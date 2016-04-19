#lang plai-typed

;; Grammar
;; Λ -> v
;; Λ -> (Λ Λ)
;; Λ -> (λ v Λ)
(define-type λ
  [λ-var (name : symbol)]
  [λ-fnc (name : symbol) (expr : λ)]
  [λ-app (lhs : λ) (rhs : λ)])

;; parse : s-expression -> λ
;; Examples
;; 'x -> (λ-var 'x)
;; '(x y) -> (λ-app (λ-var 'x) (λ-var 'y))
;; '(λ x y) -> (λ-fnc 'x (λ-var 'y))
;; '((λ x y) (x y)) -> (λ-app (λ-fnc 'x (λ-var 'y)) (λ-app (λ-var 'x) (λ-var 'y)))
;; Template
;; (define (parse [expr : s-expression]) : λ
;;   (cond
;;     [(s-exp-symbol? expr) (λ-var (... expr))]
;;     [(s-exp-list? expr)
;;      (let ([sl (...expr)])
;;        (cond
;;          [(> (length sl) 2) (λ-fnc (... (second sl)) (parse ...(third sl)))]
;;          [else (λ-app (parse ...(first sl)) (parse ...(second sl)))])
;;        )]
;;     [else (error 'parse ...)]))
(define (parse [expr : s-expression]) : λ
  (cond
    [(s-exp-symbol? expr) (λ-var (s-exp->symbol expr))]
    [(s-exp-list? expr)
     (let ([sl (s-exp->list expr)])
       (cond
         [(> (length sl) 2) (λ-fnc (s-exp->symbol (second sl)) (parse (third sl)))]
         [(= (length sl) 1) (λ-var (s-exp->symbol (first sl)))]
         [else (λ-app (parse (first sl)) (parse (second sl)))])
       )]
    [else (error 'parse "Wrong input.")]))
;; Parse Tests
(test (parse (symbol->s-exp 'x)) (λ-var 'x))
(test (parse '(x y)) (λ-app (λ-var 'x) (λ-var 'y)))
(test (parse '(λ x y)) (λ-fnc 'x (λ-var 'y)))
(test (parse '((λ x y) (x y))) (λ-app (λ-fnc 'x (λ-var 'y)) (λ-app (λ-var 'x) (λ-var 'y))))

;; Examples
;; (λ-var 'x) -> 'x
;; (λ-app (λ-var 'x) (λ-var 'y)) -> '(x y)
;; (λ-fnc 'x (λ-var 'y)) -> '(λ x y)
;; (λ-app (λ-fnc 'x (λ-var 'y)) (λ-app (λ-var 'x) (λ-var 'y))) -> '( (λ x y) (x y) )
;; Template
;; (define (unparse [lmbd : λ]) : s-expression
;;   (type-case λ lmbd
;;     [λ-var (name) (... name)]
;;     [λ-fnc (name expr) (list->s-exp (list (... 'λ) (... name) (... expr)))]
;;     [λ-app (lhs rhs) (list->s-exp (list (... lhs) (... rhs)))]))
(define (unparse [lmbd : λ]) : s-expression
  (type-case λ lmbd
    [λ-var (name) (symbol->s-exp name)]
    [λ-fnc (name expr) (list->s-exp (list (symbol->s-exp 'λ) (symbol->s-exp name) (unparse expr)))]
    [λ-app (lhs rhs) (list->s-exp (list (unparse lhs) (unparse rhs)))]))
;; Unparse Tests
(test (unparse (λ-var 'x)) (symbol->s-exp 'x))
(test (unparse (λ-app (λ-var 'x) (λ-var 'y))) '(x y))
(test (unparse (λ-fnc 'x (λ-var 'y))) '(λ x y))
(test (unparse (λ-app (λ-fnc 'x (λ-var 'y)) (λ-app (λ-var 'x) (λ-var 'y))))'((λ x y) (x y)))