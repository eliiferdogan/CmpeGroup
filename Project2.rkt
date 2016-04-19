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
;; Parse Unparse Combined Tests
(test (parse (unparse (λ-var 'x))) (λ-var 'x))
(test (parse (unparse (λ-app (λ-var 'x) (λ-var 'y)))) (λ-app (λ-var 'x) (λ-var 'y)))
(test (parse (unparse (λ-fnc 'x (λ-var 'y)))) (λ-fnc 'x (λ-var 'y)))
(test (parse (unparse (λ-app (λ-fnc 'x (λ-var 'y)) (λ-app (λ-var 'x) (λ-var 'y))))) (λ-app (λ-fnc 'x (λ-var 'y)) (λ-app (λ-var 'x) (λ-var 'y))))

;; Free Variables
;; getFreeVariables : λ -> (listof symbol)
;; Examples
;; (λ-var 'x) -> {x}
;; (λ-fnc 'x (λ-var 'x)) -> {}
;; (λ-fnc 'x (λ-var 'y)) -> {y}
;; (λ-fnc 'x (λ-fnc 'y (λ-var 'x))) -> {}
;; (λ-fnc 'x (λ-fnc 'y (λ-var 'z))) -> {z}
;; (λ-app (λ-var 'x) (λ-var 'y)) -> {'x 'y}
;; Template
;;(define (getFreeVariables [lmbd : λ]) : (listof symbol)
;;  (type-case λ lmbd
;;    [λ-var (name) (cons...name)]
;;    [λ-fnc (name expr) (if (member ...name ...expr) ...empty (cons...name))]
;;    [λ-app (lhs rhs) (append ...lhs ...rhs)]))
(define (getFreeVariables [lmbd : λ]) : (listof symbol)
  (type-case λ lmbd
    [λ-var (name) (cons name empty)]
    [λ-fnc (name expr) (if (member name (getFreeVariables (λ-fnc-expr lmbd))) (removeElement name (getFreeVariables (λ-fnc-expr lmbd)) empty) (getFreeVariables (λ-fnc-expr lmbd)))]
    [λ-app (lhs rhs) (uniqueAppend (getFreeVariables lhs) (getFreeVariables rhs))]))

;; removeElement : (listof symbol) -> symbol
;; Remove element from list if exists
;; Examples
;; { } 'x -> {}
;; {'x} 'x -> {}
;; {'x 'y} 'x -> {'y}
;; {'x 'y} 'z -> {'x 'y}
;; Template
;;(define (removeElement [elem : symbol] [l : (listof symbol)] [cur : (listof symbol)]) : (listof symbol)
;;  (cond
;;    [(empty? l) ...cur]
;;    [else (if (equal? (first l) elem ) ... ...)]
;;    ))
(define (removeElement [elem : symbol] [l : (listof symbol)] [cur : (listof symbol)]) : (listof symbol)
  (cond
    [(empty? l) cur]
    [else (if (equal? (first l) elem) (removeElement elem (rest l) cur) (removeElement elem (rest l) (cons (first l) cur)) )]
    ))

;; uniqueAppend : (listof symbol) (listof symbol) -> (listof symbol)
;; Append two lists' unique elements
;; Examples
;; {'x 'y} {'x 'y} -> {'x 'y}
;; {'a} { } -> {'a}
;;(define (uniqueAppend [l1 : (listof symbol)] [l2 : (listof symbol)]) : (listof symbol)
;;  (cond
;;    [(empty? l1) ...l2]
;;    [else (uniqueAppend ...(rest l1) ...l2)]
;;    ))
(define (uniqueAppend [l1 : (listof symbol)] [l2 : (listof symbol)]) : (listof symbol)
  (cond
    [(empty? l1) l2]
    [else (uniqueAppend (rest l1) (append (removeElement (first l1) l2 empty) (list (first l1))))]
    ))

;; Beta Reduction
;; betaReduction : λ-app -> λ
;; Examples
;; (λ-app (λ-fnc 'x (λ-var 'x)) (λ-fnc 'y (λ-var 'y))) -> (λ-fnc 'y (λ-var 'y))
;; (λ-app (λ-fnc 'x (λ-app (λ-var 'x) (λ-var 'x))) (λ-app (λ-var 'y) (λ-var 'y))) -> (λ-app (λ-var 'y) (λ-var 'y))
(define (betaReduction [lmbd : λ]) : λ
  (let ([M (λ-app-lhs lmbd)])
    (let ([N (λ-app-rhs lmbd)])
        (helperBetaParser (λ-fnc-name M) (λ-fnc-expr M) N)
      )
    )
  )

;; Helper Beta Parser
;; helperBetaParser : λ λ -> λ
;; Examples
;; (λ-var 'x) (λ-fnc 'y (λ-var 'y)) -> (λ-fnc 'y (λ-var 'y))
;; (λ-app (λ-var 'x) (λ-var 'x)) (λ-app (λ-var 'y) (λ-var 'y)) -> (λ-app (λ-app (λ-var 'y) (λ-var 'y)) (λ-app (λ-var 'y) (λ-var 'y)))
;; (λ-app (λ-var 'x) (λ-var 'a)) (λ-app (λ-var 'y) (λ-var 'y)) -> (λ-app (λ-app (λ-var 'y) (λ-var 'y)) (λ-var 'a))
;; Template
;;(define (helperBetaParser [v : symbol] [M : λ] [N : λ]) : λ
;;  (let ([v (λ-fnc-name M)])
;;    (type-case λ (λ-fnc-expr M)
;;      [λ-var (name) ...N]
;;      [λ-fnc (name expr) ...N]
;;      [λ-app (lhs rhs) (λ-app (helperBetaParser ...) (helperBetaParser ...))]
;;      )
;;    )
;;  )
(define (helperBetaParser [v : symbol] [M : λ] [N : λ]) : λ
  (type-case λ M
    [λ-var (name) (if (symbol=? v name) N M)]
    [λ-fnc (name expr) N]
    [λ-app (lhs rhs) (λ-app (helperBetaParser v lhs N) (helperBetaParser v rhs N))]
    )
  )

(test (uniqueAppend (list 'x 'y) (list 'x 'y)) (list 'x 'y))
(test (uniqueAppend (list 'a 'y) (list 'x 'y)) (list 'a 'x 'y))
(test (uniqueAppend (list 'a) (list )) (list 'a))

(test (removeElement 'x (list 'y) empty) (list 'y))
(test (removeElement 'x (list 'x 'y) empty) (list 'y))
(test (removeElement 'x empty empty) empty)
(test (removeElement 'x (list 'x) empty) empty)

(test (getFreeVariables (λ-fnc 'x (λ-var 'x))) empty)
(test (getFreeVariables (λ-fnc 'x (λ-var 'y))) (list 'y))
(test (getFreeVariables (λ-fnc 'x (λ-fnc 'y (λ-var 'x)))) empty)
(test (getFreeVariables (λ-fnc 'x (λ-fnc 'y (λ-var 'z)))) (list 'z))

(test (helperBetaParser 'x (λ-var 'x) (λ-fnc 'y (λ-var 'y))) (λ-fnc 'y (λ-var 'y)))
(test (helperBetaParser 'x (λ-app (λ-var 'x) (λ-var 'x)) (λ-app (λ-var 'y) (λ-var 'y))) (λ-app (λ-app (λ-var 'y) (λ-var 'y)) (λ-app (λ-var 'y) (λ-var 'y))))

(test (betaReduction (λ-app (λ-fnc 'x (λ-var 'x)) (λ-fnc 'y (λ-var 'y)))) (λ-fnc 'y (λ-var 'y)))
(test (betaReduction (λ-app (λ-fnc 'x (λ-app (λ-var 'x) (λ-var 'x))) (λ-app (λ-var 'y) (λ-var 'y)))) (λ-app (λ-app (λ-var 'y) (λ-var 'y)) (λ-app (λ-var 'y) (λ-var 'y))))
(test (betaReduction (λ-app (λ-fnc 'x (λ-app (λ-var 'x) (λ-var 'a))) (λ-app (λ-var 'y) (λ-var 'y)))) (λ-app (λ-app (λ-var 'y) (λ-var 'y)) (λ-var 'a)))

;; Tests - Quiz 5 / Q100
(test (getFreeVariables (parse '(λ x (λ y (λ z ((λ x (y y)) (λ y (x x)))))))) (list ))
(test (getFreeVariables (parse '((λ b b) b))) (list 'b))
(test (getFreeVariables (parse '(a))) (list 'a))
(test (getFreeVariables (parse '(λ x (λ y ((λ x (y y)) (λ y (x x))))))) (list ))
(test (getFreeVariables (parse '((λ b c) b))) (list 'b 'c))
(test (getFreeVariables (parse '((λ x (y y)) (λ y (x x))))) (list 'x 'y)) 
(test (getFreeVariables (parse '(λ y (λ x (y y))))) (list ))
(test (getFreeVariables (parse '(λ b ((a b)(b c))))) (list 'a 'c))
(test (getFreeVariables (parse '((λ x (x x)) (λ x (x x))))) (list ))
(test (getFreeVariables (parse '((a b)(b c)))) (list 'b 'c 'a))
(test (getFreeVariables (parse '((λ x (x x)) (λ y (y y))))) (list ))
(test (getFreeVariables (parse '(a b))) (list 'b 'a))
(test (getFreeVariables (parse '(λ a a))) (list ))
(test (getFreeVariables (parse '(λ x (λ y (y y))))) (list ))
(test (getFreeVariables (parse '(λ a b))) (list 'b))