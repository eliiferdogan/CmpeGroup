#lang plai-typed
;Formal Grammar

; Alphabet {+,-,*,**,id, {,}}
; T -> Terminal symbols 
; N -> NonTerminal symbols {}
; S -> Start Point
; P -> Production Rules :
                  ; exp -> number
                  ; exp -> symbol
                  ; exp -> + exp exp
                  ; exp -> - exp exp
                  ; exp -> * exp exp
                  ; exp -> ** exp exp
                  ; exp -> -1 * exp (The best one without problem) (Uniary Minus)
                  ; exp -> (exp)

       ;; Function definition
       ; F is a function
       ; Ls is a list of parameters
       ; B is body
       ; F -> (Name)Ls{B}
       ; Name -> symbol
       ; B-> exp
       ; Ls-> listOfSymbols

       ;; Function Application
       ;Fa is function application
       ;Fs is a function symbol
       ;La is a list of arguments
       ;Fa -> FsLa
       ;La  -> listOfSymbols
       ;Fs -> symbol

(define-type ExprC
  [numC (n : number)]
  [idC (s : symbol)]
  [appC (fun : symbol) (arg : (listof ExprC))]
  [plusC (l : ExprC) (r : ExprC)]
  [subC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [igz (exp1 : ExprC) (exp2 : ExprC) (exp3 : ExprC)]
  [factC (x : number)]
  [factaccC (x : number) (acc : number)]
  [fibC (x : number)]
  [fibaccC (x : number) (acc : number)]
  )
;Extended data definition ,,Function data definition
;; Function Definition with multiple parameters. 
(define-type FunDefC
  [fdC (name : symbol) (arg : (listof symbol))  (body : ExprC)])

;; parse : s-exp -> ExprC
;; Purpose : To parse given s-exp to ExprC form

(define (parse [s :  (listof s-expression)]) : ExprC
  (cond
    [(s-exp-number? s) (numC (s-exp->number s))]
    [(s-exp-symbol? s) (idC (s-exp->symbol s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)]) 
       (cond
         [(= (length sl) 4)
          (if (symbol=? 'igz (s-exp->symbol (first sl)))
              (igz (parse (second sl))
                       (parse (third sl))
                       (parse (fourth sl)))
              (error 'parse "invalid expression as input"))]
         [(= (length sl) 3)
          (case (s-exp->symbol (first sl))
            [(+) (plusC (parse (second sl)) (parse (third sl)))]
            [(*) (multC (parse (second sl)) (parse (third sl)))]
            [(-) (subC (parse (second sl)) (parse (third sl)))]
            [else (error 'parse "invalid list input")]
            )]
         [(= (length sl) 2)
          (appC (s-exp->symbol (first sl)) (parse (second sl)))]
         [else (error 'parse "invalid list input")])
       )]
    [else (error 'parse "invalid input")]))

; Parser Tests :
(test (parse (number->s-exp 5))(numC 5))
(test (parse (symbol->s-exp 'x))(idC 'x))
(test (parse '(+ 3 4))(plusC (numC 3)(numC 4)))
(test (parse '(* 3 4))(multC (numC 3)(numC 4)))
(test (parse '(+ x x))(plusC (idC 'x)(idC 'x)))
(test (parse '(* x x))(multC (idC 'x)(idC 'x)))
(test (parse '(f (* x x)))(appC 'f (multC (idC 'x)(idC 'x))))