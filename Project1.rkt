;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Project1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
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
