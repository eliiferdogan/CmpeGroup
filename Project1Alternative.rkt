;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Project1(Alternative)|) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")))))
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