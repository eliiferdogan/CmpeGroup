#lang racket
(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(define cosine-series
  (cons-stream 1 <??>))
(define sine-series
  (cons-stream 0 <??>))


(define (mul-series s1 s2)
  (cons-stream <??> (add-streams <??> <??>)))