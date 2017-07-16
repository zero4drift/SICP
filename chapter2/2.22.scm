;; due to the statement:
;; (iter (cdr things) (cons (square (car things)) answer))

;; what's worse, the output is not a list
;; with iteration, the second parameter of iter
;; always contains the square of the beginning element of items
