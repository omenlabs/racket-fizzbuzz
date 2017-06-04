#lang racket

(define (my-if test truth falseness)
  (cond
    [test truth]
    [else falseness]))

;; This works fine
(printf "Calling displayln on the result of my-if:~n~n")
(displayln (my-if (eq? 1 3) "one is three" "one is not three"))

;; But if we invert the printing
(printf "~nInverting by passing displayln as my-if args:~n~n")
(my-if (eq? 1 3)
       (displayln "one is three")
       (displayln "one is not three"))

(printf "~nMacro my-macro-if with displayln args:~n~n")
;; So macro
(define-syntax my-macro-if
  (syntax-rules ()
    [(_ test-expr true-expr false-expr)
     (cond
       (test-expr true-expr)
       (else false-expr))]))

(my-macro-if (eq? 1 3)
       (displayln "one is three")
       (displayln "one is not three"))
