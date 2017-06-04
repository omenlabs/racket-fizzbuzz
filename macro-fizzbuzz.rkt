#lang racket

;; Non-macro, wet way to do a scheme fizzbuzz
(define (notfunzzer n)
  (cond
    ((and (eq? (modulo n 3) 0) (eq? (modulo n 5) 0)) (displayln "fizzbuzz"))
    ((eq? (modulo n 3) 0) (displayln "fizz"))
    ((eq? (modulo n 5) 0) (displayln "buzz"))
    (else
     (displayln n))))

;;(for ([i (in-range 1 101)]) (notfunzzer i))

;; The 'cond-all' syntatic extension is a version of cond that will run
;; all 'then' expressions when the test expression is true, instead of
;; returning the then-expression result of the first true test expression.
;;
;; The otherwise-expression is run only if no test expressions returned true.
;;
;; We also define an always auxiliary keyword for something that is always
;; run after evaluating all the test/then pairs.
(define-syntax cond-all
  (syntax-rules (otherwise always)
    [(_ (test-expr then-expr) ... (otherwise otherwise-expr) (always always-expr))
     (begin
       (let [(do_otherwise #t)]
         (if test-expr
             (begin
               (set! do_otherwise #f)
               then-expr)
             (void)) ...
       (if do_otherwise otherwise-expr (void))
       always-expr))
       ]))

;;
;; Now we can write a fizzbuzz that does not violate DRY
;;
(define (fizzer n)
  (cond-all
   [(eq? (modulo n 3) 0) (display "fizz")]
   [(eq? (modulo n 5) 0) (display "buzz")]
   [otherwise (display n)]
   [always (displayln "")]))

(for ([i (in-range 1 101)]) (fizzer i))