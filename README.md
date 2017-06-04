# racket-fizzbuzz

This exercise was to kick the tires on Scheme's macro facilities by
introducing a new control syntax to enable doing fizzbuzz in
a [DRY](https://en.wikipedia.org/wiki/Don%27t_repeat_yourself)
fashion.

From the [c2 wiki](http://wiki.c2.com/?FizzBuzzTest):

> I think Fizz-Buzz is "hard" for some programmers because (#1) it
> doesn't fit into any of the patterns that were given to them in school
> assignments, and (#2) it isn't possible to directly and simply
> represent the necessary tests, without duplication, in just about any
> commonly-used modern programming language.  ```

Here we are going after #2.  One of the fundamental control structures
in Scheme is `cond`.  We can write a fizzbuzz using `cond` easily:

```racket
;; Non-macro way
(define (notfunzzer n)
  (cond
    ((and (eq? (modulo n 3) 0) (eq? (modulo n 5) 0)) (displayln "fizzbuzz"))
    ((eq? (modulo n 3) 0) (displayln "fizz"))
    ((eq? (modulo n 5) 0) (displayln "buzz"))
    (else
     (displayln n))))

(for ([i (in-range 1 101)]) (notfunzzer i))
```

But, we are doing a terrible job
of [DRY](https://en.wikipedia.org/wiki/Don%27t_repeat_yourself) here.
We do the modulo check twice and fizz/buzz both occur twice.  This is
truly sub-optimal!  What if we could do something a little different
than the traditional if/elif/else style control structure.  It turns
out that defining our own control structures is easy in Scheme.

## DIY Control structures

Since Scheme has eager evaluation, we need to use a macro to write any
custom control syntax.  Let's do a quick example that demonstrates
this.  I can define a function called `my-if` that uses `cond` to
implement Scheme's `if` statement.

```racket
(define (my-if test truth falseness)
  (cond
    [test truth]
    [else falseness]))
```

We can test the function

```racket
(printf "Calling displayln on the result of my-if:~n~n")
(displayln (my-if (eq? 1 3) "one is three" "one is not three"))
```

which will output:

```
Calling displayln on the result of my-if:

one is not three
```

However, since the arguments to `my-if` are evaluated before the function 
is applied, we will end up in trouble if our functions have side effects.
If we move the `displayln` into the arguments for `my-if`

```racket
(printf "~nInverting by passing displayln as my-if args:~n~n")
(my-if (eq? 1 3)
       (displayln "one is three")
       (displayln "one is not three"))
```

both messages will print:

```
Inverting by passing displayln as my-if args:

one is three
one is not three
```

So we need to use a macro to write `my-if`:

```racket
(define-syntax my-macro-if
  (syntax-rules ()
    [(_ test-expr true-expr false-expr)
     (cond
       (test-expr true-expr)
       (else false-expr))]))
```

Using `my-macro-if` the code is rewritten before evaluation, running
this,

```racket
(my-macro-if (eq? 1 3)
       (displayln "one is three")
       (displayln "one is not three"))
```

yields the correct result:

```
Macro my-macro-if with displayln args:

one is not three
```

## A new control structure

What if `cond` ran the 'then' part of every true test case, rather
than returning the 'then' of the first true test?  Also, what if we
had a way to always run something at the end of the `cond` like
`finally` in a Python try/except?  I will call this `cond-all`.

Here is the macro for `cond-all` that lets us create a DRY fizzbuzz
instead of the initial wet one:

```racket
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
```

We can now write a DRY fizzbuzz:

```racket
(define (fizzer n)
  (cond-all
   [(eq? (modulo n 3) 0) (display "fizz")]
   [(eq? (modulo n 5) 0) (display "buzz")]
   [otherwise (display n)]
   [always (displayln "")]))

(for ([i (in-range 1 101)]) (fizzer i))
```
