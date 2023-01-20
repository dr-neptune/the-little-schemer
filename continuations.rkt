#lang racket
(require racket)

(display
 (call/cc (λ (cc)
            (display "I got here.\n")
            (cc "This string was passed to the continuation.\n")
            (display "but not here.\n"))))

(let ([start #f])
  (when (not start)
    (call/cc (λ (cc) (set! start cc))))
  (display "going to invoke (start)\n")
  (start))

(current-continuation)

; right-now : -> moment
(define (right-now)
  (call-with-current-continuation
   (λ (cc) (cc cc))))

; go-when : moment -> ...
(define (go-when then)
  (then then))

; an infinite loop
(let ([the-beginning (right-now)])
  (display "Hello, world!")
  (newline)
  (go-when the-beginning))


; add up numbers
(let ([curr-count 0])
  (let ([up-top (right-now)])
    (println (format "count: ~a" curr-count))
    (set! curr-count (add1 curr-count))
    (go-when up-top)))


;; implement some kind of break
(let ([curr-count 0])
  (let ([placement (right-now)])
    (if (< curr-count 10)
        (begin
          (println curr-count)
          (set! curr-count (add1 curr-count))
          (go-when placement))
        (println "success!"))))


; current-continuation : -> continuation
(define (current-continuation)
  (call-with-current-continuation
   (λ (cc) (cc cc))))

; fail-stack : list[continuation]
(define fail-stack '())

; fail : -> ...
(define (fail)
  (if (not (pair? fail-stack))
      (error "back-tracking stack exhausted")
      (begin
        (let ([back-track-point (car fail-stack)])
          (set! fail-stack (cdr fail-stack))
          (back-track-point back-track-point)))))

(define (amb choices)
  (let ([cc (current-continuation)])
    (cond [(null? choices) (fail)]
          [(pair? choices)
           (let ([choice (car choices)])
             (set! choices (cdr choices))
             (set! fail-stack (cons cc fail-stack))
             choice)])))

; (assert condition) will cause
; condition to be true, and if there
; is no way to make it true, then
; it signals an error in the program
(define (assert condition)
  (if (not condition)
      (fail)
      #t))

(let ([a (amb (list 1 2 3 4 5 6 7))]
      [b (amb (list 1 2 3 4 5 6 7))]
      [c (amb (list 1 2 3 4 5 6 7))])
  (assert (= (* c c) (+ (* a a) (* b b))))
  (display (list a b c))
  (newline)
  (assert (< b a))
  (display (list a b c))
  (newline))

;; SAT-solving with amb
(define (implies a b) (or (not a) b))


(define-syntax sat-solve
  (syntax-rules (and or implies not)
    ((_ formula body)
     ; =>
     (sat-solve formula body formula))

    ((_ (not phi) body assertion)
     ; =>
     (sat-solve phi body assertion))

    ((_ (and phi) body assertion)
     ; =>
     (sat-solve phi body assertion))

    ((_ (and phi1 phi2 ...) body assertion)
     ; =>
     (sat-solve phi1 (sat-solve (and phi2 ...) body assertion)))

    ((_ (or phi) body assertion)
     ; =>
     (sat-solve phi body assertion))

    ((_ (or phi1 phi2 ...) body assertion)
     ; =>
     (sat-solve phi1 (sat-solve (or phi2 ...) body assertion)))

    ((_ (implies phi1 phi2) body assertion)
     ; =>
     (sat-solve phi1 (sat-solve phi2 body assertion)))

    ((_ #t body assertion)
     ; =>
     body)

    ((_ #f body assertion)
     ; =>
     (fail))

    ((_ v body assertion)
     (let ((v (amb (list #t #f))))
       (if (not assertion)
           (fail)
           body)))))


(display
 (sat-solve (and (implies a (not b)) (not a) c) (list a b c)))

;; concensus theorem
;; https://math.stackexchange.com/a/2435421
(display
 (sat-solve (implies (and (or a b) (or (not b) c))
                     (and (or a b) (or (not b) c) (or a c))) (list a b c)))

;; Exception Handling

; exception-stack : list[continuation]
(define exception-stack '())

; (try exp ... catch catch-procedure) runs
; exp ..., and invokes catch-procedure with
; the value passed to throw
(define-syntax try
  (syntax-rules (catch)
    ((_ exp ... catch proc)
     ; =>
     (let ((cc (current-continuation)))
       (cond
         ((procedure? cc)
          (dynamic-wind (λ () (set! exception-stack (cons cc exception-stack)))
                        (λ () exp ...)
                        (λ () (set! exception-stack (cdr exception-stack)))))
         ((pair? cc)
          (proc (cadr cc))))))))

(define (throw exception-value)
  (let ((handler (car exception-stack)))
    (handler (list 'exception exception-value))))

(try (try (throw 'foo)
          catch
          (λ (exn)
            (display (format "got inner exception ~a" exn))
            (newline)
            (throw 'bar)))
     catch
     (λ (exn)
       (display (format "got outer exception ~a" exn))
       (newline)))

;; Generators
