#lang racket
(require racket)

#|...and Again, and Again, and Again, ...|#


(define (pick n lat)
  (match n
    [1 (first lat)]
    [_ (pick (sub1 n) (rest lat))]))


(define (looking a lat)
  (keep-looking a (pick 1 lat) lat))

; (looking 'caviar '(6 2 4 caviar 5 7 3)) -> #t
; =>
; (keep-looking 'caviar (pick 1 '(6 2 4 caviar 5 7 3)) '(6 2 4 caviar 5 7 3)) -> #t
; (keep-looking 'caviar 6 '(6 2 4 caviar 5 7 3)) -> #t
; (pick 6 '(6 2 4 caviar 5 7 3)) -> 3
; (pick 3 '(6 2 4 caviar 5 7 3)) -> 4
; (pick 4 '(6 2 4 caviar 5 7 3)) -> 'caviar

; my version, also handles numbers
#;
(define (keep-looking a choice lat)
  (let ([new-choice (pick choice lat)])
    (cond [(eq? new-choice a) #t]
          [(number? new-choice)
           (keep-looking a new-choice lat)]
          [else #f])))

(looking 'caviar '(6 2 4 caviar 5 7 3))
(looking 'caviar '(6 2 grits caviar 5 7 3))
(looking 3 '(6 2 grits caviar 5 7 3))

; book version
(define (keep-looking a sorn lat)
  (cond [(number? sorn) (keep-looking a (pick sorn lat) lat)]
        [else (eq? sorn a)]))

(define (eternity x) (eternity x))

#;
(define (∞) (∞))
; (∞)

(define (build s1 s2)
  (cons s1 (cons s2 '())))

(define (shift pair)
  (build (first (first pair))
         (build (second (first pair))
                (second pair))))

(shift '((a b) (c d)))
(shift '(((a b) c) (d (e f))))

(define (atom? x)
  (and (not (pair? x))
       (not (null? x))))

(define (a-pair? x)
  (eq? (length* x) 2))

(define (align pora)
  (cond [(atom? pora) pora]
        [(a-pair? (first pora))
         (align (shift pora))]
        [else
         (build (second (first pora))
                (second pora))]))

(align '((a b) (c d)))
(align '((a (b c))(d e)))
(align '(((a b) c) (d (e f))))

(define (length* pora)
  (cond [(null? pora) 0]
        [(atom? pora) 1]
        [else
         (+ (length* (first pora))
            (length* (rest pora)))]))

(length* '((a b) (c d)))

(define (weight* pora)
  (cond [(atom? pora) 1]
        [else
         (+ (* (weight* (first pora)) 2)
            (weight* (second pora)))]))

(weight* '((a b) c))
(weight* '(a (b c)))

(define (revpair pair)
  (build (second pair) (first pair)))

(define (shuffle pora)
  (cond [(atom? pora) pora]
        [(a-pair? (first pora))
         (shuffle (revpair pora))]
        [else
         (build (first pora)
                (shuffle (second pora)))]))

(shuffle '((a b) (c d)))
(shuffle '((a (b c))(d e)))       ; no-op
(shuffle '(((a b) c) (d (e f))))  ; no-op

(define (collatz n [curr-step 1])
  (begin
    (displayln (format "number: ~a\tstep: ~a" n curr-step))
    (cond [(eq? 1 n) 1]
          [(even? n) (collatz (quotient n 2) (add1 curr-step))]
          [else (collatz (add1 (* 3 n)) (add1 curr-step))])))

(collatz 5600000000000000)

(define (ackermann n m)
  (cond [(zero? n) (add1 m)]
        [(zero? m) (ackermann (sub1 n) 1)]
        [else
         (ackermann (sub1 n) (ackermann n (sub1 m)))]))

(ackermann 1 0)
(ackermann 1 1)
(ackermann 2 2)
(ackermann 3 5)

(define (will-stop? f)
  (when (null? (f '())) #t))

(define (last-try x)
  (and (will-stop? last-try)
       (eternity x)))

((λ (length)
   (λ (l)
     (cond ((null? l) 0)
           (else
            (add1 (length (cdr l)))))))
 eternity)

((λ (f)
   (λ (l)
     (cond ((null? l) 0)
           (else (add1 (f (cdr l)))))))
 ((λ (g)
    (λ (l)
      (cond ((null? l) 0)
            (else (add1 (g (cdr l)))))))
  eternity))

(((λ (f)
   (λ (l)
     (cond ((null? l) 0)
           (else (add1 (f (cdr l)))))))
 ((λ (g)
    (λ (l)
      (cond ((null? l) 0)
            (else (add1 (g (cdr l)))))))
  eternity))
 '(a))

(((λ (length)
   (λ (l)
     (cond ((null? l) 0)
           (else
            (add1 (length (cdr l)))))))
 ((λ (length)
    (λ (l)
      (cond ((null? l) 0)
            (else
             (add1 (length (cdr l)))))))
  ((λ (length)
     (λ (l)
       (cond ((null? l) 0)
             (else
              (add1 (length (cdr l)))))))
   eternity)))
 '(a b))

(((λ (mk-length)
   (mk-length eternity))
 (λ (length)
   (λ (l)
     (cond ((null? l) 0)
           (else (add1 (length (cdr l))))))))
 '())

(((λ (mk-length)
   (mk-length
    (mk-length eternity)))
  (λ (length)
    (λ (l)
      (cond ((null? l) 0)
            (else (add1 (length (cdr l))))))))
 '(a))

(((λ (mk-length)
   (mk-length
    (mk-length
     (mk-length eternity))))
  (λ (length)
    (λ (l)
      (cond ((null? l) 0)
            (else (add1 (length (cdr l))))))))
 '(a b))

(((λ (mk-length)
   (mk-length mk-length))
 (λ (length)
   (λ (l)
     (cond ((null? l) 0)
           (else (add1 (length (cdr l))))))))
 '())

(((λ (mk-length)
   (mk-length mk-length))
 (λ (mk-length)
   (λ (l)
     (cond ((null? l) 0)
           (else (add1 ((mk-length eternity)
                        (cdr l))))))))
 '(a))

(((λ (mk-length)
   (mk-length mk-length))
 (λ (mk-length)
   (λ (l)
     (cond ((null? l) 0)
           (else (add1 ((mk-length mk-length)
                        (cdr l))))))))
 '(a))

(((λ (mk-length)
   (mk-length mk-length))
 (λ (mk-length)
   ((λ (length)
      (λ (l)
        (cond ((null? l) 0)
              (else (add1 (length (cdr l)))))))
    (mk-length mk-length)))) '())

(((λ (mk-length)
   (mk-length mk-length))
 (λ (mk-length)
   ((λ (length)
      (λ (l)
        (cond ((null? l) 0)
              (else (add1 (λ (x) ((mk-length mk-length) x)))))))
    (mk-length mk-length)))) '())

((λ (mk-length)
   (mk-length mk-length))
 (λ (mk-length)
   ((λ (length)
      (λ (l)
        (cond ((null? l) 0)
              (else
               (add1 (length (cdr l)))))))
    (λ (x)
      ((mk-length mk-length) x)))))

(λ (le)
  ((λ (mk-length)
     (mk-length mk-length))
   (λ (mk-length)
     (le (λ (x)
           ((mk-length mk-length) x))))))

(define Y
  (λ (le)
    ((λ (f)
       (f f))
     (λ (f)
       (le (λ (x)
             ((f f) x)))))))

(Y Y)

#|Does your hat still fit?|#
#|Perhaps not after such a mind stretcher|#
