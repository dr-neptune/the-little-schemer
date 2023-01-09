#lang racket
(require racket)

(define (atom? x)
  (and (not (pair? x))
       (not (null? x))))

(define (rember* a l)
  (cond [(null? l) '()]
        [(atom? (first l))
         (if (eq? a (first l))
             (rember* a (rest l))
             (cons (first l)
                   (rember* a (rest l))))]
        [else
         (cons (rember* a (car l))
               (rember* a (cdr l)))]))

(rember* 'sauce '(((tomato sauce))
                  ((bean) sauce)
                  (and ((flying)) sauce)))

(define (match/rember* a l)
  (match l
    ['() '()]
    [(list* (? atom? head) tail)
     #:when (eq? a head)
     (match/rember* a tail)]
    [(list* (? atom? head) tail)
     (cons head (match/rember* a tail))]
    [_
     (cons (match/rember* a (first l))
           (match/rember* a (rest l)))]))

(match/rember* 'sauce '(((tomato sauce))
                        ((bean) sauce)
                        (and ((flying)) sauce)))

(define (try-it l)
  (match l
    ['() '()]
    [(list* (? odd? head) tail)
     (begin
       (println (format "odd: ~a tail: ~a" head tail))
       (try-it (rest l)))]
    [(list* (? even? head) tail)
     (begin
       (println (format "even: ~a tail: ~a" head tail))
       (try-it (rest l)))]))

(try-it '(1 2 3 4 5))

(define (try-it l)
  (match l
    ['() '()]
    [(list* head tail)
     #:when (even? head)
     (begin
       (println (format "odd: ~a tail: ~a" head tail))
       (try-it (rest l)))]
    [(list* head tail)
     #:when (odd? head)
     (begin
       (println (format "even: ~a tail: ~a" head tail))
       (try-it (rest l)))]))

(try-it '(1 2 3 4 5))
;; (? p? expr) is syntactic sugar for #:when most of the time
;; sometimes it works where when doesn't, like when using
;; `not` around a pattern

(match/rember* 'sauce '(((tomato sauce))
                        ((bean) sauce)
                        (and ((flying)) sauce)))

(define (insertR* new old l)
  (cond [(null? l) '()]
        [(atom? (first l))
         (if (eq? (first l) old)
             (cons old (cons new (insertR* new old (rest l))))
             (cons (first l) (insertR* new old (rest l))))]
        [else
         (cons (insertR* new old (first l))
               (insertR* new old (rest l)))]))

(insertR* 'roast 'chuck
          '((how much (wood))
            could
            ((a (wood) chuck))
            (((chuck)))
            (if (a) ((wood chuck)))
            could chuck wood))

(define (match/insertR* new old l)
  (match l
    ['() '()]
    [(list* (== old) tail)
     (cons old (cons new (match/insertR* new old tail)))]
    [(list* (? atom? head) tail)
     (cons head (match/insertR* new old tail))]
    [_ (cons (match/insertR* new old (first l))
             (match/insertR* new old (rest l)))]))

(match/insertR* 'roast 'chuck
                '((how much (wood))
                  could
                  ((a (wood) chuck))
                  (((chuck)))
                  (if (a) ((wood chuck)))
                  could chuck wood))

(define (occur* a l)
  (cond [(null? l) 0]
        [(atom? (first l))
         (if (eq? a (first l))
             (add1 (occur* a (rest l)))
             (occur* a (rest l)))]
        [else
         (+
          (occur* a (first l))
          (occur* a (rest l)))]))

(occur* 'banana
        '((banana)
          (split ((((banana ice)))
                  (cream (banana))
                  sherbet))
          (banana)
          (bread)
          (banana brandy)))
