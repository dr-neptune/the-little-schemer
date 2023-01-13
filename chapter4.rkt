#lang racket
(require racket)

(define (plus x y)
  (cond [(zero? x) y]
        [else
         (plus (sub1 x) (add1 y))]))

(define (book/plus n m)
  (cond [(zero? m) n]
        [else (add1 (book/plus n (sub1 m)))]))

(define (minus x y)
  (cond [(zero? y) x]
        [else
         (sub1 (minus x (sub1 y)))]))

(minus 5 3)
(minus 5 6)

(define (match/minus x y)
  (match y
    [0 x]
    [_ (sub1 (match/minus x (sub1 y)))]))

(match/minus 5 3)
(match/minus 5 6)

(define (addtup tup)
  (cond [(null? tup) 0]
        [else
         (+ (first tup) (addtup (rest tup)))]))

(addtup '(1 2 3 4 5))

(define (book/mult n m)
  (cond [(zero? m) 0]
        [else
         (+ n (book/mult n (sub1 m)))]))

(define (match/mult n m)
  (match m
    [0 0]
    [_ (+ n (match/mult n (sub1 m)))]))


(book/mult 4 5)
(book/mult 9 16)

(match/mult 5 4)
(match/mult 9 16)

(define (tup+ tup1 tup2)
  (cond [(null? tup1) '()]
        [else
         (cons (+ (first tup1) (first tup2))
               (tup+ (rest tup1) (rest tup2)))]))

(tup+ '(1 2 3) '(3 2 1))
(tup+ '(3 7) '(4 6))

(define (match/tup+ tup1 tup2)
  (match (list tup1 tup2)
    [(list a b)
     #:when (not (equal? (length a) (length b)))
     (displayln "Error! tup1 and tup2 must be same length")]
    [(list '() '()) '()]
    [(list (list* a atail) (list* b btail))
     (cons (+ a b)
           (match/tup+ atail btail))]))

(match/tup+ '(1 2 3) '(3 2 1))
(match/tup+ '(3 7) '(4 6))
(match/tup+ '(3 7) '(4 6 8 1))

(define (match/carry/tup+ tup1 tup2)
  (match (list tup1 tup2)
    [(list '() '()) '()]
    [(list a '()) a]
    [(list '() b) b]
    [(list (list* a atail) (list* b btail))
     (cons (+ a b)
           (match/carry/tup+ atail btail))]))

(match/carry/tup+ '(3 7) '(4 6 8 1))
(match/carry/tup+ '(3 7 8 1) '(4 6))

(define (book/carry/tup+ tup1 tup2)
  (cond [(null? tup1) tup2]
        [(null? tup2) tup1]
        [else
         (cons (+ (first tup1) (first tup2))
               (book/carry/tup+ (rest tup1) (rest tup2)))]))

(book/carry/tup+ '(3 7) '(4 6 8 1))
(book/carry/tup+ '(3 7 8 1) '(4 6))

(define (book/> m n)
  (cond [(zero? m) #f]
        [(zero? n) #t]
        [else (book/> (sub1 m) (sub1 n))]))

(book/> 12 10)
(book/> 12 100)

(define (match/> m n)
  (match (list m n)
    [(list 0 n) #f]
    [(list m 0) #t]
    [_ (match/> (sub1 m) (sub1 n))]))

(match/> 12 10)
(match/> 12 100)

(define (book/< m n)
  (book/> n m))

(book/< 12 10)
(book/< 12 100)

(define (book/= m n)
  (and (not (book/> m n))
       (not (book/< m n))))

(book/= 4 5)
(book/= 5 5)
(book/= 5 6)

(define (book/↑ m n)
  (cond [(book/= n 0) 1]
        [else
         (* m (book/↑ m (sub1 n)))]))

(book/↑ 2 3)
(book/↑ 5 3)

(define (↑ m n)
  (cond [(book/= n 0) 1]
        [else
         (* m (↑ m (sub1 n)))]))


(define (match/↑ m n)
  (match n
    [0 1]
    [_ (* m (match/↑ m (sub1 n)))]))

(match/↑ 2 3)
(match/↑ 5 3)

(define (book/÷ m n)
  (cond [(< m n) 0]
        [else
         (add1 (book/÷ (- m n) n))]))

(book/÷ 15 4)

(define (match/÷ m n)
  (match m
    [m #:when (< m n) 0]
    [_ (add1 (match/÷ (- m n) n))]))

(match/÷ 15 4)

(define (book/length lat)
  (if (null? lat) 0
      (add1 (book/length (rest lat)))))

(book/length '(hotdogs with mustard sauerkraut and pickles))

(define (match/length lat)
  (match lat
    ['() 0]
    [_ (add1 (match/length (rest lat)))]))

(match/length '(hotdogs with mustard sauerkraut and pickles))

(define (book/pick n lat)
  (cond [(eq? n 1) (first lat)]
        [else
         (book/pick (sub1 n) (rest lat))]))

(book/pick 4 '(lasagna spaghetti ravioli macaroni meatball))

(define (match/pick n lat)
  (match n
    [1 (first lat)]
    [_ (match/pick (sub1 n) (rest lat))]))

(match/pick 4 '(lasagna spaghetti ravioli macaroni meatball))

(define (book/rempick n lat)
  (cond [(eq? n 1) (cdr lat)]
        [else
         (cons (first lat) (book/rempick (sub1 n) (rest lat)))]))

(book/rempick 3 '(hotdogs with hot mustard))

(define (match/rempick n lat)
  (match n
    [1 (rest lat)]
    [_ (cons (first lat) (match/rempick (sub1 n) (rest lat)))]))

(match/rempick 3 '(hotdogs with hot mustard))

(define (book/no-nums lat)
  (cond [(null? lat) '()]
        [(number? (first lat)) (book/no-nums (rest lat))]
        [else
         (cons (first lat) (book/no-nums (rest lat)))]))

(book/no-nums '(5 pears 6 prunes 9 dates))

(define (match/no-nums lat)
  (match lat
    ['() '()]
    [(list* a tail) #:when (number? a) (match/no-nums tail)]
    [_ (cons (first lat) (match/no-nums (rest lat)))]))

(match/no-nums '(5 pears 6 prunes 9 dates))

(define (book/allnums lat)
  (cond [(null? lat) '()]
        [(not (number? (first lat))) (book/allnums (rest lat))]
        [else
         (cons (first lat) (book/allnums (rest lat)))]))

(book/allnums '(1 pear 2 peaches 3 apples))


(define (atom? x)
  (and (not (pair? x))
       (not (null? x))))


(define (eqan? a1 a2)
  (cond [(and (number? a1)
              (number? a2))
         (= a1 a2)]
        [(and (atom? a1)
              (atom? a2))
         (eq? a1 a2)]
        [else #f]))

(eqan? 5 5)
(eqan? 'hi 'hi)
(eqan? 'hi 5)

(define (book/occur a lat)
  (cond [(null? lat) 0]
        [(eqan? a (first lat))
         (add1 (book/occur a (rest lat)))]
        [else
         (book/occur a (rest lat))]))

(book/occur 'cup '(tea cup coffee cup and hick cup))

(define (book/one? n)
  (eqan? n 1))

(book/one? 1)
(book/one? 2)

(define (book/one/rempick n lat)
  (cond [(null? lat) '()]
        [(book/one? n) (rest lat)]
        [else
         (cons (first lat)
               (book/one/rempick (sub1 n) (rest lat)))]))

(book/one/rempick 3 '(lemon meringue salty pie))
