#lang racket
(require racket)

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


(define (eqlist? l1 l2)
  (cond [(and (null? l1) (null? l2)) #t]
        [(and (null? l1) (atom? (first l2))) #f]
        [(null? l1) #f]
        [(and (atom? (first l1)) (null? (first l2))) #f]
        [(and (atom? (first l1)) (atom? (first l2)))
         (and (eqan? (first l1) (first l2))
              (eqlist? (rest l1) (rest l2)))]
        [(atom? (first l1)) #f]
        [(null? l2) #f]
        [(atom? (first l2)) #f]
        [else
         (and (eqlist? (first l1)(first l2))
              (eqlist? (rest l2)(rest l2)))]))


(define (equal? s1 s2)
  (cond [(and (atom? s1) (atom? s2)) (eqan? s1 s2)]
        [(or (atom? s1)
             (atom? s2)) #f]
        [else
         (eqlist? s1 s2)]))

(define (rember s l)
  (cond [(null? l) '()]
        [(equal? (first l) s)
         (rember s (rest l))]
        [else
         (cons (first l) (rember s (rest l)))]))


(rember 'a '(a b c))
(rember 'c '(a b c))

(define (rember-f pred? s l)
  (cond [(null? l) '()]
        [(pred? (first l) s) (rember-f pred? s (rest l))]
        [else (cons (first l) (rember-f pred? s (rest l)))]))

(rember-f = 5 '(6 2 5 3))
(rember-f eq? 'jelly '(jelly beans are good))
(rember-f equal? '(pop corn) '(lemonade (pop corn) and (cake)))

(define eq?-c
  (λ (a)
    (λ (x)
      (eq? x a))))

(define (refined/rember-f test?)
  (lambda (s l)
    (cond [(null? l) '()]
          [(test? (first l) s)
           ((refined/rember-f test?) s (rest l))]
          [else (cons (first l) ((refined/rember-f test?) s (rest l)))])))


((refined/rember-f =) 5 '(6 2 5 3))
((refined/rember-f eq?) 'jelly '(jelly beans are good))
((refined/rember-f equal?) '(pop corn) '(lemonade (pop corn) and (cake)))
((refined/rember-f eq?) 'tuna '(shrimp salad and tuna salad))

(define (insertL-f test?)
  (λ (new old l)
    (cond [(null? l) '()]
          [(test? (first l) old)
           (cons new (cons old (rest l)))]
          [else (cons (first l)
                      ((insertL-f test?) new old (rest l)))])))

(define (insertR-f test?)
  (λ (new old l)
    (cond [(null? l) '()]
          [(test? (first l) old)
           (cons old (cons new (rest l)))]
          [else (cons (first l)
                      ((insertR-f test?) new old (rest l)))])))

((insertL-f eq?) 8 5 '(1 2 3 4 5 6 7))
((insertR-f eq?) 8 5 '(1 2 3 4 5 6 7))

(define (insert-g side test?)
  (λ (new old l)
    (cond [(null? l) '()]
          [(test? (first l) old)
           (if (equal? side 'L)
               (cons new (cons old ((insert-g 'L test?) new old (rest l))))
               (cons old (cons new ((insert-g 'R test?) new old (rest l)))))]
          [else (cons (first l)
                      ((insert-g side test?) new old (rest l)))])))

((insert-g 'L eq?) 8 5 '(1 2 3 4 5 6 7))
((insert-g 'R eq?) 8 5 '(1 2 3 4 5 6 7))

(define (seqL a b c)
  (cons a (cons b c)))

(define (seqR a b c)
  (cons b (cons a c)))


(define (helper/insert-g side test?)
  (λ (new old l)
    (cond [(null? l) '()]
          [(test? (first l) old)
           (if (equal? side 'L)
               (seqL new old ((insert-g 'L test?) new old (rest l)))
               (seqR new old ((insert-g 'L test?) new old (rest l))))]
          [else (cons (first l)
                      ((insert-g side test?) new old (rest l)))])))


((insert-g 'L eq?) 8 5 '(1 2 3 4 5 6 7))
((insert-g 'R eq?) 8 5 '(1 2 3 4 5 6 7))

(define (book/insert-g seq)
  (λ (new old l)
    (cond [(null? l) '()]
          [(eq? (car l) old)
           (seq new old (cdr l))]
          [else (cons (car l)
                      ((book/insert-g seq) new old (cdr l)))])))

(define insertL
  (book/insert-g seqL))

(define insertR
  (book/insert-g seqR))

(insertL 2 5 '(1 2 3 4 5))
(insertR 2 5 '(1 2 3 4 5))

(define (subst new old l)
  (cond [(null? l) '()]
        [(eq? (first l) old)
         (cons new (rest l))]
        [else (cons (first l) (subst new old (rest l)))]))

(subst 'almond 'peanut '(peanut butter and grape flavored jelly))

(define (seqS new old l)
  (cons new l))

(define book/subst
  (book/insert-g seqS))

(book/subst 'almond 'peanut '(peanut butter and marmalade))

(define (new/rember a l)
  ((book/insert-g (λ (new old l) l)) #f a l))

(new/rember 'peanut '(peanut butter and marmalade))

(define (↑ m n)
  (cond [(eq? n 0) 1]
        [else
         (* m (↑ m (sub1 n)))]))

(define (operator aexp) (car aexp))

(define (atom-to-function x)
  (match x
    ['+ +]
    ['* *]
    [_ ↑]))

(atom-to-function (operator '(+ 5 3)))

(define (value nexp)
  (cond [(number? nexp) nexp]
        [else ((atom-to-function (operator nexp))
               (value (second nexp))
               (value (third nexp)))]))

(value '(+ 5 3))

(define (multirember-f pred?)
  (λ (a lat)
    (cond [(null? lat) '()]
          [(pred? (car lat) a)
           ((multirember-f pred?) a (cdr lat))]
          [else
           (cons (car lat) ((multirember-f pred?) a (cdr lat)))])))

((multirember-f eq?) 'peanut '(peanut brittle and peanut toffee))
((multirember-f eq?) 'tuna '(shrimp salad tuna salad and tuna))

(define multirember-eq? (multirember-f eq?))

(define eq?-tuna (eq?-c 'tuna))

(define (multiremberT test? lat)
  (cond [(null? lat) '()]
        [(test? (first lat))
         (multiremberT test? (rest lat))]
        [else
         (cons (first lat)
               (multiremberT test? (rest lat)))]))

(multiremberT eq?-tuna '(shrimp salad tuna salad and tuna))

(define (multirember&co a lat col)
  (cond [(null? lat) (begin
                       (displayln (format "hitting end: ~a ~a ~a" a lat col))
                       (col '() '()))]
        [(eq? (car lat) a)
         (begin
           (displayln (format "found a: ~a ~a ~a" a lat col))
           (multirember&co a (cdr lat) (λ (newlat seen)
                                       (col newlat
                                            (cons (car lat) seen)))))]
        [else
         (begin
           (displayln (format "did not find a: ~a ~a ~a" a lat col))
           (multirember&co a (cdr lat) (λ (newlat seen)
                                         (col (cons (car lat) newlat) seen))))]))

(define a-friend (λ (x y) (null? y)))

(multirember&co 'tuna '(strawberries tuna and swordfish) a-friend)

(multirember&co 'tuna '() a-friend)
(multirember&co 'tuna '(tuna) a-friend)

(define (new-friend newlat seen)
  (a-friend newlat (cons 'tuna seen)))

(multirember&co 'tuna '(tuna) new-friend)
(multirember&co 'tuna '(and tuna) new-friend)
(multirember&co 'tuna '(strawberries tuna and swordfish) (λ (x y) (length x)))

(define (multiinsertLR new oldL oldR lat)
  (cond [(null? lat) '()]
        [(eq? (first lat) oldL)
         (cons new (cons oldL (multiinsertLR new oldL oldR (rest lat))))]
        [(eq? (first lat) oldR)
         (cons oldR (cons new (multiinsertLR new oldL oldR (rest lat))))]
        [else
         (cons (first lat) (multiinsertLR new oldL oldR (rest lat)))]))

(multiinsertLR 'apple 'pear 'banana '(there was an apple and a pear and a banana))

;; (define (multiinsertLR&co new oldL oldR lat col)
;;   (cond [(null? lat) (col '() 0 0)]
;;         [(eq? (first lat) oldL)
;;          (cons new (cons oldL (multiinsertLR&co new oldL oldR (rest lat)
;;                                                 (λ (newlat L R) newlat))))]
;;         [(eq? (first lat) oldR)
;;          (cons oldR (cons new (multiinsertLR&co new oldL oldR (rest lat)
;;                                                 (λ (newlat L R) newlat))))]
;;         [else
;;          (cons (first lat) (multiinsertLR&co new oldL oldR (rest lat)
;;                                              (λ (newlat L R) newlat)))]))

;; (λ (newlat L R) (col (cons (car lat) newlat) L R))

(define (multiinsertLR&co new oldL oldR lat col)
  (cond [(null? lat) (col '() 0 0)]
        [(eq? (car lat) oldL)
         (multiinsertLR&co new oldL oldR (cdr lat)
                           (λ (newlat L R)
                             (col (cons new (cons oldL newlat))
                                  (add1 L) R)))]
        [(eq? (car lat) oldR)
         (multiinsertLR&co new oldL oldR (cdr lat)
                           (λ (newlat L R)
                             (col (cons oldR (cons new newlat))
                                  L (add1 R))))]
        [else
         (multiinsertLR&co new oldL oldR (cdr lat)
                           (λ (newlat L R)
                             (col (cons (car lat) newlat) L R)))]))

(define (evens-only* l)
  (cond [(null? l) '()]
        [(atom? (first l))
         (cond [(even? (first l))
                (cons (first l) (evens-only* (rest l)))]
               [else
                (evens-only* (rest l))])]
        [else
         (cons (evens-only* (first l))
               (evens-only* (rest l)))]))

(evens-only* '(1 2 (3 4) 5 6 (7 8 (9 10))))
(evens-only* '((9 1 2 8) 3 10 ((9 9) 7 6) 2))

(define (evens-only*&co l col)
  (cond [(null? l) (col '() 1 0)]
        [(atom? (first l))
         (cond [(even? (first l))
                (evens-only*&co (rest l)
                                (λ (evenlat evenmult oddsum)
                                  (col (cons (first l) evenlat)
                                       (* (first l) evenmult)
                                       oddsum)))]
               [else
                (evens-only*&co (rest l)
                                (λ (evenlat evenmult oddsum)
                                  (col evenlat
                                       evenmult
                                       (+ (first l) oddsum))))])]
        [else
         (evens-only*&co (first l)
                         (λ (al ap as)
                           (evens-only*&co (rest l)
                                           (λ (dl dp ds)
                                             (col (cons al dl)
                                                  (* ap dp)
                                                  (+ as ds))))))]))

(evens-only*&co '((9 1 2 8) 3 10 ((9 9) 7 6) 2)
                (λ (newl product sum)
                  (cons sum (cons product newl))))
