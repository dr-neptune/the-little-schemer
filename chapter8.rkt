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
