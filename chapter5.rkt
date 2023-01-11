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

(define (match/occur* a l)
  (match l
    ['() 0]
    [(list* (? (λ (v) (eq? v a)) head) tail)
     (add1 (match/occur* a tail))]
    [(list* (? atom? head) tail)
     (match/occur* a tail)]
    [_ (+ (match/occur* a (first l))
          (match/occur* a (rest l)))]))

(match/occur* 'banana
              '((banana)
                (split ((((banana ice)))
                        (cream (banana))
                        sherbet))
                (banana)
                (bread)
                (banana brandy)))

(define (subst* new old l)
  (cond [(null? l) '()]
        [(atom? (first l))
         (if (eq? old (first l))
             (cons new (subst* new old (rest l)))
             (cons (first l) (subst* new old (rest l))))]
        [else
         (cons (subst* new old (first l))
               (subst* new old (rest l)))]))

(subst* 'orange 'banana
        '((banana)
          (split ((((banana ice)))
                  (cream (banana))
                  sherbet))
          (banana)
          (bread)
          (banana brandy)))

(define (match/subst* new old l)
  (match l
    ['() '()]
    [(list* (? (curry eq? old) head) tail)
     (cons new (match/subst* new old tail))]
    [(list* (? atom? head) tail)
     (cons head (match/subst* new old tail))]
    [_ (cons (match/subst* new old (first l))
             (match/subst* new old (rest l)))]))

(match/subst* 'orange 'banana
              '((banana)
                (split ((((banana ice)))
                        (cream (banana))
                        sherbet))
                (banana)
                (bread)
                (banana brandy)))

(define (insertL* new old l)
  (cond [(null? l) '()]
        [(atom? (first l))
         (if (eq? (first l) old)
             (cons new (cons old (insertL* new old (rest l))))
             (cons (first l) (insertL* new old (rest l))))]
        [else
         (cons (insertL* new old (first l))
               (insertL* new old (rest l)))]))

(insertL* 'pecker 'chuck
          '((how much (wood))
            could
            ((a (wood) chuck))
            (((chuck)))
            (if (a) ((wood chuck)))
            could chuck wood))

(define (match/insertL* new old l)
  (match l
    ['() '()]
    [(list* (? (curry eq? old) head) tail)
     (flatten (list new old (match/insertL* new old tail)))]
    [(list* (? atom? head) tail)
     (cons head (match/insertL* new old tail))]
    [_ (cons (match/insertL* new old (first l))
             (match/insertL* new old (rest l)))]))

(match/insertL* 'pecker 'chuck
                '((how much (wood))
                  could
                  ((a (wood) chuck))
                  (((chuck)))
                  (if (a) ((wood chuck)))
                  could chuck wood))

(define (member* a l)
  (cond [(null? l) #f]
        [(atom? (first l))
         (or (eq? a (first l))
             (member* a (rest l)))]
        [else
         (or (member* a (first l))
             (member* a (rest l)))]))

(member* 'chips '((potato)
                  (chips ((with) fish)
                         (chips))))


(define (other/member* a l)
  ((compose not null?) (member a (flatten l))))

(define (match/member* a l)
  (match (flatten l)
    [(list _ ... (? (curry equal? a) x) _ ...) #t]
    [_ #f]))

(match/member* 'chips '((potato)
                        (chips ((with) fish)
                               (chips))))

(match/member* 'chips '(chips (potato)
                              (chip ((with) fish)
                                    (chip))))

(define (leftmost l)
  (cond [(atom? (first l)) (first l)]
        [else (leftmost (car l))]))

(leftmost '((potato) (chips ((with) fish) (chips))))
(leftmost '(((hot) (tuna (and))) cheese))
(leftmost '((() found)) 17 (seventeen))
(leftmost (quote ()))


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

(define (rkt/eqlist? l1 l2)
  (equal? l1 l2))

(eqlist?
 '(beef ((sausage)) (and (soda)))
 '(beef ((sausage)) (and (soda))))

(eqlist?
 '(beef ((sausage)) (and (soda)))
 '(beef ((salami)) (and (soda))))

(define (eqlist? l1 l2)
  (cond [(and (null? l1) (null? l2)) #t]
        [(or (null? l1)(null? l2)) #f]
        [(and (atom? (car l1))
              (atom? (car l2)))
         (and (eqan? (car l1)(car l2))
              (eqlist? (cdr l1)(cdr l2)))]
        [(or (atom? (car l1))
             (atom? (car l2))) #f]
        [else
         (and (eqlist? (car l1)(car l2))
              (eqlist? (cdr l1)(cdr l2)))]))

(define (book/equal? s1 s2)
  (cond [(and (atom? s1) (atom? s2)) (eqan? s1 s2)]
        [(or (atom? s1)
             (atom? s2)) #f]
        [else
         (eqlist? s1 s2)]))

(define (eqlist? l1 l2)
  (cond [(and (null? l1) (null? l2)) #t]
        [(and (book/equal? (first l1) (first l2)))
         (eqlist? (rest l1) (rest l2))]
        [else #f]))

(define (new/rember s l)
  (cond [(null? l) '()]
        [(book/equal? (first l) s)
         (new/rember s (rest l))]
        [else
         (cons (first l) (new/rember s (rest l)))]))

(new/rember 'cow '(the cow goes moo))
