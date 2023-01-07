#lang racket
(require racket rackunit)

(define (rember a lat)
  (cond [(null? lat) '()]
        [(eq? a (first lat)) (rest lat)]
        [else (cons
               (first lat)
               (rember a (rest lat)))]))

(define (match/rember a lat)
  (match lat
    ['() '()]
    [(list* (== a) tail) tail]
    [_ (cons (first lat) (match/rember a (rest lat)))]))

(define (match/rember* a lat)
  (match lat
    ['() '()]
    [(list* (== a) tail) (match/rember* a tail)]
    [_ (cons (first lat) (match/rember* a (rest lat)))]))

(define (firsts ls)
  (cond [(null? ls) '()]
        [else (cons (first (first ls))
                    (firsts (rest ls)))]))

(define (seconds ls)
  (cond [(null? ls) '()]
        [else (cons (second (first ls))
                    (seconds (rest ls)))]))

(define (match/firsts ls)
  (match ls
    ['() '()]
    [(list* (list* a others) tail) (cons a (match/firsts tail))]))

(define (match/seconds ls)
  (match ls
    ['() '()]
    [(list* (list* a b others) tail)
     (cons b (match/seconds tail))]))

(define (insertR new old lat)
  (cond [(null? lat) '()]
        [(eq? (first lat) old)
         (cons old (cons new (rest lat)))]
        [else (cons (first lat) (insertR new old (rest lat)))]))

;; book version
(define book/insertR
  (λ (new old lat)
    (cond [(null? lat) '()]
          [else
           (cond [(eq? (car lat) old)
                  (cons old (cons new (cdr lat)))]
                 [else
                  (cons (car lat) (book/insertR new old (cdr lat)))])])))

(define (match/insertR new old lat)
  (match lat
    ['() '()]
    [(list* (== old) tail) (flatten (list old new tail))]
    [_ (cons (first lat) (match/insertR new old (rest lat)))]))

(book/insertR 'e 'd '(a b c d f g d h))
(insertR 'e 'd '(a b c d f g d h))
(match/insertR 'e 'd '(a b c d f g d h))

(define (insertL new old lat)
  (cond [(null? lat) '()]
        [(eq? (first lat) old)
         (cons new lat)]
        [else (cons (first lat) (insertL new old (rest lat)))]))

(define (match/insertL new old lat)
  (match lat
    ['() '()]
    [(list* (== old) tail)
     (cons new lat)]
    [_ (cons (first lat)
             (match/insertL new old (rest lat)))]))

(insertL 'd 'e '(a b c e f g))
(match/insertL 'd 'e '(a b c e f g))

(define (subst new old lat)
  (cond [(null? lat) '()]
        [(eq? (first lat) old) (cons new (rest lat))]
        [else (cons (first lat) (subst new old (rest lat)))]))

(subst 'topping 'fudge '(ice cream with fudge for dessert))

(define (subst2 new o1 o2 lat)
  (cond [(null? lat) '()]
        [(or (eq? (first lat) o1)
             (eq? (first lat) o2))
         (cons new (rest lat))]
        [else (cons (first lat) (subst2 new o1 o2 (rest lat)))]))

(define (match/subst2 new o1 o2 lat)
  (match lat
    ['() '()]
    [(or (list* (== o1) tail)
         (list* (== o2) tail))
     (cons new (rest lat))]
    [_ (cons (first lat) (match/subst2 new o1 o2 (rest lat)))]))

(subst2 'vanilla 'chocolate 'banana '(banana ice cream with chocolate topping))
(match/subst2 'vanilla 'chocolate 'banana '(banana ice cream with chocolate topping))

(define (multirember a lat)
  (cond [(null? lat) '()]
        [(eq? a (first lat))
         (multirember a (rest lat))]
        [else (cons (first lat) (multirember a (rest lat)))]))

(multirember 'cup '(coffee cup tea cup and hick cup))

(define (match/multirember a lat)
  (match lat
    ['() '()]
    [(list* (== a) tail)
     (match/multirember a tail)]
    [_ (cons (first lat) (match/multirember a (rest lat)))]))

(match/multirember 'cup '(coffee cup tea cup and hick cup))

(define (multiinsertR new old lat)
  (cond [(null? lat) '()]
        [(eq? (car lat) old)
         (cons old
               (cons new (multiinsertR new old (cdr lat))))]
        [else
         (cons (car lat)
               (multiinsertR new old (cdr lat)))]))

(multiinsertR 'CuPpA 'cup '(coffee cup tea cup and hick cup))

(define (match/multiinsertR new old lat)
  (match lat
    ['() '()]
    [(list* (== old) tail)
     (flatten (list old new (match/multiinsertR new old tail)))]
    [_ (cons (first lat)
             (match/multiinsertR new old (rest lat)))]))

(match/multiinsertR 'CuPpA 'cup '(coffee cup tea cup and hick cup))

(define multiinsertL
  (λ (new old lat)
    (cond [(null? lat) '()]
          [else
           (cond [(eq? (car lat) old)
                  (cons new
                        (cons old
                              (multiinsertL new old (cdr lat))))]
                 [else
                  (cons (car lat)
                        (multiinsertL new old (cdr lat)))])])))

(multiinsertL 'CuPpA 'cup '(coffee cup tea cup and hick cup))

(define (multisubst new old lat)
  (cond [(null? lat) '()]
        [(eq? (car lat) old)
         (cons new (multisubst new old (cdr lat)))]
        [else (cons (car lat) (multisubst new old (rest lat)))]))

(multisubst 'CuPpA 'cup '(coffee cup tea cup and hick cup))

(module+ test
  (require rackunit)
  "test book rember"
  (check-equal? (rember 'mint '(lamb chops and mint flavored jelly))
                '(lamb chops and flavored jelly))
  (check-equal? (rember 'lamb '(lamb chops and mint flavored jelly))
                '(chops and mint flavored jelly))
  (check-equal? (rember 'toast '(bacon lettuce and tomato))
                '(bacon lettuce and tomato))
  (check-equal? (rember 'and '(bacon lettuce and tomato))
                '(bacon lettuce tomato))
  (check-equal? (rember 'mint '(mint lamb chops and mint flavored jelly))
                '(lamb chops and mint flavored jelly))
  "test singular rember"
  (check-equal? (match/rember 'mint '(lamb chops and mint flavored jelly))
                '(lamb chops and flavored jelly))
  (check-equal? (match/rember 'lamb '(lamb chops and mint flavored jelly))
                '(chops and mint flavored jelly))
  (check-equal? (match/rember 'toast '(bacon lettuce and tomato))
                '(bacon lettuce and tomato))
  (check-equal? (match/rember 'mint '(mint lamb chops and mint flavored jelly))
                '(lamb chops and mint flavored jelly))
  "test rember*"
  (check-equal?
   (match/rember* 'mint '(mint lamb chops and mint flavored jelly))
   '(lamb chops and flavored jelly))
  "test firsts"
  (check-equal? (firsts '((apple peach pumpkin)
                          (plum pear cherry)
                          (grape raisin pea)
                          (bean carrot eggplant)))
                '(apple plum grape bean))
  "test seconds"
  (check-equal? (seconds '((apple peach pumpkin)
                           (plum pear cherry)
                           (grape raisin pea)
                           (bean carrot eggplant)))
                '(peach pear raisin carrot))
  "test match/firsts"
  (check-equal? (match/firsts '((apple peach pumpkin)
                                (plum pear cherry)
                                (grape raisin pea)
                                (bean carrot eggplant)))
                '(apple plum grape bean))
  "test match/seconds"
  (check-equal? (match/seconds '((apple peach pumpkin)
                                 (plum pear cherry)
                                 (grape raisin pea)
                                 (bean carrot eggplant)))
                '(peach pear raisin carrot)))
