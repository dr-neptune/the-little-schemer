#lang racket
(require racket)

(define (set? lat)
  (cond [(null? lat) #t]
        [(member (first lat) (rest lat)) #f]
        [else (set? (rest lat))]))

(set? '(apple peaches apple plum))
(set? '(apple peaches pears plum))
(set? '(apple 3 pear 4 9 apple 3 4))

(define (match/set? lat)
  (match lat
    ['() #t]
    [(list _ ... a _ ... a _ ...) #f]
    [_ (match/set? (rest lat))]))

(match/set? '(apple peaches apple plum))
(match/set? '(apple peaches pears plum))
(match/set? '(apple 3 pear 4 9 apple 3 4))

(define (makeset lat)
  (cond [(null? lat) '()]
        [(member (first lat) (rest lat))
         (makeset (rest lat))]
        [else
         (cons (first lat)
               (makeset (rest lat)))]))

(makeset '(apple peach pear peach plum apple lemon peach))

(define (multi/makeset lat)
  (cond [(null? lat) '()]
        [else
         (cons (first lat)
               (multi/makeset (remove* (list (first lat)) (rest lat))))]))

(makeset '(apple peach pear peach plum apple lemon peach))

(define (match/makeset lat)
  (match lat
    ['() '()]
    [(list _ ... a _ ... a _ ...)
     (cons a (match/makeset (remove* (list a) (rest lat))))]
    [_ (cons (first lat) (match/makeset (rest lat)))]))

(match/makeset '(apple peach pear peach plum apple lemon peach))

(define (subset? s1 s2)
  (cond [(null? s1) #t]
        [(member (first s1) s2)
         (subset? (rest s1) s2)]
        [else #f]))

(subset? '(5 chicken wings) '(5 hamburgers
                                2 pieces fried chicken and
                                light duckling wings))

(define (member? a lat)
  (match lat
    [(list _ ... (== a) _ ...) #t]
    [_ #f]))

(member? 'hello '(you say goodbye i say hello))

(define (and/subset? s1 s2)
  (cond [(null? s1) #t]
        [else
         (and (member? (first s1) s2)
              (and/subset? (rest s1) s2))]))

(and/subset? '(5 chicken wings) '(5 hamburgers
                                    2 pieces fried chicken and
                                    light duckling wings))

(define (match/subset s1 s2)
  (match s1
    ['() #t]
    [(list* a tail)
     (and (member? a s2) (match/subset tail s2))]))

(match/subset '(5 chicken wings) '(5 hamburgers
                                     2 pieces fried chicken and
                                     light duckling wings))

(define (eqset? s1 s2)
  (and (subset? s1 s2) (subset? s2 s1)))

(eqset? '(6 large chickens with wings)
        '(6 chickens with large wings))

(define (intersect? s1 s2)
  (cond [(null? s1) #f]
        [(or (member? (first s1) s2)
             (intersect? (rest s1) s2))]))

(intersect? '(stewed tomatoes and macaroni)
            '(macaroni cheese))

(define (intersect s1 s2)
  (cond [(null? s1) '()]
        [(member? (first s1) s2)
         (cons (first s1) (intersect (rest s1) s2))]
        [else (intersect (rest s1) s2)]))

(intersect '(stewed tomatoes and macaroni)
           '(macaroni and cheese))

(define (match/intersect s1 s2)
  (match s1
    ['() '()]
    [(list* (? (λ (v) (member? v s2))) tail)
     (cons (first s1) (match/intersect tail s2))]
    [_ (match/intersect (rest s1) s2)]))

(match/intersect '(stewed tomatoes and macaroni)
                 '(macaroni and cheese))
