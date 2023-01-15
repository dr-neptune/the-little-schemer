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
    [(list* (? (curryr member? s2) head) tail)
     (cons head (match/intersect tail s2))]
    [_ (match/intersect (rest s1) s2)]))

(match/intersect '(stewed tomatoes and macaroni)
                 '(macaroni and cheese))

(define (union s1 s2)
  (cond [(null? s1) s2]
        [(member? (first s1) s2)
         (union (rest s1) s2)]
        [else (union (rest s1) (cons (first s1) s2))]))

(union '(stewed tomatoes and macaroni casserole)
       '(macaroni and cheese))

(define (match/union s1 s2)
  (match s1
    ['() s2]
    [(list* (? (curryr member? s2) head) tail)
     (match/union tail s2)]
    [_ (match/union (rest s1) (cons (first s1) s2))]))

(match/union '(stewed tomatoes and macaroni casserole)
             '(macaroni and cheese))


(define (difference s1 s2)
  (cond [(null? s1) '()]
        [(member? (first s1) s2)
         (difference (rest s1) s2)]
        [else (cons (first s1)
                    (difference (rest s1) s2))]))


(difference '(stewed tomatoes and macaroni casserole)
            '(macaroni and cheese))

(define (match/difference s1 s2)
  (match s1
    ['() '()]
    [(list* (? (compose not (curryr member s2)) head) tail)
     (cons head (match/difference tail s2))]
    [_ (match/difference (rest s1) s2)]))

(match/difference '(stewed tomatoes and macaroni casserole)
                  '(macaroni and cheese))

(define (book/intersectall l-set)
  (cond [(null? (cdr l-set)) (car l-set)]
        [else
         (intersect (car l-set)
                    (intersectall (cdr l-set)))]))

(book/intersectall '((a b c) (c a d e) (e f g h a b)))

(define (intersectall l-set)
  (foldl intersect (first l-set) (rest l-set)))

(intersectall '((a b c) (c a d e) (e f g h a b)))

(define (atom? x)
  (and (not (pair? x))
       (not (null? x))))

(define (lat? ls)
  (andmap atom? ls))

(define (match/intersectall l-set)
  (match l-set
    [(list a) #:when (lat? a) a]
    [_ (intersect (first l-set)
                  (match/intersectall (rest l-set)))]))

(match/intersectall '((a b c) (c a d e) (e f g h a b)))

(define (a-pair? x)
  (eq? (length x) 2))

(a-pair? '(pear pear))
(a-pair? '(3 7))
(a-pair? '((2) (pair)))
(a-pair? '(full (house)))
(a-pair? '(very (full (house))))
(a-pair? '(very very (full (house))))

(define (match/a-pair? x)
  (match x
    [(list a b) #t]
    [_ #f]))

(match/a-pair? '(pear pear))
(match/a-pair? '(3 7))
(match/a-pair? '((2) (pair)))
(match/a-pair? '(full (house)))
(match/a-pair? '(very (full (house))))
(match/a-pair? '(very very (full (house))))


(define (firsts ls)
  (cond [(null? ls) '()]
        [else (cons (first (first ls))
                    (firsts (rest ls)))]))


(define (fun? rel)
  (set? (firsts rel)))

(fun? '((8 3)(4 2)(7 6)(6 2)(3 4)))

(define (revrel rel)
  (map reverse rel))

(define (build s1 s2)
  (cons s1 (cons s2 '())))

(define (book/revrel rel)
  (cond [(null? rel) '()]
        [else (cons (build (second (first rel))
                           (first (first rel)))
                    (book/revrel (rest rel)))]))

(revrel '((8 a) (pumpkin pie) (got sick)))
(book/revrel '((8 a) (pumpkin pie) (got sick)))

(define (revpair pair)
  (build (second pair) (first pair)))


(define (book/revised/revrel rel)
  (cond [(null? rel) '()]
        [else (cons (revpair (car rel))
                    (book/revised/revrel (rest rel)))]))

(book/revised/revrel '((8 a) (pumpkin pie) (got sick)))

(define (seconds ls)
  (cond [(null? ls) '()]
        [else (cons (second (first ls))
                    (seconds (rest ls)))]))

(define (fullfun? fun)
  (set? (seconds fun)))
