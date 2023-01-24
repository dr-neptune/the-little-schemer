#lang racket
(require racket)

'((apple pie)
  (strawberry pie))

(define (set? lat)
  (cond [(null? lat) #t]
        [(member (first lat) (rest lat)) #f]
        [else (set? (rest lat))]))

(define (new-entry build)
  (when (and (set? (first build))
             (eq? (length (first build))
                  (length (second build))))
    build))

(new-entry '((appetizer entree beverage)
             (pate boeuf vin)))

(new-entry '((appetizer entree beverage)
             (beer beer beer)))

(define (lookup-in-entry name entry entry-f)
  (lookup-in-entry-help name
                        (first entry)
                        (second entry)
                        entry-f))

(define (lookup-in-entry-help name names values entry-f)
  (cond [(null? names) (entry-f name)]
        [(eq? name (first names))
         (first values)]
        [else
         (lookup-in-entry-help name
                               (rest names)
                               (rest values)
                               entry-f)]))

(define table
  (cons
   (new-entry '((appetizer entree beverage)
                (pate boeuf vin)))
   (new-entry '((appetizer entree beverage)
                (beer beer beer)))))

table

(define (extend-table entry table)
  (cons entry table))

(extend-table '((appetizer entree beverage)
                (fries buffalo-mac beer))
              table)

(define (member? a lat)
  (match lat
    ['() #f]
    [(list (? (λ (v) (equal? a v))) tail) #t]
    [_ (member? a (rest lat))]))

(define (lookup-in-table name table table-f)
  (cond [(null? table) (table-f name)]
        [(member? name (first (first table)))
         (lookup-in-entry name (first table) table-f)]
        [else
         (lookup-in-table name (rest table) table-f)]))

(lookup-in-table 'entree '(((entree dessert)
                            (spaghetti spumoni))
                           ((appetizer entree beverage)
                            (food tastes good)))
                 (λ (name) name))

(define (lookup-in-table name table table-f)
  (cond [(null? table) (table-f name)]
        [else
         (lookup-in-entry name (car table)
                          (λ (name)
                            (lookup-in-table name (cdr table) table-f)))]))


(define (lookup-in-table name table table-f)
  (if (null? table)
      (table-f name)
      (lookup-in-entry
       name (car table)
       (λ (name)
         (lookup-in-table name (cdr table) table-f)))))

(define (match/lookup-in-table name table table-f)
  (match table
    ['() (table-f name)]
    [(list (list (list _ ... (== name) _ ...)
                 (list _ ...))
           (list _ ...) ...)
     (lookup-in-entry name (first table) table-f)]
    [_ (match/lookup-in-table name (rest table) table-f)]))

(match/lookup-in-table 'entree '(((entree dessert)
                                  (spaghetti spumoni))
                                 ((appetizer entree beverage)
                                  (food tastes good)))
                       identity)


(match/lookup-in-table 'post-game '(((entree dessert)
                                     (spaghetti spumoni))
                                    ((appetizer entree beverage)
                                     (food tastes good))
                                    ((pre-game game post-game)
                                     (beer beer water))
                                    ((appetizer entree beverage)
                                     (tater-tots impossible-burger beer)))
                       identity)

((λ (nothing)
   (cons nothing '()))
 '(from nothing comes something))

(define (atom? x)
  (and (not (pair? x))
       (not (null? x))))

(define (expression-to-action e)
  (cond [(atom? e) (atom-to-action e)]
        [else (list-to-action e)]))

(define (atom-to-action e)
  (cond [(number? e) *const]
        [(eq? e #t) *const]
        [(eq? e #f) *const]
        [(eq? e 'cons) *const]
        [(eq? e 'car) *const]
        [(eq? e 'cdr) *const]
        [(eq? e 'null) *const]
        [(eq? e 'eq?) *const]
        [(eq? e 'atom?) *const]
        [(eq? e 'zero?) *const]
        [(eq? e 'add1) *const]
        [(eq? e 'sub1) *const]
        [(eq? e 'number?) *const]
        [else *identifier]))

(define (list-to-action e)
  (cond [(atom? (car e))
         (cond [(eq? (car e) (quote quote)) *quote]
               [(eq? (car e) 'λ) *lambda]
               [(eq? (car e) 'cond) *cond]
               [else *application])]
        [else *application]))

(define (value e) (meaning e '()))
(define (meaning e table) ((expression-to-action e) e table))

(define (build s1 s2)
  (cons s1 (cons s2 '())))

(define (*const e table)
  (cond [(number? e) e]
        [(eq? e #t) #t]
        [(eq? e #f) #f]
        [else
         (build 'primitive e)]))

(define (*quote e table)
  (second e))

(define (*identifier e table)
  (lookup-in-table e table '()))

(define (*lambda e table)
  (build 'non-primitive
         (cons table (cdr e))))

(meaning '(λ (x) (cons x y))
         '(((y z)
            ((8) 9))))

(define table-of first)
(define formals-of second)
(define body-of third)

(define (evcon lines table)
  (cond [(else? (question-of (car lines)))
         (meaning (answer-of (car lines)) table)]
        [(meaning (question-of (car lines)) table)
         (meaning (answer-of (car lines)) table)]
        [else (evcon (cdr lines) table)]))

(define (else? x)
  (cond [(atom? x) (eq? x 'else)]
        [else #f]))

(define question-of first)
(define answer-of second)

(define (*cond e table)
  (evcon (cond-lines-of e) table))

(define cond-lines-of cdr)

(*cond '(cond (coffee klatsch) (else party))
       '(((coffee) (#t))
         ((klatsch party) (5 (6)))))

(define (evlis args table)
  (cond [(null? args) '()]
        [else
         (cons (meaning (car args) table)
               (evlis (cdr args) table))]))

(define (*application e table)
  (apply
   (meaning (function-of e) table)
   (evlis (arguments-of e) table)))

(define function-of car)
(define arguments-of cdr)

(define (primitive? e)
  (eq? (first e) 'primitive))

(define (non-primitive? e)
  (eq? (first e) 'non-primitive))

(define (apply fun vals)
  (cond [(primitive? fun)
         (apply-primitive (second fun) vals)]
        [(non-primitive? fun)
         (apply-closure (second fun) vals)]))

(define (apply-primitive name vals)
  (cond [(eq? name 'cons)
         (cons (first vals) (second vals))]
        [(eq? name 'car)
         (car (first vals))]
        [(eq? name 'cdr)
         (cdr (first vals))]
        [(eq? name 'null?)
         (null? (first vals))]
        [(eq? name 'eq?)
         (eq? (first vals) (second vals))]
        [(eq? name 'atom?)
         (:atom? (first vals))]
        [(eq? name 'zero?)
         (zero? (first vals))]
        [(eq? name 'add1)
         (add1 (first vals))]
        [(eq? name 'sub1)
         (sub1 (first vals))]
        [(eq? name 'number?)
         (number? (first vals))]))

(define (:atom? x)
  (cond [(atom? x) #t]
        [(null? x) #f]
        [(eq? (car x) 'primitive) #t]
        [(eq? (car x) 'non-primitive) #t]
        [else #f]))

(define (apply-closure closure vals)
  (meaning (body-of closure)
           (extend-table
            (new-entry
             (formals-of closure)
             vals)
            (table-of closure))))

(apply-closure '((((u v w)
                   (1 2 3))
                  ((x y z)
                   (4 5 6)))
                 (x y)
                 ('cons z x))
               '((a b c)
                 (d e f)))

(meaning '6 '(((x y)
               ((a b c)(d e f)))
              ((u v w)
               (1 2 3))
              ((x y z)
               (4 5 6))))

(meaning 'x '(((x y)
               ((a b c)(d e f)))
              ((u v w)
               (1 2 3))
              ((x y z)
               (4 5 6))))

(meaning 'z '(((x y)
              ((a b c)(d e f)))
             ((u v w)
              (1 2 3))
             ((x y z)
              (4 5 6))))

(evlis '(z x)
       '(((x y)
          ((a b c)(d e f)))
         ((u v w)
          (1 2 3))
         ((x y z)
          (4 5 6))))

(meaning 'cons '())

(apply-primitive 'cons '(6 (a b c)))
