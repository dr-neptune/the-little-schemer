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

(value 'hi)
