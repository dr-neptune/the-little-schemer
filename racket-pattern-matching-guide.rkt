#lang racket
(require racket)

;; literal matching
(match 2
  [1 'one]
  [2 'two]
  [3 'three])

(match #f
  [#t 'yes]
  [#f 'no])

(match "apple"
  ['apple 'symbol]
  ["apple" 'string]
  [#f 'boolean])

;; constructors like cons, list, vector
(match '(1 2)
  [(list 0 1) 'one]
  [(list 1 2) 'two])

(match '(1 . 2)
  [(list 1 2) 'list]
  [(cons 1 2) 'pair])

(match #(1 2)
  [(list 1 2) 'list]
  [(vector 1 2) 'vector])

;; struct
(struct shoe (size color))
(struct hat (size style))

(shoe? (shoe 5 "blue"))

(shoe-size (shoe 5 "blue"))
(shoe-color (shoe 5 "blue"))

;; pattern variables
(match '(1)
  [(list x) (add1 x)]
  [(list x y) (+ x y)])

(match '(1 5)
  [(list x) (add1 x)]
  [(list x y) (+ x y)])

(match (hat 23 'bowler)
  [(shoe sz col) sz]
  [(hat sz stl) sz])

(match (hat 11 'cowboy)
  [(shoe sz 'black) 'a-good-shoe]
  [(hat sz 'bowler) 'a-good-hat]
  [_ 'something-else])

;; else identifier
;; throws error
(match 1
  [else
   (case 2
     [(a 1 b) 3]
     [else 4])])

(match #f
  [else
   (cond
     [#f 'not-evaluated]
     [else 'also-not-evaluated])])

;; matching against a bound identifier
(define val 42)

(match (list 42)
  [(list (== val)) 'matched])

(match (list 43)
  ;; without ==, val is a pattern variable
  [(list val) (format "match binds val to ~a" val)])

;; ellipsis ...
(match '(1 1 1)
  [(list 1 ...) 'ones]
  [_ 'other])

(match '(1 1 2)
  [(list 1 ...) 'ones]
  [_ 'other])

(match '(1 2 3 4)
  [(list 1 x ... 4) x])

(match (list (hat 23 'bowler)
             (hat 22 'pork-pie))
  [(list (hat sz styl) ...) (apply + sz)])

(match '((! 1) (! 2 2) (! 3 3 3))
  [(list (list '! x ...) ...) x])

;; quasiquote form
(match `{with {x 1} {+ x 1}}
  [`{with {,id ,rhs} ,body}
   `{{lambda {,id} ,body} ,rhs}])
