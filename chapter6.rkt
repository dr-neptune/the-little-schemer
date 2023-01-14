#lang racket
(require racket)

(define (atom? x)2
  (and (not (pair? x))
       (not (null? x))))

(define (numbered? aexp)
  (cond [(atom? aexp) (number? aexp)]
        [(eq? (cadr aexp) '+)
         (and (numbered? (car aexp))
              (numbered? (caddr aexp)))]
        [(eq? (cadr aexp) 'x)
         (and (numbered? (car aexp))
              (numbered? (caddr aexp)))]
        [(eq? (cadr aexp) '↑)
         (and (numbered? (car aexp))
              (numbered? (caddr aexp)))]
        [else
         'pass]))

(numbered? '(3 + (4 ↑ 5)))
(numbered? '(3 + (4 ↑ sausage)))

(define (refined/numbered? aexp)
  (cond [(atom? aexp) (number? aexp)]
        [else
         (and (refined/numbered? (car aexp))
              (refined/numbered? (caddr aexp)))]))

(refined/numbered? '(3 + (4 ↑ 5)))
(refined/numbered? '(3 + (4 ↑ sausage)))

(define (match/numbered? aexp)
  (match aexp
    [(? atom?) (number? aexp)]
    [(list x _ y) (and (number? x)
                       (match/numbered? y))]))

(match/numbered? '(3 + (4 ↑ 5)))
(match/numbered? '(3 + (4 ↑ sausage)))

(define (value nexp)
  (cond [(atom? nexp)
         (if (number? nexp) nexp #f)]
        [(eq? (cadr nexp) '+)
         (+ (car nexp) (caddr nexp))]
        [(eq? (cadr nexp) '*)
         (* (car nexp) (caddr nexp))]
        [else
         (↑ (car nexp) (caddr nexp))]))

(value 10)
(value '(2 + 3))
(value '(2 * 3))
(value '(2 ↑ 3))

(define (↑ m n)
  (cond [(= n 0) 1]
        [else
         (* m (↑ m (sub1 n)))]))

(define (refined/value nexp)
  (cond [(atom? nexp) nexp]
        [(eq? (cadr nexp) '+)
         (+ (refined/value (car nexp))
            (refined/value (caddr nexp)))]
        [(eq? (cadr nexp) '*)
         (* (refined/value (car nexp))
            (refined/value (caddr nexp)))]
        [else
         (↑
          (refined/value (car nexp))
          (refined/value (caddr nexp)))]))

(refined/value '(2 ↑ 3))

(define (match/infix/value nexp)
  (match nexp
    [(list (? number? val) op (? number? other))
     ((eval op) val other)]
    [(list (? number? val) op tail)
     ((eval op) val (match/infix/value tail))]
    [(list (? list? head) op tail)
     ((eval op)
      (match/infix/value head)
      (match/infix/value tail))]))

(match/infix/value '(2 + 3))
(match/infix/value '(2 * 3))
(match/infix/value '(2 ↑ 3))
(match/infix/value '(2 ↑ (6 + 3)))
(match/infix/value '((2 + 3) ↑ (6 + 3)))
(match/infix/value '(((2 + (3 * 4)) * (6 + (5 - 3))) ↑ (2 + (3 * 1))))



(define (prefix/value nexp)
  (cond [(atom? nexp) nexp]
        [(eq? '+ (car nexp))
         (+ (prefix/value (cadr nexp))
            (prefix/value (caddr nexp)))]
        [(eq? '* (car nexp))
         (* (prefix/value (cadr nexp))
            (prefix/value (caddr nexp)))]
        [else
         (↑ (prefix/value (cadr nexp))
            (prefix/value (caddr nexp)))]))

(prefix/value '(↑ 2 3))
(prefix/value '(* 2 3))
(prefix/value '(+ 2 3))

(define (1st-sub-exp aexp) (car aexp))
(define (2nd-sub-exp aexp) (caddr aexp))
(define (operator aexp) (cadr aexp))

(define (refined/2/value nexp)
  (cond [(atom? nexp) nexp]
        [(eq? (operator nexp) '+)
         (+ (refined/2/value (1st-sub-exp nexp))
            (refined/2/value (2nd-sub-exp nexp)))]
        [(eq? (operator nexp) '*)
         (* (refined/2/value (1st-sub-exp nexp))
            (refined/2/value (2nd-sub-exp nexp)))]
        [else
         (↑ (refined/2/value (1st-sub-exp nexp))
            (refined/2/value (2nd-sub-exp nexp)))]))


(refined/2/value '(2 ↑ 3))
(refined/2/value '(2 * 3))
(refined/2/value '(2 + 3))

(define-syntax-rule (match/infix/2/value nexp)
  (let-syntax ([(list val1 op val2) nexp])
       (op val1 val2)))

(match/infix/2/value '(2 + 4))

(define-syntax-rule (match/infix/2/value nexp)
  (let* ([val1 (first nexp)]
         [op (second nexp)]
         [val2 (last nexp)]
         [new-expression (list op val1 val2)])
      (eval new-expression)))

(match/infix/2/value '(2 + 4))


(define-syntax match/infix/3/value
  (syntax-rules ()
    [(list val1 op val2) (list op val1 val2)]))

(match/infix/3/value 2 + 4)
(match/infix/3/value (2 + 4))

(let ([f 2] [s +] [t 4])
  (match/infix/3/value f s t))


(require syntax/parse/define
         (for-syntax syntax/parse))

(begin-for-syntax
  (define-syntax-class infix-expr
    #:description "infix expression"
    #:commit
    #:attributes (prefix-expr)
    (pattern (lhs:infix-expr op:id rhs:infix-expr)
      #:with prefix-expr
      (syntax/loc this-syntax (op lhs.prefix-expr rhs.prefix-expr)))
    (pattern prefix-expr:number)))

(define-syntax-parser infix [(_:id :infix-expr) #'prefix-expr])

(expand-once
 #'(infix (((2 + (3 * 4)) * (6 + (5 - 3))) ↑ (2 + (3 * 1)))))

(define (make-number n)
  (match n
    [0 '()]
    [_ (cons '() (make-number (sub1 n)))]))

(define (sero? n) (null? n))

(define (edd1 n)
  (cons '() n))

(edd1 (make-number 5))

(define (zub1 n)
  (match n
    ['() '()]
    [_ (rest n)]))

(zub1 (make-number 0))
(zub1 (make-number 5))

(define (ls/+ a b)
  (match a
    ['() b]
    [_ (ls/+ (zub1 a) (cons '() b))]))

(zub1 (make-number 2))

(ls/+ (make-number 5) (make-number 2))

(define (lat? l)
  (cond [(null? l) #t]
        [(atom? (car l))
         (lat? (rest l))]
        [else #f]))

(lat? (make-number 5))

(lat? (map make-number '(1 2 3)))
