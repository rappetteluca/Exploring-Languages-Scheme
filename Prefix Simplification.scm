#lang Scheme

(define (syn-create root left right)
  (cons root (cons left (cons right '()))))

(define (funct x y)
  (cond ((and (symbol? x) (number? y)) #t)
        ((and (symbol? y) (number? x)) #f)
        ((and (list? x) (not (list? y))) #f)
        ((and (list? y) (not (list? x))) #t)
        ((and (list? x) (list? y)) (string<? (symbol->string (car x)) (symbol->string (car y))))
        (else (string<? (symbol->string x) (symbol->string y)))))

(define (simplify exp)
  (cond ((null? exp) '())
        ((not (list? exp)) exp)
        ((or (list? (car (cdr exp))) (list? (car (cdr (cdr exp))))) (eval-exp (syn-create (car exp) (simplify (car (cdr exp))) (simplify (car (cdr (cdr exp)))))))
        (else (eval-exp exp))))

(define (eval-exp exp)
  (cond ((or (equal? (car (cdr exp)) 'ERROR) (equal? (car (cdr (cdr exp))) 'ERROR)) 'ERROR)
        ((and (equal? (car exp) '/) (equal? (car (cdr (cdr exp))) '0)) 'ERROR)
        ((and (equal? (car exp) '/) (equal? (car (cdr (cdr exp))) '1)) (car (cdr exp)))
        ((and (equal? (car exp) '/) (equal? (car (cdr exp)) (car (cdr (cdr exp))))) '1)
        ((and (equal? (car exp) '/) (equal? (car (cdr exp)) '0)) '0)
        ((and (equal? (car exp) '-) (equal? (car (cdr (cdr exp))) (car (cdr exp)))) '0)
        ((and (equal? (car exp) '+) (equal? (car (cdr (cdr exp))) '0)) (car (cdr exp)))
        ((and (equal? (car exp) '+) (equal? (car (cdr exp)) '0)) (car (cdr (cdr exp))))
        ((and (equal? (car exp) '*) (equal? (car (cdr exp)) '0)) '0)
        ((and (equal? (car exp) '*) (equal? (car (cdr (cdr exp))) '0)) '0)
        ((and (equal? (car exp) '*) (equal? (car (cdr (cdr exp))) '1)) (car (cdr exp)))
        ((and (equal? (car exp) '*) (equal? (car (cdr exp)) '1)) (car (cdr (cdr exp))))
        ((and (equal? (car exp) '-) (equal? (car (cdr (cdr exp))) '0)) (car (cdr exp)))
        ((and (or (equal? (car exp) '/) (equal? (car exp) '-)) (or (not (number? (car (cdr exp)))) (not (number? (car (cdr (cdr exp))))))) (cons (car exp) (cons (car (cdr exp)) (cons (car (cdr (cdr exp))) '()))))
        ((and (or (not (number? (car (cdr exp)))) (not (number? (car (cdr (cdr exp)))))) (funct (car (cdr exp))  (car (cdr (cdr exp))))) (cons (car exp) (cons (car (cdr exp)) (cons (car (cdr (cdr exp))) '()))))
        ((and (or (not (number? (car (cdr exp)))) (not (number? (car (cdr (cdr exp)))))) (not (funct (car (cdr exp))  (car (cdr (cdr exp)))))) (cons (car exp) (cons (car (cdr (cdr exp))) (cons (car (cdr exp)) '()))))
        (else (eval exp))))

(define (footprint exp)
  (* 4 (+ (count-ints exp '0) (count-vars (simplify exp) '0 '()))))

(define (count-ints exp count)
  (cond ((null? exp) count)
        ((number? exp) (+ 1 count))
        ((not (list? exp)) count)
        (else (+ (count-ints (car (cdr exp)) '0) (count-ints (car (cdr (cdr exp))) '0)))))

(define (count-vars exp count seen)
  (cond ((null? exp) count)
        ((not (list? exp)) count)
        ((and (symbol? (car (cdr exp))) (not (member (car (cdr exp)) seen))) (count-vars exp (+ 1 count) (cons (car (cdr exp)) seen)))
        ((and (symbol? (car (cdr (cdr exp)))) (not (member (car (cdr (cdr exp))) seen))) (count-vars exp (+ 1 count) (cons (car (cdr (cdr exp))) seen)))
        (else (+ (+ (count-vars (car (cdr exp)) '0 seen) (count-vars (car (cdr (cdr exp))) '0 seen)) count))))




         