#lang Scheme

(define (bst-create-empty)
  '())

(define (bst-create root left right)
  (cons root (cons left (cons right '()))))

(define (get-root node)
  (car node))

(define (get-left node)
  (car (cdr node)))

(define (get-right node)
  (car (cdr (cdr node))))

(define (deepest-left bst)
  (cond ((null? bst) (bst-create-empty))
        ((null? (get-left bst)) bst)
        (else (deepest-left (get-left bst)))))

(define (bst-insert bst f x)
  (cond ((null? bst) (bst-create x (bst-create-empty) (bst-create-empty)))
        ((equal? (get-root bst) x) bst)
        ((f x (get-root bst)) (bst-create (get-root bst) (bst-insert (get-left bst) f x) (get-right bst)))
        (else (bst-create (get-root bst) (get-left bst) (bst-insert (get-right bst) f x)))))

(define (bst-contains bst f g x)
  (cond ((null? bst) #f)
        ((f x (get-root bst)) #t)
        ((g x (get-root bst)) (bst-contains (get-left bst) f g x))
        (else (bst-contains (get-right bst) f g x))))
  
(define (bst-remove bst f g x)
  (cond ((null? bst) (bst-create-empty))
        ((f x (get-root bst)) (remove-root bst f g))
        ((g x (get-root bst)) (bst-create (get-root bst) (bst-remove (get-left bst) f g x) (get-right bst)))
        (else (bst-create (get-root bst) (get-left bst) (bst-remove (get-right bst) f g x)))))
        

(define (remove-root bst f g)
  (cond ((and (null? (get-right bst)) (null? (get-left bst))) (bst-create-empty))
        ((null? (get-right bst)) (get-left bst))
        ((null? (get-left bst)) (get-right bst))
        (else (let* ((val (get-root (deepest-left (get-right bst)))) (new-right (bst-remove (get-right bst) f g val)))
                (bst-create val (get-left bst) new-right)))))

(define (bst-pre-elements bst)
  (cond ((null? bst) '())
        ((and (null? (get-left bst)) (null? (get-right bst))) (cons (get-root bst) '()))
        ((null? (get-left bst)) (cons (get-root bst) (bst-pre-elements (get-right bst))))
        ((null? (get-right bst)) (cons (get-root bst) (bst-pre-elements (get-left bst))))
        (else (cons (get-root bst) (append (bst-pre-elements (get-left bst)) (bst-pre-elements (get-right bst)))))))

(define (bst-in-elements bst)
  (cond ((null? bst) '())
        ((and (null? (get-left bst)) (null? (get-right bst))) (cons (get-root bst) '()))
        ((null? (get-left bst)) (cons (get-root bst) (bst-in-elements (get-right bst))))
        (else (append (bst-in-elements (get-left bst)) (append (cons (get-root bst) '()) (bst-in-elements (get-right bst)))))))

(define (bst-post-elements bst)
  (cond ((null? bst) '())
        ((and (null? (get-left bst)) (null? (get-right bst))) (cons (get-root bst) '()))
        ((null? (get-left bst)) (append (bst-post-elements (get-right bst)) (cons (get-root bst) '())))
        (else (append (bst-post-elements (get-left bst)) (append (bst-post-elements (get-right bst)) (cons (get-root bst) '()))))))

(define (list->bst xs f)
  (cond ((null? xs) (bst-create-empty))
        (else (bst-insert (list->bst (reverse (cdr (reverse xs))) f) f (car (reverse xs))))))

(define (funct x y)
  (< x y))

(define (equality x y)
  (equal? x y))