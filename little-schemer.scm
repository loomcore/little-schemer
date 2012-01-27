#lang scheme
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))
(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a)
                (member? a (cdr lat)))))))
(define rember
  (lambda (a lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) a) (cdr lat))
      (else (cons (car lat)
                  (rember a (cdr lat)))))))
(define firsts
  (lambda (l)
    (cond
      ((null? l) (quote ()))
      ((null? (car l)) (firsts (cdr l)))
      (else (cons (car (car l))
                  (firsts (cdr l)))))))
;; page 47