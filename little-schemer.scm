#lang scheme
(define atom?
  (λ (x)
    (and (not (pair? x)) (not (null? x)))))
(define lat?
  (λ (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))
(define member?
  (λ (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a)
                (member? a (cdr lat)))))))
(define rember
  (λ (a lat)
    (cond
      ((null? lat) (quote ()))
      ((eq? (car lat) a) (cdr lat))
      (else (cons (car lat)
                  (rember a (cdr lat)))))))
(define firsts
  (λ (l)
    (cond
      ((null? l) (quote ()))
      ((null? (car l)) (firsts (cdr l)))
      (else (cons (car (car l))
                  (firsts (cdr l)))))))

(define insertR
  (λ (new old lat)
    (cond ((null? lat) (quote ()))
          ((eq? old (car lat)) (cons old
                                     (cons new
                                           (cdr lat))))
          (else (cons (car lat)
                      (insertR new old (cdr lat)))))))

(define insertL
  (λ (new old lat)
    (cond ((null? lat) (quote ()))
          ((eq? old (car lat)) (cons new lat))
          (else (cons (car lat)
                      (insertL new old (cdr lat)))))))

(define subst
  (λ (new old lat)
    (cond ((null? lat) (quote ()))
          ((eq? old (car lat)) (cons new (cdr lat)))
          (else (cons (car lat)
                      (subst new old (cdr lat)))))))

(define subst2
  (λ (new o1 o2 lat)
    (cond ((null? lat) (quote ()))
          ((or (eq? o1 (car lat))
               (eq? o2 (car lat))) (cons new (cdr lat)))
          (else (cons (car lat)
                      (subst2 new o1 o2 (cdr lat)))))))

;; page 53