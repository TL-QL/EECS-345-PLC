#lang racket
;;;;****************************************
;;;;  Qiwen Luo (qxl216)
;;;;  EECS 345 Spring 2019
;;;;  Programming Exercise 1
;;;;****************************************


;; choose
;; parameters: n and k
;; returns C(n,k)
(define choose
  (lambda (n k)
    (cond
      [(or (zero? k)   (= n k)) 1]
      [else            (* (/ n k) (choose (- n 1) (- k 1)))])))

;; rotate
;; parameters: a1, a2, a3 and lis
;; returns the same lis except that each occurence of a1 is replaced by a2,
;;         a2 is replaced by a3, a3 is replaced by a1
(define rotate
  (lambda (a1 a2 a3 lis)
    (cond
      [(null? lis)         '()]
      [(eq? a1 (car lis))  (cons a2 (rotate a1 a2 a3 (cdr lis)))]
      [(eq? a2 (car lis))  (cons a3 (rotate a1 a2 a3 (cdr lis)))]
      [(eq? a3 (car lis))  (cons a1 (rotate a1 a2 a3 (cdr lis)))]
      [else                (cons (car lis) (rotate a1 a2 a3 (cdr lis)))])))

;; moveXleft
;; parameters: a and lis
;; return the same lis except that shift 'a one to the left if 'a is not the first element
(define moveXleft
  (lambda (a lis)
    (cond
      [(null? lis)             '()]
      [(null? (cdr lis))       lis]
      [(eq? a (car (cdr lis))) (cons a (moveXleft a (cons (car lis) (cdr (cdr lis)))))]
      [else                    (cons (car lis) (moveXleft a (cdr lis)))])))

;; myfilter
;; parameters: f and lis
;; returns a list which contains all elements in lis that can make f return true
(define myfilter
  (lambda (f lis)
    (cond
      [(null? lis)   '()]
      [(f (car lis)) (cons (car lis) (myfilter f (cdr lis)))]
      [else          (myfilter f (cdr lis))])))

;; squareroot
;; parameters: n and iter
;; returns the result of the squareroot of the value using iteration rounds of Newton's method
(define squareroot
  (lambda (n iter)
    (mysquareroot n iter n)))

;; rotate*
;; parameters: a1, a2, a3 and lis
;; returns the same lis except that each occurence of a1 is replaced by a2,
;;         a2 is replaced by a3, a3 is replaced by a1
(define rotate*
  (lambda (a1 a2 a3 lis)
    (cond
      [(null? lis)        '()]
      [(list? (car lis))  (cons (rotate* a1 a2 a3 (car lis)) (rotate* a1 a2 a3 (cdr lis)))]
      [(eq? a1 (car lis)) (cons a2 (rotate* a1 a2 a3 (cdr lis)))]
      [(eq? a2 (car lis)) (cons a3 (rotate* a1 a2 a3 (cdr lis)))]
      [(eq? a3 (car lis)) (cons a1 (rotate* a1 a2 a3 (cdr lis)))]
      [else               (cons (car lis) (rotate* a1 a2 a3 (cdr lis)))])))

;; flattenN
;; parameters: n and lis
;; returns the same lis without any parentheses that is nested more than n deep
(define flattenN
  (lambda (n lis)
    (cond
      [(null? lis)                     '()]
      [(and (> n 1) (list? (car lis))) (cons (flattenN (- n 1) (car lis)) (flattenN n (cdr lis)))]
      [(> n 1)                         (cons (car lis) (flattenN n (cdr lis)))]
      [(and (< n 2) (null? lis)) '()]
      [(and (< n 2) (list? (car lis))) (append (flattenN (- n 1) (car lis)) (flattenN n (cdr lis)))]
      [else                            (cons (car lis) (flattenN n (cdr lis)))])))

;; outerproduct
;; parameters: v1 and v2
;; returns the matrix that is the outerproduct of the vectors
(define outerproduct
  (lambda (v1 v2)
    (cond
      [(or (null? v1) (null? v2)) '()]
      [else                        (cons (myouterproduct v1 (car v2)) (outerproduct v1 (cdr v2)))])))

;; maxvalue*
;; parameter: lis
;; returns the largest number in the list
(define maxvalue*
  (lambda (lis)
    (cond
      [(null? lis)                                                                      'novalue]
      [(and (list? (car lis)) (mygreater? (maxvalue* (car lis)) (maxvalue* (cdr lis)))) (maxvalue* (car lis))]
      [(list? (car lis))                                                                (maxvalue* (cdr lis))]
      [(mygreater? (car lis) (maxvalue* (cdr lis)))                                     (car lis)]
      [else                                                                             (maxvalue* (cdr lis))])))

;; moveXleft*
;; parameters: a and lis
;; return the same lis except that shift 'a one to the left if 'a is not the first element
(define moveXleft*
  (lambda (a lis)
   (cond
     [(null? lis)                                                                   '()]
     [(and (list? (car lis)) (eq? a (car (car lis))))                               (cons a (cons (moveXleft* a (cdr (car lis))) (moveXleft* a (cdr lis))))]
     [(and (and (list? (car lis)) (not (null? (cdr lis)))) (eq? a (car (cdr lis)))) (cons (append (moveXleft* a (car lis)) (cons a '())) (moveXleft* a (cdr (cdr lis))))]
     [(list? (car lis))                                                             (cons (moveXleft* a (car lis)) (moveXleft* a (cdr lis)))]
     [(and (not (null? (cdr lis))) (eq? a (car (cdr lis))))                         (cons a (cons (car lis) (moveXleft* a (cdr (cdr lis)))))]
     [else                                                                          (cons (car lis) (moveXleft* a (cdr lis)))])))

       
;;;; Helper Functions--------------------------------------------------------------

;; mysquareroot
;; parameters: n, iter and value(initial value)
;; returns the result of the squareroot of the value after one iteration
(define mysquareroot
  (lambda (n iter value)
    (cond
      [(zero? iter) n]
      [else        (mysquareroot (- n (/ (- (* n n) value) (* 2 n))) (- iter 1) value)])))

;; myouterproduct
;; parameters: v1 and a
;; returns the multilication of av1
(define myouterproduct
  (lambda (v1 a)
    (cond
     [(null? v1) '()]
     [else       (cons (* (car v1) a) (myouterproduct (cdr v1) a))])))

;; mymaxvalue
;; parameters: n1 and n2
;; return #t if n1 > n2, else return #f
(define mygreater?
  (lambda (n1 n2)
    (cond
      [(eq? 'novalue n2) #t]
      [(eq? 'novalue n1) #f]
      [(> n1 n2)         #t]
      [else              #f])))