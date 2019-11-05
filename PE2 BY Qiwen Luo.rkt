#lang racket

;;;;****************************************
;;;;  Qiwen Luo (qxl216)
;;;;  EECS 345 Spring 2019
;;;;  Programming Exercise 2
;;;;****************************************

;; choose
;; takes two integers and returns C(n,k)
(define choose
  (lambda (n k)
    (cond
      [(or (zero? k)   (= n k)) 1]
      [else            (* (/ n k) (choose (- n 1) (- k 1)))])))

;; choose-cps
;; takes two integers and returns C(n,k)
(define choose-cps
  (lambda (n k return)
    (cond
      [(or (zero? k) (= n k)) (return 1)]
      [else                   (choose-cps (- n 1) (- k 1) (lambda (v) (return (* (/ n k) v))))])))

;; rotate
;; takes 3 atoms and a list of atoms
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

;; rotate-cps
;; takes 3 atoms and a list of atoms
;; returns the same lis except that each occurence of a1 is replaced by a2,
;;         a2 is replaced by a3, a3 is replaced by a1
(define rotate-cps
  (lambda (a1 a2 a3 lis return)
    (cond
      [(null? lis)        (return '())]
      [(eq? a1 (car lis)) (rotate-cps a1 a2 a3 (cdr lis)(lambda (v) (return (cons a2 v))))]
      [(eq? a2 (car lis)) (rotate-cps a1 a2 a3 (cdr lis) (lambda (v) (return (cons a3 v))))]
      [(eq? a3 (car lis)) (rotate-cps a1 a2 a3 (cdr lis) (lambda (v) (return (cons a1 v))))]
      [else               (rotate-cps a1 a2 a3 (cdr lis) (lambda (v) (return (cons (car lis) v))))])))

;; squareroot
;; parameters: n and iter
;; returns the result of the squareroot of the value using iteration rounds of Newton's method
(define squareroot
  (lambda (n iter)
    (if (zero? iter)
        n
        (- (squareroot n (- iter 1))
           (/ (- (* (squareroot n (- iter 1)) (squareroot n (- iter 1))) n) (* 2 (squareroot n (- iter 1))))))))


;; squareroot-cps
;; parameters: n and iter
;; returns the result of the squareroot of the value using iteration rounds of Newton's method
(define squareroot-cps
  (lambda (n iter return)
    (if (zero? iter)
        (return n)
        (squareroot-cps n (- iter 1) (lambda (v) (return (- v (/ (- (* v v) n) (* 2 v)))))))))

;; partition
;; parameters: lis and value
;; return a pair of sublists where first group all smaller than value
;;        second group all greater than value
(define partition
  (lambda (lis value)
    (cond
      [(null? lis)         (cons '() (cons '() '()))]
      [(< (car lis) value) (cons (cons (car lis) (car (partition (cdr lis) value))) (cdr (partition (cdr lis) value)))]
      [else                (cons (car (partition (cdr lis) value))
                                 (cons (cons (car lis) (car (cdr (partition (cdr lis) value)))) '()))])))

;; partition-cps
;; parameters: lis and value
;; returns a pair of sublists where first group all smaller than value
;;         second group all greater than value
(define partition-cps
  (lambda (lis value return)
    (cond
      [(null? lis)         (return '() '())]
      [(> (car lis) value) (partition-cps (cdr lis) value (lambda (v1 v2) (return v1 (cons (car lis) v2))))]
      [(< (car lis) value) (partition-cps (cdr lis) value (lambda (v1 v2) (return (cons (car lis) v1) v2)))])))

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

;; rotate*-cps
;; parameters: a1, a2, a3 and lis
;; returns the same lis except that each occurence of a1 is replaced by a2,
;;         a2 is replaced by a3, a3 is replaced by a1??
(define rotate*-cps
  (lambda (a1 a2 a3 lis return)
    (cond
      [(null? lis)        (return '())]
      [(list? (car lis))  (rotate*-cps a1 a2 a3 (car lis) (lambda (v1) (rotate*-cps a1 a2 a3 (cdr lis) (lambda (v2) (return (cons v1 v2))))))]
      [(eq? a1 (car lis)) (rotate*-cps a1 a2 a3 (cdr lis) (lambda (v) (return (cons a2 v))))]
      [(eq? a2 (car lis)) (rotate*-cps a1 a2 a3 (cdr lis) (lambda (v) (return (cons a3 v))))]
      [(eq? a3 (car lis)) (rotate*-cps a1 a2 a3 (cdr lis) (lambda (v) (return (cons a1 v))))]
      [else               (rotate*-cps a1 a2 a3 (cdr lis) (lambda (v) (return (cons (car lis) v))))])))

;; myfilter
;; parameters: f and lis
;; returns a list which contains all elements in lis that can make f return true
(define myfilter
  (lambda (f lis)
    (cond
      [(null? lis)                  '()]
      [(f (car lis) (lambda (v) v)) (cons (car lis) (myfilter f (cdr lis)))]
      [else                         (myfilter f (cdr lis))])))

;; myfilter-cps
;; parameters: f and lis
;; returns a list which contains all elements in lis that can make f return true
(define myfilter-cps
  (lambda (f lis return)
    (cond
      [(null? lis)                   (return '())]
      [(f (car lis) (lambda (v ) v)) (myfilter-cps f (cdr lis) (lambda (v) (return (cons (car lis) v))))]
      [else                          (myfilter-cps f (cdr lis) (lambda (v) (return v)))])))

;; quicksort
;; parameter: lis
;; returns a sorted version
(define quicksort
  (lambda (lis)
    (cond
      [(null? lis)       '()]
      [(null? (cdr lis)) (cons (car lis) '())]
      [else              (append (quicksort (car (partition (cdr lis) (car lis))))
                                 (cons (car lis) (quicksort (car (cdr (partition (cdr lis) (car lis)))))))])))

;; quicksort-cps
;; parameter: lis
;; returns a sorted version
(define quicksort-cps
  (lambda (lis return)
    (cond
      [(null? lis)       (return '())]
      [(null? (cdr lis)) (return (cons (car lis) '()))]
      [else              (quicksort-cps (car (partition-cps (cdr lis) (car lis) (lambda (v1 v2) (list v1 v2))))
                                        (lambda (v3) (quicksort-cps (car (cdr (partition-cps (cdr lis) (car lis) (lambda (v4 v5) (list v4 v5)))))
                                                                    (lambda (v6) (return (append-cps v3 (cons (car lis) v6) (lambda (v7) v7)))))))])))

;; append-cps
(define append-cps
  (lambda (l1 l2 return)
    (if (null? l1)
        (return l2)
        (append-cps (cdr l1) l2 (lambda (v) (return (cons (car l1) v)))))))

;; replaceatoms
;; parameter: lis1 and lis2
;; returns the same as the first list
;;         but each atom of the first list, from left to right, is replaced by the corresponding atom of the second list
;;         until the second list runs out of atoms
(define replaceatoms
  (lambda (lis1 lis2)
    (replaceatoms-cps lis1 lis2 (lambda (v1 v2) v1))))

;; replaceatoms-cps
;; parameter: lis1 and lis2
;; returns the same as the first list
;;         but each atom of the first list, from left to right, is replaced by the corresponding atom of the second list
;;         until the second list runs out of atoms
(define replaceatoms-cps
  (lambda (lis1 lis2 return)
    (cond
      [(null? lis2)       (return lis1 '())]
      [(null? lis1)       (return '() lis2)]
      [(list? (car lis1)) (replaceatoms-cps (car lis1) lis2
                                            (lambda (v1 v2) (replaceatoms-cps (cdr lis1) v2
                                                                              (lambda (v3 v4) (return (cons v1 v3) v4)))))]
      [else               (replaceatoms-cps (cdr lis1) (cdr lis2) (lambda (v1 v2) (return (cons (car lis2) v1) v2)))])))

;; suffix
;; parameter: a and lis
;; returns a list containing all elements that occur after the last occurrence of the atom
(define suffix
  (lambda (a lis)
    (call/cc
     (lambda (k)
       (mysuffix a lis k)))))

;; mysuffix
;; parameter: a, lis and return
;; returns a list containing all elements that occur after the last occurrence of the atom
(define mysuffix
  (lambda (a lis return)
       (cond
         [(null? lis)       lis]
         [(eq? a (car lis)) (return (mysuffix a (cdr lis) return))]
         [else              (cons (car lis) (mysuffix a (cdr lis) return))])))

;; xindex
;; parameter: a and lis
;; return the same as the input list except that any sublist
;;        that contains the given atom should be emptied of all contents
;;        and instead, the only content of that sublist should be the index of the first occurrence of the atom in that list
(define xindex
  (lambda (a lis)
    (call/cc
     (lambda (k)
       (myxindex a lis 1 k)))))

;; myxindex
;; parameter: a, lis, acc and break 
;; return the same as the input list except that any sublist
;;        that contains the given atom should be emptied of all contents
;;        and instead, the only content of that sublist should be the index of the first occurrence of the atom in that list
(define myxindex
  (lambda (a lis acc break)
    (cond
      [(null? lis)       '()]
      [(list? (car lis)) (cons (call/cc (lambda (break) (myxindex a (car lis) 1 break))) (myxindex a (cdr lis) (+ acc 1) break))]
      [(eq? (car lis) a) (break (cons acc '()))]
      [else              (cons (car lis) (myxindex a (cdr lis) (+ acc 1) break))])))