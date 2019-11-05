#lang racket
(require "simpleParser.rkt")
;;;;****************************************
;;;;  Qiwen Luo (qxl216) Yixin Pan (yxp212)
;;;;  EECS 345 Spring 2019
;;;;  Programming Project 1
;;;;****************************************

;; interpret
;; takes a filename, calls parser with the filename, evaluates the parse tree returned by parser,
;; and returns the proper value.
(define interpret
  (lambda (filename)
    (myInterpret (parser filename) '())))

;; myinterpret
;; takes the parse tree and state, evaluates the parse tree and returns the proper value
(define myInterpret
  (lambda (lis state)
    (cond
      [(number? state) state]
      [(or (eq? state 'true) (eq? state 'false)) state]
      [(null? lis) (error 'myInterpreter "Invalid statements")]
    ;  [(eq? (car (car lis)) 'return) (return (car lis) state)]
      [else (myInterpret (cdr lis) (mState (car lis) state))])))
    
;; mState
;; defines mState to update the current state of program by calling various functions
(define mState
  (lambda (lis state)
    (cond
       [(null? lis)                      (error 'mState "invalid statement")]
       [(eq? (command-type lis) 'var)    (declare lis state)]
       [(eq? (command-type lis) '=)      (assign lis state)]
       [(eq? (command-type lis) 'while)  (while-loop lis state)]
       [(eq? (command-type lis) 'if)     (if-else lis state)]
       [(eq? (command-type lis) 'return) (return lis state)]
       [else                             (error 'mState "invalid statement")])))

;; declare
;; declares a variable
(define declare
  (lambda (cmd state)
    (cond
      [(null? (next cmd))                         (error 'declare "invalid declaration")]
      [(hasDeclared? (variable cmd) state)        (error 'declare "The variable has been declared.")]
      [(hasValue? cmd)                            (add-state (variable cmd) (mValue (value cmd) state) state)]
      [else                                       (add-state (variable cmd) 'novalue state)]         )))


(define hasValue?
  (lambda (cmd)
    (if (null? (cdr (cdr cmd)))
        #f
        #t)))

;; mValue
;; evaluates an expression with or without variables
(define mValue
  (lambda (exp state)
     (cond
      [(null? exp)              (error 'mValue "undefined expression")]
      [(number? exp)            exp]
      [(and (not (pair? exp)) (hasAssigned? exp state))        (var-value exp state)]
      [(eq? exp 'true) #t]
      [(eq? exp 'false) #f]
      [(not (pair? exp))           (error 'mValue "using before assigning")]
      [(eq? (operator exp) '+)  (+ (mValue (operand1 exp) state) (mValue (operand2 exp) state))]
      [(and (eq? (operator exp) '-) (null? (cddr exp)))  (- 0 (mValue (operand1 exp) state))]
      [(eq? (operator exp) '-)  (- (mValue (operand1 exp) state) (mValue (operand2 exp) state))]
      [(eq? (operator exp) '*)  (* (mValue (operand1 exp) state) (mValue (operand2 exp) state))]
      [(eq? (operator exp) '/)  (quotient (mValue (operand1 exp) state) (mValue (operand2 exp) state))]
      [(eq? (operator exp) '%)  (remainder (mValue (operand1 exp) state) (mValue (operand2 exp) state))]
      [else (mBoolean exp state)])))

;;
(define hasAssigned?
  (lambda (varName state)
    (cond
      [(null? state) #f]
      [(and (eq? (caar state) varName) (not (eq? (getValue state) 'novalue))) #t]
      [else (hasAssigned? varName (cdr state))])))

;;
(define var-value
  (lambda (var state)
    (cond
      [(null? state) (error 'var-value "Variable has not declared yet")]
      [(eq? var (getName state)) (getValue state)]
      [else (var-value var (next state))])))

;;
(define assign
  (lambda (cmd state)
    (cond
      [(null? (next cmd)) (error 'assign "invalid assignmnet.")]
      [(hasDeclared? (variable cmd) state) (update-state (variable cmd) (mValue (value cmd) state) state)]
      [else (error 'assign "Variable has not been declared yet.")])))


;; hasDeclared, a helper function to examine whether a variable has already declared
(define hasDeclared?
  (lambda (varName state)
    (cond
      [(null? state)              #f]
      [(eq? (caar state) varName) #t]
      [else                       (hasDeclared? varName (cdr state))])))

;;
(define add-state
  (lambda (var value state)
    (append state (cons (cons var (cons value '())) '()))))

;;
(define update-state
  (lambda (var value state)
    (cond
      [(null? state) '()]
      [(eq? var (getName state))  (cons (cons var (cons value '())) (update-state var value (next state)))]
      [else (cons (current state) (update-state var value (next state)))])))
;;
(define while-loop
  (lambda (cmd state)
    (cond
      [(null? (next cmd)) (error 'while-loop "invalid while-loop")]
      [(mBoolean (condition cmd) state) (mState (cdr (cdr cmd)) (mState (cdr (cdr cmd)) state))]
      [(not (mBoolean (condition cmd) state)) (state)]
      [else (error 'while-loop "invalid while-loop")])))
;;
(define if-else
  (lambda (cmd state)
    (cond
      [(null? (next cmd)) (error 'if-else "invalid if-else statement")]
      [(mBoolean (condition cmd) state) (mState (stmt1 cmd) state)]
      [(and (not (mBoolean (condition cmd) state)) (null? (cdr (cdr (cdr cmd))))) state]   ;;too much cdr
      [(not (mBoolean (condition cmd) state)) (mState (stmt2 cmd) state)]
      [else (error 'if-else "invalid if-else statement")])))

;;
(define mBoolean
  (lambda (if-cond state)
    (cond
      [(null? if-cond) (error 'mBoolean "invalid if-else statement")]
      [(eq? (operator if-cond) '==) (eq? (mValue (operand1 if-cond) state) (mValue (operand2 if-cond) state))]
      [(eq? (operator if-cond) '!=) (not (eq? (mValue (operand1 if-cond) state) (mValue (operand2 if-cond) state)))]
      [(eq? (operator if-cond) '<)  (< (mValue (operand1 if-cond) state) (mValue (operand2 if-cond) state))]
      [(eq? (operator if-cond) '>)  (> (mValue (operand1 if-cond) state) (mValue (operand2 if-cond) state))]
      [(eq? (operator if-cond) '<=) (<= (mValue (operand1 if-cond) state) (mValue (operand2 if-cond) state))]
      [(eq? (operator if-cond) '>=) (>= (mValue (operand1 if-cond) state) (mValue (operand2 if-cond) state))]
      [(eq? (operator if-cond) '&&) (and (mValue (operand1 if-cond) state) (mValue (operand2 if-cond) state))]
      [(eq? (operator if-cond) '||) (or (mValue (operand1 if-cond) state) (mValue (operand2 if-cond) state))]
      [(eq? (operator if-cond) '!)  (not (mValue (operand1 if-cond) state))])))
      

;;
(define return
  (lambda (cmd state)
    (cond
      [(null? (next cmd)) (error 'return "invalid return statement")]
      [(eq? (mValue (car (cdr cmd)) state) #t) 'true]
      [(eq? (mValue (car (cdr cmd)) state) #f) 'false]
      [else               (mValue (car (cdr cmd)) state)])))
      
      

                     
;;-----------------------------------------------------------------------------------
;; helper methods

;; determine the command-type to be the car of lis
;; An abstraction make thing clear
(define command-type
  (lambda (lis)
    (car lis)))
  
;; an abstraction getting varible's name
(define variable
  (lambda (lis)
    (car (cdr lis))))

;;
(define value
  (lambda (lis)
   (car (cdr (cdr lis)))))


;;
(define operator
  (lambda (exp)
    (car exp)))

;;
(define operand1
  (lambda (exp)
    (car (cdr exp))))

;;
(define operand2
  (lambda (exp)
    (car (cdr (cdr exp)))))

;;
(define getName
  (lambda (lis)
    (car (car lis))))

;;
(define getValue
  (lambda (lis)
    (car (cdr (car lis)))))

;;
(define next
  (lambda (lis)
    (cdr lis)))
;;
(define current
  (lambda (lis)
    (car lis)))

;;
(define condition
  (lambda (lis)
    (car (cdr lis))))

;;
(define stmt1
  (lambda (lis)
    (car (cdr (cdr lis)))))

;;
(define stmt2
  (lambda (lis)
    (car (cdr (cdr (cdr lis))))))


  