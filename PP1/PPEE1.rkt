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
      [(number? state)                            state]
      [(or (eq? state 'true) (eq? state 'false))  state]
      [(null? lis)                                (error 'myInterpreter "Invalid statements")]
      [else                                       (myInterpret (nextCmd lis) (mState (command-type lis) state))])))
    
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

;; mValue
;; evaluates an expression with or without variables
(define mValue
  (lambda (exp state)
     (cond
      [(null? exp)                                        (error 'mValue "undefined expression")]
      [(number? exp)                                      exp]
      [(and (not (pair? exp)) (hasAssigned? exp state))   (var-value exp state)]
      [(eq? exp 'true)                                    #t]
      [(eq? exp 'false)                                   #f]
      [(and (not (pair? exp)) (hasDeclared? exp state))   (error 'mValue "using before assigning")]
      [(not (pair? exp))                                  (error 'mValue "using before declaring")]
      [(eq? (operator exp) '+)                            (+ (mValue (operand1 exp) state) (mValue (operand2 exp) state))]
      [(and (eq? (operator exp) '-) (unary? exp))   (- 0 (mValue (operand1 exp) state))]
      [(eq? (operator exp) '-)                            (- (mValue (operand1 exp) state) (mValue (operand2 exp) state))]
      [(eq? (operator exp) '*)                            (* (mValue (operand1 exp) state) (mValue (operand2 exp) state))]
      [(eq? (operator exp) '/)                            (quotient (mValue (operand1 exp) state) (mValue (operand2 exp) state))]
      [(eq? (operator exp) '%)                            (remainder (mValue (operand1 exp) state) (mValue (operand2 exp) state))]
      [else                                               (mBoolean exp state)])))

;; assign
;; assigns a declared variable a (new) value
(define assign
  (lambda (cmd state)
    (cond
      [(null? (next cmd))                   (error 'assign "invalid assignmnet.")]
      [(hasDeclared? (variable cmd) state)  (update-state (variable cmd) (mValue (value cmd) state) state)]
      [else                                 (error 'assign "Variable has not been declared yet.")])))


;; while-loop
;; executes a while loop
(define while-loop
  (lambda (cmd state)
    (cond
      [(null? (next cmd))                      (error 'while-loop "invalid while-loop")]
      [(mBoolean (condition cmd) state)        (mState cmd (mState (loop-body cmd) state))]
      [(not (mBoolean (condition cmd) state))  state]
      [else                                    (error 'while-loop "invalid while-loop")])))

;; if-else
;; executes if-else statement
(define if-else
  (lambda (cmd state)
    (cond
      [(null? (next cmd))                                                      (error 'if-else "invalid if-else statement")]
      [(mBoolean (condition cmd) state)                                        (mState (stmt1 cmd) state)]
      [(and (not (mBoolean (condition cmd) state)) (null? (checkElse cmd)))    state]   
      [(not (mBoolean (condition cmd) state))                                  (mState (stmt2 cmd) state)]
      [else                                                                    (error 'if-else "invalid if-else statement")])))

;; return
;; returns the proper value
(define return
  (lambda (cmd state)
    (cond
      [(null? (next cmd))                        (error 'return "invalid return statement")]
      [(eq? (mValue (expReturn cmd) state) #t)   'true]
      [(eq? (mValue (expReturn cmd) state) #f)   'false]
      [else                                      (mValue (expReturn cmd) state)])))
      
;;-----------------------------------------------------------------------------------
;; helper functions

;; hasDeclared
;; examines whether the variable has already declared
(define hasDeclared?
  (lambda (varName state)
    (cond
      [(null? state)                    #f]
      [(eq? (firstVar state) varName)   #t]
      [else                             (hasDeclared? varName (next state))])))

;; hasVariable
;; checks whether the variable will be declared and assigned a value at the same time
(define hasValue?
  (lambda (cmd)
    (if (null? (valueAssign cmd))
        #f
        #t)))

;; add-state
;; creates a new binding pairs
(define add-state
  (lambda (var value state)
    (append state (cons (cons var (cons value '())) '()))))

;; hasAssigned
;; checks whether the variabled has been assigned a value
(define hasAssigned?
  (lambda (varName state)
    (cond
      [(null? state)                                                               #f]
      [(and (eq? (firstVar state) varName) (not (eq? (getValue state) 'novalue)))  #t]
      [else                                                                        (hasAssigned? varName (next state))])))

;; unary
;;
(define unary?
  (lambda (exp)
    (if (null? (operand exp))
        #t
        #f)))

;; var-value
;; returns the value of the variable
(define var-value
  (lambda (var state)
    (cond
      [(null? state)               (error 'var-value "Variable has not declared yet")]
      [(eq? var (getName state))   (getValue state)]
      [else                        (var-value var (next state))])))

;; update-state
;; updates an existing binding pair
(define update-state
  (lambda (var value state)
    (cond
      [(null? state)               '()]
      [(eq? var (getName state))   (cons (cons var (cons value '())) (update-state var value (next state)))]
      [else (cons (current state)  (update-state var value (next state)))])))

;; mBoolean
;; checks whether the condition for a while-loop or if-else statement(s) is true
(define mBoolean
  (lambda (if-cond state)
    (cond
      [(null? if-cond)               (error 'mBoolean "invalid if-else statement")]
      [(eq? (operator if-cond) '==)  (eq? (mValue (operand1 if-cond) state) (mValue (operand2 if-cond) state))]
      [(eq? (operator if-cond) '!=)  (not (eq? (mValue (operand1 if-cond) state) (mValue (operand2 if-cond) state)))]
      [(eq? (operator if-cond) '<)   (< (mValue (operand1 if-cond) state) (mValue (operand2 if-cond) state))]
      [(eq? (operator if-cond) '>)   (> (mValue (operand1 if-cond) state) (mValue (operand2 if-cond) state))]
      [(eq? (operator if-cond) '<=)  (<= (mValue (operand1 if-cond) state) (mValue (operand2 if-cond) state))]
      [(eq? (operator if-cond) '>=)  (>= (mValue (operand1 if-cond) state) (mValue (operand2 if-cond) state))]
      [(eq? (operator if-cond) '&&)  (and (mValue (operand1 if-cond) state) (mValue (operand2 if-cond) state))]
      [(eq? (operator if-cond) '||)  (or (mValue (operand1 if-cond) state) (mValue (operand2 if-cond) state))]
      [(eq? (operator if-cond) '!)   (not (mValue (operand1 if-cond) state))])))
      
;;-----------------------------------------------------------------------------------
;; abstractions

;; nextCmd
;; cdr of lis of parse code
;; determine the rest parse code to run
(define nextCmd
  (lambda (lis)
    (cdr lis)))
    
;; command-type
;; car of command
;; determines the command-type
(define command-type
  (lambda (cmd)
    (car cmd)))
  
;; variable
;; car of cdr of a command
;; gets the varible's name
(define variable
  (lambda (cmd)
    (car (cdr cmd))))

;; value
;; car of cdr of cdr of a command
;; gets the value of the variable
(define value
  (lambda (cmd)
   (car (cdr (cdr cmd)))))


;; operator
;; car of the expression
;; gets the operator of the expression
(define operator
  (lambda (exp)
    (car exp)))

;; operand1
;; car of cdr of the expression
;; gets the left operand of the expression
(define operand1
  (lambda (exp)
    (car (cdr exp))))

;; operand2
;; car of cdr of cdr of exp
;; gets the right operand of the expression
(define operand2
  (lambda (exp)
    (car (cdr (cdr exp)))))

;; operand
;; gets the right operand with its brackets '(right operand)
(define operand
  (lambda (exp)
    (cdr (cdr exp))))
 
;; getName
;; gets the name of variable in each binding pairs
(define getName
  (lambda (lis)
    (car (car lis))))

;; getValue
;; gets the value of the variable in each binding pairs
(define getValue
  (lambda (lis)
    (car (cdr (car lis)))))

;; next
;; returns the same state without the first binding pairs
(define next
  (lambda (state)
    (cdr state)))

;; current
;; returns the first binding pairs
(define current
  (lambda (state)
    (car state)))

;; condition
;; returns the condition part of while-loop or if-else
(define condition
  (lambda (cmd)
    (car (cdr cmd))))

;; loop-body
;; returns the body of the while loop
(define loop-body
  (lambda (cmd)
    (car (cdr (cdr cmd)))))

;; stmt1
;; return the first statement of if-else statement (if condition)
(define stmt1
  (lambda (lis)
    (car (cdr (cdr lis)))))

;; stmt2
;; return the second statement in if-else statement (else condition)
(define stmt2
  (lambda (lis)
    (car (cdr (cdr (cdr lis))))))

;; checkElse
;; return if the second statement in if-else statement exist
(define checkElse
  (lambda (lis)
    (cdr (cdr (cdr lis)))))

;; expReturn
;; return the statement of the expression to return its value
(define expReturn
  (lambda (lis)
    (car (cdr lis))))

;; firstVar
;; return the name of first varible stored in the state
(define firstVar
  (lambda (lis)
    (car (car lis))))

;; valueAssign
;; return the statement of assignment of value in the declaration
(define valueAssign
  (lambda (lis)
    (cdr (cdr lis))))
  