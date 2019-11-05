#lang racket

;;;;****************************************
;;;;  Qiwen Luo (qxl216) Yixin Pan (yxp212)
;;;;  EECS 345 Spring 2019
;;;;  Programming Project 2
;;;;****************************************

(require "simpleParser.rkt")
;; Examine method (interpret "filename.txt") for testing

;; Main Functions
;; ***************************************************************************************

;; interpret
;; takes a filename, calls parser with the filename, evaluates the parse tree returned by parser,
;; and returns the proper value.
(define interpret
  (lambda (filename)
    (myInterpret (parser filename) '(()))))

;; myinterpret
;; takes the parse tree and state, evaluates the parse tree and returns the proper value
(define myInterpret
  (lambda (lis state)
    (call/cc
     (lambda (return)
       (cond
         [(null? lis)                                (error 'myInterpreter "Invalid statements")]
         [else                                       (myInterpret (nextCmd lis) (mState (command-type lis) state return
                                                                                        (lambda (v) (error 'continue "invalid continue"))
                                                                                        (lambda (v2) (error 'break "invalid break"))
                                                                                        (lambda (v3) error 'throw "invalid throw")))])))))
    
;; mState
;; defines mState to update the current state of program by calling various functions
(define mState
  (lambda (lis state return continue break throw)
    (cond
       [(null? lis)                         (error 'mState "invalid statement")]
       [(not (list? state))                 (return (error 'error state))]
       [(eq? (command-type lis) 'try)       (try lis state return continue break throw)]
       [(eq? (command-type lis) 'catch)     (catch lis state return continue break throw)]
       [(eq? (command-type lis) 'finally)   (finally lis state return continue break throw)]
       [(eq? (command-type lis) 'throw)     (throw (add-state 'exception (mValue (throw-value lis) state) state))]
       [(eq? (command-type lis) 'begin)     (block lis state return continue break throw)]
       [(eq? (command-type lis) 'continue)  (continue state)]
       [(eq? (command-type lis) 'break)     (break (popLayer state))]
       [(eq? (command-type lis) 'var)       (declare lis state)]
       [(eq? (command-type lis) '=)         (assign lis state)]
       [(eq? (command-type lis) 'while)     (while-loop lis state return continue break throw)]
       [(eq? (command-type lis) 'if)        (if-else lis state return continue break throw)]
       [(eq? (command-type lis) 'return)    (return (return-value lis state))]
       [else                                (error 'mState "invalid statement")])))

;; try 
;; try a bunch of code
;; try - catch - finally
(define try
  (lambda (lis state return continue break throw)
    (if (and (null? (catch-body lis)) (null? (finally-body lis)))
        (error 'try "try without catch and finally")
        (finally (finally-body lis)
                 (catch (catch-body lis)
                   (call/cc (lambda (throw) (try-helper lis (try-body lis) state return continue break throw))) return continue break throw) return continue break throw))))
                                                 
;; catch
;; execute when an exception has been thrown
(define catch
  (lambda (catchbody state return continue break throw)
       (cond
         [(null? catchbody)                                                            state]
         [(and (eq? (command-type catchbody) 'catch) (hasDeclared? 'exception state))  (catch (first-stmt catchbody)
                                                                                         (rename (exception-name catchbody) state) return continue break throw)]
         [(eq? (command-type catchbody) 'catch)                                        state]
         [else                                                                         (catch (nextCmd catchbody)
                                                                                         (mState (firstCmd catchbody) state return continue break throw) return continue break throw)])))

;; finally
;; execute when try and catch have done
(define finally
  (lambda (finallybody state return continue break throw)
    (cond
      [(null? finallybody) state]
      [else                (finally (nextCmd finallybody)
                             (mState (firstCmd finallybody) state return continue break throw) return continue break throw)])))

;; block
;; execute a block of code 
(define block
  (lambda (blockcode state return continue break throw)
    (cond
      [(null? blockcode)                      (popLayer state)]
      [(eq? (command-type blockcode) 'begin)  (block (nextCmd blockcode) (pushLayer state) return continue break throw)]
      [else                                   (block (nextCmd blockcode)
                                                 (mState (firstCmd blockcode) state return continue break throw) return continue break throw)])))

;; declare
;; declares a variable
(define declare
  (lambda (cmd state)
    (cond
      [(null? (next cmd))                         (error 'declare "invalid declaration")]
      [(hasDeclared? (variable cmd) state)        (error 'declare "The variable has been declared.")]
      [(hasValue? cmd)                            (add-state (variable cmd) (mValue (value cmd) state) state)]
      [else                                       (add-state (variable cmd) 'novalue state)])))

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
      [(and (eq? (operator exp) '-) (unary? exp))         (- 0 (mValue (operand1 exp) state))]
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
  (lambda (cmd state return continue break throw)
    (call/cc
     (lambda (break)
       (cond
         [(null? (next cmd))                      (error 'while-loop "invalid while-loop")]
         [(mBoolean (condition cmd) state)        (mState cmd (call/cc (lambda (continue) (mState (loop-body cmd) state return continue break throw))) return continue break throw)]
         [(not (mBoolean (condition cmd) state))  state]
         [else                                    (error 'while-loop "invalid while-loop")])))))

;; if-else
;; executes if-else statement
(define if-else
  (lambda (cmd state return continue break throw)
    (cond
      [(null? (next cmd))                                                      (error 'if-else "invalid if-else statement")]
      [(mBoolean (condition cmd) state)                                        (mState (stmt1 cmd) state return continue break throw)]
      [(and (not (mBoolean (condition cmd) state)) (null? (checkElse cmd)))    state]   
      [(not (mBoolean (condition cmd) state))                                  (mState (stmt2 cmd) state return continue break throw)]
      [else                                                                    (error 'if-else "invalid if-else statement")])))

;; return-value
;; returns the proper value
(define return-value
  (lambda (cmd state)
    (cond
      [(null? (next cmd))                        (error 'return "invalid return statement")]
      ;;[(eq? state "invalid throw")               (error 'throw "invalid throw")]
      [(eq? (mValue (expReturn cmd) state) #t)   'true]
      [(eq? (mValue (expReturn cmd) state) #f)   'false]
      [else                                      (mValue (expReturn cmd) state)])))
      
;;-----------------------------------------------------------------------------------
;; helper functions

;; add-state
;; creates a new binding pairs to the top layer
(define add-state
  (lambda (var value state)
    (cons (append (current state) (cons (cons var (cons value '())) '())) (next state))))

;; try-helper
;; executes codes in try-body one by one
(define try-helper
  (lambda (cmd trybody state return continue break throw)
    (cond
      [(null? trybody)                                  state]
      [(eq? 'return (command-type (firstCmd trybody)))  (mState (car trybody) (finally (finally-body cmd) state return continue break throw) return continue break throw)]
      [(eq? 'break (command-type (firstCmd trybody)))   (mState (car trybody) (finally (finally-body cmd) state return continue break throw) return continue break throw)]
      [else                                             (try-helper cmd (nextCmd trybody)
                                                            (mState (firstCmd trybody) state return continue break throw) return continue break throw)])))

;; rename
;; returns the state where the exception gets the name
(define rename
  (lambda (exceptionname state)
    (cons (rename-helper exceptionname (toplayer state)) (nextlayer state))))

;; rename-helper
;; change the name of exception from 'exception to the name in catch 
(define rename-helper
  (lambda (e thislayer)
    (cond
      [(null? thislayer)                    '()]
      [(eq? (getName thislayer) 'exception)  (cons (cons e (cons (getValue thislayer) '())) (next thislayer))]
      [else                                  (cons (current thislayer) (rename-helper e (next thislayer)))]))) 
    
;; pushLayer
;; add a layer at the top of state
(define pushLayer
  (lambda (state)
    (cons '() state)))

;; popLayer
;; remove a layer at the top of state and update-state to whole state
(define popLayer
  (lambda (state)
    (next state)))

;; hasDeclared
;; examines whether the variable has declared globally
(define hasDeclared?
  (lambda (varName state)
    (cond 
      [(null? state)                               #f]
      [(hasDeclared?-helper varName (car state))   #t]
      [else                                        (hasDeclared? varName (next state))])))

;; hasDeclared-helper
;; examines whether the variable has already declared
;; check at the current layer
(define hasDeclared?-helper
  (lambda (varName state)
    (cond
      [(null? state)                    #f]
      [(eq? (firstVar state) varName)   #t]
      [else                             (hasDeclared?-helper varName (next state))])))

;; hasVariable
;; checks whether the variable will be declared and assigned a value at the same time
(define hasValue?
  (lambda (cmd)
    (if (null? (valueAssign cmd))
        #f
        #t)))

;; hasAssigned
;; checks whether the variabled has been assigned a value
(define hasAssigned?
  (lambda (varName state)
    (cond
      [(null? state)                                  #f]
      [(hasAssigned?-helper varName (current state))  #t]
      [else                                           (hasAssigned? varName (next state))])))

;; hasAssigned?-helper
;; checks whether the variabled has been assigned a value
;; check at the current layer
(define hasAssigned?-helper
  (lambda (varName state)
    (cond
      [(null? state)                                                               #f]
      [(and (eq? (firstVar state) varName) (not (eq? (getValue state) 'novalue)))  #t]
      [else                                                                        (hasAssigned?-helper varName (next state))])))

;; unary
;; check whether the expression is a unary
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
      [(null? state)                                 (error 'var-value "Variable has not declared yet")]
      [(number? (var-value-helper var (car state)))  (var-value-helper var (current state))]
      [(eq? (var-value-helper var (car state)) #t)   #t]
      [(eq? (var-value-helper var (car state)) #f)   #f]
      [else                                          (var-value var (next state))])))
      
;; var-value-helper
;; returns the value of the variable (if it is on the current layer)
(define var-value-helper
  (lambda (var state)
    (cond
      [(null? state)               '()]
      [(eq? var (getName state))   (getValue state)]
      [else                        (var-value-helper var (next state))])))

;; update-state
;; updates an existing binding pair in the state
(define update-state
  (lambda (var value state)
    (cond
      [(null? state)                         '()]
      [(hasDeclared?-helper var (car state)) (cons (update-state-helper var value (current state)) (next state))]
      [else                                  (cons (current state) (update-state var value (next state)))])))

;; update-state-helper
;; updates an existing binding pair if it is in the current layer
(define update-state-helper
  (lambda (var value state)
    (cond
      [(null? state)               '()]
      [(eq? var (getName state))   (cons (cons var (cons value '())) (update-state-helper var value (next state)))]
      [else (cons (current state)  (update-state-helper var value (next state)))])))

;; mBoolean
;; checks whether the condition for a while-loop or if-else statement(s) is true
(define mBoolean
  (lambda (if-cond state)
    (cond
      [(null? if-cond)               (error 'mBoolean "invalid if-else statement")]
      [(eq? if-cond 'true)           #t]
      [(eq? if-cond 'false)          #f]
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

;; command-type
;; car of command
;; determines the command-type
(define command-type
  (lambda (cmd)
    (car cmd)))

;; try-body
;; try body of try-catch-finally block
(define try-body
  (lambda (cmd)
    (car (cdr cmd))))

;; catch-body
;; catch body of try-catch-finally block
(define catch-body
  (lambda (cmd)
    (if (null? (car (cdr (cdr cmd))))
        '()
        (car (cdr (cdr cmd))))))

;; throw-value
;; the value the statement throw return
(define throw-value
  (lambda (lis)
    (car (cdr lis))))

;; toplayer
(define toplayer
  (lambda (state)
    (car state)))

;; nextlayer
(define nextlayer
  (lambda (state)
    (cdr state)))

;; exception-name
(define exception-name
  (lambda (catchbody)
    (car (car (cdr catchbody)))))

;; firstCmd
;; first statement of catch
(define first-stmt
  (lambda (stmt-list)
    (car (cdr (cdr stmt-list)))))

;; firstCmd
;; first command of a block of code
(define firstCmd
  (lambda (cmd)
    (car cmd)))

;; nextCmd
;; cdr of lis of parse code
;; determine the rest parse code to run
(define nextCmd
  (lambda (cmd)
    (cdr cmd)))

;; finally-body
;; finally body of try-catch-finally block
(define finally-body
  (lambda (cmd)
    (if (null? (car (cdr (cdr (cdr cmd)))))
        '()
         (car (cdr (car (cdr (cdr (cdr cmd)))))))))

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
  