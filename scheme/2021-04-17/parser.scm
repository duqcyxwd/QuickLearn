;;;; Meta-Circular Interpreter
;;;; Based on The Structure and Interpretation of Computer Programs, Ch4.

;;;alias built-in apply -- must precede def of metacircular apply

;; New code
;; Changes include
;; 1. eval
;; 2. let?
;; 3. let*?
;; 4. let-binding
;; 5. let-exp
;; 6. eval-let
;; 6. eval-let*

(define apply-in-underlying-scheme apply)

;; --Eval/Apply--
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((let? exp) (eval-let exp env))
        ((let*? exp) (eval-let* exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence (procedure-body procedure)
                        (extend-environment (procedure-parameters procedure)
                                            arguments
                                            (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))


;; --Special Evals--
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)



;; -- Expression Abstraction Barriers--
(define (self-evaluating? exp)
  (cond ((number? exp) #t)
        ((string? exp) #t)
        (else #f)))

(define (variable? exp) (symbol? exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))

; - Special form: quote -
(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp)(cadr exp))

; - Special form: set! - 
(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))

(define (assignment-value exp) (caddr exp))

; - Special form: define - 
(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))

; - Special form: lambda -
(define (lambda? exp) (tagged-list? exp 'lambda))

(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

; - Special forms: if and cond -
(define (if? exp) (tagged-list? exp 'if))

(define (if-predicate exp) (cadr exp))

(define (if-consequent exp) (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))
  
(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))

(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))

(define (cond-predicate clause) (car clause))

(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false                          ; no else clause
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))


; - Special form: begin -
(define (begin? exp) (tagged-list? exp 'begin))

(define (begin-actions exp) (cdr exp))

(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

; - Default application -
;    an application is any compound expression that's not one of the above special forms
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

; - Abstraction for procedure objects - 
;      procedure objects are created by eval'ing lambdas
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))



; -- The Environment -- 

(define (true? x)
  (not (eq? x #f)))
(define (false? x)
  (eq? x #f))

; - Environment accessors - 
(define (enclosing-environment env) (cdr env))

(define (first-frame env) (car env))

(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))

; - Environment mutators - 
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env) 
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)   ;add new (local) frame
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)   ;looks through one frame
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))

; - Initialize the environment - 
(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true #t initial-env)
    (define-variable! 'false #f initial-env)
    initial-env))

; - Primitive environment abstraction definitions -
(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc) (cadr proc)) 

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        (list '* *)
        (list '- -)
        (list '/ /)
        (list '= =)
        ;;      more primitives
        ))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primitive-implementation proc) args))


; -- User Interface --

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

;hacky error procedure
(define (error message . args)
  (display message)
  (for-each (lambda (arg) (display " ")(display arg)) args)
  (newline)
  ('error);cause a native error
  )

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>)) 
      (display object)))


; -- Main Loop to drive metacircular interpreter --
(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))


(define D (lambda args (if (pair? args) (map (lambda (a) (display (string a " "))) args)) (display "\n")))
(define DD (lambda args (apply-in-underlying-scheme D args) (display "\n")))

; -- Ignition! --
'METACIRCULAR-INTERPRETER-LOADED

(define the-global-environment (setup-environment))
(define (let? exp) (tagged-list? exp 'let))
(define (let*? exp) (tagged-list? exp 'let*))


(define (let-binding exp) (cadr exp))
(define (let-exp exp) (caddr exp))


(define (eval-let exp env)
  (define (let-bind-keys exp)
    (map car (cadr exp)))
  (define (let-bind-values exp)
    (map cadr (cadr exp)))
  (let ((bind  (let-binding exp)))
    (if (null? bind)
      (eval (let-exp exp) env)
      (eval (let-exp exp) (extend-environment (let-bind-keys exp)
                                              (list-of-values (let-bind-values exp) env)
                                              env)))))

;; (D "\nLet result: " (eval '(let () (+ 100 1)) the-global-environment))
;; (D "\nLet result: " (eval '(let ((x (* 4 100)) (y 201)) (+ y x)) the-global-environment))
;; (D "\nLet result: " (eval '(let ((a 1) (b 2)) (+ a b)) the-global-environment))

(define (eval-let* exp env)
  (define (extend-let-environment exp)
    (extend-environment (list (caar (let-binding exp)))
                        (list-of-values (cdar (let-binding exp)) env)
                        env))
  (if (null? (let-binding exp))
    (eval (let-exp exp) env)
    (if (null? (cdr (let-binding exp)))
      (eval (let-exp exp) (extend-let-environment exp))
      (eval-let* (list (car exp) (cdr (let-binding exp)) (let-exp exp))
                 (extend-let-environment exp)))))

;; (D "\nLet result: " (eval '(let* () (+ 1 1)) the-global-environment))
;; (D "\nLet result: " (eval '(let* ((x (* 4 100)) (y 201) (z 2000)) (+ x (+ y z))) the-global-environment))
;; (D "\nLet result: " (eval '(let* ((x 100)) (+ x 1)) the-global-environment))
;; (D "\nLet result: " (eval '(let* ((x (* 2 2)) (y (* x 2)) (z 100)) (* z (+ y x))) the-global-environment))

(DD "\n\n--------------------End---------------------")


;; (driver-loop)
