(begin
  (display "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n\n")
  )

;; Test code
(define D (lambda args (if (pair? args) (map (lambda (a) (display (string a " "))) args)) (display "\n")))
(define DD (lambda args (apply D args) (display "\n")))

(DD "hi" "hi2")

(define test1 '(+ 1 1))

;; (define (plus seq)
;;   (D "Seq: " seq)
;;   ;; (begin (if (not (null? seq)) (D (number? (car seq)))))
;;   (cond ((null? seq) 0)
;;         ((number? (car seq)) (+ (car seq)
;;                                 (plus (cdr seq))))
;;         ((pair? (car seq)) (plus (append (list (eval-prefix (car seq)))
;;                                          (cdr seq))))
;;         ;; TODO cdl seq
;;         (else (begin
;;                 (D "Unknow: " seq)
;;                 (D "Unknow: " (pair? seq))
;;                 0))))

;; (D (plus '(1 2 3 4 5 (1 2))))

;; (define (execute-procedure procedure args)
;;   (cond ((equal? procedure '+) (plus args))
;;         (else (begin
;;                 (D "Unknown procedure" procedure)
;;                 0))))

;; (D (execute-procedure '+ '(1 2 3 4)))
;; (D (execute-procedure '+ '(1 (+ 2 2))))
;; (D (execute-procedure '- '(1 2 3 4)))

;; (define (eval-prefix exp)
;;   (D "prefix: " exp)
;;   (cond
;;     ((pair? exp)
;;      (execute-procedure (car exp) (cdr exp)))
;;     ))

;; (D "Res" (eval-prefix '(+ 1 (+ 1 1) 100 (+ 1000))))

;; (define (tagged-list? exp tag) (if (pair? exp) (eq? (car exp) tag) #f))

;; Question 1 i
(define (execute-procedure procedure tree tree2)
  (cond ((equal? procedure '+) (+ (eval-prefix tree) (eval-prefix tree2)))
        ((equal? procedure '-) (- (eval-prefix tree) (eval-prefix tree2)))
        ((equal? procedure '*) (* (eval-prefix tree) (eval-prefix tree2)))
        ((equal? procedure '/) (/ (eval-prefix tree) (eval-prefix tree2)))
        (else (display "Unknown procedure" procedure))))

(define (eval-prefix tree)
  (cond
    ((number? tree) tree)
    ((pair? tree) (execute-procedure (car tree) (car (cdr tree)) (car (cdr (cdr tree)))))))

;; (D (eval-prefix '(+ 1 2)))
;; (D (eval-prefix '(+ (+ 100 100) (+ 200 200))))
;; (D (eval-prefix '(+ (* 1 100) (/ 200 200))))
;; (D (eval-prefix '(+ (* 1 100) (/ 200 200))))
;; (D (eval-prefix '(* (+ 1 2) (* 1 (+ 1 (- 4 2))))))
;; (D (eval-prefix '(+ (+ 1 2) 3)))



;; Question 1 ii
;; 1 number
;; 2. pair
;; Pair to process

;; Solution 1

;; (1 + 2 * 3) -> (+ 1 (* 2 3))
;; (2 * 2 * 3) -> (* 2 3) (* (* 2 2) 3)

(define (plus-minus? proc)
  (or (equal? '+ proc)
      (equal? '- proc)))

(define (multi-div? proc)
  (or (equal? '* proc)
      (equal? '/ proc)))

(define (oper? proc)
  (or (equal? '+ proc)
      (equal? '- proc)
      (equal? '* proc)
      (equal? '/ proc)))

;; Solution 0
(define (infix->prefix infix level)
  (let ((arg (parse-infix (car infix)))
        (proc (car (cdr infix)))
        (arg2 (parse-infix (car (cdr (cdr infix)))))
        (rest (cdr (cdr (cdr infix)))))
    (D "-> " infix level proc)
    (cond
      ((and (null? rest)
            (oper? proc))
       (begin
         (D "null?")
         (list proc arg arg2)))
      ((null? rest) infix)
      ((plus-minus? proc)
       (D "+/-" (list (list proc arg arg2)) level)
       (if (equal? 'first level)
         ;; Skip + and - for first run
         (append (list arg proc) (list (infix->prefix (cdr (cdr infix)) level)))
         (infix->prefix (append (list (list proc arg arg2)) rest)
                        level)))
      ;; First run
      ((multi-div? proc)
       (D "*/")
       (infix->prefix (append (list (list proc arg arg2)) rest)
                      level))
      (else
        (D "Rest I don't want use this rest")
        ;; SKip
        (append (list arg proc) (infix->prefix (cdr (cdr infix)) level))
        ;; (infix->prefix (append (list (list proc arg arg2)) rest) level)
        ))))

;; Solution 1 impliment
(define (parse-infix infix)
  (cond ((number? infix) infix)
        ((symbol? (car infix)) infix)
        ((pair? infix) (pm-infix->prefix (md-infix->prefix infix) ))))
(define (md-infix->prefix infix)
  (if (symbol? (car infix))
    infix
    (let ((arg (parse-infix (car infix)))
          (proc (car (cdr infix)))
          (arg2 (parse-infix (car (cdr (cdr infix)))))
          (rest (cdr (cdr (cdr infix)))))

      (cond ((and (null? rest) (multi-div? proc)) (list proc arg arg2))
            ((null? rest) (list arg proc arg2))
            ((multi-div? proc) (md-infix->prefix (append (list (list proc arg arg2)) rest)))
            (else (append (list arg proc) (list (md-infix->prefix (cdr (cdr infix))))))))))

(define (pm-infix->prefix infix)
  (if (symbol? (car infix))
    infix
    (let ((arg (parse-infix (car infix)))
          (proc (car (cdr infix)))
          (arg2 (parse-infix (car (cdr (cdr infix)))))
          (rest (cdr (cdr (cdr infix)))))
      (cond
        ((and (null? rest) (oper? proc)) (list proc arg arg2))
        ((plus-minus? proc) (pm-infix->prefix (append (list (list proc arg arg2)) rest)))
        (else (append (list arg proc) (pm-infix->prefix (cdr (cdr infix)))))))))

;; Question 1 ii ;; Solution 2

(define (md-add-bracket infix)
  (D "md-ADD" infix)
  (let ((arg (add-bracket (car infix)))
        (proc (car (cdr infix)))
        (arg2 (add-bracket (car (cdr (cdr infix)))))
        (rest (cdr (cdr (cdr infix)))))
    (cond ((and (null? rest) (multi-div? proc)) (list arg proc arg2))
          ((null? rest) infix)
          ((multi-div? proc) (md-add-bracket (append (list (list arg proc arg2)) rest)))
          (else (begin
                  (D "else Process plus minus" (list arg proc))
                  (let ((fix (md-add-bracket (cdr (cdr infix)))))
                    (if (multi-div? (car (cdr fix)))
                      (append (list arg proc) (list fix))
                      (append (list arg proc) fix))))))))

(define (pm-add-bracket infix)
  (D "pm-ADD" infix)
  (let ((arg (add-bracket (car infix)))
        (proc (car (cdr infix)))
        (arg2 (add-bracket (car (cdr (cdr infix)))))
        (rest (cdr (cdr (cdr infix)))))
    (cond ((and (null? rest) (oper? proc)) (list arg proc arg2))
          ((plus-minus? proc) (pm-add-bracket (append (list (list arg proc arg2)) rest)))
          (else (begin
                  (D "else----------Something wrong" (list arg proc))
                  (append (list arg proc)
                          (list (pm-add-bracket (cdr (cdr infix))))))))))

(define (add-bracket infix)
  (cond ((number? infix) infix)
        ((symbol? (car infix)) infix)
        ((pair? infix) (if (> (length infix) 3)
                         (pm-add-bracket (md-add-bracket infix))
                         infix))))

;; (D (length '+))

;; (DD (md-add-bracket '(1 * 2)))
;; (DD (md-add-bracket '(1 * 2 * 3)))
;; (DD (md-add-bracket '(1 * 2 * 3 * 4)))


(define (infix->prefix infix)
  (cond
    ((number? infix) infix)
    ((oper? (car (cdr infix)))
     (let ((arg (infix->prefix (car infix)))
           (proc (car (cdr infix)))
           (arg2 (infix->prefix (car (cdr (cdr infix))))))
       (list proc arg arg2)))

    (else "Somehting wrong")))

(define (parse-infix infix)
  (infix->prefix (add-bracket infix)))


;; Question 1 ii ;; Solution 3
(define (parse-infix infix)
  (define (plus-minus? proc)
    (or (equal? '+ proc) (equal? '- proc)))
  (define (multi-div? proc)
    (or (equal? '* proc) (equal? '/ proc)))

  (define (add-bracket-impliment infix)
    "Add bracket to first 3 elements"
    (cons (list (add-bracket (car infix))
                (car (cdr infix))
                (add-bracket (car (cdr (cdr infix)))))
          (cdr (cdr (cdr infix)))))

  (define (add-bracket infix)
    "parse infix and add bracket based on math logic"
    (cond ((number? infix) infix)
          ((and (pair? infix)
                (> (length infix) 3)) (cond
                                        ((multi-div? (car (cdr infix))) (add-bracket (add-bracket-impliment infix)))
                                        ((and (plus-minus? (car (cdr infix)))
                                              (multi-div? (car (cdr (cdr (cdr infix))))))
                                         (add-bracket (append (list (car infix) (car (cdr infix)))
                                                              (add-bracket-impliment (cdr (cdr infix))))))
                                        (else (add-bracket (add-bracket-impliment infix)))))
          (else infix)))
  (define (infix->prefix infix)
    "change format from infix to prefix"
    (cond
      ((number? infix) infix)
      (else (list (car (cdr infix))
                  (infix->prefix (car infix))
                  (infix->prefix (car (cdr (cdr infix))))))))

  (infix->prefix (add-bracket infix)))


;; (D (length '+))

;; (DD (md-add-bracket '(1 * 2)))
;; (DD (md-add-bracket '(1 * 2 * 3)))
;; (DD (md-add-bracket '(1 * 2 * 3 * 4)))

;; (DD (add-bracket '(1 + 2)))
;; (DD (add-bracket '(1 + 2 * 3 + 4)))
;; (DD (add-bracket '(1 + 2 * 3 * 5 + 4)))
;; (DD (add-bracket '(1 + (2 * 3) + 4)))
;; (DD (add-bracket '(1 + 2 * 3 * 4 + 5)))
;; (DD (add-bracket '(1 * 2 * 3 + 4)))
;; (DD (add-bracket '(1 + 2 * 3 + 4)))
;; (DD (add-bracket '(1 + (2 + 2) * 3)))

;; (DD (add-bracket '(1 + 2 * 3 + 4 * 5)))
;; (DD (add-bracket '(1 + 2 * 3 + 4 * 5 * 6)))


;; (DD (parse-infix '(100 + 20 * (10 - 5 * (6 + 1 * 7) + 100))))
;; (DD (parse-infix '(1 + 2 * 3)))

;; Test
;; (DD (parse-infix '(10 - 5)))
;; → (- 10 5)
;; (DD (parse-infix '(1 + 2 - 3)))
;; → (- (+ 1 2) 3)
;; (DD (parse-infix '(1 + 2 * 3 - 4)))
;; → (- (+ 1 (* 2 3)) 4)
;; (DD (parse-infix '(1 + (2 + 2) * 3)))
;;→ (+ 1 (* (+ 2 2) 3))


;; Test data

;; (DD (parse-infix '(10 * (5 * 5))))
;; (DD (parse-infix '(1 + (2 + 2) * 3)))

;; Test Example
;; (DD (parse-infix '(10 - 5)))
;; → (- 10 5)
;; (DD (parse-infix '(1 + 2 - 3)))
;; → (- (+ 1 2) 3)
;; (DD (parse-infix '(1 + 2 * 3 - 4)))
;; → (- (+ 1 (* 2 3)) 4)
;; (DD (parse-infix '(1 + (2 + 2) * 3)))
;;→ (+ 1 (* (+ 2 2) 3))

;; (D test1)
;; (D (symbol? 'test1))
;; (D (symbol? test1))
;; (D (list? test1))
;; (D (pair? test1))
;; (D (pair? +))
;;
;; (D "")
;;
;;
;; (D (symbol? '+))
;; (D (symbol? +))
;; (D (pair? test1))
;;
;; (D "")
;; (D (pair? '()))
;; (D (pair? '(1 )))
;; (D (pair? '(1 2)))
;; (D (pair? '(1 2 3)))

(begin (display "\nEnd!!!\n\n\n\n") 1)
