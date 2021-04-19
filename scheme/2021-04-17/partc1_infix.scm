
;; Test code
(define D (lambda args (if (pair? args) (map (lambda (a) (display (string a " "))) args)) (display "\n")))
(define DD (lambda args (apply D args) (display "\n")))


;;  Question 1 i
(define (execute-procedure procedure tree tree2)
  (let ((value1 (eval-prefix tree))
        (value2 (eval-prefix tree2)))
    (cond ((equal? procedure '+) (+ value1 value2))
          ((equal? procedure '-) (- value1 value2))
          ((equal? procedure '*) (* value1 value2))
          ((equal? procedure '/) (/ value1 value2))
          (else (display "Unknown procedure" procedure)))))

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

;; Test Example
;; (DD (parse-infix '(10 - 5)))
;; → (- 10 5)
;; (DD (parse-infix '(1 + 2 - 3)))
;; → (- (+ 1 2) 3)
;; (DD (parse-infix '(1 + 2 * 3 - 4)))
;; → (- (+ 1 (* 2 3)) 4)
;; (DD (parse-infix '(1 + (2 + 2) * 3)))
;;→ (+ 1 (* (+ 2 2) 3))
