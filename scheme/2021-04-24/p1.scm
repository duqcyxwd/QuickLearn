(begi
  (display "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n\n"))

;; Test code
(define D (lambda args (if (pair? args) (map (lambda (a) (display (string a " "))) args)) (display "\n")))
(define DD (lambda args (apply D args) (display "\n")))

(define testdata "test data")
(DD "hi" '(1 2 23) testdata)

;; (define f (list-generator (lambda(x)#t) (lambda(x)x)))



(define (add-y y)
  (let (add-temp))
  )

(define add-5 (add-y 5))

(add-5 2)

;;          Testing

(define fact
  (lambda (n)
    (if (equal? n 1)
        1
        (* n (fact (- n 1))))))

(fact 5)

(define fact-maker
  (lambda (f)
    ; This is body of fact, except using the parameter f
    (lambda (n)
      (if (equal? n 1)
        1
        (* n (f (- n 1)))))))

> (fact-maker fact)
#<procedure>
> ((fact-maker fact) 5)

(define fact-maker
  (lambda (f)
    (lambda (n)
      (if (equal? n 1)
        1
        ; We've replaced f by (fact-maker f)
        (* n ((fact-maker f) (- n 1)))))))

(fact-maker "Hi")
; substitute "Hi" for f
(lambda (n)
  (if (equal? n 1)
  1
  (* n ((fact-maker "Hi") (- n 1)))))

> ((fact-maker "Hi") 5)

> (fact-maker fact)
#<procedure>
> ((fact-maker fact) 5)


;; ((l 1 1) 1 2 3)

(define (list-p start end step)
  (cond
        ((= step 0) ())
        ((= start end) (list start ))
        ((and (> step 0)
               (> start end)) '())

        ((and (< step 0)
               (< start end)) '())
        (else
          (cons start (list-p (+ start step) end step)))))

(list-p 0 10 1)
(list-p 0 10 2)
(list-p 0 10 3)

(list-p 10 -10 -3)

(cons 1 2)
(cons 1 '(2 3))
(append '(1 2) '(2 3))


(list 0 1 2 3)



;; ---------------------------------------

;; Problem 1
(define (list-generator predicate mutator)
  (lambda (start end step)
    (define (list-p start)
      (cond
        ((= step 0) '())
        ((and (> step 0)
              (> start end) '()))
        ((and (< step 0)
              (< start end) '()))
        ((and (= start end)
              (predicate start)) (list (mutator start)))
        ((predicate start) (cons (mutator start) (list-p (+ start step))))
        (else (list-p (+ start step)))))
    (list-p start)))

((list-generator odd? (lambda(x)(* x 2))) 1 20 3)
((list-generator (lambda(x)#t)(lambda(x)x)) 16 0 -4)
((list-generator (lambda(x)#t)(lambda(x)x)) 0 0 -4)
((list-generator (lambda(x)#t)(lambda(x)x)) 0 100 -4)
((list-generator (lambda(x)#t)(lambda(x)x)) 100 0 4)
((list-generator (lambda(x)#t)(lambda(x)x)) 100 0 0)

;; Problem 2
