(begin
  (display "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
  "")

(define (p data)
  (display "\n")
  (map display
       data))
(define (pl data)
  (display "\n")
  (display data))


(define (make-graph)
  (let ((graph (list)))
    (define (contains list x)
      (cond ((null? list) #f)
            ((equal? (car (car list)) x) #t)
            (else (contains (cdr list) x))))
    (define (rm-from-list x ls)
      (if (null? ls)
        '()
        (if (eqv? x (car ls))
          (rm-from-list x (cdr ls))
          (cons (car ls)
                (rm-from-list x (cdr ls))))))
    (define (rm-node x ls)
      (if (null? ls)
        '()
        (if (eqv? x (car (car ls)))
          (rm-node x (cdr ls))
          (cons (car ls)
                (rm-node x (cdr ls))))))
    (define (remove-edge-from-node-pair node-pair edge)
      (list (car node-pair)
            (rm-from-list edge (car (cdr node-pair)))))

    (define (add-node node)
      (let ((cont (contains graph node)))
        (if (not cont)
          (set! graph (append graph (list (list node '())) )))
        (not cont)))
    (define (remove-node node)
      (let ((cont (contains graph node)))
        (if cont (set! graph
                       (map (lambda (e)
                                    (remove-edge-from-node-pair e node))
                            (rm-node node graph))))
        cont))

    (define (add-edge node1 edge)
      (let ((contain-node? (contains graph node1))
            (contain-edge? (contains graph edge)))
        (if (and contain-node? contain-edge?)
          (set! graph
                (map (lambda (e)
                             (if (equal? (car e) node1)
                               (list (car e) (append (car (cdr e)) (list edge)))
                               e))
                     graph)))
        (and contain-node? contain-edge?)))
    (define (remove-edge node1 edge)
      (let ((contain-node? (contains graph node1))
            (contain-edge? (contains graph edge)))
        (if (and contain-node? contain-edge?)
          (set! graph
                (map (lambda (e)
                             (if (equal? (car e) node1)
                               (remove-edge-from-node-pair e edge)
                               e))
                     graph)))
        (and contain-node? contain-edge?)))
    (define (graph-display)
      (let ((space-join (lambda (str-list)
                                (apply string-append
                                       (map (lambda (x) (string x " "))
                                            str-list)))))
        (apply string-append
               (map (lambda (e)
                            (string-append (string (car e))
                                           ": "
                                           (space-join (car (cdr e))) "\n"))
                    graph))))
    (define (dispath msg)
      (cond ((eq? msg 'add-node) add-node)
            ((eq? msg 'add-edge) add-edge)
            ((eq? msg 'remove-node) remove-node)
            ((eq? msg 'remove-edge) remove-edge)
            ((eq? msg 'display) graph-display)))
    dispath))

(define G (make-graph))
(begin
  ((G 'add-node) 'a)       ;=> #t
  ((G 'add-node) 'b)       ;=> #t
  ((G 'add-node) 'c)       ;=> #t
  ((G 'add-node) 'a)       ;=> #f
  ((G 'add-node) 'd)       ;=> #f
  ((G 'remove-node) 'd)    ;=> #t
  )

(p '("\nA------------"))
((G 'add-edge) 'a 'b)    ;=> #t
((G 'add-edge) 'a 'c)    ;=> #t
;; ((G 'add-edge) 'b 'b)    ;=> #t
;; ((G 'add-edge) 'b 'c)    ;=> #t
;; ((G 'add-edge) 'c 'd)    ;=> #f
((G 'add-edge) 'b 'b)    ;=> #t
((G 'add-edge) 'b 'c)    ;=> #t
((G 'add-edge) 'c 'd)    ;=> #f
;; (p (list ((G 'display))))
(pl ((G 'display)))


(p '("\nRemove------------"))
;; (pl ((G 'remove-edge) 'a 'c)) ;=> #t
;; (pl ((G 'display)))

;; ((G 'remove-node) 'c)    ;=> #t
;; (pl ((G 'remove-node) 'd))    ;=> #t
;; (p ((G 'display)))

;; (cons (list 1 2) (list 3 4 5))
;; (append (list 1 2) (list 3 4 5))
;; (contains '(1 2 3) 2)
;; (rm 2 '((1) (2) (3) (2)))

;; add-edge
;; display

;; remove edge
;; remove node
;; display


;; (begin (display "\nEnd!!!\n\n\n\n") 1)

;; (let ((space-join (lambda (str-list) (apply string-append (map (lambda (x) (string x " ")) str-list))))) (map (lambda (e) (string-append (string (car e)) ": " (space-join (car (cdr e))) "\n"))) (list (list 'a (list 'a 'b)) (list 'b (list 'a 'b))))
