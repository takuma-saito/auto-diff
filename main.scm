(define (d var expr)
  (cond
   ((and (list? expr) (eq? (length expr) 3))
    (let ((v (car expr)) (f (cadr expr)) (g (caddr expr)))
      (case v
          ('+ `(+ ,(d var f) ,(d var g)))
          ('- `(- ,(d var f) ,(d var g)))
          ('* `(+ (* ,(d var f) ,g) (* ,f ,(d var g))))
          ('/ `(/ (- (* ,g ,(d var f)) (* ,(d var g) ,f)) (^ ,g 2)))
          ('^ `(* (* ,g (^ ,f (- ,g 1))) ,(d var f)))
          (else 'none))))
   ((and (list? expr) (eq? (length expr) 2))
    (let ((v (car expr)) (f (cadr expr)))
      (case v
        ('sin `(* ,(d var f) (cos ,f)))
        ('cos `(* ,(d var f) (- (sin ,f) 0)))
        ('exp `(* ,(d var f) (exp ,f)))
        ('log `(/ ,(d var f) ,f))
        ('sqrt `(/ ,(d var f) (* 2 (sqrt ,f)))))))
   ((eq? expr var) 1)
   (else 0)))

(define (reduce expr)
  (cond
   ((not (list? expr)) expr)
   ((and (list? expr) (eq? (length expr) 3))
    (let ((v (car expr)) (f (reduce (cadr expr))) (g (reduce (caddr expr))))
      (cond
       ((and (number? f) (number? g))
        (case v
          ('+ (+ f g))
          ('- (- f g))
          ('* (* f g))
          ('/ (/ f g))))
       ((and (eq? f 1) (eq? v '*)) g)
       ((and (eq? g 1) (eq? v '*)) f)
       ((and (eq? f 0) (eq? v '*)) 0)
       ((and (eq? g 0) (eq? v '*)) 0)
       ((and (eq? f 0) (eq? v '+)) g)
       ((and (eq? g 0) (eq? v '+)) f)
       ((and (eq? f 0) (eq? v '-)) g)
       ((and (eq? g 0) (eq? v '-)) f)
       ((and (eq? g 1) (eq? v '^)) f)
       (else `(,v ,f ,g)))))
   ((and (list? expr) (eq? (length expr) 2))
    `(,(car expr) ,(reduce (cadr expr))))
   (else 'none)))
    

(define (dr expr)
  (reduce (d 'x expr)))

(define (dr2 expr)
  (dr (dr expr)))

(d 'x '(/ (+ (+ (* (^ x 2) 2) (* 2 x)) 1) (sqrt x)))
(dr 'x '(* (+ x 1) (+ (^ x 2) 3)))
(dr 'x '(* (- 2 x) (+ (* 2 (^ x 2)) (- 4 (* 3 x)))))


