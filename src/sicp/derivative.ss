;; derivation computation (wait, where does dx come from?)
;; (define deriv
;;   (lambda (f) (lambda (x) (/ (- (f (+ x dx))
;;                                 (f x))
;;                              dx))))

;; basic test framework

(define expect eq?)

(define (atom? x) "Define atom? which should already be defined by scheme."
  (and (not (pair? x))
       (not (null? x))))

(define (constant? exp var)
  (and (atom? exp)
       (not (eq? exp var))))

(define (same-var? exp var)
    (and (atom? exp)
         (eq? exp var)))

(define (sum? exp)
  (and (not (atom? exp))
       (eq? (car exp) '+)))

(define (pdt? exp)
  (and (not (atom? exp))
       (eq? (car exp) '*)))

(define (make-sum a1 a2)
  (cond ((and (number? a1) (number? a2)) (+ a1 a2))
        ((and (number? a1) (eq? a1 0))   a2)
        ((and (number? a2) (eq? a2 0))   a1)
        (else                            (list '+ a1 a2))))

(define (make-pdt a1 a2)
  (cond ((and (number? a1) (number? a2)) (* a1 a2))
        ((and (number? a1) (eq? a1 0))   0)
        ((and (number? a1) (eq? a1 1))   a2)
        ((and (number? a2) (eq? a2 0))   0)
        ((and (number? a2) (eq? a2 1))   a1)
        (else                            (list '* a1 a2))))

(define a1 car)
(define a2 cadr)

(define m1 cadr)
(define m2 caddr)

(define (deriv exp var)
  (cond ((constant? exp var) 0)
        ((same-var? exp var) 1)
        ((sum? exp)          (make-sum (deriv (a1 exp) var)
                                       (deriv (a2 exp) var)))
        ((pdt? exp)          (make-sum (make-pdt (m1 exp)
                                                 (deriv (m2 exp) var))
                                       (make-pdt (deriv (m1 exp) var)
                                                 (m2 exp))))
        ;; other definition here...
        ))

;; run samples

(define foo-fn '(+ (* a (* x x))
                   (+ (* b x)
                      c)))

(deriv foo-fn 'x)
;; (* a (+ x x))

(deriv foo-fn 'b)
;; o

(deriv foo-fn 'a)
;; (* x x)

(deriv foo-fn 'c)
;; 0