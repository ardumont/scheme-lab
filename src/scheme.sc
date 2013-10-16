;; derivation computation (wait, where does dx come from?)
;; (define deriv
;;   (lambda (f) (lambda (x) (/ (- (f (+ x dx))
;;                                 (f x))
;;                              dx))))

;; should already be defined by scheme
(define (atom? x)
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

(define (make-sum exp1 exp2)
  (list '+ exp1 exp2))

(define (make-pdt exp1 exp2)
  (list '* exp1 exp2))

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
                                       (make-pdt (m2 exp)
                                                 (deriv (m1 exp) var))))
        ;; other definition here...
        ))

(define foo-fn '(+ (* a (* x x))
                   (+ (* b x))
                   c))

(deriv foo-fn 'x)
;; (+ 0
;;    (+ (* a (+ (* x 1)
;;               (* x 1)))
;;       (* (* x x) 0)))
