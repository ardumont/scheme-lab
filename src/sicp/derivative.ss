;; ######### basic test framework

(define true? (lambda (x) (not (eq? #f x))))

(eq? #t (true? #t))
(eq? #f (true? #f))
(eq? #t (true? 1))
(eq? #t (true? '()))
(eq? #t (true? '()))

(define (expectations . expects) "Ensure every expects are true."
  (every true? expects))

(expectations
 (expect #t #t)
 (expect #f #f))

(define (expect exp1 exp2) "Define a basic test framework."
  (cond ((and (atom? exp1) (atom? exp2)) (eq? exp1 exp2))
        ((atom? exp1)                    #f)
        ((atom? exp2)                    #f)
        (else                            (and (eq? (length exp1) (length exp2))
                                              (every true? (map expect exp1 exp2))))))

(expectations
 (expect #t (expect 'a 'a))
 (expect #t (expect '() '()))
 (expect #t (expect '(a) '(a)))
 (expect #t (expect '(a (x y)) '(a (x y))))
 (expect #f (expect '(a) '(a b)))
 (expect #f (expect '(a (x x)) '(a (x y))))
 (expect #f (expect 'a 'b))
 (expect #f (expect 'a '())))

;; ######### basic brick

(define (atom? x) "Define atom? which should already be defined by scheme."
  (and (not (pair? x))
       (not (null? x))))

(expectations
 (expect #t (atom? "a"))
 (expect #f (atom? '(a))))

;; ######### basic derivative

;; derivation computation (wait, where does dx come from?)
;; (define deriv
;;   (lambda (f) (lambda (x) (/ (- (f (+ x dx))
;;                                 (f x))
;;                              dx))))

(define (constant? exp var) "is the exp a constant in respect to var?"
  (and (atom? exp)
       (not (eq? exp var))))

(expectations
 (expect #t (constant? 'a 'x))
 (expect #f (constant? 'a 'a))
 (expect #f (constant? '(a) 'x))
 (expect #f (constant? '(a) 'a)))

(define (same-var? exp var) "is the exp a variable in respect to var?"
    (and (atom? exp)
         (eq? exp var)))

(expectations
 (expect #t (same-var? 'x 'x))
 (expect #f (same-var? 'x 'y))
 (expect #f (same-var? '(x) 'x)))

(define (sum? exp) "is the expression a sum?"
  (and (not (atom? exp))
       (eq? (car exp) '+)))

(expectations
 (expect #t (sum? '(+ a b)))
 (expect #f (sum? '(* a b)))
 (expect #f (sum? 'a))
 (expect #f (sum? '(a))))

(define (pdt? exp) "is the expression a product?"
  (and (not (atom? exp))
       (eq? (car exp) '*)))

(expectations
 (expect #t (pdt? '(* a b)))
 (expect #f (pdt? '(+ a b)))
 (expect #f (pdt? 'a))
 (expect #f (pdt? '(a))))

(define (make-sum a1 a2) "Make a sum expression."
  (cond ((and (number? a1) (number? a2)) (+ a1 a2))
        ((and (number? a1) (eq? a1 0))   a2)
        ((and (number? a2) (eq? a2 0))   a1)
        (else                            (list '+ a1 a2))))

(expectations
 (expect 3        (make-sum 1 2))
 (expect 'a       (make-sum 'a 0))
 (expect 'b       (make-sum 0 'b))
 (expect '(+ a b) (make-sum 'a 'b)))

(define (make-pdt a1 a2) "Make a product expression."
  (cond ((and (number? a1) (number? a2)) (* a1 a2))
        ((and (number? a1) (eq? a1 0))   0)
        ((and (number? a1) (eq? a1 1))   a2)
        ((and (number? a2) (eq? a2 0))   0)
        ((and (number? a2) (eq? a2 1))   a1)
        (else                            (list '* a1 a2))))

(expectations
 (expect 2        (make-pdt 1 2))
 (expect 'a       (make-pdt 'a 1))
 (expect 0        (make-pdt 'a 0))
 (expect 'b       (make-pdt 1 'b))
 (expect 0        (make-pdt 0 'b))
 (expect '(* a b) (make-pdt 'a 'b)))

(define a1 car)
(define a2 cadr)

(define m1 cadr)
(define m2 caddr)

(define (deriv exp var) "Compute the derivation of the expression exp in respect to the variable var."
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

(define foo-fn-exp '(+ (* a (* x x))
                       (+ (* b x)
                          c)))

(expectations
 (expect '(* a (+ x x)) (deriv foo-fn-exp 'x))
 (expect 0              (deriv foo-fn-exp 'b))
 (expect '(* x x)       (deriv foo-fn-exp 'a))
 (expect 0              (deriv foo-fn-exp 'c)))
