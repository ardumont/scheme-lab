;; derivation computation (wait, where does dx come from?)
;; (define deriv
;;   (lambda (f) (lambda (x) (/ (- (f (+ x dx))
;;                                 (f x))
;;                              dx))))

(define (deriv exp var)
  (cond ((constant? exp var) 0)
        ((same-var? exp var) 1)
        ((sum? exp var) (make-sum (deriv (a1 exp) var)
                                  (deriv (a2 exp) var)))
        ((pdt? exp var) (make-sum (make-pdt (m1 exp)
                                            (deriv (m2 exp) var))
                                  (make-pdt (m2 exp)
                                            (deriv (m1 exp) var))))
        ;; other derivative definition ...
        ))
