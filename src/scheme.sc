;; derivation (wait, where does dx come from?)
(define deriv
  (lambda (f) (lambda (x) (/ (- (f (+ x dx))
                                (f x))
                             dx))))
