;Euler published the remarkable quadratic formula:

;n² + n + 41

;It turns out that the formula will produce 40 primes for the
;consecutive values n = 0 to 39. However, when n = 40, 402 + 40 + 41 =
;40(40 + 1) + 41 is divisible by 41, and certainly when n = 41, 41² +
;41 + 41 is clearly divisible by 41.

;Using computers, the incredible formula n² 79n + 1601 was discovered,
;which produces 80 primes for the consecutive values n = 0 to 79. The
;product of the coefficients, 79 and 1601, is 126479.

;Considering quadratics of the form:

;n² + an + b, where |a|  1000 and |b|  1000

;Find the product of the coefficients, a and b, for the quadratic
;expression that produces the maximum number of primes for consecutive
;values of n, starting with n = 0.

(ql:quickload "euler-utils")
(use-package :euler-utils)

(defmacro prime-quad (a b n)
  `(+ (* ,n ,n) (* ,a ,n) ,b))

(defun produced-primes (a b)
  "returns number of primes produced for consecutive vales of n in the
quadratic n^2 + an + b"
  (labels ((quad (n)
             (+ (* n n) (* a n) b))
           (count-primes (n)
             (if (is-prime (quad n))
                 (count-primes (1+ n))
                 n)))
    (count-primes 1)))
    
(defun find-quad ()
  (let ((best 1)
        (best-pair nil))
    (dotimes (i 2000)
      (dotimes (j 2000)
        (let* ((a (- i 1000))
               (b (- j 1000))
               (attempt (produced-primes a b)))
          (when (> attempt best)
            (setf best attempt
                  best-pair (cons a b))))))
    best-pair))