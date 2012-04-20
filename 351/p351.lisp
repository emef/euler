(ql:quickload :euler-utils)
(ql:quickload :zpng)
(use-package :euler-utils)
(use-package :zpng)

(defun hex-orch (size)
  
  

(defun h (n &optional (acc 0))
  (if (= n 2)
      (+ 6 acc)
    (let* ((ds (divisors n))
           (count (- (length ds) 2))
           (extra (- (* 2 count) (if (evenp n) 1 0))))
     (h (1- n) (+ acc 6 (* 6 extra))))))