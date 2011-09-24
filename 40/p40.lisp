(ql:quickload :euler-utils)
(use-package :euler-utils)

(defun offset-for-digits (d)
  (cond ((eql d 0) 0)
        ((eql d 1) 9)
        (t (+ (offset-for-digits (1- d)) 
              (* d (- (expt 10 d) (expt 10 (1- d))))))))

(defun digit-bracket (n &optional (check 1))
  (if (<= n (offset-for-digits check))
      check
      (digit-bracket n (1+ check))))

(defun dn (n)
  (let* ((bracket (digit-bracket n))
         (diff (1- (- n (offset-for-digits (1- bracket)))))
         (preceding (floor (/ diff bracket)))
         (digit-position (rem diff bracket))
         (target (+ preceding (expt 10 (1- bracket)))))
    (nth digit-position (to-digits target))))

(defun solve ()
  (apply #'* (mapcar #'(lambda (x) (dn (expt 10 x))) (range 0 6))))