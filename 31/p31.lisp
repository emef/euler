(defparameter *coins* '(1 2 5 10 20 50 100 200))

(defun split (n ways &optional (acc '(0)))
  (if (>= n ways)
      (split (- n ways) ways (cons (1+ (car acc)) acc))
      acc))

(defun p-ways (n ix)
  (if (eql ix 0)
      1
      (let ((c 0)
            (p (elt *coins* ix)))
        (dolist (x (split n p))
          (incf c (p-ways (- n (* x p)) (- ix 1))))
        c)))

(defun solve (n)
  (p-ways n 7))