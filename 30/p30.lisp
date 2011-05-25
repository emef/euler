;Surprisingly there are only three numbers that can be written as the
;sum of fourth powers of their digits:

;1634 = 1^4 + 6^4 + 3^4 + 4^4
;8208 = 8^4 + 2^4 + 0^4 + 8^4
;9474 = 9^4 + 4^4 + 7^4 + 4^4
;As 1 = 1^4 is not a sum it is not included.

;The sum of these numbers is 1634 + 8208 + 9474 = 19316.

;Find the sum of all the numbers that can be written as the sum of fifth powers of their digits.

(defun range (i n &optional acc)
  (if (>= n i)
      (range i (1- n) (cons n acc))
      acc))

(defun filter (fn lst)
  (let (acc)
    (dolist (el lst)
      (when (funcall fn el)
        (push el acc)))
    acc))

(defun rcurry (fn &rest more-args)
  #'(lambda (&rest args)
      (apply fn (append args more-args))))

(defun digits (n &optional acc)
  (let ((digit (mod n 10)))
    (if (< n 10)
        (cons digit acc)
        (digits (floor (/ n 10)) (cons digit acc)))))

(defun sum-of-digit-powers-p (n pow)
  (eql n (reduce #'+ (mapcar #'(lambda (n) (expt n pow)) 
                             (digits n)))))

(format t "sum of numbers who are equal to sum of their digits raised to the power 4 is: ~A~%"
         (filter (rcurry #'sum-of-digit-powers-p 4)
                            (range 1 1000000)))