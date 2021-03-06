;A perfect number is a number for which the sum of its proper divisors
;is exactly equal to the number. For example, the sum of the proper
;divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28
;is a perfect number.

;A number n is called deficient if the sum of its proper divisors is
;less than n and it is called abundant if this sum exceeds n.

;As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the
;smallest number that can be written as the sum of two abundant
;numbers is 24. By mathematical analysis, it can be shown that all
;integers greater than 28123 can be written as the sum of two abundant
;numbers. However, this upper limit cannot be reduced any further by
;analysis even though it is known that the greatest number that cannot
;be expressed as the sum of two abundant numbers is less than this
;limit.

;Find the sum of all the positive integers which cannot be written as
;the sum of two abundant numbers.

(defparameter *max-n* 28123)

(let ((divisor-table (make-hash-table)))
  (loop for i from 1 to *max-n* do
    (loop for j from 1 to (ceiling (sqrt *max-n*)) do
      (when (eql 0 (mod i j))
          (pushnew j (gethash i divisor-table))
          (pushnew (/ i j) (gethash i divisor-table)))))
  (defun divisors (n)
    (gethash n divisor-table)))

(defun abundant-p (n)
  (< n (- (reduce #'+ (divisors n)) n)))

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

(let ((abundant-sums (make-hash-table))
      (abundant-lst (filter #'abundant-p (range 1 *max-n*))))
  (dolist (x abundant-lst)
    (dolist (y abundant-lst)
      (setf (gethash (+ x y) abundant-sums) t)))
  (defun non-abundant-sums ()
    (filter (complement (rcurry #'gethash abundant-sums))
            (range 1 (+ 1000 *max-n*)))))

(format t "sum of numbers that cannot be written as a sum of two abundant numbers is ~a ~%"
        (reduce #'+ (non-abundant-sums)))