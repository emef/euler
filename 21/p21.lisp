(defparameter *max-amicable* 10000)
(defparameter *max-n* 25320)

(let ((divisor-table (make-hash-table)))
  (loop for i from 1 to *max-n* do
    (loop for j from 1 to (ceiling (sqrt *max-n*)) do
      (when (eql 0 (mod i j))
          (pushnew j (gethash i divisor-table))
          (pushnew (/ i j) (gethash i divisor-table)))))
  (defun divisors (n)
    (gethash n divisor-table)))

;; note n is not a *proper* divisor of n 
(defun d-sum (n)
  (- (reduce #'+ (divisors n)) n))

(let ((d-sum-table (make-hash-table)))
  (dotimes (i (1+ *max-n*))
    (setf (gethash i d-sum-table) (d-sum i)))
  (defun amicable-p (a)
    (let ((b (gethash a d-sum-table)))
      (and (not (eql a b))
           (eql a (d-sum b))))))

(defun filter (fn lst)
  (let (acc)
    (dolist (el lst)
      (when (funcall fn el)
        (push el acc)))
    acc))

(defun range (start stop)
  (loop for i from start to stop
       collect i))

(defun amic-sum ()
  (reduce #'+ (filter 'amicable-p (range 1 *max-amicable*))))

(time
 (format t "Sum is ~A~%" (amic-sum)))