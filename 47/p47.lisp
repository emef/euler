(ql:quickload :euler-utils)
(use-package :euler-utils)

(let ((tbl (make-hash-table :test #'eq)))
  (defun unique-factors (n)
    (multiple-value-bind (val succ) (gethash n tbl)
      (if (and val succ)
          val
          (setf (gethash n tbl)
                (length (remove-duplicates (prime-factors n))))))))

(defun 4-consecutive (n)
  (and (= 4 (unique-factors n))
       (= 4 (unique-factors (+ n 1)))
       (= 4 (unique-factors (+ n 2)))
       (= 4 (unique-factors (+ n 3)))))
       
(defun solve (&optional (atmpt 1))
  (if (4-consecutive atmpt)
      atmpt
      (solve (1+ atmpt))))
  

