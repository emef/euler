(ql:quickload :euler-utils)
(use-package :euler-utils)

(let ((memo-table (make-hash-table)))
  (setf (gethash 0 memo-table) 1
        (gethash 1 memo-table) 1)
  (defun factorial (n)
    (let ((lu (gethash n memo-table)))
      (if lu 
          lu
          (setf (gethash n memo-table) (* n (factorial (1- n))))))))
    
(defun digit-factorial (n)
  (sum (mapcar #'factorial (to-digits n))))

(defun solve (&optional (n 10) (max 500))
  (let ((result (digit-factorial n)))
    (cond ((eq result n) (cons n (solve (1+ n) max)))
          ((> n max) nil)
          (t (solve (1+ n) max))))))