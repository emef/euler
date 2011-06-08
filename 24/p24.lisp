;A permutation is an ordered arrangement of objects. For example, 3124
;is one possible permutation of the digits 1, 2, 3 and 4. If all of
;the permutations are listed numerically or alphabetically, we call it
;lexicographic order. The lexicographic permutations of 0, 1 and 2
;are:

;012   021   102   120   201   210

;What is the millionth lexicographic permutation of the digits 0, 1,
;2, 3, 4, 5, 6, 7, 8 and 9?

(defparameter *target* 1000000)
(defparameter *digits* '(0 1 2 3 4 5 6 7 8 9))

(let ((f-table (make-hash-table)))
  (setf (gethash 1 f-table ) 1)
  (defun factorial (n)
    (or (gethash n f-table)
        (setf (gethash n f-table) (* n (factorial (1- n)))))))

(defun first-match (pred lst)
  (and (consp lst)
       (let ((x (car lst)))
         (if (funcall pred x)
             x
             (first-match pred (cdr lst))))))

(defun find-perm (target digits)
  (let ((n-digits (length digits)))
    (cond ((and (eql target 1) (eql 1 n-digits)) digits)
          ((or (<= n-digits 1) (<= target 0) nil))
          (t (let* ((combos (factorial (1- n-digits)))
                    (possible (loop for i from (1- n-digits) downto 0
                                 when (< (* i combos) target)
                                 collect i )))
               (loop for p in possible do
                    (let* ((digit (elt digits p))
                           (result (find-perm (- target (* combos p))
                                            (remove digit digits))))
                      (when result
                        (return (cons digit result))))))))))

;; solution
(find-perm *target* *digits*)
               
                 
             
           
    
             