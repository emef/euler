;If p is the perimeter of a right angle triangle with integral length
;sides, {a,b,c}, there are exactly three solutions for p = 120.

;{20,48,52}, {24,45,51}, {30,40,50}

;For which value of p <= 1000, is the number of solutions maximised?

(defun tri-table (n)
  (let ((table (make-hash-table)))
    (loop for a from 1 to n do
         (loop for b from 1 to a do
              (loop for c from  1 to b do
                   (when (and (> 1000 (+ a b c))
                              (eql (expt a 2)
                                   (+ (expt b 2) (expt c 2))))
                     (push (list a b c) (gethash (+ a b c) table))))))
    table))


(defun range (i n &optional acc)
  (if (>= n i)
      (range i (1- n) (cons n acc))
      acc))

(defun most-triangles (n)
  (let ((tris (tri-table n))
        (best-p 0)
        (best-count 0))
    (dolist (x (range 1 n))
      (let ((count (length (gethash x tris))))
        (when (> count best-count)
          (setf best-count count)
          (setf best-p x))))
    best-p))