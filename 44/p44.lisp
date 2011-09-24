(ql:quickload :euler-utils)
(use-package :euler-utils)

(defparameter *num-pentagonals* 10000)

(let ((tbl (make-hash-table :test #'eq))
      (ary (make-array *num-pentagonals*)))
  (loop for n from 1 to *num-pentagonals* do
       (let ((val (/ (* n (- (* 3 n)  1)) 2)))
         (setf (gethash val tbl) t
               (svref ary (1- n)) val)))
       
  (defun pentagonal-p (n)
    (and (> n 0)
         (gethash n tbl)))

  (defun nth-pentagonal (n)
    (svref ary (1- n))))

(defun candidates (n)
  (let* ((p (nth-pentagonal n))
         (diff (- (nth-pentagonal (1+ n)) p)))
    (do* ((atmpt-n (1- n) (1- atmpt-n))
          (atmpt (nth-pentagonal atmpt-n) (nth-pentagonal atmpt-n))
          (cs (list (list atmpt p)) (cons (list atmpt p) cs)))
         ((or (< atmpt-n 1) (< atmpt diff)) cs))))

(defun keep (pair)
  (destructuring-bind (a b) pair
    (and (pentagonal-p (+ a b))
         (pentagonal-p (- b a)))))

(defun solve ()
  (filter #'consp 
          (loop for n from (1- *num-pentagonals*) downto 2
             collect (filter #'keep (candidates n)))))
       
           
    
    


  
