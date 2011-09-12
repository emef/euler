(declaim (optimize (speed 0) (space 0) (debug 3)))

(defparameter *max-a* 100)
(defparameter *max-b* 100)

(defun solve ()
  (let ((vals (make-hash-table))
        (c 0)
        acc)
    (loop for a from 2 to *max-a* do 
      (loop for b from 2 to *max-b* do
        (let* ((a^b (expt a b))
               (lu (gethash a^b vals)))
          (pushnew a^b acc)
          (cond ((null lu)
                 (setf (gethash a^b vals) 0)
                 (incf c))
                ((eql 1 lu)
                 (decf c)))
          (incf (gethash a^b vals)))))
    acc))
          
