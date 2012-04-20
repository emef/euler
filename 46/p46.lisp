(ql:quickload :euler-utils)
(use-package :euler-utils)

(defun next-odd-composite (n)
  (let ((next (1+ n)))
    (cond ((evenp next) (next-odd-composite next))
          ((is-prime next) (next-odd-composite (+ 2 n)))
          (t next))))

(defun prime-candidates (n)
  (labels ((fn (atmpt &optional acc)
             (cond ((and (is-prime atmpt)
                         (evenp (- n atmpt)))
                    (fn (1- atmpt) (cons atmpt acc)))
                   ((> atmpt 1) (fn (1- atmpt) acc))
                   (t acc))))
    (fn (- n 2))))

(defun is-square (n)
  (let ((sqrt (isqrt n)))
    (= (* sqrt sqrt) n)))

(defun has-46-property (n)
  (let ((cs (prime-candidates n)))
    (dolist (c cs)
      (let ((sq (/ (- n c) 2)))
        (when (is-square sq)
          (return t))))))

(defun solve (&optional (atmpt 9))
  (if (has-46-property atmpt)
      (solve (next-odd-composite atmpt))
      atmpt))
  
                 
                   
  