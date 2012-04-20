(ql:quickload :euler-utils)
(use-package :euler-utils)

(defun perm-p (lst)
  (destructuring-bind (a b c) lst
    (let ((ds (to-digits a)))
      (and (set-eql ds (to-digits b))
           (set-eql ds (to-digits c))))))

(defun candidates (nth)
  (let ((p (nth-prime nth))
        (acc nil))
    (dolist (next-n (range (1+ nth) 1227))
      (let* ((next-p (nth-prime next-n))
             (diff (- next-p p)))
        (when (is-prime (+ diff next-p))
          (push (list p next-p (+ next-p diff)) acc))))
    (filter #'perm-p acc)))

(defun solve ()
  (let ((acc nil))
    (dolist (i (range 168 1228))
      (setf acc (append acc (candidates i))))
    acc))
            
        