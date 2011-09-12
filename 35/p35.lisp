(ql:quickload :euler-utils)
(use-package :euler-utils)

(defun rotations (n)
  (let ((last n)
        acc)
    (dolist (d (reverse (to-digits n)))
      (let ((next (cons d (butlast last))))
        (push next acc)
        (setf last acc)))
    (mapcar #'from-digits acc)))
      