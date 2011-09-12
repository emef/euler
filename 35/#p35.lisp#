(ql:quickload :euler-utils)
(use-package :euler-utils)

(defun rotations (n)
  (let ((last (to-digits n))
        acc)
    (dolist (d (reverse (to-digits n)))
      (let ((next (cons d (butlast last))))
        (push next acc)
        (setf last next)))
    (mapcar #'from-digits acc)))

(defun all (lst)
  (if lst
      (and (car lst) (all (cdr lst)))
      t))

(defun circular-prime-p (p)
  (all (mapcar #'is-prime (rotations p))))

(defun solve ()
  (length (filter #'circular-prime-p (primes))))
    