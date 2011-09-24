(ql:quickload :euler-utils)
(use-package :euler-utils)

(defun truncate-l (lst)
  (when (cdr lst)
    (cons (cdr lst) (truncate-l (cdr lst)))))

(defun truncate-r (lst)
  (when (cdr lst)
    (cons (butlast lst) (truncate-r (butlast lst)))))

(defun truncated (n)
  (let ((lst (to-digits n)))
    (mapcar #'from-digits 
            (append (list lst) (truncate-l lst) (truncate-r lst)))))

(defun truncated-prime-p (n)
  (all (mapcar #'is-prime (truncated n))))

(defun solve ()
  (sum (filter #'(lambda (p)
                   (and (> p 9)
                        (truncated-prime-p p)))
               (primes))))
          