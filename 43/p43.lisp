(ql:quickload :euler-utils)
(use-package :euler-utils)

(defun non-duplicate-p (lst)
  (let ((d-lst nil))
    (not (dolist (d lst)
           (if (assoc d d-lst)
               (return t)
               (push (cons d t) d-lst))))))
    
(defun 2-3-4 ()
  (let ((lst (filter #'(lambda (x) (zerop (mod x 2))) (range 10 999))))
    (filter #'non-duplicate-p
            (mapcar #'(lambda (n)
                        (let ((n-list (to-digits n)))
                          (if (eql 2 (length n-list))
                              (cons 0 n-list)
                              n-list)))
                    lst))))

(defun more (space divisor)
  (labels ((produce-more (n-list)
             (let ((last-2 (drop n-list (- (length n-list) 2)))
                   (poten-digits (set-difference (range 0 9) n-list))
                   (acc nil))
               (dolist (d poten-digits)
                 (when (zerop (mod (from-digits (append last-2 (list d))) divisor))
                   (push (append n-list (list d)) acc)))
               acc)))
    (let ((acc nil))
      (dolist (n space)
        (setf acc (append acc (produce-more n))))
      acc)))

(defun solve ()
  (let ((numbers (more
                  (more
                   (more
                    (more
                     (more
                      (more (2-3-4) 3)
                      5)
                     7)
                    11)
                   13)
                  17)))
    (sum (mapcar #'(lambda (n-lst)
                     (let ((d (car (set-difference (range 0 9) n-lst))))
                       (from-digits (cons d n-lst))))
                 numbers))))
        
       