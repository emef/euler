(declaim (optimize (debug 3) (safety 3) (speed 0)))

(ql:quickload :euler-utils)
(use-package :euler-utils)

(defun set-equal (l1 l2)
  (and (subsetp l1 l2)
       (subsetp l2 l1)))

(defun naive-reduce-p (nu-lst de-lst)
  (let* ((nu (from-digits nu-lst))
         (de (from-digits de-lst))
         (val (/ nu de)))
    (when (and (not (and (eql 0 (mod nu 10))
                         (eql 0 (mod de 10))))
               (< val 1))
      (handler-case
          (let* ((naive-nu-lst (set-difference nu-lst de-lst))
                 (naive-de-lst (set-difference de-lst nu-lst))
                 (naive-val (/ (from-digits naive-nu-lst) (from-digits naive-de-lst))))
            (when (and (not (set-equal nu-lst naive-nu-lst))
                       (eql val naive-val))
              val))
        (division-by-zero nil)))))
  

(defun solve ()
  (let (acc)
    (loop for nu from 10 to 99 do
      (loop for de from 10 to 99 do
        (when (and (< (/ nu de) 1)
                   (naive-reduce-p (to-digits nu) (to-digits de)))
          (pushnew (/ nu de) acc))))
    (apply #'* acc)))
            
  