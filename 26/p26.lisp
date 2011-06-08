;A unit fraction contains 1 in the numerator. The decimal
;representation of the unit fractions with denominators 2 to 10 are
;given:

;1/2	= 	0.5
;1/3	= 	0.(3)
;1/4	= 	0.25
;1/5	= 	0.2
;1/6	= 	0.1(6)
;1/7	= 	0.(142857)
;1/8	= 	0.125
;1/9	= 	0.(1)
;1/10	= 	0.1

;Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It
;can be seen that 1/7 has a 6-digit recurring cycle.

;Find the value of d 1000 for which 1/d contains the longest recurring
;cycle in its decimal fraction part.

;; ** huge hint: periodicity of decimal recurrence is equal to
;; ** periodicity of remainders!!!
;; 7 \ 10
;;      30
;;       20
;;        60
;;         40
;;          50
;;           10
;; these are remainders * 10 that you would get while doing long division.
;; the periodicity of these is equal to periodicity of decimal recurrence

(declaim (OPTIMIZE (DEBUG 3)))

(ql:quickload "euler-utils")
(use-package :euler-utils)

(defun periodicity (n)
  (do* ((remainder 10 (* 10 (rem remainder n)))
        (rem-lst '(10) (push remainder rem-lst)))
       ((> (count remainder rem-lst) 1)
        (let* ((first-pos (position remainder rem-lst))
               (last-pos (position remainder rem-lst :start (1+ first-pos))))
          (- last-pos first-pos)))))

(defun max-periodicity (max-n)
  (maximum (mapcar #'(lambda (n) (cons (periodicity n) n))
                   (range 1 max-n))
           :> #'(lambda (a b)
                  (> (car a) (car b)))))
           
;; solution
(max-periodicity 1000)
           
    