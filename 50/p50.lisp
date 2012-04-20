(ql:quickload :euler-utils)
(use-package :euler-utils)

(defparameter *1mil* 1000000)

(defstruct pseq
  lst
  sum
  max-n)

(defun new-pseq (len)
  (let ((lst (reverse (mapcar #'nth-prime (range 0 (1- len))))))
    (make-pseq
     :max-n (1- len)
     :lst lst
     :sum (sum lst))))
    
(defun next-pseq (len pseq)
  (let* ((next-n (1+ (pseq-max-n pseq)))
	 (next-lst (cons (nth-prime next-n) (take (pseq-lst pseq) (1- len)))))
    (make-pseq 
     :max-n next-n
     :lst next-lst
     :sum (sum next-lst))))

(defun find-seq (len &optional pseq)
  (cond ((null pseq) (find-seq len (new-pseq len)))
	((> (pseq-sum pseq) *1mil*) nil)
	((is-prime (pseq-sum pseq)) pseq)
	(t (find-seq len (next-pseq len pseq)))))

(defun solve (max &optional (len 21) (best 0))
  (if (> len max) 
      (pseq-sum (find-seq best))
      (let ((res (find-seq len)))
	(solve max (1+ len) (if res len best)))))

(defun consec-primes (nth n-consec)
  (sum (mapcar #'nth-prime (range nth (+ nth n-consec)))))  
