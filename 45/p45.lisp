(ql:quickload :euler-utils)
(use-package :euler-utils)

(defun pent-p (p)
  (let* ((p24+1 (+ 1 (* 24 p)))
         (sqrt (isqrt p24+1)))
    (when (= (* sqrt sqrt) p24+1)
      (zerop (mod (+ 1 sqrt) 6)))))

(defun nth-hex (n)
  (* n (- (* 2 n) 1)))
   
(defun solve ()
  (do* ((n 144 (+ n 1))
        (hx (nth-hex n) (nth-hex n)))
       ((pent-p hx) hx)))
        
        
       