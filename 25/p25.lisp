;The Fibonacci sequence is defined by the recurrence relation:

;Fn = Fn1 + Fn2, where F1 = 1 and F2 = 1.
;Hence the first 12 terms will be:

;F11 = 89
;F12 = 144
;The 12th term, F12, is the first term to contain three digits.

;What is the first term in the Fibonacci sequence to contain 1000 digits?

(defun range (i n)
  (when (<= i n)
    (cons i (range (1+ i) n))))

(let ((fib-table (make-hash-table)))
  (defun fib (n)
    (if (< n 3)
        1
        (let ((memo (gethash n fib-table)))
          (or memo
              (setf (gethash n fib-table) 
                    (+ (fib (- n 1)) (fib (- n 2)))))))))

(defun count-digits (n &optional (count 0))
  (if (> n 0)
      (count-digits (floor (/ n 10)) (1+ count))
      count))

(defun first-fib-n-digits (n &optional (start 1))
  (if (eql n (count-digits (fib start)))
      start
      (first-fib-n-digits n (1+ start))))
  