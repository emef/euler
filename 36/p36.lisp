;The decimal number, 585 = 10010010012 (binary), is palindromic in
;both bases.

;Find the sum of all numbers, less than one million, which are
;palindromic in base 10 and base 2.

;(Please note that the palindromic number, in either base, may not
;include leading zeros.)

(defun palindrome-p (x)
  "determines if x is a palindrome. x must be a sequence"
  (equal x (reverse x)))

(defun double-palindrome-p (n)
  "determines if n (integer) is a palindrome in both base 10 and 2"
  (and (palindrome-p (format nil "~a" n))
       (palindrome-p (format nil "~b" n))))

(defun filter (fn lst &optional keep-order)
  (let (acc)
    (dolist (el lst)
      (when (funcall fn el)
        (push el acc)))
    (if keep-order
        (nreverse acc)
        acc)))

(defun range (i n &optional acc)
  (if (>= n i)
      (range i (1- n) (cons n acc))
      acc))

(reduce #'+ 
        (filter #'double-palindrome-p
                (range 1 (1- 1000000))))