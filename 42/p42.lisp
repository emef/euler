;The nth term of the sequence of triangle numbers is given by, tn =
;½n(n+1); so the first ten triangle numbers are:

;1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...

;By converting each letter in a word to a number corresponding to its
;alphabetical position and adding these values we form a word
;value. For example, the word value for SKY is 19 + 11 + 25 = 55 =
;t10. If the word value is a triangle number then we shall call the
;word a triangle word.

;Using words.txt (right click and 'Save Link/Target As...'), a 16K text
;file containing nearly two-thousand common English words, how many are
;triangle words?

(defparameter *max-tri-n* 1000)
(defparameter *words* (with-open-file (str "words.txt" :direction :input)
                (read str)))

(defun char-value (c)
  (1+ (- (char-code c) (char-code #\A))))

(defun word-value (str)
  (reduce #'(lambda (acc c)
              (+ acc (char-value c)))
          str 
          :initial-value 0))

(defun filter (fn lst)
  (let (acc)
    (dolist (el lst)
      (when (funcall fn el)
        (push el acc)))
    acc))

(let ((tri-table (make-hash-table)))
  (dotimes (i *max-tri-n*)
    (setf (gethash (/ (* i (+ 1 i)) 2) tri-table) t))
  (defun tri-n-p (n)
    (gethash n tri-table)))

(time
 (format t "number of triangle words: ~a~%"
         (length (filter #'tri-n-p (mapcar #'word-value *words*)))))