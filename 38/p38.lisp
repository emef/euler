(ql:quickload :euler-utils)
(use-package :euler-utils)

(defparameter *digits* '(1 2 3 4 5 6 7 8 9))

(defun multipliers-lst (n)
  (let (acc)
    (loop for val in *max-vals*
          for i from 1 to 9 do
         (when (< n val)
           (push (cons i (car acc)) acc)))
    acc))

(defun pandigital-p (n-lst)
  (set-eql n-lst *digits*))

(defun concatenated-product (n multipliers)
  (let ((products (mapcar (curry #'* n) multipliers)))
    (apply #'append (mapcar #'to-digits products))))

(defun get-products (n &optional (lst '(1 2)))
  (let ((product (concatenated-product n lst)))
    (when (<= (length product) 9)
      (cons product (get-products n (cons (1+ (car lst)) lst))))))

(defun get-pandigital-products (n)
  (mapcar #'from-digits (filter #'pandigital-p (get-products n))))

(defun solve ()
  (let ((best 0))
    (dolist (i (range 0 4))
      (loop for n from (* 9 (expt 10 i)) to (1- (expt 10 (1+ i))) do
        (let ((result (get-pandigital-products n)))
          (when result
            (setf best (max best (maximum result)))))))
    best))
      
  
