(ql:quickload :euler-utils)
(use-package :euler-utils)

(defparameter *digits* '(1 2 3 4 5 6 7 8 9))

(defun get-multipliers (n-list)
  (let ((possible-digits (remove-if (rcurry #'member n-list) *digits*)))
    (remove nil (powerset possible-digits))))

(defun get-products (l1 l2)
  (let (products)
    (dolist (a (permutations l1))
      (dolist (b (permutations l2))
        (let* ((result (to-digits (* (from-digits a) (from-digits b))))
               (combined (append result (append a b))))
          (when (and (eql (length *digits*) (length combined))
                     (null (set-difference *digits* combined)))
              (pushnew (from-digits result) products)))))
    products))
      
         
(defun solve ()
  (let ((multiplicands (remove nil (powerset *digits*)))
        (products nil))
    (dolist (m multiplicands)
      (let ((multipliers (get-multipliers m)))
        (dolist (n multipliers)
          (setf products (union products (get-products m n))))))
    products))
         