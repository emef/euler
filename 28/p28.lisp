(defconstant +goal-length+ 1001)

(defun side-lengths-r (a s up-to)
  (let* ((br (+ a (1- s)))
         (bl (+ br (1- s)))
         (tl (+ bl (1- s)))
         (tr (+ tl (1- s))))
    (cons (list br bl tl tr)
          (if (< s up-to)
              (side-lengths-r tr (+ s 2) up-to)
              nil))))

(defun side-lengths (up-to)
  (side-lengths-r 1 3 up-to))

(defun solve ()
  (1+ (reduce #'(lambda (acc corners)
                  (+ acc (apply #'+ corners)))
              (side-lengths +goal-length+)
              :initial-value 0)))
          
