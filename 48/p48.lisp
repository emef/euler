(reduce #'+ (mapcar #'(lambda (n) (expt n n)) (range 1 1000)))