(defun check-one-move (x1 y1 x2 y2)
    (or
        (= (abs (- x2 x1)) (abs (- y2 y1)))
        (= x1 x2)
        (= y1 y2)
    )
)

(defun queen-moves (x1 y1 x2 y2)
    (if (check-one-move x1 y1 x2 y2)
        T
        (values x1 y2)
    )
)

(defun print-answer (i j)
    (if (null j)
        (print i)
        (print (list i j))
    )
)

(multiple-value-bind (i j)
    (queen-moves 4 4 7 7)
    (print-answer i j)
)

(multiple-value-bind (i j)
    (queen-moves 1 1 2 7)
    (print-answer i j)
)
