(defun matrix-sum-prod (a b)
    (let*
        (
            (m (first (array-dimensions a)))
            (n (second (array-dimensions b)))
            (res (make-array (list m n)))
            (product)
        )
        (dotimes (j n)
            (setq product (column-prod b j))
            (dotimes (i m)
                (setf (aref res i j) (+ (aref a i j) product))
            )
        )
    res)
)

(defun column-prod (matrix j)
    (let
        (
            (m (first (array-dimensions matrix)))
            (res 1)
        )
        (dotimes (i m)
            (setq res (* res (aref matrix i j)))
        )
    res)
)

(defun print-matrix (matrix &optional (chars 5))
    (let
        (
            (*print-right-margin* (+ 6 (* (1+ chars) (array-dimension matrix 1))))
        )
        (pprint matrix)
        (values)
    )
)

(setq a (make-array '(4 5) :initial-contents '(
    (1 2 3 4 5)
    (6 7 8 9 10)
    (11 12 13 14 15)
    (16 17 18 19 20))))

(setq b (make-array '(4 5) :initial-contents '(
    (1 2 3 4 5)
    (6 7 8 9 10)
    (11 12 13 14 15)
    (16 17 18 19 20))))

(setq c (matrix-sum-prod a b))
(print-matrix c)
