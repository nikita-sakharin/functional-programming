(defgeneric add2 (arg1 arg2)
    (:method ((n1 number) (n2 number))
        (+ n1 n2)
    )
)

(defgeneric negate (arg1)
    (:method ((n1 number))
        (- n1)
    )
)

(defgeneric subtract (arg1 arg2)
    (:method ((n1 number) (n2 number))
        (add2 n1 (negate n2))
    )
)

(defclass cart ()
    (
        (x :initarg :x :accessor cart-x)
        (y :initarg :y :accessor cart-y)
    )
)

(defmethod print-object ((c cart) stream)
    (format stream "(Cart x ~d y ~d)"
        (cart-x c) (cart-y c)
    )
)

(defmethod add2 ((c1 cart) (c2 cart))
    (make-instance 'cart
        :x (+ (cart-x c1) (cart-x c2))
        :y (+ (cart-y c1) (cart-y c2))
    )
)

(defmethod negate ((c1 cart))
    (make-instance 'cart
        :x (- (cart-x c1))
        :y (- (cart-y c1))
    )
)

(defmethod subtract ((c1 cart) (c2 cart))
    (add2 c1 (negate c2))
)

(defclass line ()
    (
        (start :initarg :start :accessor line-start)
        (end :initarg :end :accessor line-end)
    )
)

(defmethod print-object ((lin line) stream)
    (format stream "(Line ~s ~s)"
          (line-start lin) (line-end lin)
    )
)

(defun line-angle (lin)
    (let
        (
            (r (subtract (line-end lin) (line-start lin)))
        )
        ()
        (atan (/ (cart-y r) (cart-y r)))
    )
)

(defun rad-to-deg (radians)
    (* (/ 180.0 pi) radians)
)

(setq lin
    (make-instance 'line
        :start (make-instance 'cart :x 1 :y 1)
        :end (make-instance 'cart :x 2 :y 2)
    )
)

(print lin)
(print (rad-to-deg (line-angle lin)))
