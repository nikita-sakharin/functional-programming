(defun min-word-length (text)
    (let
        (
            (min-word "")
            (len most-positive-fixnum)
        )
        (dolist (sentence text)
            (dolist (word (word-list (replace-punctuation sentence)))
                (if (>= len (length word))
                    (progn
                        (setq min-word word)
                        (setq len (length word))
                    )
                )
            )
        )
        (values min-word len)
    )
)

(defun whitespace-char-p (ch)
    (member ch '(#\Space #\Tab #\Newline))
)

(defun word-list (string)
    (loop with len = (length string)
        for left = 0 then (1+ right)
        for right =
            (or
                (position-if #'whitespace-char-p string :start left)
                len
            )
        unless (= right left)
        collect (subseq string left right)
        while (< right len)
    )
)

(defun replace-punctuation (str)
    (let
        (
            (punctuation (coerce ",.;:?!" 'list))
            (key-char #\Space)
            (res (copy-seq str))
        )
        (dolist (ch punctuation)
            (setq res (substitute key-char ch res))
        )
        res
    )
)

(multiple-value-bind (word len)
    (min-word-length '("Встаёт рассвет во мгле холодной." "На нивах шум работ умолк."))
    (pprint (list word len))
)
