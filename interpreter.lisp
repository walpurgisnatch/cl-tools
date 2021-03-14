;;;
;;; interpreter.lisp
;;; 

(defun interactive-interpreter (prompt transformer)
    "Read an expression, transform it, and print the result."
    (loop
      (handler-case
          (progn
              (if (stringp prompt)
                  (print prompt)
                  (funcall prompt))
              (print (funcall transformer (read))))
        (error (condition)
            (format t "~&;; Error ~a ignored, back to top level."
                    condition)))))

(defun prompt-generator (&optional (num 0) (ctl-string "[~d] "))
    "Return a function that prints prompts like [1], [2], etc."
    #'(lambda () (format t ctl-string (incf num))))

