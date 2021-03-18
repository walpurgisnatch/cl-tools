;;;
;;; rbt.lisp
;;;

(defun rule-based-translator
    (input rules &key (matcher #'pat-match)
                   (rule-if #'car) (rule-then #'cdr) (action #'sublis))
    "Find the first rule in rules that matches input.
	and apply the action to that rule."
    (some
     #'(lambda (rule)
           (let ((result (funcall matcher (funcall rule-if rule)
                                  input)))
               (if (not (eq result fail))
                   (funcall action result (funcall rule-then rule)))))
     rules))

(defun use-rules (input)
    "Find some rule with which to transform the input."
    (rule-based-translator input *rules*
                           :action #'(lambda (bindings responses)
                                         (sublis (switch-viewpoint bindings)
                                                 (random-elt responses)))))
