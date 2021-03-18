;;;
;;; patmatch.lisp
;;;

(defconstant fail nil "Indicates pat-match failure")

(defconstant no-bindings '((t . t)))

(setf (get '?is 	'single-match) 'match-is)
(setf (get '?or 	'single-match) 'match-or)
(setf (get '?and 	'single-match) 'match-and)
(setf (get '?not	'single-match) 'match-not)

(setf (get '?* 		'segment-match) 'segment-match)
(setf (get '?+ 		'segment-match) 'segment-match+)
(setf (get '?? 		'segment-match) 'segment-match?)
(setf (get '?if 	'segment-match) 'match-if)

(defun pat-match (pattern input &optional (bindings no-bindings))
    "Match pattern against input in the context of the bindings"
    (cond ((eq bindings fail) fail)
          ((variable-p pattern)
           (match-variable pattern input bindings))
          ((eql pattern input) bindings)
          ((segment-pattern-p pattern)
           (segment-matcher pattern input bindings))
          ((single-pattern-p pattern)
           (single-matcher pattern input bindings))
          ((and (consp pattern) (consp input))
           (pat-match (cdr pattern) (cdr input)
                      (pat-match (car pattern) (car input) bindings)))
          (t fail)))

(defun variable-p (x)
    (and (symbolp x) (equal (char (symbol-name x) 0) #\?)))

(defun get-binding (var bindings)
    (assoc var bindings))

(defun binding-var (binding)
    (car binding))

(defun binding-val (binding)
    (cdr binding))

(defun make-binding (var val) (cons var val))

(defun lookup (var bindings)
    (binding-val (get-binding var bindings)))

(defun extend-bindings (var val bindings)
    (cons (make-binding var val)
          (if (eq bindings no-bindings)
              nil
              bindings)))

(defun match-variable (var input bindings)
    (let ((binding (get-binding var bindings)))
        (cond ((not binding) (extend-bindings var input bindings))
              ((equal input (binding-val binding)) bindings)
              (t fail))))

(defun segment-pattern-p (pattern)
    "Is this a segment-matching pattern like ((?* var) . pat)?"
    (and (consp pattern) (consp (car pattern))
         (symbolp (caar pattern))
         (segment-match-fn (caar pattern))))

(defun single-pattern-p (pattern)
    "Is this a single-matching pattern?
	E.g. (?is x predicate) (?and . patterns) (?or . patterns)."
    (and (consp pattern)
         (single-match-fn (car pattern))))

(defun segment-matcher (pattern input bindings)
    "Call the right function for this kind of segment pattern."
    (funcall (segment-match-fn (caar pattern))
             pattern input bindings))

(defun single-matcher (pattern input bindings)
    "Call the right function for this kind of single pattern."
    (funcall (single-match-fn (car pattern))
             (cdr pattern) input bindings))

(defun segment-match-fn (x)
    "Get the segment-match function for x.
	if it is a symbol that has one."
    (when (symbolp x) (get x 'segment-match)))

(defun single-match-fn (x)
    "Get the single-match function for x.
	if it is a symbol that has one."
    (when (symbolp x) (get x 'single-match)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun match-is (var-and-pred input bindings)
    "Succeed and bind var if the input satisfies pred,
	where var-and-pred is the list (var pred)."
    (let* ((var (car var-and-pred))
            (pred (second var-and-pred))
            (new-bindings (pat-match var input bindings)))
        (if (or (eq new-bindings fail)
                (not (funcall pred input)))
            fail
            new-bindings)))

(defun match-and (patterns input bindings)
    "Succeed if all the patterns match the input."
    (cond ((eq bindings fail) fail)
          ((null patterns) bindings)
          (t (match-and (cdr patterns) input
                        (pat-match (car patterns) input
                                   bindings)))))

(defun match-or (patterns input bindings)
    "Succeed if any one of the patterns match the input."
    (if (null patterns)
        fail
        (let ((new-bindings (pat-match (car patterns)
                                       input bindings)))
            (if (eq new-bindings fail)
                (match-or (cdr patterns) input bindings)
                new-bindings))))

(defun match-not (patterns input bindings)
    "Succeed if none of the patterns match the input.
	This will never bind any variables."
    (if (match-or patterns input bindings)
        fail
        bindings))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun segment-match (pattern input bindings &optional (start 0))
    "Match the segment pattern ((?* var) . pat) against input."
    (let ((var (second (car pattern)))
          (pat (cdr pattern)))
        (if (null pat)
            (match-variable var input bindings)
            (let ((pos (first-match-pos (car pat) input start)))
                (if (null pos)
                    fail
                    (let ((b2 (pat-match
                               pat (subseq input pos)
                               (match-variable var (subseq input 0 pos)
                                               bindings))))
                        ;; If this match failed, try another longer one
                        (if (eq b2 fail)
                            (segment-match pattern input bindings (+ pos 1))
                            b2)))))))

(defun first-match-pos (pat input start)
    "Find the first position that pat could possibly match input,
	 starting at position start. If pat is non-constant, then just
	return start."
    (cond ((and (atom pat) (not (variable-p pat)))
           (position pat input :start start :test #'equal))
          ((< start (length input)) start)
          (t nil)))

(defun segment-match+ (pattern input bindings)
    "Match one or more elements of input."
    (segment-match pattern input bindings 1))

(defun segment-match? (pattern input bindings)
    "Match zero or one element of input."
    (let ((var (second (car pattern)))
          (pat (cdr pattern)))
        (or (pat-match (cons var pat) input bindings)
            (pat-match pat input bindings))))

(defun match-if (pattern input bindings)
    "Test an arbitrary expression involving variables.
	The pattern looks like ((?if code) . rest)."
    (and (progv (mapcar #'car bindings)
             (mapcar #'cdr bindings)
             (eval (second (car pattern))))
         (pat-match (cdr pattern) input bindings)))

(defun pat-match-abbrev (symbol expansion)
    "Define symbol as a macro standing for a pat-match pattern."
    (setf (get symbol 'expand-pat-match-abbrev)
          (expand-pat-match-abbrev expansion)))

(defun expand-pat-match-abbrev (pat)
    "Expand out all pattern matching abbreviations in pat."
    (cond ((and (symbolp pat) (get pat 'expand-pat-match-abbrev)))
          ((atom pat) pat)
          (t (cons (expand-pat-match-abbrev (car pat))
                   (expand-pat-match-abbrev (cdr pat))))))
