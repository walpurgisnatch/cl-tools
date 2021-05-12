(defun single (lst)
    (and (consp lst) (not (cdr lst))))

(defun append-to-obj (lst obj)
    (append lst (list obj)))

(defun conc-to-obj (lst obj)
    (nconc lst (list obj)))

(defun mklist (obj)
    (if (listp obj) obj (list obj)))

(defun longer (x y)
    (labels ((compare (x y)
                 (and (consp x)
                      (or (null y)
                          (compare (cdr x) (cdr y))))))
        (if (and (listp x) (listp y))
            (compare x y)
            (> (length x) (length y)))))

(defun filter (fn lst)
    (let ((acc nil))
        (dolist (x lst)
            (let ((val (funcall fn x)))
                (if val (push val acc))))
        (nreverse acc)))

(defun group (source n)
    (if (zerop n) (error "zero length"))
    (labels ((rec (source acc)
                 (let ((rest (nthcdr n source)))
                     (if (consp rest)
                         (rec rest (cons (subseq source 0 n) acc))
                         (nreverse (cons source acc))))))
        (if source (rec source nil) nil)))

(defun flatten (x)
    (labels ((rec (x acc)
                 (cond ((null x) acc)
                       ((atom x) (cons x acc))
                       (t (rec (car x) (rec (cdr x) acc))))))
        (rec x nil)))

(defun prune (test tree)
    (labels ((rec (tree acc)
                 (cond ((null tree) (nreverse acc))
                       ((consp (car tree))
                        (rec (cdr tree)
                             (cons (rec (car tree) nil) acc)))
                       (t (rec (cdr tree)
                               (if (funcall test (car tree))
                                   acc
                                   (cons (car tree) acc)))))))
        (rec tree nil)))

;; Search

(defun find2 (fn lst)
    (if (null lst)
        nil
        (let ((val (funcall fn (car lst))))
            (if val
                (values (car lst) val)
                (fund 2 fn (cdr lst))))))

(defun before (x y lst &key (test #'eql))
    (and lst
         (let ((first (car lst)))
             (cond ((funcall test y first) nil)
                   ((funcall test x first) lst)
                   (t (before x y (cdr lst) :test test))))))

(defun after (x y lst &key (test #'eql))
    (let ((rest (before y x lst :test test)))
        (and rest (member x rest :test test))))

(defun duplicate (obj lst &key (test #'eql))
    (member obj (cdr (member obj lst :test test))
            :test test))

(defun split-if (fn lst)
    (let ((acc nil))
        (do ((src lst (cdr src)))
            ((or (null src) (funcall fn (car src)))
             (values (nreverse acc) src))
            (push (car src) acc))))

(defun most (fn lst)
    (if (null lst)
        (values nil nil)
        (let* ((wins (car lst))
               (max (funcall fn wins)))
            (dolist (obj (cdr lst))
                (let ((score (funcall fn obj)))
                    (when (> score max)
                        (setq wins obj
                              max  score))))
            (values wins max))))

(defun best (fn lst)
    (if (null lst)
        nil
        (let ((wins (car lst)))
            (dolist (obj (cdr lst))
                (if (funcall fn obj wins)
                    (setq wins obj)))
            wins)))

(defun mostn (fn lst)
    (if (null lst)
        (values nil nil)
        (let ((result (list (car lst)))
              (max (funcall fn (car lst))))
            (dolist (obj (cdr lst))
                (let ((score (funcall fn obj)))
                    (cond ((> score max)
                           (setq max    score
                                 result (list obj)))
                          ((= score max)
                           (push obj result)))))
            (values (nreverse result) max))))

;; Mapping

(defun mappend (fn &rest lsts)
    (apply #'append (apply #'mapcar fn lsts)))

(defun mapcars (fn &rest lsts)
    (let ((result nil))
        (dolist (lst lsts)
            (dolist (obj lst)
                (push (funcall fn obj) result)))
        (nreverse result)))

(defun rmapcar (fn &rest args)
    (if (some #'atom args)
        (apply fn args)
        (apply #'mapcar
               #'(lambda (&rest args)
                     (apply #'rmapcar fn args))
               args)))
