(defmacro let1 (var val &body body)
  `(let ((,var, val))
     ,@body))


(defmacro split (val yes no)
  (let1 g (gensym)
    `(let1 ,g ,val
       (if ,g
           (let ((head (car ,g))
                 (tail (cdr ,g)))
             ,yes)
           ,no))))


(split '(2 3)
  (format t "This can be split into ~a and ~a." head tail)
  (format t "This cannot be split."))


; splitマクロを使うver1
(defun my-length (lst)
  (labels ((f (lst acc)
             (split lst
               (f tail (1+ acc))
               acc)))
    (f lst 0)))


(defun pairs (lst)
  (labels ((f (lst acc)
              (split lst
                (if tail
                    (f (cdr tail) (cons (cons head (car tail)) acc))
                    (reverse acc))
                (reverse acc))))
    (f lst nil)))
(defmacro recurse (vars &body body)
  (let1 p (pairs vars)
    `(labels ((self ,(mapcar #'car p)
                ,@body))
       (self ,@(mapcar #'cdr p)))))


; recurseを使うver2
(defun my-length (lst)
  (recurse (lst lst acc 0)
           (split lst
             (self tail (1+ acc))
             acc)))


; reduceを使うver3
(defun my-length (lst)
  (reduce (lambda (x i)
            (1+ x))
          lst
          :initial-value 0))