(print "foo")

(progn (print "this")
       (print "is")
       (print "a")
       (print "pen"))

(progn (prin1 "this")
       (prin1 "is")
       (prin1 "a")
       (prin1 "pen"))

(defun say-hello()
  (print "Please type your name:")
  (let ((name (read)))
    (print "Nice to meet you, ")
    (print name)))