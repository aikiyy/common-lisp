(loop for i
      below 5
      do (print i))
(loop for i
      from 0
      do (print i)
      when (= i 5)
      return 'falafel)
(loop for x below 10
      collect (loop for y below 10
                    collect (+ x y)))
(loop for i
      for day
      in '(monday tuesday wednesday thursday friday satrueday sunday)
      collect (cons i day))