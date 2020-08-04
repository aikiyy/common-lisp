; 前者は変数のcar, 後者は関数のcar
; -->変数の名前空間と関数の名前空間は別
(let ((car "Honda Civic"))
  (mapcar #'car '((foo bar) (baz qux))))

(append '(a) '(b) '(c))
(apply #'append '((a) (b) (c)))

(find 'y '((5 x) (3 y) (7 z)) :key #'cadr)