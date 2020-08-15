; コンスセル: listのようなもの, (1 2)
; シンボル: 基本的なデータ型, 'foo
; 数値: 1
; 文字列: "foo"

(make-array 3)
(defparameter x (make-array 3))
(setf (aref x 1) 'foo)

; ジェネリックなセッター
; CommonLispでは値を取り出すコードとその構造に値を入れるコードが同じ形で書ける

(defparameter x (make-hash-table))
(gethash 'yup x)


; 複数の値を返す
(defun foo ()
  (values 3 7))
(+ (foo) 5) ;1番目の値だけ使う --> 8
(multiple-value-bind (a b) (foo)
  (* a b)) ;複数の値を使う --> 21


; 構造体
(defstruct person
           name
           age
           waist-size
           favorite-color)
(defparameter *bob* (make-person :name "Bob"
                                 :age 35
                                 :waist-size 32
                                 :favorite-color "blue"))
(person-age *bob*) ; --> 35
(setf (person-age *bob*) (+ 1 (person-age *bob*)))