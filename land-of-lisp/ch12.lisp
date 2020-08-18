(with-open-file (my-stream "data.txt" :direction :output)
  (print "my data" my-stream))
(with-open-file (my-stream "data.txt" :direction :input)
  (read my-stream))


; ＠サーバー
; サーバー側でポートの使用権を得る
(defparameter my-socket (socket-server 4321))
; ソケットへ接続したクライアントとの通信を扱うストリームを作る
(defparameter my-stream (socket-accept my-socket))

; @クライアント
; プロセス同士が通信するためのストリームを作った
(defparameter my-stream (socket-connect 4321 "127.0.0.1"))
(print "Yo Server!" my-stream)

; @サーバー
(read my-stream) ; --> Yo Server!

; @サーバー @クライアント
; 閉じる
(close my-stream)
; @サーバー
; ポートを返却してソケットを開放する
(socket-server-close my-socket)