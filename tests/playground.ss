(load "stdlib.scm")

(define (inc x) (+ x 1))

(define text (read-contents "Setup.hs"))
(write text)

(map inc '(1 3 4 56 1))
