(define (not x) (if x #f #t))
(define (null? obj) (if (eqv? obj '()) #t #f))

(define (trace m x) (car (cons x (write m))))

(define (list . objs) objs)

(define (id obj) obj)
(define (flip func) (lambda (arg1 arg2) (func arg2 arg1)))

(define (curry func arg1) (lambda (arg) (apply func (cons arg1 (list arg)))))
(define (compose f g) (lambda (args) (f (apply g args))))

(define zero? (curry = 0))
(define positive? (curry < 0))
(define negative? (curry > 0))

(define (odd? num) (= (mod num 2) 1))
(define (even? num) (= (mod num 2) 0))

(define (foldr func end lst)
  (if (null? lst)
    end
    (func (car lst) (foldr func end (cdr lst)))))

(define (map func lst) (foldr (lambda (x acc) (cons (func x) acc)) '() lst))
(define (filter pred lst) (foldr (lambda (x acc) (if (pred x) (cons x acc) acc)) '() lst))
