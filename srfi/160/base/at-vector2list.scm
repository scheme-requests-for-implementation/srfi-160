;;;; Implementation of SRFI 160 base @vector->list

;; List to vec

(define (list->@vector list)
  (define len (length list))
  (define vec (make-@vector len))
  (let loop ((i 0) (list list))
    (if (= i len)
      vec
      (begin
        (@vector-set! vec i (car list))
        (loop (+ i 1) (cdr list))))))

;; Vec to list

(define (@vector->list vec)
  (let loop ((i (- (@vector-length vec) 1))
             (list '()))
    (if (< i 0)
      list
      (loop (- i 1) (cons (@vector-ref vec i) list)))))

;; Simple fill! (not exported)

(define (@vector-simple-fill! vec value)
  (define len (@vector-length vec))
  (let loop ((i 0))
    (if (= i len)
      vec
      (begin
        (@vector-set! vec i value)
        (loop (+ i 1))))))

