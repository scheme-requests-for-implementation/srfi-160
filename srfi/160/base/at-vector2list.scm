;;;; Implementation of SRFI 160 base @vector->list

(define @vector->list
  (case-lambda
    ((vec) (@vector->list* vec 0 (@vector-length vec)))
    ((vec start) (@vector->list* vec start (@vector-length vec)))
    ((vec start end) (@vector->list* vec start end))))

(define (@vector->list* vec start end)
  (let loop ((i (- end 1))
             (list '()))
    (if (< i start)
      list
      (loop (- i 1) (cons (@vector-ref vec i) list)))))

