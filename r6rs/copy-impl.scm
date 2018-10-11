;;;; Bytevector-copy, bytevector-copy!, and r6rs:bytevector-copy!

;; From Chibi

(define (bytevector-copy! to at from . o)
  (let* ((start (if (pair? o) (car o) 0))
         (end (if (and (pair? o) (pair? (cdr o)))
                  (cadr o)
                  (bytevector-length from)))
         (limit (min end (+ start (- (bytevector-length to) at)))))
    (if (<= at start)
        (do ((i at (+ i 1)) (j start (+ j 1)))
            ((>= j limit))
          (bytevector-u8-set! to i (bytevector-u8-ref from j)))
        (do ((i (+ at (- end start 1)) (- i 1)) (j (- limit 1) (- j 1)))
            ((< j start))
          (bytevector-u8-set! to i (bytevector-u8-ref from j))))))
 
(define (bytevector-copy from)
  (let ((to (make-bytevector (bytevector-length from))))
     (bytevector-copy! to 0 from)
     to))
 
;; Define R6RS bytevector-copy! in terms of R7RS version

(define (r6rs:bytevector-copy! source source-start target target-start count)
  (bytevector-copy! target target-start source source-start (+ source-start count)))
 

