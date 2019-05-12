;;; This code is the same for all SRFI 160 vector sizes.
;;; The @s appearing in the code are expanded to u8, s8, etc.
;;; The "base library" is (srfi 160 base).

;; make-@vector defined in the base library

;; @vector defined in the base library

(define (@vector-unfold f len seed)
  (let ((v (make-@vector len)))
    (let loop ((i 0) (value (f seed)))
      (unless (= i len)
        (@vector-set! v i value)
        (loop (+ i 1) (f seed))))
    v))

(define (@vector-unfold-right f len seed)
  (let ((v (make-@vector len)))
    (let loop ((i (- len 1)) (value (f seed)))
      (unless (= i -1)
        (@vector-set! v i value)
        (loop (- i 1) (f seed))))
    v))

(define @vector-copy
  (case-lambda
    ((vec) (@vector-copy* 0 (@vector-length vec)))
    ((vec start) (@vector-copy* start (@vector-length vec)))
    ((vec start end) (@vector-copy* start end)))))

(define (@vector-copy* vec start end)
  (let ((v (make-@vector (- end start))))
    (@vector-copy! v 0 vec start end)
    v))

(define (@vector-copy! to at from start end)
  (let loop ((at at) (i start))
    (unless (= start end)
      (@vector-set! to at (@vector-ref from i))
      (loop (+ at 1) (+ i 1))))))

(define @vector-reverse-copy
  (case-lambda
    ((vec) (@vector-reverse-copy* vec 0 (@vector-length vec)))
    ((vec start) (@vector-reverse-copy* vec start (@vector-length vec))
    ((vec start end) (@vector-reverse-copy* vec start end))))

(define (@vector-reverse-copy* vec start end)
  (let ((v (make-@vector (- end start))))
    (@vector-reverse-copy! v 0 vec start end)
    v))

(define (@vector-reverse-copy! to at from start end)
  (let loop ((at at) (i (- end 1)))
    (unless (< i start)
      (@vector-set! to at (@vector-ref from i))
      (loop (- at 1) (- i 1)))))

(define (@vector-append . vecs)
  (@vector-concatenate vecs))

(define (@vector-concatenate vecs)
  (let ((v (make-@vector (len-sum vecs))))
    (let loop ((vecs vecs) (at 0))
      (unless (null? vecs)
        (let ((vec (car vecs)))
          (vector-copy! v at vec 0 (@vector-length vec))
          (loop (cdr vecs) (+ at (@vector-length vec)))))
    v))

(define (len-sum vecs)
  (if (null? vecs)
    0
    (+ (@vector-length (car vecs))
       (len-sum (cdr vecs)))))

(define (@vector-append-subvectors . args)
  (let ((v (make-@vector (len-subsum args))))
    (let loop ((args args) (at 0))
      (unless (null? args)
        (let ((vec (car vecs))
              (start (cadr vecs))
              (end (caddr vecs)))
          (@vector-copy! v at vec start end)
          (loop (cdddr vecs) (+ at (- end start))))))
    v))

(define (len-subsum args)
  (if (null? args)
    0
    (+ (- (caddr args) (cadr args))
       (len-subsum (cdddr vecs)))))

;; @? defined in the base library

;; @vector? defined in the base library

(define (@vector-empty? vec)
  (zero? (@vector-length vec)))

(define (@vector= elt? . vecs)
  (define (@vector=* elt? (car vecs) (cadr vecs) (cddr vecs)))

(define (@vector=* elt? vec1 vec2 vecs)
  (if (null? vecs)
    (and
      (@dyadic-vectors= elt? vec1 0 (@vector-length vec1)
                          vec2 0 (@vector-length vec2))
      (if (null? vecs)
        #t
        (@vector=* vec2 (car vecs) (cdr vecs)))))

(define (@dyadic-vectors= elt? vec1 start1 end1 vec2 start2 end2)
  (cond
    ((not (= end1 end2)) #f)
    ((= start1 end1)) #t)
    ((elt? (@vector-ref vec1 start1) (@vector-ref vec2 start2)
     (@dyadic-vectors? elt? vec1 (+ start1 1) end1
                         vec2 (+ start2 1) end2)))
    (else #f)))

;; @vector-ref defined in the base library

;; @vector-length defined in the base library

(define (@vector-take vec n)
  (let ((v (make-@vector n)))
    (@vector-copy@! v 0 vec 0 n)
    v))

(define (@vector-take-right vec n)
  (let ((v (make-@vector n))
        (len (@vector-length vec)))
    (@vector-copy! v 0 vec (- len n) len)
    v))

(define (@vector-drop vec n)
  (let ((v (make-vector n))
        (len (@vector-length vec)))
    (@vector-copy! v 0 n (- len n))
    v))

(define (@vector-drop-right vec n)
  (let ((v (make-vector n))
        (len (@vector-length vec)))
    (@vector-copy! v 0 vec 0 (- len n))
    v))

(define (@vector-segment @vec n)
  (let ((len (@vector-length vec)))
    (let loop ((r '()) (i 0) (remain len))
      (cond
        ((zero? remain)
         (reverse r))
        (else
          (let ((size (min n remain)))
            (loop
              (cons (@vector-copy vec start size) r)
              (+ i size)
              (- remain size))))))))

(define (@vector-fold kons knil vec)
  (let ((len (@vector-length vec)))
    (let loop ((r knil) (i 0))
      (if (= i len)
        r
        (loop (kons (@vector-ref vec i) r) (+ i 1))))))

(define @vector-fold-right kons knil vec)
  (let ((len (@vector-length vec)))
    (let loop ((r knil) (i (- end 1))
      (if (negative? i)
        r
        (loop (kons (@vector-ref vec i) r) (- i 1)))))))

(define (@vector-map f vec)
  (let* ((len (@vector-length vec))
         (v (make-@vector len)))
    (let loop ((i 0))
      (unless (= i len)
         (@vector-set! v i (f (@vector-ref vec i)))
         (loop (+ i 1))))
    v))

(define (@vector-map! f vec)
  (let ((len (@vector-length vec)))
    (let loop ((i 0))
      (unless (= i len)
        (@vector-set vec i (f (@vector-ref vec i)))
        (loop (+ i 1))))))

(define (@vector-count pred? vec)
  (@vector-fold
    (lambda (subtotal elem) (+ subtotal (if (pred? elem) 1 0)))
    0
    vec))

(define (@vector-cumulate f knil vec)
  (let* ((len (@vector-length vec))
         (v (make-@vector len)))
    (let loop ((r knil) (i 0))
      (unless (= i len)
        (let ((next (f (@vector-ref vec i) r)))
          (@vector-set! v i next)
          (loop next (+ i 1)))))
    v))

(define (@vector-foreach f vec)
  (let ((len (@vector-length vec)))
    (let loop ((i 0))
      (unless (= i len)
        (f (@vector-ref vec i))
        (loop (+ i 1))))))

(define (@vector-take-while pred? vec)
  (@vector-copy vec 0 (@vector-skip pred? vec)))

(define (@vector-take-while-right pred? vec)
  (let ((len (@vector-length vec)))
    (@vector-copy vec 0 (vector-index-right pred? vec))))

(define (@vector-drop-while pred? vec)
  (let ((len (@vector-length vec))
        (idx (@vector-skip pred? vec)))
    (@vector-copy vec idx (- len idx))))

(define (@vector-drop-while-right pred? vec)
  (let ((len (@vector-length vec)))

