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
        (loop (+ i 1) (f value))))
    v))

(define (@vector-unfold-right f len seed)
  (let ((v (make-@vector len)))
    (let loop ((i (- len 1)) (value (f seed)))
      (unless (= i -1)
        (@vector-set! v i value)
        (loop (- i 1) (f value))))
    v))

(define @vector-copy
  (case-lambda
    ((vec) (@vector-copy* vec 0 (@vector-length vec)))
    ((vec start) (@vector-copy* vec start (@vector-length vec)))
    ((vec start end) (@vector-copy* vec start end))))

(define (@vector-copy* vec start end)
  (let ((v (make-@vector (- end start))))
    (@vector-copy! v 0 vec start end)
    v))

(define @vector-copy!
  (case-lambda
   ((to at from) (@vector-copy! to at from 0 (@vector-length from)))
   ((to at from start) (@vector-copy! to at from start (@vector-length from)))
   ((to at from start end)
    (let loop ((at at) (i start))
      (unless (= i end)
        (@vector-set! to at (@vector-ref from i))
        (loop (+ at 1) (+ i 1)))))))

(define @vector-reverse-copy
  (case-lambda
    ((vec) (@vector-reverse-copy* vec 0 (@vector-length vec)))
    ((vec start) (@vector-reverse-copy* vec start (@vector-length vec)))
    ((vec start end) (@vector-reverse-copy* vec start end))))

(define (@vector-reverse-copy* vec start end)
  (let ((v (make-@vector (- end start))))
    (@vector-reverse-copy! v 0 vec start end)
    v))

(define @vector-reverse-copy!
  (case-lambda
    ((to at from) (@vector-reverse-copy! to at from 0 (@vector-length from)))
    ((to at from start) (@vector-reverse-copy! to at from start 
                                               (@vector-length from)))
    ((to at from start end)
     (let loop ((at at) (i (- end 1)))
       (unless (< i start)
         (@vector-set! to at (@vector-ref from i))
         (loop (+ at 1) (- i 1)))))))

(define (@vector-append . vecs)
  (@vector-concatenate vecs))

(define (@vector-concatenate vecs)
  (let ((v (make-@vector (len-sum vecs))))
    (let loop ((vecs vecs) (at 0))
      (unless (null? vecs)
        (let ((vec (car vecs)))
          (@vector-copy! v at vec 0 (@vector-length vec))
          (loop (cdr vecs) (+ at (@vector-length vec))))))
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
        (let ((vec (car args))
              (start (cadr args))
              (end (caddr args)))
          (@vector-copy! v at vec start end)
          (loop (cdddr args) (+ at (- end start))))))
    v))

(define (len-subsum args)
  (if (null? args)
    0
    (+ (- (caddr args) (cadr args))
       (len-subsum (cdddr args)))))

;; @? defined in the base library

;; @vector? defined in the base library

(define (@vector-empty? vec)
  (zero? (@vector-length vec)))

(define @vector= 
  (case-lambda
    (() #t)
    ((vec) #t)
    ((v1 v2 . vecs) (@vector=* v1 v2 vecs))))

(define (@vector=* vec1 vec2 vecs)
  (and
   (@dyadic-vectors= vec1 0 (@vector-length vec1)
                     vec2 0 (@vector-length vec2))
   (if (null? vecs)
     #t
     (apply @vector= vecs))))

(define (@dyadic-vectors= vec1 start1 end1 vec2 start2 end2)
  (cond
    ((not (= end1 end2)) #f)
    ((= start1 end1) #t)
    ((= (@vector-ref vec1 start1) (@vector-ref vec2 start2))
     (@dyadic-vectors= vec1 (+ start1 1) end1
                       vec2 (+ start2 1) end2))
    (else #f)))

;; @vector-ref defined in the base library

;; @vector-length defined in the base library

(define (@vector-take vec n)
  (@vector-copy vec 0 n))

(define (@vector-take-right vec n)
  (let ((len (@vector-length vec)))
    (@vector-copy vec (- len n) len)))

(define (@vector-drop vec n)
  (@vector-copy vec n))

(define (@vector-drop-right vec n)
  (let ((len (@vector-length vec)))
    (@vector-copy vec 0 (- len n))))

(define (@vector-segment vec n)
  (let ((len (@vector-length vec)))
    (let loop ((r '()) (i 0) (remain len))
      (cond
        ((zero? remain)
         (reverse r))
        (else
          (let ((size (min n remain)))
            (loop
              (cons (@vector-copy vec i (+ i size)) r)
              (+ i size)
              (- remain size))))))))

(define (@vector-fold kons knil vec)
  (let ((len (@vector-length vec)))
    (let loop ((r knil) (i 0))
      (if (= i len)
        r
        (loop (kons r (@vector-ref vec i)) (+ i 1))))))

(define (@vector-fold-right kons knil vec)
  (let ((len (@vector-length vec)))
    (let loop ((r knil) (i (- len 1)))
      (if (negative? i)
        r
        (loop (kons (@vector-ref vec i) r) (- i 1))))))

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
        (@vector-set! vec i (f (@vector-ref vec i)))
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

(define (@vector-for-each f vec)
  (let ((len (@vector-length vec)))
    (let loop ((i 0))
      (unless (= i len)
        (f (@vector-ref vec i))
        (loop (+ i 1))))))

(define (@vector-take-while pred? vec)
  (@vector-copy vec 0 (or (@vector-skip pred? vec)
                          (@vector-length vec))))

(define (@vector-take-while-right pred? vec)
  (@vector-copy vec (or (@vector-index-right pred? vec) 0)))

(define (@vector-drop-while pred? vec)
  (let ((idx (@vector-skip pred? vec)))
    (if idx
      (@vector-copy vec idx)
      (@vector))))

(define (@vector-drop-while-right pred? vec)
  (let ((idx (@vector-skip-right pred? vec)))
    (if idx
      (@vector-copy vec 0 (+ idx 1))
      (@vector))))

(define (@vector-index pred? vec)
  (let ((len (@vector-length vec)))
    (let loop ((i 0))
      (cond
        ((= i len) #f)
        ((pred? (@vector-ref vec i)) i)
        (else (loop (+ i 1)))))))

(define (@vector-index-right pred? vec)
  (let ((len (@vector-length vec)))
    (let loop ((i (- len 1)))
      (cond
        ((negative? i) #f)
        ((pred? (@vector-ref vec i)) i)
        (else (loop (- i 1)))))))

(define (@vector-skip pred? vec)
    (@vector-index (lambda (x) (not (pred? x))) vec))

(define (@vector-skip-right pred? vec)
    (@vector-index-right (lambda (x) (not (pred? x))) vec))

(define (@vector-any pred? vec)
  (let ((idx (@vector-index pred? vec)))
    (if idx (@vector-ref vec idx) #f)))

(define (@vector-every pred? vec)
  (let ((len (@vector-length vec)))
    (let loop ((i 0))
      (cond
        ((= len 0) #t)
        ((= i len) (@vector-ref vec (- len 1)))
        ((pred? (@vector-ref vec i)) (loop (+ i 1)))
        (else #f)))))

(define (@vector-partition pred? vec)
  (let* ((len (@vector-length vec))
         (cnt (@vector-count pred? vec))
         (r (make-@vector len)))
    (let loop ((i 0) (yes 0) (no cnt))
      (cond
        ((= i len) r)
        ((pred? (@vector-ref vec i))
         (@vector-set! r yes (@vector-ref vec i))
         (loop (+ i 1) (+ yes 1) no))
        (else
         (@vector-set! r no (@vector-ref vec i))
         (loop (+ i 1) yes (+ no 1)))))))

(define (@vector-filter pred? vec)
  (let* ((len (@vector-length vec))
         (cnt (@vector-count pred? vec))
         (r (make-@vector cnt)))
    (let loop ((i 0) (j 0))
      (cond
        ((= i len) r)
        ((pred? (@vector-ref vec i))
         (@vector-set! r j (@vector-ref vec i))
         (loop (+ i 1) (+ j 1)))
        (else
         (loop (+ i 1) j))))))

(define (@vector-remove pred? vec)
  (@vector-filter (lambda (x) (not (pred? x))) vec))

;; @vector-set! defined in the base library

(define (@vector-swap! vec i j)
  (let ((ival (@vector-ref vec i))
        (jval (@vector-ref vec j)))
    (@vector-set! vec i jval)
    (@vector-set! vec j ival)))

(define @vector-fill!
  (case-lambda
    ((vec fill) (@vector-fill-some! vec fill 0 (@vector-length vec)))
    ((vec fill start) (@vector-fill-some! vec fill start (@vector-length vec)))
    ((vec fill start end) (@vector-fill-some! vec fill start end))))

(define (@vector-fill-some! vec fill start end)
  (unless (= start end)
    (@vector-set! vec start fill)
    (@vector-fill-some! vec fill (+ start 1) end)))

(define @vector-reverse!
  (case-lambda
    ((vec) (@vector-reverse-some! vec 0 (@vector-length vec)))
    ((vec start) (@vector-reverse-some! vec start (@vector-length vec)))
    ((vec start end) (@vector-reverse-some! vec start end))))

(define (@vector-reverse-some! vec start end)
  (let ((len (@vector-length vec)))
    (let loop ((i 0) (j (- len 1)))
      (when (< i j)
        (@vector-swap! vec i j)
        (loop (+ i 1) (- j 1))))))

(define @vector->list
  (case-lambda
    ((vec) (reverse (reverse-@vector->list* vec 0 (@vector-length vec))))
    ((vec start) (reverse (reverse-@vector->list* vec start (@vector-length vec))))
    ((vec start end) (reverse (reverse-@vector->list* vec start end)))))

(define reverse-@vector->list
  (case-lambda
    ((vec) (reverse-@vector->list* vec 0 (@vector-length vec)))
    ((vec start) (reverse-@vector->list* vec start (@vector-length vec)))
    ((vec start end) (reverse-@vector->list* vec start end))))

(define (reverse-@vector->list* vec start end)
  (let loop ((i start) (r '()))
    (if (= i end)
      r
      (loop (+ 1 i) (cons (@vector-ref vec i) r)))))

(define (list->@vector list)
  (let* ((len (length list))
         (r (make-@vector len)))
    (let loop ((i (- len 1)) (list list))
      (cond
        ((negative? i) r)
        (else
          (@vector-set! r i (car list))
          (loop (+ i 1) (cdr list)))))))

(define (reverse-list->@vector list)
  (let* ((len (length list))
         (r (make-@vector len)))
    (let loop ((i 0) (list list))
      (cond
        ((= i len) r)
        (else
          (@vector-set! r i (car list))
          (loop (+ i 1) (cdr list)))))))

(define (@vector->vector vec)
  (let* ((len (@vector-length vec))
         (r (make-vector len)))
    (let loop ((i 0))
      (cond
        ((= i len) r)
        (else
          (vector-set! r i (@vector-ref vec i))
          (loop (+ i 1)))))))

(define (vector->@vector vec)
  (let* ((len (vector-length vec))
         (r (make-@vector len)))
    (let loop ((i 0))
      (cond
        ((= i len) r)
        (else
          (@vector-set! r i (vector-ref vec i))
          (loop (+ i 1)))))))

(define @vector->generator
  (case-lambda ((vec) (@vector->generator vec 0 (@vector-length vec)))
               ((vec start) (@vector->generator vec start (@vector-length vec)))
               ((vec start end)
                (lambda () (if (>= start end)
                             (eof-object)
                             (let ((next (@vector-ref vec start)))
                              (set! start (+ start 1))
                              next))))))

(define @vector-write
  (case-lambda
    ((vec) (@vector-write* vec (current-output-port)))
    ((vec port) (@vector-write* vec port))))


(define (@vector-write* vec port)
  (display "@(" port)  ; @-expansion is blind, so will expand this too
  (let ((last (- (@vector-length vec) 1)))
    (let loop ((i 0))
      (cond
        ((= i last)
         (write (@vector-ref vec i) port)
         (display ")" port))
        (else
          (write (@vector-ref vec i) port)
          (display " " port))))))
