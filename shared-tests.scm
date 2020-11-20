(define (times2 x) (* x 2))
(define s5 (s16vector 1 2 3 4 5))
(define s4 (s16vector 1 2 3 4))
(define s5+ (s16vector 1 2 3 4 6))

(define (steady i x) (values x x))
(define (count-up i x) (values x (+ x 1)))
(define (count-down i x) (values x (- x 1)))
(define (odd+1 x) (if (odd? x) (+ 1 x) #f))
(define s16vector< (comparator-ordering-predicate s16vector-comparator))
(define s16vector-hash (comparator-hash-function s16vector-comparator))

(define g (make-s16vector-generator s5))
(define-syntax test-equiv
  (syntax-rules ()
    ((test-equiv expect expr)
     (test expect (s16vector->list expr)))
    ((test-equiv name expect expr)
     (test name expect (s16vector->list expr)))))

(test-group "s16vector"
(test-group "s16vector/constructors"
  (test-equiv "make" '(3 3 3 3 3) (make-s16vector 5 3))
  (test-equiv "s16vector" '(-2 -1 0 1 2) (s16vector -2 -1 0 1 2))
  (test-equiv "unfold up" '(10 11 12 13 14)
              (s16vector-unfold count-up 5 10))
  (test-equiv "unfold down" '(10 9 8 7 6)
              (s16vector-unfold count-down 5 10))
  (test-equiv "unfold steady" '(10 10 10 10 10)
              (s16vector-unfold steady 5 10))
  (test-equiv "unfold-right up" '(14 13 12 11 10)
              (s16vector-unfold-right count-up 5 10))
  (test-equiv "unfold-right down" '(6 7 8 9 10)
              (s16vector-unfold-right count-down 5 10))
  (test-equiv "unfold-right steady" '(10 10 10 10 10)
              (s16vector-unfold-right steady 5 10))
  (test-equiv "copy" '(1 2 3 4 5) (s16vector-copy s5))
  (test-assert "copy2" (not (eqv? s5 (s16vector-copy s5))))
  (test-equiv "copy3" '(2 3) (s16vector-copy s5 1 3))
  (test-equiv "reverse-copy" '(5 4 3 2 1) (s16vector-reverse-copy s5))
  (test-equiv "append" '(1 2 3 4 5 1 2 3 4 5)
              (s16vector-append s5 s5))
  (test-equiv "concatenate" '(1 2 3 4 5 1 2 3 4 5)
              (s16vector-concatenate (list s5 s5)))
  (test-equiv "append-subvectors" '(2 3 2 3)
              (s16vector-append-subvectors s5 1 3 s5 1 3))
) ; end s16vector/constructors

(test-group "s16vector/predicates"
  (test-assert "s16?" (s16? 5))
  (test-assert "not s16?" (not (s16? 65536)))
  (test-assert "s16vector?" (s16vector? s5))
  (test-assert "not s16vector?" (not (s16vector? #t)))
  (test-assert "empty" (s16vector-empty? (s16vector)))
  (test-assert "not empty" (not (s16vector-empty? s5)))
  (test-assert "=" (s16vector= (s16vector 1 2 3) (s16vector 1 2 3)))
  (test-assert "= multi" (s16vector= (s16vector 1 2 3)
                                     (s16vector 1 2 3)
                                     (s16vector 1 2 3)))
  (test-assert "not =" (not (s16vector= (s16vector 1 2 3) (s16vector 3 2 1))))
  (test-assert "not =2" (not (s16vector= (s16vector 1 2 3) (s16vector 1 2))))
  (test-assert "not = multi" (not (s16vector= (s16vector 1 2 3)
                                              (s16vector 1 2 3)
                                              (s16vector 3 2 1))))
) ; end s16vector/predicates

(test-group "s16vector/selectors"
  (test "ref" 1 (s16vector-ref (s16vector 1 2 3) 0))
  (test "length" 3 (s16vector-length (s16vector 1 2 3)))
) ; end s16vector/selectors

(test-group "s16vector/iteration"
  (test-equiv "take" '(1 2) (s16vector-take s5 2))
  (test-equiv "take-right" '(4 5) (s16vector-take-right s5 2))
  (test-equiv "drop" '(3 4 5) (s16vector-drop s5 2))
  (test-equiv "drop-right" '(1 2 3) (s16vector-drop-right s5 2))
  (test "segment" (list (s16vector 1 2 3) (s16vector 4 5))
        (s16vector-segment s5 3))
  (test "fold" -6 (s16vector-fold - 0 (s16vector 1 2 3)))
  (test "fold" '(((0 1 4) 2 5) 3 6)
        (s16vector-fold list 0 (s16vector 1 2 3) (s16vector 4 5 6)))
  (test "fold-right" -6 (s16vector-fold-right - 0 (s16vector 1 2 3)))
  (test "fold-right" '(((0 3 6) 2 5) 1 4)
        (s16vector-fold-right list 0 (s16vector 1 2 3) (s16vector 4 5 6)))
  (test-equiv "map" '(-1 -2 -3 -4 -5) (s16vector-map - s5))
  (test-equiv "map" '(-2 -4 -6 -8 -10) (s16vector-map - s5 s5 s5 s5))
  (let ((v (s16vector 1 2 3 4 5)))
    (s16vector-map! - v)
    (test-equiv "map!" '(-1 -2 -3 -4 -5) v))
  (let ((v (s16vector 1 2 3 4 5))
        (v2 (s16vector 6 7 8 9 10)))
    (s16vector-map! + v v2)
    (test-equiv "map!" '(7 9 11 13 15) v))
  (let ((list '()))
    (s16vector-for-each
      (lambda (e) (set! list (cons e list)))
      s5)
    ;; stupid hack to shut up test egg about testing the value of a variable
    (test "for-each" '(5 4 3 2 1) (cons (car list) (cdr list))))
  (let ((list '()))
    (s16vector-for-each
      (lambda (e1 e2) (set! list (cons (cons e1 e2) list)))
      s5
      (s16vector 6 7 8 9 10))
    ;; stupid hack to shut up test egg about testing the value of a variable
    (test "for-each" '((5 . 10) (4 . 9) (3 . 8) (2 . 7) (1 . 6))
          (cons (car list) (cdr list))))
  (test "count" 3 (s16vector-count odd? s5))
  (test "count" 2 (s16vector-count > s5 (s16vector 9 2 1 5 3)))
  (test-equiv "cumulate" '(1 3 6 10 15)
              (s16vector-cumulate + 0 s5))
) ; end s16vector/iteration

(test-group "s16vector/searching"
  (test-equiv "take-while" '(1) (s16vector-take-while odd? s5))
  (test-equiv "take-while-right" '(5) (s16vector-take-while-right odd? s5))
  (test-equiv "drop-while" '(2 3 4 5) (s16vector-drop-while odd? s5))
  (test-equiv "drop-while-right" '(1 2 3 4) (s16vector-drop-while-right odd? s5))
  (test-equiv "degenerate take-while" '() (s16vector-take-while inexact? s5))
  (test-equiv "degenerate take-while-right" '() (s16vector-take-while-right inexact? s5))
  (test-equiv "degenerate drop-while" '(1 2 3 4 5) (s16vector-drop-while inexact? s5))
  (test-equiv "degenerate drop-while-right" '(1 2 3 4 5) (s16vector-drop-while-right inexact? s5))
  (test "index" 1 (s16vector-index even? s5))
  (test "index" 2 (s16vector-index < s5 (s16vector 0 0 10 10 0)))
  (test "index-right" 3 (s16vector-index-right even? s5))
  (test "index-right" 3 (s16vector-index-right < s5 (s16vector 0 0 10 10 0)))
  (test "skip" 1 (s16vector-skip odd? s5))
  (test "skip" 2 (s16vector-skip > s5 (s16vector 0 0 10 10 0)))
  (test "skip-right" 3 (s16vector-skip-right odd? s5))
  (test "skip-right" 3 (s16vector-skip-right > s5 (s16vector 0 0 10 10 0)))
  (test "any" 4 (s16vector-any (lambda (x) (and (even? x) (* x 2))) s5))
  (test-assert "not any" (not (s16vector-any inexact? s5)))
  (test "any + 1" 2 (s16vector-any odd+1 s5))
  (test-assert "every" (s16vector-every exact? s5))
  (test-assert "not every" (not (s16vector-every odd? s5)))
  (test-assert "every + 1" (not (s16vector-every odd+1 s5)))
  (test "multi-any" 10 (s16vector-any (lambda (x y) (and (even? x) (even? y) (+ x y)))
                                s5 (s16vector 0 1 2 6 4)))
  (test "multi-any 2" #f (s16vector-any (lambda (x y) (and (even? x) (even? y) (+ x y)))
                                s5 (s16vector 0 1 2 5 4)))
  (test "multi-every" 10 (s16vector-every (lambda (x) (and (exact? x) (* x 2))) s5))
  (test "multi-every-2" 10 (s16vector-every (lambda (x y) (and (exact? x) (exact? y) (+ x y)))
                                    s5 s5))
  (test-assert "multi-not every" (not (s16vector-every < s5 (s16vector 10 10 10 10 0))))
  (test-equiv "partition" '(1 3 5 2 4) (s16vector-partition odd? s5))
  (test-equiv "filter" '(1 3 5) (s16vector-filter odd? s5))
  (test-equiv "remove" '(2 4) (s16vector-remove odd? s5))
) ; end s16vector/searching

(test-group "s16vector/mutators"
  (let ((v (s16vector 1 2 3)))
    (display "set!\n")
    (s16vector-set! v 0 10)
    (test-equiv "set!" '(10 2 3) v))
  (let ((v (s16vector 1 2 3)))
    (display "swap!\n")
    (s16vector-swap! v 0 1)
    (test-equiv "swap!" '(2 1 3) v))
  (let ((v (s16vector 1 2 3)))
    (display "fill!\n")
    (s16vector-fill! v 2)
    (test-equiv "fill!" '(2 2 2) v))
  (let ((v (s16vector 1 2 3)))
    (display "fill2!\n")
    (s16vector-fill! v 10 0 2)
    (test-equiv "fill2!" '(10 10 3) v))
  (let ((v (s16vector 1 2 3)))
    (display "reverse!\n")
    (s16vector-reverse! v)
    (test-equiv "reverse!" '(3 2 1) v))
  (let ((v (s16vector 10 20 30 40 50)))
    (display "copy!\n")
    (s16vector-copy! v 1 s5 2 4)
    (test-equiv "copy!" '(10 3 4 40 50) v))
  (let ((v (s16vector 10 20 30 40 50)))
    (display "reverse-copy!\n")
    (s16vector-reverse-copy! v 1 s5 2 4)
    (test-equiv "reverse-copy!" '(10 4 3 40 50) v))
  (let ((v (s16vector 1 2 3 4 5 6 7 8)))
    (display "unfold!")
    (s16vector-unfold! (lambda (x) (values (* x 2) (* x 2)))
                       v 1 6 -1)
    (test-equiv "vector-unfold!" '(1 -2 -4 -8 -16 -32 7 8) v))
  (let ((v (s16vector 1 2 3 4 5 6 7 8)))
    (display "unfold-right!")
    (s16vector-unfold-right! (lambda (x) (values (* x 2) (* x 2)))
                             v 1 6 -1)
    (test-equiv "vector-unfold!" '(1 -32 -16 -8 -4 -2 7 8) v))
) ; end s16vector/mutators

(test-group "s16vector/conversion"
  (test "@vector->list 1" '(1 2 3 4 5)
        (s16vector->list s5))
  (test "@vector->list 2" '(2 3 4 5)
        (s16vector->list s5 1))
  (test "@vector->list 3" '(2 3 4)
        (s16vector->list s5 1 4))
  (test "@vector->vector 1" #(1 2 3 4 5)
        (s16vector->vector s5))
  (test "@vector->vector 2" #(2 3 4 5)
        (s16vector->vector s5 1))
  (test "@vector->vector 3" #(2 3 4)
        (s16vector->vector s5 1 4))
  (test-equiv "list->@vector" '(1 2 3 4 5)
              (list->s16vector '(1 2 3 4 5)))
  (test-equiv "reverse-list->@vector" '(5 4 3 2 1)
              (reverse-list->s16vector '(1 2 3 4 5)))
  (test-equiv "vector->@vector 1" '(1 2 3 4 5)
        (vector->s16vector #(1 2 3 4 5)))
  (test-equiv "vector->@vector 2" '(2 3 4 5)
        (vector->s16vector #(1 2 3 4 5) 1))
  (test-equiv "vector->@vector 3" '(2 3 4)
        (vector->s16vector #(1 2 3 4 5) 1 4))
) ; end s16vector/conversion

(test-group "s16vector/misc"
  (let ((port (open-output-string)))
    (write-s16vector s5 port)
    (test "write-@vector" "#s16(1 2 3 4 5)" (get-output-string port))
    (close-output-port port))

  (test-assert "@vector< short" (s16vector< s4 s5))
  (test-assert "not @vector< short" (not (s16vector< s5 s4)))
  (test-assert "@vector< samelen" (s16vector< s5 s5+))
  (test-assert "not @vector< samelen" (not (s16vector< s5+ s5+)))
  (test-assert "@vector=" (s16vector= s5+ s5+))
  (test "@vector-hash" 15 (s16vector-hash s5))

  (test "@vector-gen 0" 1 (g))
  (test "@vector-gen 1" 2 (g))
  (test "@vector-gen 2" 3 (g))
  (test "@vector-gen 3" 4 (g))
  (test "@vector-gen 4" 5 (g))
  (test-assert (eof-object? (g)))
) ; end s16vector/misc

) ; end s16vector
(test-exit)
