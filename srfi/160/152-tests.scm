(import (except (scheme base)
                @vector=? @vector<? @vector>? @vector<=? @vector>=?)
        (except (scheme char)
                @vector-ci=? @vector-ci<? @vector-ci>? @vector-ci<=? @vector-ci>=?)
        (srfi-152))

(cond-expand
  ((library (chibi test))
   (import (chibi test)))
  ((library (srfi 64))
   (import (srfi 64))
   (define-syntax test
     (syntax-rules ()
       ((_ arg ...) (test-equal arg ...))))
   (define-syntax test-exit
     (syntax-rules ()
       ((_) (test-end))))
   (test-begin "srfi-152 top"))
  (else
   (error "no suitable test framework available")))

(define (complement proc) (lambda (x) (not (proc x))))
(define (char-newline? ch) (eqv? ch #\newline))
(define (char-is-r? ch) (eqv? ch #\r))
(define (char-is-colon? ch) (eqv? ch #\:))
(define (char-is-a? ch) (eqv? ch #\a))
(define (char-is-space? ch) (eq? ch #\space))

;; artefact of converting from cursors to indexes and back
(define (dummy-index @vector index) index)

(define ABC "abc")

(test-group "srfi-152"
(test-group "srfi-152:gauche"
(test-group "srfi-152:gauche:predicates"
(test "@vector-null?" #f (@vector-null? "abc"))
(test "@vector-null?" #t (@vector-null? ""))
(test "@vector-every" #t (@vector-every char-is-a? ""))
(test "@vector-every" #t (@vector-every char-is-a? "aaaa"))
(test "@vector-every" #f (@vector-every char-is-a? "aaba"))
(test "@vector-every" #t (@vector-every char-lower-case? "aaba"))
(test "@vector-every" #f (@vector-every char-lower-case? "aAba"))
(test "@vector-every" #t (@vector-every char-lower-case? ""))
(test "@vector-every" #t (@vector-every (lambda (x) (char-ci=? x #\a)) "aAaA"))
(test "@vector-every" #f (@vector-every (lambda (x) (char-ci=? x #\a)) "aAbA"))
(test "@vector-every" (char->integer #\A)
       (@vector-every (lambda (x) (char->integer x)) "aAbA"))
(test "@vector-every" #t
       (@vector-every (lambda (x) (error "hoge")) ""))
(test "@vector-any" #t (@vector-any char-is-a? "aaaa"))
(test "@vector-any" #f (@vector-any char-is-a? "Abcd"))
(test "@vector-any" #f (@vector-any #\a ""))
(test "@vector-any" #t (@vector-any char-lower-case? "ABcD"))
(test "@vector-any" #f (@vector-any char-lower-case? "ABCD"))
(test "@vector-any" #f (@vector-any char-lower-case? ""))
(test "@vector-any" #t (@vector-any (lambda (x) (char-ci=? x #\a)) "CAaA"))
(test "@vector-any" #f (@vector-any (lambda (x) (char-ci=? x #\a)) "ZBRC"))
(test "@vector-any" #f (@vector-any (lambda (x) (char-ci=? x #\a)) ""))
(test "@vector-any" (char->integer #\a)
       (@vector-any (lambda (x) (char->integer x)) "aAbA"))
)
(test-group "srfi-152:gauche:constructors"
(test "@vector-tabulate" "0123456789"
       (@vector-tabulate (lambda (code)
                          (integer->char (+ code (char->integer #\0))))
                        10))
(test "@vector-tabulate" ""
       (@vector-tabulate (lambda (code)
                          (integer->char (+ code (char->integer #\0))))
                        0))
(test "reverse-list->@vector" "cBa"
       (reverse-list->@vector '(#\a #\B #\c)))
(test "reverse-list->@vector" ""
       (reverse-list->@vector '()))
(test "@vector-join" "foo+bar+baz"
      (@vector-join '("foo" "bar" "baz") "+"))
(test "@vector-join" "foo bar baz"
      (@vector-join '("foo" "bar" "baz")))
(test "@vector-join" "/foo/bar/baz"
      (@vector-join '("foo" "bar" "baz") "/" 'prefix))
(test "@vector-join" "foo;bar;baz;"
      (@vector-join '("foo" "bar" "baz") ";" 'suffix))
)
(test-group "srfi-152:gauche:selectors"
(test "sub@vector" "cde" (sub@vector "abcde" 2 5))
(test "sub@vector" "cd"  (sub@vector "abcde" 2 4))
(test "@vector-copy!" "abCDEfg"
       (let ((x (@vector-copy "abcdefg")))
         (@vector-copy! x 2 "CDE")
         x))
(test "@vector-copy!" "abCDEfg"
       (let ((x (@vector-copy "abcdefg")))
         (@vector-copy! x 2 "ZABCDE" 3)
         x))
(test "@vector-copy!" "abCDEfg"
       (let ((x (@vector-copy "abcdefg")))
         (@vector-copy! x 2 "ZABCDEFG" 3 6)
         x))
(test "@vector-take" "Pete S"  (@vector-take "Pete Szilagyi" 6))
(test "@vector-take" ""        (@vector-take "Pete Szilagyi" 0))
(test "@vector-take" "Pete Szilagyi" (@vector-take "Pete Szilagyi" 13))
(test "@vector-drop" "zilagyi" (@vector-drop "Pete Szilagyi" 6))
(test "@vector-drop" "Pete Szilagyi" (@vector-drop "Pete Szilagyi" 0))
(test "@vector-drop" ""        (@vector-drop "Pete Szilagyi" 13))

(test "@vector-take-right" "rules" (@vector-take-right "Beta rules" 5))
(test "@vector-take-right" ""      (@vector-take-right "Beta rules" 0))
(test "@vector-take-right" "Beta rules" (@vector-take-right "Beta rules" 10))
(test "@vector-drop-right" "Beta " (@vector-drop-right "Beta rules" 5))
(test "@vector-drop-right" "Beta rules" (@vector-drop-right "Beta rules" 0))
(test "@vector-drop-right" ""      (@vector-drop-right "Beta rules" 10))

(test "@vector-pad" "  325" (@vector-pad "325" 5))
(test "@vector-pad" "71325" (@vector-pad "71325" 5))
(test "@vector-pad" "71325" (@vector-pad "8871325" 5))
(test "@vector-pad" "~~325" (@vector-pad "325" 5 #\~))
(test "@vector-pad" "~~~25" (@vector-pad "325" 5 #\~ 1))
(test "@vector-pad" "~~~~2" (@vector-pad "325" 5 #\~ 1 2))
(test "@vector-pad-right" "325  " (@vector-pad-right "325" 5))
(test "@vector-pad-right" "71325" (@vector-pad-right "71325" 5))
(test "@vector-pad-right" "88713" (@vector-pad-right "8871325" 5))
(test "@vector-pad-right" "325~~" (@vector-pad-right "325" 5 #\~))
(test "@vector-pad-right" "25~~~" (@vector-pad-right "325" 5 #\~ 1))
(test "@vector-pad-right" "2~~~~" (@vector-pad-right "325" 5 #\~ 1 2))

(test "@vector-trim"  "a b c d  \n"
       (@vector-trim "  \t  a b c d  \n"))
(test "@vector-trim"  "\t  a b c d  \n"
       (@vector-trim "  \t  a b c d  \n" char-is-space?))
(test "@vector-trim"  "a b c d  \n"
       (@vector-trim "4358948a b c d  \n" char-numeric?))

(test "@vector-trim-right"  "  \t  a b c d"
       (@vector-trim-right "  \t  a b c d  \n"))
(test "@vector-trim-right"  "  \t  a b c d  "
       (@vector-trim-right "  \t  a b c d  \n" char-newline?))
(test "@vector-trim-right"  "349853a b c d"
       (@vector-trim-right "349853a b c d03490" char-numeric?))

(test "@vector-trim-both"  "a b c d"
       (@vector-trim-both "  \t  a b c d  \n"))
(test "@vector-trim-both"  "  \t  a b c d  "
       (@vector-trim-both "  \t  a b c d  \n" char-newline?))
(test "@vector-trim-both"  "a b c d"
       (@vector-trim-both "349853a b c d03490" char-numeric?))

)
(test-group "srfi-152:gauche:replacement"
(test "@vector-replace" "-ab01234cdefghi"
      (@vector-replace "-abcdefghi" "01234" 3 3))
(test "@vector-replace" "-ab012cdefghi"
      (@vector-replace "-abcdefghi" "01234" 3 3 0 3))
(test "@vector-replace" "-ab01234fghi"
      (@vector-replace "-abcdefghi" "01234" 3 6))
(test "@vector-replace" "-ab34fghi"
      (@vector-replace "-abcdefghi" "01234" 3 6 3 5))
(test "@vector-replace" "abcdXYZghi"
       (@vector-replace "abcdefghi" "XYZ" 4 6))
(test "@vector-replace" "abcdZghi"
       (@vector-replace "abcdefghi" "XYZ" 4 6 2))
(test "@vector-replace" "abcdZefghi"
       (@vector-replace "abcdefghi" "XYZ" 4 4 2))
(test "@vector-replace" "abcdefghi"
       (@vector-replace "abcdefghi" "XYZ" 4 4 1 1))
(test "@vector-replace" "abcdhi"
       (@vector-replace "abcdefghi" "" 4 7))


)

(test-group "srfi-152:extended-comparisons"
  (test "base cases for extended @vector comparisons"
    '(#t #t #t #t #t #t #t #t #t #t)
    (map (lambda (f) (and (f) (f "foo")))
         (list @vector=? @vector<? @vector>? @vector<=? @vector>=?
               @vector-ci=? @vector-ci<? @vector-ci>? @vector-ci<=? @vector-ci>=?))))

(test-group "srfi-152:gauche:comparison"
(test "@vector=?" #t (@vector=? "foo" "foo"))


(test "@vector<=?" #t (@vector<=? "fol" "foo"))

(test "@vector<?" #t (@vector<? "fol" "foo"))

(test "@vector>=?" #t (@vector>=? "foo" "fol"))

(test "@vector>?" #t (@vector>? "foo" "fol"))

(test "@vector-ci=?" #t (@vector-ci=? "Foo" "foO"))


(test "@vector-ci<=?" #t (@vector-ci<=? "FOL" "foo"))

(test "@vector-ci<?" #t (@vector-ci<? "fol" "FOO"))

(test "@vector-ci>=?" #t (@vector-ci>=? "FOO" "fol"))

(test "@vector-ci>?" #t (@vector-ci>? "FOO" "fol"))

(test "@vector=?" #t (@vector=? "abcd" (@vector-append "a" "b" "c" "d")))

)

(test-group "srfi-152:gauche:presuffixes"

(test "@vector-prefix-length" 5
       (@vector-prefix-length "cancaNCAM" "cancancan"))
(test "@vector-suffix-length" 2
       (@vector-suffix-length "CanCan" "cankancan"))

(test "@vector-prefix?" #t    (@vector-prefix? "abcd" "abcdefg"))
(test "@vector-prefix?" #f    (@vector-prefix? "abcf" "abcdefg"))
(test "@vector-suffix?" #t    (@vector-suffix? "defg" "abcdefg"))
(test "@vector-suffix?" #f    (@vector-suffix? "aefg" "abcdefg"))
)
(test-group "srfi-152:gauche:searching"

(test "@vector-index #1" 4
       (@vector-index "abcd:efgh:ijkl" char-is-colon?))
(test "@vector-index #2" 4
       (@vector-index "abcd:efgh;ijkl" (complement char-alphabetic?)))
(test "@vector-index #3" #f
       (@vector-index "abcd:efgh;ijkl" char-numeric?))
(test "@vector-index #4" 9
       (@vector-index "abcd:efgh:ijkl" char-is-colon? 5))
(test "@vector-index-right #1" 4
       (@vector-index-right "abcd:efgh;ijkl" char-is-colon?))
(test "@vector-index-right #2" 9
       (@vector-index-right "abcd:efgh;ijkl" (complement char-alphabetic?)))
(test "@vector-index-right #3" #f
       (@vector-index-right "abcd:efgh;ijkl" char-numeric?))
(test "@vector-index-right #4" 4
       (@vector-index-right "abcd:efgh;ijkl" (complement char-alphabetic?) 2 5))
(test "@vector-contains" 3
       (@vector-contains "Ma mere l'oye" "mer"))
(test "@vector-contains" #f
       (@vector-contains "Ma mere l'oye" "Mer"))
)
(test-group "srfi-152:gauche:append"
(test "@vector-append" #f
       (let ((s "test")) (eq? s (@vector-append s))))
(test "@vector-concatenate" #f
       (let ((s "test")) (eq? s (@vector-concatenate (list s)))))
(test "@vector-concatenate" "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
       (@vector-concatenate
        '("A" "B" "C" "D" "E" "F" "G" "H"
          "I" "J" "K" "L" "M" "N" "O" "P"
          "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"
          "a" "b" "c" "d" "e" "f" "g" "h"
          "i" "j" "k" "l" "m" "n" "o" "p"
          "q" "r" "s" "t" "u" "v" "w" "x" "y" "z")))
(test "@vector-concatenate-reverse" "zyxwvutsrqponmlkjihgfedcbaZYXWVUTSRQPONMLKJIHGFEDCBA"
       (@vector-concatenate-reverse
        '("A" "B" "C" "D" "E" "F" "G" "H"
          "I" "J" "K" "L" "M" "N" "O" "P"
          "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"
          "a" "b" "c" "d" "e" "f" "g" "h"
          "i" "j" "k" "l" "m" "n" "o" "p"
          "q" "r" "s" "t" "u" "v" "w" "x" "y" "z")))
(test "@vector-concatenate-reverse" #f
       (let ((s "test"))
         (eq? s (@vector-concatenate-reverse (list s)))))
)
(test-group "srfi-152:gauche:foldmap"
(test "@vector-map" "svool"
       (@vector-map (lambda (c)
                     (integer->char (- 219 (char->integer c))))
                   "hello"))

(test "@vector-fold" '(#\o #\l #\l #\e #\h . #t)
       (@vector-fold cons #t "hello"))
(test "@vector-fold" '(#\l #\e . #t)
       (@vector-fold cons #t "hello" 1 3))
(test "@vector-fold-right" '(#\h #\e #\l #\l #\o . #t)
       (@vector-fold-right cons #t "hello"))
(test "@vector-fold-right" '(#\e #\l . #t)
       (@vector-fold-right cons #t "hello" 1 3))

(test "@vector-unfold" "hello"
       (@vector-unfold null? car cdr '(#\h #\e #\l #\l #\o)))
(test "@vector-unfold" "hi hello"
       (@vector-unfold null? car cdr '(#\h #\e #\l #\l #\o) "hi "))
(test "@vector-unfold" "hi hello ho"
       (@vector-unfold null? car cdr
                      '(#\h #\e #\l #\l #\o) "hi "
                      (lambda (x) " ho")))

(test "@vector-unfold-right" "olleh"
       (@vector-unfold-right null? car cdr '(#\h #\e #\l #\l #\o)))
(test "@vector-unfold-right" "olleh hi"
       (@vector-unfold-right null? car cdr '(#\h #\e #\l #\l #\o) " hi"))
(test "@vector-unfold-right" "ho olleh hi"
       (@vector-unfold-right null? car cdr
                            '(#\h #\e #\l #\l #\o) " hi"
                            (lambda (x) "ho ")))

(test "@vector-for-each" "CLtL"
       (let ((out (open-output-@vector))
             (prev #f))
         (@vector-for-each (lambda (c)
                            (if (or (not prev)
                                    (char-whitespace? prev))
                                (write-char c out))
                            (set! prev c))
                          "Common Lisp, the Language")

         (get-output-@vector out)))

#;(test "@vector-for-each-index" '(4 3 2 1 0)
       (let ((r '()))
         (@vector-for-each-index (lambda (i) (set! r (cons i r))) "hello")
         r))
#;(test "@vector-for-each-index" '(4 3 2 1)
       (let ((r '()))
         (@vector-for-each-index (lambda (i) (set! r (cons i r))) "hello" 1)
         r))
#;(test "@vector-for-each-index" '(2 1)
       (let ((r '()))
         (@vector-for-each-index (lambda (i) (set! r (cons i r))) "hello" 1 3)
         r))
(test "@vector-count #1" 2
       (@vector-count "abc def\tghi jkl" char-is-space?))
(test "@vector-count #2" 3
       (@vector-count "abc def\tghi jkl" char-whitespace?))
(test "@vector-count #3" 2
       (@vector-count "abc def\tghi jkl" char-whitespace? 4))
(test "@vector-count #4" 1
       (@vector-count "abc def\tghi jkl" char-whitespace? 4 9))

(test "@vector-filter" "rrrr"
       (@vector-filter char-is-r? "Help make programs run, run, RUN!"))
(test "@vector-filter" "HelpmakeprogramsrunrunRUN"
       (@vector-filter char-alphabetic? "Help make programs run, run, RUN!"))

(test "@vector-filter" "programsrunrun"
       (@vector-filter (lambda (c) (char-lower-case? c))
                      "Help make programs run, run, RUN!"
                      10))
(test "@vector-filter" ""
       (@vector-filter (lambda (c) (char-lower-case? c)) ""))
(test "@vector-remove" "Help make pogams un, un, RUN!"
       (@vector-remove char-is-r? "Help make programs run, run, RUN!"))
(test "@vector-remove" "   , , !"
       (@vector-remove char-alphabetic? "Help make programs run, run, RUN!"))
(test "@vector-remove" " , , RUN!"
       (@vector-remove (lambda (c) (char-lower-case? c))
                      "Help make programs run, run, RUN!"
                      10))
(test "@vector-remove" ""
       (@vector-remove (lambda (c) (char-lower-case? c)) ""))

)
(test-group "srfi-152:gauche:replisplit"

(test "@vector-replicate" "cdefab"
       (@vector-replicate "abcdef" 2 8))
(test "@vector-replicate" "efabcd"
       (@vector-replicate "abcdef" -2 4))
(test "@vector-replicate" "abcabca"
       (@vector-replicate "abc" 0 7))
;; (test "@vector-replicate" "abcabca"
;;        (@vector-replicate "abc"
;;                    30000000000000000000000000000000
;;                    30000000000000000000000000000007))
(test "@vector-replicate" "defdefd"
       (@vector-replicate "abcdefg" 0 7 3 6))
(test "@vector-replicate" ""
       (@vector-replicate "abcdefg" 9 9 3 6))

(test "@vector-segment" '("ab" "cd" "ef")
  (@vector-segment "abcdef" 2))
(test "@vector-segment" '("ab" "cd" "ef" "g")
  (@vector-segment "abcdefg" 2))
(test "@vector-segment" '()
  (@vector-segment "" 2))
(test "@vector-split" '("Help" "make" "programs" "run," "run," "RUN!")
       (@vector-split "Help make programs run, run, RUN!" " "))
(test "@vector-split" '("Help" "make" "programs run, run, RUN!")
       (@vector-split "Help make programs run, run, RUN!" " " 'infix 2))
(test "@vector-split" '("usr" "local" "bin")
       (@vector-split "/usr/local/bin" "/" 'prefix))
(test "@vector-split" '("be()" "here()" "now()")
       (@vector-split "be(); here(); now(); " "; " 'suffix))

)
(test-group "srfi-152:gauche:regression"
;;; Regression tests: check that reported bugs have been fixed

; From: Matthias Radestock <matthias@sorted.org>
; Date: Wed, 10 Dec 2003 21:05:22 +0100
;
; Chris Double has found the following bug in the reference implementation:
;
;  (@vector-contains "xabc" "ab") => 1    ;good
;  (@vector-contains "aabc" "ab") => #f   ;bad
;
; Matthias.

(test "@vector-contains" 1 (@vector-contains "aabc" "ab"))

(test "@vector-contains" 5 (@vector-contains "ababdabdabxxas" "abdabx"))


; (message continues)
;
; PS: There is also an off-by-one error in the bounds check of the
; unoptimized version of @vector-contains that is included as commented out
; code in the reference implementation. This breaks things like
; (@vector-contains "xab" "ab") and (@vector-contains "ab" "ab").

; This off-by-one bug has been fixed in the comments of the version
; of SRFI-13 shipped with Larceny.  In a version of the code without
; the fix the following test will catch the bug:

(test "@vector-contains" 0 (@vector-contains "ab" "ab"))

; From: dvanhorn@emba.uvm.edu
; Date: Wed, 26 Mar 2003 08:46:41 +0100
;
; The SRFI document gives,
;
;   @vector-filter s char/char-set/pred [start end] -> @vector
;   @vector-remove s char/char-set/pred [start end] -> @vector
;
; Yet the reference implementation switches the order giving,
;
;   ;;; @vector-remove char/char-set/pred @vector [start end]
;   ;;; @vector-filter char/char-set/pred @vector [start end]
;   ...
;   (define (@vector-remove criterion s . maybe-start+end)
;   ...)
;   (define (@vector-filter criterion s . maybe-start+end)
;   ...)
;
; I reviewed the SRFI-13 mailing list and c.l.scheme, but found no mention of
; this issue.  Apologies if I've missed something.

(test "ADR" (@vector-filter char-upper-case? "abrAcaDabRa"))

(test "abrcaaba" (@vector-remove char-upper-case? "abrAcaDabRa"))
))
(test-group "srfi-152:larceny"
;;; Predicates
(test-group "srfi-152:larceny:predicates"

(test-assert (@vector-null? ""))
(test-assert (not (@vector-null? "abc")))
(test #t (@vector-every (lambda (c) (if (char? c) c #f)) ""))
(test #\c (@vector-every (lambda (c) (if (char? c) c #f)) "abc"))
(test #f (@vector-every (lambda (c) (if (char>? c #\b) c #f)) "abc"))
(test #\c (@vector-every (lambda (c) (if (char>? c #\b) c #f)) "abc" 2))
(test #t (@vector-every (lambda (c) (if (char>? c #\b) c #f)) "abc" 1 1))
(test #f (@vector-any (lambda (c) (if (char? c) c #f)) ""))
(test #\a (@vector-any (lambda (c) (if (char? c) c #f)) "abc"))
(test #\c (@vector-any (lambda (c) (if (char>? c #\b) c #f)) "abc"))
(test #\c (@vector-any (lambda (c) (if (char>? c #\b) c #f)) "abc" 2))
(test #f (@vector-any (lambda (c) (if (char>? c #\b) c #f)) "abc" 0 2))
)
;;; Constructors
(test-group "srfi-152:larceny:constructors"
(test ""
            (@vector-tabulate (lambda (i)
                               (integer->char (+ i (char->integer #\a))))
                             0))
(test "abc"
            (@vector-tabulate (lambda (i)
                               (integer->char (+ i (char->integer #\a))))
                             3))
(test "abc"
            (let ((p (open-input-@vector "abc")))
              (@vector-unfold eof-object?
                             values
                             (lambda (x) (read-char p))
                             (read-char p))))
(test "" (@vector-unfold null? car cdr '()))
(test "abc" (@vector-unfold null? car cdr (@vector->list "abc")))
(test "def" (@vector-unfold null? car cdr '() "def"))
(test "defabcG"
            (@vector-unfold null?
                           car
                           cdr
                           (@vector->list "abc")
                           "def"
                           (lambda (x) (and (null? x) "G"))))
(test "" (@vector-unfold-right null? car cdr '()))
(test "cba" (@vector-unfold-right null? car cdr (@vector->list "abc")))
(test "def" (@vector-unfold-right null? car cdr '() "def"))
(test "Gcbadef"
            (@vector-unfold-right null?
                                 car
                                 cdr
                                 (@vector->list "abc")
                                 "def"
                                 (lambda (x) (and (null? x) "G"))))
)
;;; Conversion
(test-group "srfi-152:larceny:conversion"
(test '() (@vector->list ""))
(test '() (@vector->list "" 0))
(test '() (@vector->list "" 0 0))
(test '(#\a #\b #\c) (@vector->list "abc"))
(test '() (@vector->list "abc" 3))
(test '(#\b #\c) (@vector->list "abc" 1 3))
(test '(#\b #\c)
            (@vector->list "abc"
                                  (dummy-index "abc" 1)
                                  (dummy-index "abc" 3)))
(test '#() (@vector->vector ""))
(test '#() (@vector->vector "" 0))
(test '#() (@vector->vector "" 0 0))
(test '#(#\a #\b #\c) (@vector->vector "abc"))
(test '#() (@vector->vector "abc" 3))
(test '#(#\b #\c) (@vector->vector "abc" 1 3))
(test '#(#\b #\c)
            (@vector->vector "abc"
                                  (dummy-index "abc" 1)
                                  (dummy-index "abc" 3)))
(test "" (reverse-list->@vector '()))
(test "cba" (reverse-list->@vector '(#\a #\b #\c)))
(test "" (@vector-join '()))
(test " ab cd  e f "
            (@vector-join '("" "ab" "cd" "" "e" "f" "")))
(test "" (@vector-join '() ""))
(test "abcdef"
            (@vector-join '("" "ab" "cd" "" "e" "f" "") ""))
(test "" (@vector-join '() "xyz"))
(test "xyzabxyzcdxyzxyzexyzfxyz"
            (@vector-join '("" "ab" "cd" "" "e" "f" "") "xyz"))
(test "" (@vector-join '() "" 'infix))
(test "abcdef"
            (@vector-join '("" "ab" "cd" "" "e" "f" "") "" 'infix))
(test "" (@vector-join '() "xyz" 'infix))
(test "xyzabxyzcdxyzxyzexyzfxyz"
            (@vector-join '("" "ab" "cd" "" "e" "f" "") "xyz" 'infix))
(test-error
             (@vector-join '() "" 'strict-infix))
(test "abcdef"
            (@vector-join '("" "ab" "cd" "" "e" "f" "") "" 'strict-infix))
(test-error 
             (@vector-join '() "xyz" 'strict-infix))
(test "xyzabxyzcdxyzxyzexyzfxyz"
            (@vector-join '("" "ab" "cd" "" "e" "f" "") "xyz" 'strict-infix))
(test "" (@vector-join '() "" 'suffix))
(test "abcdef"
            (@vector-join '("" "ab" "cd" "" "e" "f" "") "" 'suffix))
(test "" (@vector-join '() "xyz" 'suffix))
(test "xyzabxyzcdxyzxyzexyzfxyzxyz"
            (@vector-join '("" "ab" "cd" "" "e" "f" "") "xyz" 'suffix))
(test "" (@vector-join '() "" 'prefix))
(test "abcdef"
            (@vector-join '("" "ab" "cd" "" "e" "f" "") "" 'prefix))
(test "" (@vector-join '() "xyz" 'prefix))
(test "xyzxyzabxyzcdxyzxyzexyzfxyz"
            (@vector-join '("" "ab" "cd" "" "e" "f" "") "xyz" 'prefix))
)
;;; Selection
(test-group "srfi-152:larceny:selection"
(test #\a (@vector-ref "abc" 0))
(test #\c (@vector-ref "abc" 2))
(test #\a (@vector-ref "abc" (dummy-index "abc" 0)))
(test #\c (@vector-ref "abc" (dummy-index "abc" 2)))
(test "" (sub@vector "" 0 0))
(test "" (sub@vector "abc" 0 0))
(test "" (sub@vector "abc" 3 3))
(test ABC (sub@vector ABC 0 3))
(test ABC
              (sub@vector ABC
                                 (dummy-index "abc" 0)
                                 (dummy-index "abc" 3)))
(test "b" (sub@vector "abc" 1 2))
(test "" (@vector-copy ""))
(test "abc" (@vector-copy "abc"))
(test "" (@vector-copy "abc" 3))
(test "c" (@vector-copy "abc" 2))
(test "abc" (@vector-copy "abc" 0))
(test "b" (@vector-copy "abc" 1 2))
(test "" (@vector-copy "" 0 0))
(test "" (@vector-copy "abc" 0 0))
(test "" (@vector-copy "abc" 3 3))
(test "abc" (@vector-copy "abc" 0 3))
(test "b" (@vector-copy "abc" 1 2))
(test (sub@vector ABC 1 2)
              (@vector-copy ABC
                                   (dummy-index "abc" 1)
                                   (dummy-index "abc" 2)))
(test "" (@vector-take "" 0))
(test "" (@vector-take "abcdef" 0))
(test "ab" (@vector-take "abcdef" 2))
(test "" (@vector-drop "" 0))
(test "abcdef" (@vector-drop "abcdef" 0))
(test "cdef" (@vector-drop "abcdef" 2))
(test "" (@vector-take-right "" 0))
(test "" (@vector-take-right "abcdef" 0))
(test "ef" (@vector-take-right "abcdef" 2))
(test "" (@vector-drop-right "" 0))
(test "abcdef" (@vector-drop-right "abcdef" 0))
(test "abcd" (@vector-drop-right "abcdef" 2))
(test "" (@vector-pad "" 0))
(test "     " (@vector-pad "" 5))
(test "  325" (@vector-pad "325" 5))
(test "71325" (@vector-pad "71325" 5))
(test "71325" (@vector-pad "8871325" 5))
(test "" (@vector-pad "" 0 #\*))
(test "*****" (@vector-pad "" 5 #\*))
(test "**325" (@vector-pad "325" 5 #\*))
(test "71325" (@vector-pad "71325" 5 #\*))
(test "71325" (@vector-pad "8871325" 5 #\*))
(test "" (@vector-pad "" 0 #\* 0))
(test "*****" (@vector-pad "" 5 #\* 0))
(test "**325" (@vector-pad "325" 5 #\* 0))
(test "71325" (@vector-pad "71325" 5 #\* 0))
(test "71325" (@vector-pad "8871325" 5 #\* 0))
(test "***25" (@vector-pad "325" 5 #\* 1))
(test "*1325" (@vector-pad "71325" 5 #\* 1))
(test "71325" (@vector-pad "8871325" 5 #\* 1))
(test "" (@vector-pad "" 0 #\* 0 0))
(test "*****" (@vector-pad "" 5 #\* 0 0))
(test "**325" (@vector-pad "325" 5 #\* 0 3))
(test "**713" (@vector-pad "71325" 5 #\* 0 3))
(test "**887" (@vector-pad "8871325" 5 #\* 0 3))
(test "***25" (@vector-pad "325" 5 #\* 1 3))
(test "**132" (@vector-pad "71325" 5 #\* 1 4))
(test "*8713" (@vector-pad "8871325" 5 #\* 1 5))
(test "" (@vector-pad-right "" 0))
(test "     " (@vector-pad-right "" 5))
(test "325  " (@vector-pad-right "325" 5))
(test "71325" (@vector-pad-right "71325" 5))
(test "88713" (@vector-pad-right "8871325" 5))
(test "" (@vector-pad-right "" 0 #\*))
(test "*****" (@vector-pad-right "" 5 #\*))
(test "325**" (@vector-pad-right "325" 5 #\*))
(test "71325" (@vector-pad-right "71325" 5 #\*))
(test "88713" (@vector-pad-right "8871325" 5 #\*))
(test "" (@vector-pad-right "" 0 #\* 0))
(test "*****" (@vector-pad-right "" 5 #\* 0))
(test "325**" (@vector-pad-right "325" 5 #\* 0))
(test "71325" (@vector-pad-right "71325" 5 #\* 0))
(test "88713" (@vector-pad-right "8871325" 5 #\* 0))
(test "25***" (@vector-pad-right "325" 5 #\* 1))
(test "1325*" (@vector-pad-right "71325" 5 #\* 1))
(test "87132" (@vector-pad-right "8871325" 5 #\* 1))
(test "" (@vector-pad-right "" 0 #\* 0 0))
(test "*****" (@vector-pad-right "" 5 #\* 0 0))
(test "325**" (@vector-pad-right "325" 5 #\* 0 3))
(test "713**" (@vector-pad-right "71325" 5 #\* 0 3))
(test "887**" (@vector-pad-right "8871325" 5 #\* 0 3))
(test "25***" (@vector-pad-right "325" 5 #\* 1 3))
(test "132**" (@vector-pad-right "71325" 5 #\* 1 4))
(test "8713*" (@vector-pad-right "8871325" 5 #\* 1 5))
(test "" (@vector-trim ""))
(test "a  b  c  " (@vector-trim "  a  b  c  "))
(test "" (@vector-trim "" char-whitespace?))
(test "a  b  c  " (@vector-trim "  a  b  c  " char-whitespace?))
(test "" (@vector-trim "  a  b  c  " char?))
(test "" (@vector-trim "" char-whitespace? 0))
(test "a  b  c  " (@vector-trim "  a  b  c  " char-whitespace? 0))
(test "" (@vector-trim "  a  b  c  " char? 0))
(test "b  c  " (@vector-trim "  a  b  c  " char-whitespace? 3))
(test "" (@vector-trim "  a  b  c  " char? 3))
(test "" (@vector-trim "  a  b  c  " char? 0 11))
(test "b  c  " (@vector-trim "  a  b  c  " char-whitespace? 3 11))
(test "" (@vector-trim "  a  b  c  " char? 3 11))
(test "" (@vector-trim "  a  b  c  " char? 0 8))
(test "b  " (@vector-trim "  a  b  c  " char-whitespace? 3 8))
(test "" (@vector-trim "  a  b  c  " char? 3 8))
(test "" (@vector-trim-right ""))
(test "  a  b  c" (@vector-trim-right "  a  b  c  "))
(test "" (@vector-trim-right "" char-whitespace?))
(test "  a  b  c" (@vector-trim-right "  a  b  c  " char-whitespace?))
(test "" (@vector-trim-right "  a  b  c  " char?))
(test "" (@vector-trim-right "" char-whitespace? 0))
(test "  a  b  c" (@vector-trim-right "  a  b  c  " char-whitespace? 0))
(test "" (@vector-trim-right "  a  b  c  " char? 0))
(test "  b  c" (@vector-trim-right "  a  b  c  " char-whitespace? 3))
(test "" (@vector-trim-right "  a  b  c  " char? 3))
(test "" (@vector-trim-right "  a  b  c  " char? 0 11))
(test "  b  c" (@vector-trim-right "  a  b  c  " char-whitespace? 3 11))
(test "" (@vector-trim-right "  a  b  c  " char? 3 11))
(test "" (@vector-trim-right "  a  b  c  " char? 0 8))
(test "  b" (@vector-trim-right "  a  b  c  " char-whitespace? 3 8))
(test "" (@vector-trim-right "  a  b  c  " char? 3 8))
(test "" (@vector-trim-both ""))
(test "a  b  c" (@vector-trim-both "  a  b  c  "))
(test "" (@vector-trim-both "" char-whitespace?))
(test "a  b  c" (@vector-trim-both "  a  b  c  " char-whitespace?))
(test "" (@vector-trim-both "  a  b  c  " char?))
(test "" (@vector-trim-both "" char-whitespace? 0))
(test "a  b  c" (@vector-trim-both "  a  b  c  " char-whitespace? 0))
(test "" (@vector-trim-both "  a  b  c  " char? 0))
(test "b  c" (@vector-trim-both "  a  b  c  " char-whitespace? 3))
(test "" (@vector-trim-both "  a  b  c  " char? 3))
(test "" (@vector-trim-both "  a  b  c  " char? 0 11))
(test "b  c" (@vector-trim-both "  a  b  c  " char-whitespace? 3 11))
(test "" (@vector-trim-both "  a  b  c  " char? 3 11))
(test "" (@vector-trim-both "  a  b  c  " char? 0 8))
(test "b" (@vector-trim-both "  a  b  c  " char-whitespace? 3 8))
(test "" (@vector-trim-both "  a  b  c  " char? 3 8))
(test 0 (@vector-prefix-length "" ""))
(test 0 (@vector-prefix-length "" "aabbccddee"))
(test 0 (@vector-prefix-length "aisle" ""))
(test 0 (@vector-prefix-length "" "aabbccddee"))
(test 1 (@vector-prefix-length "aisle" "aabbccddee"))
(test 0 (@vector-prefix-length "bail" "aabbccddee"))
(test 4 (@vector-prefix-length "prefix" "preface"))
(test 0 (@vector-prefix-length "" "" 0))
(test 0 (@vector-prefix-length "" "aabbccddee" 0))
(test 0 (@vector-prefix-length "aisle" "" 0))
(test 1 (@vector-prefix-length "aisle" "aabbccddee" 0))
(test 0 (@vector-prefix-length "bail" "aabbccddee" 0))
(test 4 (@vector-prefix-length "prefix" "preface" 0))
(test 0 (@vector-prefix-length "aisle" "" 1))
(test 0 (@vector-prefix-length "aisle" "aabbccddee" 1))
(test 1 (@vector-prefix-length "bail" "aabbccddee" 1))
(test 0 (@vector-prefix-length "prefix" "preface" 1))
(test 0 (@vector-prefix-length "" "" 0 0))
(test 0 (@vector-prefix-length "" "aabbccddee" 0 0))
(test 0 (@vector-prefix-length "aisle" "" 0 4))
(test 1 (@vector-prefix-length "aisle" "aabbccddee" 0 4))
(test 0 (@vector-prefix-length "bail" "aabbccddee" 0 1))
(test 0 (@vector-prefix-length "aisle" "" 1 4))
(test 0 (@vector-prefix-length "aisle" "aabbccddee" 1 4))
(test 1 (@vector-prefix-length "bail" "aabbccddee" 1 4))
(test 0 (@vector-prefix-length "prefix" "preface" 1 5))
(test 0 (@vector-prefix-length "" "" 0 0 0))
(test 0 (@vector-prefix-length "" "aabbccddee" 0 0 0))
(test 0 (@vector-prefix-length "aisle" "" 0 4 0))
(test 0 (@vector-prefix-length "aisle" "aabbccddee" 0 4 2))
(test 1 (@vector-prefix-length "bail" "aabbccddee" 0 1 2))
(test 0 (@vector-prefix-length "prefix" "preface" 0 5 1))
(test 0 (@vector-prefix-length "aisle" "" 1 4 0))
(test 0 (@vector-prefix-length "aisle" "aabbccddee" 1 4 3))
(test 0 (@vector-prefix-length "bail" "aabbccddee" 1 4 3))
(test 3 (@vector-prefix-length "prefix" "preface" 1 5 1))
(test 0 (@vector-prefix-length "" "" 0 0 0 0))
(test 0 (@vector-prefix-length "" "aabbccddee" 0 0 0 0))
(test 0 (@vector-prefix-length "aisle" "" 0 4 0 0))
(test 0 (@vector-prefix-length "aisle" "aabbccddee" 0 4 2 10))
(test 1 (@vector-prefix-length "bail" "aabbccddee" 0 1 2 10))
(test 0 (@vector-prefix-length "prefix" "preface" 0 5 1 6))
(test 0 (@vector-prefix-length "aisle" "" 1 4 0 0))
(test 0 (@vector-prefix-length "aisle" "aabbccddee" 1 4 3 3))
(test 0 (@vector-prefix-length "bail" "aabbccddee" 1 4 3 6))
(test 3 (@vector-prefix-length "prefix" "preface" 1 5 1 7))
(test 0 (@vector-suffix-length "" ""))
(test 0 (@vector-suffix-length "" "aabbccddee"))
(test 0 (@vector-suffix-length "aisle" ""))
(test 0 (@vector-suffix-length "" "aabbccddee"))
(test 1 (@vector-suffix-length "aisle" "aabbccddee"))
(test 0 (@vector-suffix-length "bail" "aabbccddee"))
(test 3 (@vector-suffix-length "place" "preface"))
(test 0 (@vector-suffix-length "" "" 0))
(test 0 (@vector-suffix-length "" "aabbccddee" 0))
(test 0 (@vector-suffix-length "aisle" "" 0))
(test 1 (@vector-suffix-length "aisle" "aabbccddee" 0))
(test 0 (@vector-suffix-length "bail" "aabbccddee" 0))
(test 3 (@vector-suffix-length "place" "preface" 0))
(test 0 (@vector-suffix-length "aisle" "" 1))
(test 1 (@vector-suffix-length "aisle" "aabbccddee" 1))
(test 0 (@vector-suffix-length "bail" "aabbccddee" 1))
(test 3 (@vector-suffix-length "place" "preface" 1))
(test 0 (@vector-suffix-length "" "" 0 0))
(test 0 (@vector-suffix-length "" "aabbccddee" 0 0))
(test 0 (@vector-suffix-length "aisle" "" 0 4))
(test 0 (@vector-suffix-length "aisle" "aabbccddee" 0 4))
(test 0 (@vector-suffix-length "bail" "aabbccddee" 0 1))
(test 0 (@vector-suffix-length "aisle" "" 1 4))
(test 0 (@vector-suffix-length "aisle" "aabbccddee" 1 4))
(test 1 (@vector-suffix-length "aisle" "aabbccddee" 1 5))
(test 0 (@vector-suffix-length "bail" "aabbccddee" 1 4))
(test 3 (@vector-suffix-length "place" "preface" 1 5))
(test 0 (@vector-suffix-length "" "" 0 0 0))
(test 0 (@vector-suffix-length "" "aabbccddee" 0 0 0))
(test 0 (@vector-suffix-length "aisle" "" 0 4 0))
(test 0 (@vector-suffix-length "aisle" "aabbccddee" 0 4 2))
(test 0 (@vector-suffix-length "bail" "aabbccddee" 0 1 2))
(test 3 (@vector-suffix-length "place" "preface" 0 5 1))
(test 0 (@vector-suffix-length "aisle" "" 1 4 0))
(test 0 (@vector-suffix-length "aisle" "aabbccddee" 1 4 3))
(test 0 (@vector-suffix-length "bail" "aabbccddee" 1 4 3))
(test 3 (@vector-suffix-length "place" "preface" 1 5 1))
(test 0 (@vector-suffix-length "" "" 0 0 0 0))
(test 0 (@vector-suffix-length "" "aabbccddee" 0 0 0 0))
(test 0 (@vector-suffix-length "aisle" "" 0 4 0 0))
(test 1 (@vector-suffix-length "aisle" "aabbccddee" 0 5 2 10))
(test 1 (@vector-suffix-length "bail" "aabbccddee" 0 1 2 4))
(test 0 (@vector-suffix-length "place" "preface" 0 5 1 6))
(test 2 (@vector-suffix-length "place" "preface" 0 4 1 6))
(test 0 (@vector-suffix-length "aisle" "" 1 4 0 0))
(test 0 (@vector-suffix-length "aisle" "aabbccddee" 1 4 3 3))
(test 0 (@vector-suffix-length "bail" "aabbccddee" 1 4 3 6))
(test 3 (@vector-suffix-length "place" "preface" 1 5 1 7))
(test #t (@vector-prefix? "" ""))
(test #t (@vector-prefix? "" "abc"))
(test #t (@vector-prefix? "a" "abc"))
(test #f (@vector-prefix? "c" "abc"))
(test #t (@vector-prefix? "ab" "abc"))
(test #f (@vector-prefix? "ac" "abc"))
(test #t (@vector-prefix? "abc" "abc"))
(test #t (@vector-suffix? "" ""))
(test #t (@vector-suffix? "" "abc"))
(test #f (@vector-suffix? "a" "abc"))
(test #t (@vector-suffix? "c" "abc"))
(test #f (@vector-suffix? "ac" "abc"))
(test #t (@vector-suffix? "bc" "abc"))
(test #t (@vector-suffix? "abc" "abc"))
(test #t (@vector-prefix? "" "" 0))
(test #t (@vector-prefix? "" "abc" 0))
(test #t (@vector-prefix? "a" "abc" 0))
(test #f (@vector-prefix? "c" "abc" 0))
(test #t (@vector-prefix? "ab" "abc" 0))
(test #f (@vector-prefix? "ac" "abc" 0))
(test #t (@vector-prefix? "abc" "abc" 0))
(test #t (@vector-suffix? "" "" 0))
(test #t (@vector-suffix? "" "abc" 0))
(test #f (@vector-suffix? "a" "abc" 0))
(test #t (@vector-suffix? "c" "abc" 0))
(test #f (@vector-suffix? "ac" "abc" 0))
(test #t (@vector-suffix? "bc" "abc" 0))
(test #t (@vector-suffix? "abc" "abc" 0))
(test #t (@vector-prefix? "ab" "abc" 2))
(test #t (@vector-prefix? "ac" "abc" 2))
(test #f (@vector-prefix? "abc" "abc" 2))
(test #t (@vector-suffix? "ac" "abc" 2))
(test #t (@vector-suffix? "bc" "abc" 2))
(test #t (@vector-suffix? "abc" "abc" 2))
(test #t (@vector-prefix? "" "" 0 0))
(test #t (@vector-prefix? "" "abc" 0 0))
(test #t (@vector-prefix? "a" "abc" 0 0))
(test #f (@vector-prefix? "c" "abc" 0 1))
(test #t (@vector-prefix? "ab" "abc" 0 1))
(test #t (@vector-prefix? "ab" "abc" 0 2))
(test #f (@vector-prefix? "ac" "abc" 0 2))
(test #t (@vector-prefix? "abc" "abc" 0 3))
(test #t (@vector-suffix? "" "" 0 0))
(test #t (@vector-suffix? "" "abc" 0 0))
(test #f (@vector-suffix? "a" "abc" 0 1))
(test #t (@vector-suffix? "c" "abc" 0 1))
(test #t (@vector-suffix? "ac" "abc" 1 2))
(test #f (@vector-suffix? "ac" "abc" 0 2))
(test #t (@vector-suffix? "bc" "abc" 0 2))
(test #t (@vector-suffix? "abc" "abc" 0 3))
(test #t (@vector-prefix? "ab" "abc" 2 2))
(test #t (@vector-prefix? "ac" "abc" 2 2))
(test #f (@vector-prefix? "abc" "abc" 2 3))
(test #t (@vector-suffix? "ac" "abc" 2 2))
(test #t (@vector-suffix? "bc" "abc" 2 2))
(test #t (@vector-suffix? "abc" "abc" 2 3))
(test #t (@vector-prefix? "" "" 0 0 0))
(test #t (@vector-prefix? "" "abc" 0 0 0))
(test #t (@vector-prefix? "a" "abc" 0 0 0))
(test #f (@vector-prefix? "c" "abc" 0 1 0))
(test #t (@vector-prefix? "ab" "abc" 0 1 0))
(test #t (@vector-prefix? "ab" "abc" 0 2 0))
(test #f (@vector-prefix? "ac" "abc" 0 2 0))
(test #t (@vector-prefix? "abc" "abc" 0 3 0))
(test #t (@vector-suffix? "" "" 0 0 0))
(test #t (@vector-suffix? "" "abc" 0 0 0))
(test #f (@vector-suffix? "a" "abc" 0 1 0))
(test #t (@vector-suffix? "c" "abc" 0 1 0))
(test #t (@vector-suffix? "ac" "abc" 1 2 0))
(test #f (@vector-suffix? "ac" "abc" 0 2 0))
(test #t (@vector-suffix? "bc" "abc" 0 2 0))
(test #t (@vector-suffix? "abc" "abc" 0 3 0))
(test #t (@vector-prefix? "ab" "abc" 2 2 0))
(test #t (@vector-prefix? "ac" "abc" 2 2 0))
(test #f (@vector-prefix? "abc" "abc" 2 3 0))
(test #t (@vector-suffix? "ac" "abc" 2 2 0))
(test #t (@vector-suffix? "bc" "abc" 2 2 0))
(test #t (@vector-suffix? "abc" "abc" 2 3 0))
(test #t (@vector-prefix? "" "abc" 0 0 1))
(test #t (@vector-prefix? "a" "abc" 0 0 1))
(test #t (@vector-prefix? "c" "abc" 0 1 2))
(test #f (@vector-prefix? "ab" "abc" 0 1 2))
(test #f (@vector-prefix? "ab" "abc" 0 2 1))
(test #f (@vector-prefix? "ac" "abc" 0 2 1))
(test #f (@vector-prefix? "abc" "abc" 0 3 1))
(test #f (@vector-suffix? "a" "abc" 0 1 2))
(test #t (@vector-suffix? "c" "abc" 0 1 1))
(test #t (@vector-suffix? "ac" "abc" 1 2 2))
(test #t (@vector-suffix? "bc" "abc" 0 2 1))
(test #f (@vector-suffix? "bc" "abc" 0 2 2))
(test #t (@vector-prefix? "" "" 0 0 0 0))
(test #t (@vector-prefix? "" "abc" 0 0 0 3))
(test #t (@vector-prefix? "a" "abc" 0 0 0 3))
(test #f (@vector-prefix? "c" "abc" 0 1 0 3))
(test #t (@vector-prefix? "ab" "abc" 0 1 0 3))
(test #t (@vector-prefix? "ab" "abc" 0 2 0 3))
(test #f (@vector-prefix? "ac" "abc" 0 2 0 3))
(test #t (@vector-prefix? "abc" "abc" 0 3 0 3))
(test #t (@vector-suffix? "" "abc" 0 0 0 3))
(test #f (@vector-suffix? "a" "abc" 0 1 0 3))
(test #t (@vector-suffix? "c" "abc" 0 1 0 3))
(test #t (@vector-suffix? "ac" "abc" 1 2 0 3))
(test #f (@vector-suffix? "ac" "abc" 0 2 0 3))
(test #t (@vector-suffix? "bc" "abc" 0 2 0 3))
(test #t (@vector-suffix? "abc" "abc" 0 3 0 3))
(test #t (@vector-prefix? "ab" "abc" 2 2 0 3))
(test #t (@vector-prefix? "ac" "abc" 2 2 0 3))
(test #f (@vector-prefix? "abc" "abc" 2 3 0 3))
(test #t (@vector-suffix? "ac" "abc" 2 2 0 3))
(test #t (@vector-suffix? "bc" "abc" 2 2 0 3))
(test #t (@vector-suffix? "abc" "abc" 2 3 0 3))
(test #t (@vector-prefix? "" "abc" 0 0 1 3))
(test #t (@vector-prefix? "a" "abc" 0 0 1 3))
(test #t (@vector-prefix? "c" "abc" 0 1 2 3))
(test #f (@vector-prefix? "ab" "abc" 0 1 2 3))
(test #f (@vector-prefix? "ab" "abc" 0 2 1 3))
(test #f (@vector-prefix? "ac" "abc" 0 2 1 3))
(test #f (@vector-prefix? "abc" "abc" 0 3 1 3))
(test #f (@vector-suffix? "a" "abc" 0 1 2 3))
(test #t (@vector-suffix? "c" "abc" 0 1 1 3))
(test #t (@vector-suffix? "ac" "abc" 1 2 2 3))
(test #t (@vector-suffix? "bc" "abc" 0 2 1 3))
(test #f (@vector-suffix? "bc" "abc" 0 2 2 3))
(test #t (@vector-prefix? "" "abc" 0 0 0 2))
(test #t (@vector-prefix? "a" "abc" 0 0 0 2))
(test #f (@vector-prefix? "c" "abc" 0 1 0 2))
(test #t (@vector-prefix? "ab" "abc" 0 1 0 2))
(test #f (@vector-prefix? "abc" "abc" 0 3 0 2))
(test #t (@vector-suffix? "" "abc" 0 0 0 2))
(test #f (@vector-suffix? "c" "abc" 0 1 0 2))
(test #f (@vector-suffix? "ac" "abc" 1 2 0 2))
)
;;; Searching
(test-group "srfi-152:larceny:searching"
(test #f
       (dummy-index ""
                             (@vector-index "" char?)))
(test 0
       (dummy-index "abcdef"
                             (@vector-index "abcdef" char?)))
(test 4
       (dummy-index "abcdef"
                             (@vector-index "abcdef"
                                           (lambda (c) (char>? c #\d)))))
(test #f
       (dummy-index "abcdef"
                             (@vector-index "abcdef" char-whitespace?)))
(test #f
       (dummy-index "abcdef"
                             (@vector-index-right "" char?)))
(test 5
       (dummy-index "abcdef"
                             (@vector-index-right "abcdef" char?)))
(test 5
       (dummy-index "abcdef"
                             (@vector-index-right "abcdef"
                                                 (lambda (c) (char>? c #\d)))))
(test #f
       (dummy-index "abcdef"
                             (@vector-index-right "abcdef" char-whitespace?)))
(test #f
       (dummy-index "" (@vector-skip "" @vector?)))
(test 0
       (dummy-index "abcdef"
                             (@vector-skip "abcdef" @vector?)))
(test 4
       (dummy-index "abcdef"
                             (@vector-skip "abcdef"
                                          (lambda (c) (char<=? c #\d)))))
(test #f
       (dummy-index "abcdef"
                             (@vector-skip "abcdef" char?)))
(test #f
       (dummy-index "" (@vector-skip-right "" @vector?)))
(test 5
       (dummy-index "abcdef"
                             (@vector-skip-right "abcdef" @vector?)))
(test 5
       (dummy-index "abcdef"
                             (@vector-skip-right "abcdef"
                                                (lambda (c) (char<=? c #\d)))))
(test #f
       (dummy-index "abcdef"
                             (@vector-skip-right "abcdef" char?)))
(test 2
       (dummy-index "abcdef"
                             (@vector-index "abcdef" char? 2)))
(test 4
       (dummy-index "abcdef"
                             (@vector-index "abcdef"
                                           (lambda (c) (char>? c #\d)) 2)))
(test #f
       (dummy-index "abcdef"
                             (@vector-index "abcdef" char-whitespace? 2)))
(test 5
       (dummy-index "abcdef"
                             (@vector-index-right "abcdef" char? 2)))
(test 5
       (dummy-index "abcdef"
                             (@vector-index-right "abcdef"
                                                 (lambda (c)
                                                   (char>? c #\d)) 2)))
(test #f
       (dummy-index "abcdef"
                             (@vector-index-right "abcdef" char-whitespace? 2)))
(test 2
       (dummy-index "abcdef"
                             (@vector-skip "abcdef" @vector? 2)))
(test 4
       (dummy-index "abcdef"
                             (@vector-skip "abcdef"
                                          (lambda (c)
                                            (char<=? c #\d)) 2)))
(test #f
       (dummy-index "abcdef"
                             (@vector-skip "abcdef" char? 2)))
(test 5
       (dummy-index "abcdef"
                             (@vector-skip-right "abcdef" @vector? 2)))
(test 5
       (dummy-index "abcdef"
                             (@vector-skip-right "abcdef"
                                                (lambda (c)
                                                  (char<=? c #\d)) 2)))
(test #f
       (dummy-index "abcdef"
                             (@vector-skip-right "abcdef" char? 2)))
(test 2
       (dummy-index "abcdef"
                             (@vector-index "abcdef" char? 2 5)))
(test 4
       (dummy-index "abcdef"
                             (@vector-index "abcdef"
                                           (lambda (c) (char>? c #\d)) 2 5)))
(test #f
       (dummy-index "abcdef"
                             (@vector-index "abcdef" char-whitespace? 2 5)))
(test 4
       (dummy-index "abcdef"
                             (@vector-index-right "abcdef" char? 2 5)))
(test 4
       (dummy-index "abcdef"
                             (@vector-index-right "abcdef"
                                                 (lambda (c)
                                                   (char>? c #\d)) 2 5)))
(test #f
       (dummy-index "abcdef"
                             (@vector-index-right "abcdef"
                                                 char-whitespace? 2 5)))
(test 2
       (dummy-index "abcdef"
                             (@vector-skip "abcdef" @vector? 2 5)))
(test 4
       (dummy-index "abcdef"
                             (@vector-skip "abcdef"
                                          (lambda (c) (char<=? c #\d)) 2 5)))
(test #f
       (dummy-index "abcdef"
                             (@vector-skip "abcdef" char? 2 5)))
(test 4
       (dummy-index "abcdef"
                             (@vector-skip-right "abcdef" @vector? 2 5)))
(test 4
       (dummy-index "abcdef"
                             (@vector-skip-right "abcdef"
                                                (lambda (c)
                                                  (char<=? c #\d)) 2 5)))
(test #f
       (dummy-index "abcdef"
                             (@vector-skip-right "abcdef" char? 2 5)))
(test 0
          (dummy-index ""
                                (@vector-contains "" "")))
(test 0
          (dummy-index "abcdeffffoo"
                                (@vector-contains "abcdeffffoo" "")))
(test 0
          (dummy-index "abcdeffffoo"
                                (@vector-contains "abcdeffffoo" "a")))
(test 5
          (dummy-index "abcdeffffoo"
                                (@vector-contains "abcdeffffoo" "ff")))
(test 4
          (dummy-index "abcdeffffoo"
                                (@vector-contains "abcdeffffoo" "eff")))
(test 8
          (dummy-index "abcdeffffoo"
                                (@vector-contains "abcdeffffoo" "foo")))
(test #f
          (dummy-index "abcdeffffoo"
                                (@vector-contains "abcdeffffoo" "efffoo")))
(test 0
          (dummy-index ""
                                (@vector-contains-right "" "")))
(test 11
          (dummy-index "abcdeffffoo"
                                (@vector-contains-right "abcdeffffoo" "")))
(test 0
          (dummy-index "abcdeffffoo"
                                (@vector-contains-right "abcdeffffoo" "a")))
(test 7
          (dummy-index "abcdeffffoo"
                                (@vector-contains-right "abcdeffffoo" "ff")))
(test 4
          (dummy-index "abcdeffffoo"
                                (@vector-contains-right "abcdeffffoo" "eff")))
(test 8
          (dummy-index "abcdeffffoo"
                                (@vector-contains-right "abcdeffffoo" "foo")))
(test #f
          (dummy-index "abcdeffffoo"
                                (@vector-contains-right "abcdeffffoo"
                                                       "efffoo")))
(test 0
          (dummy-index ""
                                (@vector-contains "" "" 0)))
(test 2
          (dummy-index "abcdeffffoo"
                                (@vector-contains "abcdeffffoo" "" 2)))
(test #f
          (dummy-index "abcdeffffoo"
                                (@vector-contains "abcdeffffoo" "a" 2)))
(test 5
          (dummy-index "abcdeffffoo"
                                (@vector-contains "abcdeffffoo" "ff" 2)))
(test 4
          (dummy-index "abcdeffffoo"
                                (@vector-contains "abcdeffffoo" "eff" 2)))
(test 8
          (dummy-index "abcdeffffoo"
                                (@vector-contains "abcdeffffoo" "foo" 2)))
(test #f
          (dummy-index "abcdeffffoo"
                                (@vector-contains "abcdeffffoo" "efffoo" 2)))
(test 0
          (dummy-index ""
                                (@vector-contains-right "" "" 0)))
(test 11
          (dummy-index "abcdeffffoo"
                                (@vector-contains-right "abcdeffffoo"
                                                       "" 2)))
(test #f
          (dummy-index "abcdeffffoo"
                                (@vector-contains-right "abcdeffffoo"
                                                       "a" 2)))
(test 7
          (dummy-index "abcdeffffoo"
                                (@vector-contains-right "abcdeffffoo"
                                                       "ff" 2)))
(test 4
          (dummy-index "abcdeffffoo"
                                (@vector-contains-right "abcdeffffoo"
                                                       "eff" 2)))
(test 8
          (dummy-index "abcdeffffoo"
                                (@vector-contains-right "abcdeffffoo"
                                                       "foo" 2)))
(test #f
          (dummy-index "abcdeffffoo"
                                (@vector-contains-right "abcdeffffoo"
                                                       "efffoo" 2)))
(test 0
          (dummy-index ""
                                (@vector-contains "" "" 0 0)))
(test 2
          (dummy-index "abcdeffffoo"
                                (@vector-contains "abcdeffffoo"
                                                 "" 2 10)))
(test #f
          (dummy-index "abcdeffffoo"
                                (@vector-contains "abcdeffffoo"
                                                 "a" 2 10)))
(test 5
          (dummy-index "abcdeffffoo"
                                (@vector-contains "abcdeffffoo"
                                                 "ff" 2 10)))
(test 4
          (dummy-index "abcdeffffoo"
                                (@vector-contains "abcdeffffoo"
                                                 "eff" 2 10)))
(test #f
          (dummy-index "abcdeffffoo"
                                (@vector-contains "abcdeffffoo"
                                                 "foo" 2 10)))
(test #f
          (dummy-index "abcdeffffoo"
                                (@vector-contains "abcdeffffoo"
                                                 "efffoo" 2 10)))
(test 0
          (dummy-index ""
                                (@vector-contains-right "" "" 0 0)))
(test 10
          (dummy-index "abcdeffffoo"
                                (@vector-contains-right "abcdeffffoo"
                                                       "" 2 10)))
(test #f
          (dummy-index "abcdeffffoo"
                                (@vector-contains-right "abcdeffffoo"
                                                       "a" 2 10)))
(test 7
          (dummy-index "abcdeffffoo"
                                (@vector-contains-right "abcdeffffoo"
                                                       "ff" 2 10)))
(test 4
          (dummy-index "abcdeffffoo"
                                (@vector-contains-right "abcdeffffoo"
                                                       "eff" 2 10)))
(test #f
          (dummy-index "abcdeffffoo"
                                (@vector-contains-right "abcdeffffoo"
                                                       "foo" 2 10)))
(test #f
          (dummy-index "abcdeffffoo"
                                (@vector-contains-right "abcdeffffoo"
                                                       "efffoo" 2 10)))
(test 0
          (dummy-index ""
                                (@vector-contains "" "" 0 0 0)))
(test 2
          (dummy-index "abcdeffffoo"
                                (@vector-contains "abcdeffffoo"
                                                 "" 2 10 0)))
(test 2
          (dummy-index "abcdeffffoo"
                                (@vector-contains "abcdeffffoo"
                                                 "a" 2 10 1)))
(test 5
          (dummy-index "abcdeffffoo"
                                (@vector-contains "abcdeffffoo"
                                                 "ff" 2 10 1)))
(test 5
          (dummy-index "abcdeffffoo"
                                (@vector-contains "abcdeffffoo"
                                                 "eff" 2 10 1)))
(test #f
          (dummy-index "abcdeffffoo"
                                (@vector-contains "abcdeffffoo"
                                                 "foo" 2 10 1)))
(test #f
          (dummy-index "abcdeffffoo"
                                (@vector-contains "abcdeffffoo"
                                                 "efffoo" 2 10 1)))
(test 0
          (dummy-index ""
                                (@vector-contains-right "" "" 0 0 0)))
(test 10
          (dummy-index "abcdeffffoo"
                                (@vector-contains-right "abcdeffffoo"
                                                       "" 2 10 0)))
(test 10
          (dummy-index "abcdeffffoo"
                                (@vector-contains-right "abcdeffffoo"
                                                       "a" 2 10 1)))
(test 8
          (dummy-index "abcdeffffoo"
                                (@vector-contains-right "abcdeffffoo"
                                                       "ff" 2 10 1)))
(test 7
          (dummy-index "abcdeffffoo"
                                (@vector-contains-right "abcdeffffoo"
                                                       "eff" 2 10 1)))
(test #f
          (dummy-index "abcdeffffoo"
                                (@vector-contains-right "abcdeffffoo"
                                                       "foo" 2 10 1)))
(test #f
          (dummy-index "abcdeffffoo"
                                (@vector-contains-right "abcdeffffoo"
                                                       "efffoo" 2 10 1)))
(test 0
          (dummy-index ""
                                (@vector-contains "" "" 0 0 0 0)))
(test 2
          (dummy-index "abcdeffffoo"
                                (@vector-contains "abcdeffffoo"
                                                 "" 2 10 0 0)))
(test 2
          (dummy-index "abcdeffffoo"
                                (@vector-contains "abcdeffffoo"
                                                 "a" 2 10 1 1)))
(test 5
          (dummy-index "abcdeffffoo"
                                (@vector-contains "abcdeffffoo"
                                                 "ff" 2 10 1 2)))
(test 5
          (dummy-index "abcdeffffoo"
                                (@vector-contains "abcdeffffoo"
                                                 "eff" 2 10 1 2)))
(test 9
          (dummy-index "abcdeffffoo"
                                (@vector-contains "abcdeffffoo"
                                                 "foo" 2 10 1 2)))
(test 4
          (dummy-index "abcdeffffoo"
                                (@vector-contains "abcdeffffoo"
                                                 "efffoo" 2 10 0 2)))
(test 0
          (dummy-index ""
                                (@vector-contains-right "" "" 0 0 0 0)))
(test 10
          (dummy-index "abcdeffffoo"
                                (@vector-contains-right "abcdeffffoo"
                                                       "" 2 10 0 0)))
(test 10
          (dummy-index "abcdeffffoo"
                                (@vector-contains-right "abcdeffffoo"
                                                       "a" 2 10 1 1)))
(test 8
          (dummy-index "abcdeffffoo"
                                (@vector-contains-right "abcdeffffoo"
                                                       "ff" 2 10 1 2)))
(test 8
          (dummy-index "abcdeffffoo"
                                (@vector-contains-right "abcdeffffoo"
                                                       "eff" 2 10 1 2)))
(test 9
          (dummy-index "abcdeffffoo"
                                (@vector-contains-right "abcdeffffoo"
                                                       "foo" 2 10 1 2)))
(test 7
          (dummy-index "abcdeffffoo"
                                (@vector-contains-right "abcdeffffoo"
                                                       "efffoo" 2 10 1 3)))
)
;;; The whole @vector
(test-group "srfi-152:larceny:whole@vector"
(test "" (@vector-concatenate '()))
(test "abcdef" (@vector-concatenate '("" "a" "bcd" "" "ef" "" "")))
(test "" (@vector-concatenate-reverse '()))
(test "efbcda"
              (@vector-concatenate-reverse '("" "a" "bcd" "" "ef" "" "")))
(test "huh?" (@vector-concatenate-reverse '() "huh?"))
(test "efbcdaxy"
              (@vector-concatenate-reverse '("" "a" "bcd" "" "ef" "" "") "xy"))
(test "huh" (@vector-concatenate-reverse '() "huh?" 3))
(test "efbcdax"
              (@vector-concatenate-reverse '("" "a" "bcd" "" "ef" "" "") "x" 1))
(test 8
       (@vector-fold (lambda (c count)
                      (if (char-whitespace? c)
                          (+ count 1)
                          count))
                    0
                    " ...a couple of spaces in this one... "))
(test 7
       (@vector-fold (lambda (c count)
                      (if (char-whitespace? c)
                          (+ count 1)
                          count))
                    0
                    " ...a couple of spaces in this one... "
                    1))
(test 6
       (@vector-fold (lambda (c count)
                      (if (char-whitespace? c)
                          (+ count 1)
                          count))
                    0
                    " ...a couple of spaces in this one... "
                    1
                    32))
(test (@vector->list "abcdef")
            (@vector-fold-right cons '() "abcdef"))
(test (@vector->list "def")
            (@vector-fold-right cons '() "abcdef" 3))
(test (@vector->list "cde")
            (@vector-fold-right cons '() "abcdef" 2 5))
(test "aabraacaadaabraa"
              (let* ((s "abracadabra")
                     (ans-len (@vector-fold (lambda (c sum)
                                             (+ sum (if (char=? c #\a) 2 1)))
                                           0 s))
                     (ans (make-@vector ans-len)))
                (@vector-fold (lambda (c i)
                               (let ((i (if (char=? c #\a)
                                            (begin (@vector-set! ans i #\a)
                                                   (+ i 1))
                                                   i)))
                                 (@vector-set! ans i c)
                             (+ i 1)))
                             0 s)
                ans))
(test '(101 100 99 98 97)
            (let ((s "abcde") (v '()))
              (@vector-for-each
               (lambda (char)
                 (set! v (cons (char->integer char) v)))
               s)
              v))
(test "cdefabcdefabcd"
              (@vector-replicate "abcdef" -4 10))
(test "bcdefbcdefbcd"
              (@vector-replicate "abcdef" 90 103 1))
(test "ecdecdecde"
              (@vector-replicate "abcdef" -13 -3 2 5))
(test 6 (@vector-count "abcdef" char?))
(test 4 (@vector-count "counting  whitespace, again " char-whitespace? 5))
(test 3 (@vector-count "abcdefwxyz"
                       (lambda (c) (odd? (char->integer c)))
                       2 8))
(test "It's lots of fun to code it up in Scheme."
              (@vector-replace "It's easy to code it up in Scheme."
                              "lots of fun"
                              5 9))
(test "The miserable perl programmer endured daily ridicule."
              (@vector-replace "The TCL programmer endured daily ridicule."
                              "another miserable perl drone"
                              4 7 8 22))
(test "It's really easy to code it up in Scheme."
              (@vector-replace "It's easy to code it up in Scheme."
                              "really "
                              5 5))
(test-assert (null? (@vector-split "" "")))
(test '("a" "b" "c") (@vector-split "abc" ""))
(test '() (@vector-split "" "" 'infix))
(test '("a" "b" "c") (@vector-split "abc" "" 'infix))
(test '("a" "b" "c") (@vector-split "abc" "" 'strict-infix))
(test '() (@vector-split "" "" 'prefix))
(test '("a" "b" "c") (@vector-split "abc" "" 'prefix))
(test '() (@vector-split "" "" 'suffix))
(test '("a" "b" "c") (@vector-split "abc" "" 'suffix))
(test '() (@vector-split "" "" 'infix #f))
(test '("a" "b" "c") (@vector-split "abc" "" 'infix #f))
(test-error
             (@vector-split "" "" 'strict-infix))
(test '("a" "b" "c") (@vector-split "abc" "" 'strict-infix 3))
(test '() (@vector-split "" "" 'prefix 3))
(test '("a" "b" "c") (@vector-split "abc" "" 'prefix 3))
(test '() (@vector-split "" "" 'suffix 3))
(test '("a" "b" "c") (@vector-split "abc" "" 'suffix 3))
(test '("b" "c") (@vector-split "abc" "" 'strict-infix 3 1))
(test '() (@vector-split "" "" 'prefix 3 0))
(test '("b" "c") (@vector-split "abc" "" 'prefix 3 1))
(test '("b") (@vector-split "abc" "" 'strict-infix 3 1 2))
(test '() (@vector-split "" "" 'prefix 3 0 0))
(test '("b") (@vector-split "abc" "" 'prefix 3 1 2))
(test '() (@vector-split "" "" 'suffix 3 0 0))
(test '("b") (@vector-split "abc" "" 'suffix 3 1 2))
(test "aiueaaaoi"
              (@vector-filter (lambda (c) (memv c (@vector->list "aeiou")))
                             "What is number, that man may know it?"))
(test "And wmn, tht sh my knw nmbr?"
              (@vector-remove (lambda (c) (memv c (@vector->list "aeiou")))
                             "And woman, that she may know number?"))
(test "iueaaaoi"
              (@vector-filter (lambda (c) (memv c (@vector->list "aeiou")))
                             "What is number, that man may know it?"
                             4))
(test "mn, tht sh my knw nmbr?"
              (@vector-remove (lambda (c) (memv c (@vector->list "aeiou")))
                             "And woman, that she may know number?"
                             6))
(test "aaao"
              (@vector-filter (lambda (c) (memv c (@vector->list "aeiou")))
                             "What is number, that man may know it?"
                             16 32))
(test "And woman, that sh may know"
              (@vector-remove (lambda (c) (memv c (@vector->list "eiu")))
                             "And woman, that she may know number?"
                             0 28))
)
)
(test-group "srfi-152:residual"
  (test #t (@vector? "abc"))
  (test #f (@vector? 32))
  (test "$$$" (make-@vector 3 #\$))
  (test "$$$" (@vector #\$ #\$ #\$))
  (test '(#\b #\c) (@vector->list "abcde" 1 3))
  (test "abcde" (list->@vector '(#\a #\b #\c #\d #\e)))
  (test "abcde" (vector->@vector '#(#\a #\b #\c #\d #\e)))
  (test '("12345" "abcde")
    (call-with-values (lambda () (@vector-span "12345abcde" char-numeric?)) list))
  (test '("12345" "abcde")
    (call-with-values (lambda () (@vector-break "12345abcde" char-alphabetic?)) list))
  (test "abcde" (@vector-take-while "abcde12345" char-alphabetic?))
  (test "abcde" (@vector-take-while-right "12345abcde" char-alphabetic?))
  (test "abcde" (@vector-drop-while "   abcde" char-whitespace?))
  (test "abcde" (@vector-drop-while-right "abcde  " char-whitespace?))
  (test 5 (@vector-length "abcde"))
  (test "ab!"
    (let ((abc (@vector-copy "abc")))
      (@vector-set! abc 2 #\!)
      abc))
  (test "ab!"
    (let ((abc (@vector-copy "abc")))
      (@vector-set! abc 2 #\!)
      abc))
  (test "!!!"
    (let ((abc (@vector-copy "abc")))
      (@vector-fill! abc #\!)
      abc))
  (test "a!c"
    (let ((abc (@vector-copy "abc")))
      (@vector-fill! abc #\! 1 2)
      abc))
)
)
(test-exit)
