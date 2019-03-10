;;; SRFI 130 @vector library reference implementation		-*- Scheme -*-
;;; Olin Shivers 7/2000
;;; John Cowan 4/2016
;;; Heavily stripped down and mutated for SRFI 160
;;;
;;; Copyright (c) 1988-1994 Massachusetts Institute of Technology.
;;; Copyright (c) 1998, 1999, 2000 Olin Shivers. 
;;; Copyright (c) 2016 John Cowan.
;;;   The details of the copyrights appear at the end of the file. Short
;;;   summary: BSD-style open source.

;;; Imports
;;; This is a fairly large library. While it was written for portability, you
;;; must be aware of its dependencies in order to run it in a given scheme
;;; implementation. Here is a complete list of the dependencies it has and the
;;; assumptions it makes beyond stock R5RS Scheme:
;;;
;;; This code has the following non-R5RS dependencies:
;;; - (RECEIVE (var ...) mv-exp body ...) multiple-value binding macro;
;;;
;;; From SRFI 8
(define-syntax receive
  (syntax-rules ()
    ((receive formals expression body ...)
     (call-with-values (lambda () expression)
                       (lambda formals body ...)))))

;;; - An n-ary ERROR procedure;
;;;   
;;; - A simple CHECK-ARG procedure for checking parameter values; it is 
;;; Inserted here
(define check-arg
   (lambda (pred val proc) 
     (if (pred val) val (error "Bad arg" val pred proc))))

;;; The code depends upon a small set of core @vector primitives from SRFI 4:
;;;     MAKE-@VECTOR @VECTOR-REF @VECTOR? @VECTOR-LENGTH 
;;; (FIXME:, SUB@VECTOR is not a primitive, but we assume that an 
;;; implementation's native version is probably faster than one we could
;;; define, so we import it from R5RS.)
;;;
;;;   

(define (add1 n) (+ 1 n))

;;; Enough introductory blather. On to the source code. (But see the end of
;;; the file for further notes on porting & performance tuning.)


;;; Support for START/END @subvector specs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This macro parses optional start/end arguments from arg lists, defaulting
;;; them to 0/(@vector-length s), and checks them for correctness.

(define-syntax let-@vector-start+end
  (syntax-rules ()
    ((let-@vector-start+end (start end) proc s-exp args-exp body ...)
     (receive (start end) (@vector-parse-final-start+end proc s-exp args-exp)
       body ...))
    ((let-@vector-start+end (start end rest) proc s-exp args-exp body ...)
     (receive (rest start end) (@vector-parse-start+end proc s-exp args-exp)
       body ...))))

;;; This one parses out a *pair* of final start/end indices. 
;;; Not exported; for internal use.
(define-syntax let-@vector-start+end2
  (syntax-rules ()
    ((l-s-s+e2 (start1 end1 start2 end2) proc s1 s2 args body ...)
     (let ((procv proc)) ; Make sure PROC is only evaluated once.
       (let-@vector-start+end (start1 end1 rest) procv s1 args
         (let-@vector-start+end (start2 end2) procv s2 rest
           body ...))))))


;;; Returns three values: rest start end

(define (@vector-parse-start+end proc s args)
  (if (not (@vector? s)) (error "Non-@vector value" proc s))
  (let ((slen (@vector-length s)))
    (if (pair? args)

	(let ((start (car args))
	      (args (cdr args)))
	  (if (and (integer? start) (exact? start) (>= start 0))
	      (receive (end args)
		  (if (pair? args)
		      (let ((end (car args))
			    (args (cdr args)))
			(if (and (integer? end) (exact? end) (<= end slen))
			    (values end args)
			    (error "Illegal @subvector END spec" proc end s)))
		      (values slen args))
		(if (<= start end) (values args start end)
		    (error "Illegal @subvector START/END spec"
			   proc start end s)))
	      (error "Illegal @subvector START spec" proc start s)))

	(values '() 0 slen))))

(define (@vector-parse-final-start+end proc s args)
  (receive (rest start end) (@vector-parse-start+end proc s args)
    (if (pair? rest) (error "Extra arguments to procedure" proc rest)
	(values start end))))

(define (@subvector-spec-ok? s start end)
  (and (@vector? s)
       (integer? start)
       (exact? start)
       (integer? end)
       (exact? end)
       (<= 0 start)
       (<= start end)
       (<= end (@vector-length s))))

(define (check-@subvector-spec proc s start end)
  (if (not (@subvector-spec-ok? s start end))
      (error "Illegal @subvector spec." proc s start end)))



;;; @subvector S START [END] 
;;; @vector-copy      S [START END]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; All this goop is just arg parsing & checking surrounding a call to the
;;; actual primitive, %SUB@VECTOR.

;;; Split out so that other routines in this library can avoid arg-parsing
;;; overhead for END parameter.
(define (%@subvector s start end)
  (if (and (zero? start) (= end (@vector-length s))) s
      (@subvector s start end)))

;;; Cutting & pasting @vectors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; @vector-take @vector nchars
;;; @vector-drop @vector nchars
;;;
;;; @vector-take-right @vector nchars
;;; @vector-drop-right @vector nchars

(define (@vector-take s n)
  (check-arg @vector? s @vector-take)
  (check-arg (lambda (val) (and (integer? n) (exact? n)
				(<= 0 n (@vector-length s))))
	     n @vector-take)
  (%@subvector s 0 n))

(define (@vector-take-right s n)
  (check-arg @vector? s @vector-take-right)
  (let ((len (@vector-length s)))
    (check-arg (lambda (val) (and (integer? n) (exact? n) (<= 0 n len)))
	       n @vector-take-right)
    (%@subvector s (- len n) len)))

(define (@vector-drop s n)
  (check-arg @vector? s @vector-drop)
  (let ((len (@vector-length s)))
    (check-arg (lambda (val) (and (integer? n) (exact? n) (<= 0 n len)))
	       n @vector-drop)
  (%@subvector s n len)))

(define (@vector-drop-right s n)
  (check-arg @vector? s @vector-drop-right)
  (let ((len (@vector-length s)))
    (check-arg (lambda (val) (and (integer? n) (exact? n) (<= 0 n len)))
	       n @vector-drop-right)
    (%@subvector s 0 (- len n))))


;;; Filtering @vectors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; @vector-remove char/pred @vector [start end]
;;; @vector-filter char/pred @vector [start end]
;;;
;;; If the criterion is a predicate, we don't do this double-scan strategy, 
;;;   because the predicate might have side-effects or be very expensive to
;;;   compute. So we preallocate a temp buffer pessimistically, and only do
;;;   one scan over S. This is likely to be faster and more space-efficient
;;;   than consing a list.

(define (@vector-remove criterion s . maybe-start+end)
  (let-@vector-start+end (start end) @vector-remove s maybe-start+end
	(let* ((slen (- end start))
	       (temp (make-@vector slen))
	       (ans-len (@vector-fold (lambda (c i)
				       (if (criterion c) i
					   (begin (@vector-set! temp i c)
						  (+ i 1))))
				     0 s start end)))
	  (if (= ans-len slen) temp (@subvector temp 0 ans-len)))))

	  

(define (@vector-filter criterion s . maybe-start+end)
  (let-@vector-start+end (start end) @vector-filter s maybe-start+end
	(let* ((slen (- end start))
	       (temp (make-@vector slen))
	       (ans-len (@vector-fold (lambda (c i)
				       (if (criterion c)
					   (begin (@vector-set! temp i c)
						  (+ i 1))
					   i))
				     0 s start end)))
	  (if (= ans-len slen) temp (@subvector temp 0 ans-len)))))
(define (@vector-take-while s criterion . maybe-start+end)
  (let-@vector-start+end (start end) @vector-take-while s maybe-start+end
    (let ((idx (@vector-skip s criterion start end)))
      (if idx
          (%@subvector s 0 idx)
          ""))))

(define (@vector-take-while-right s criterion . maybe-start+end)
  (let-@vector-start+end (start end) @vector-take-while s maybe-start+end
    (let ((idx (@vector-skip-right s criterion start end)))
      (if idx
          (%@subvector s (+ idx 1) (@vector-length s))
          ""))))

(define (@vector-drop-while s criterion . maybe-start+end)
  (let-@vector-start+end (start end) @vector-drop-while s maybe-start+end
    (let ((idx (@vector-skip s criterion start end)))
      (if idx
          (%@subvector s idx (@vector-length s))
          s))))

(define (@vector-drop-while-right s criterion . maybe-start+end)
  (let-@vector-start+end (start end) @vector-drop-while s maybe-start+end
    (let ((idx (@vector-skip-right s criterion start end)))
      (if idx
          (%@subvector s 0 (+ idx 1))
          s))))

(define (@vector-segment str k)
  (if (< k 1) (error "minimum segment size is 1" k))
  (let ((len (@vector-length str)))
    (let loop ((start 0)
               (result '()))
      (if (= start len)
        (reverse result)
        (let ((end (min (+ start k) len)))
          (loop end (cons (%@subvector str start end) result)))))))

;;; Porting & performance-tuning notes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; See the section at the beginning of this file on external dependencies.
;;;
;;; There is a fair amount of argument checking. This is, strictly speaking,
;;; unnecessary -- the actual body of the procedures will blow up if, say, a
;;; START/END index is improper. However, the error message will not be as
;;; good as if the error were caught at the "higher level." Also, a very, very
;;; smart Scheme compiler may be able to exploit having the type checks done
;;; early, so that the actual body of the procedures can assume proper values.
;;; This isn't likely; this kind of compiler technology isn't common any 
;;; longer.
;;; 
;;; The overhead of optional-argument parsing is irritating. The optional
;;; arguments must be consed into a rest list on entry, and then parsed out.
;;; Function call should be a matter of a few register moves and a jump; it
;;; should not involve heap allocation! Your Scheme system may have a superior
;;; non-R5RS optional-argument system that can eliminate this overhead. If so,
;;; then this is a prime candidate for optimising these procedures,
;;; *especially* the many optional START/END index parameters.
;;;
;;; Note that optional arguments are also a barrier to procedure integration.
;;; If your Scheme system permits you to specify alternate entry points
;;; for a call when the number of optional arguments is known in a manner
;;; that enables inlining/integration, this can provide performance 
;;; improvements.
;;;
;;; There is enough *explicit* error checking that *all* @vector-index
;;; operations should *never* produce a bounds error. Period. Feel like
;;; living dangerously? *Big* performance win to be had by replacing
;;; @VECTOR-REF's and @VECTOR-SET!'s with unsafe equivalents in the loops. 
;;; Similarly, fixnum-specific operators can speed up the arithmetic done on 
;;; the index values in the inner loops. The only arguments that are not
;;; completely error checked are
;;;   - @vector lists (complete checking requires time proportional to the
;;;     length of the list)
;;;   - procedure arguments, such as char->char maps & predicates.
;;;     There is no way to check the range & domain of procedures in Scheme.
;;; Procedures that take these parameters cannot fully check their
;;; arguments. But all other types to all other procedures are fully
;;; checked.
;;;
;;; This does open up the alternate possibility of simply *removing* these 
;;; checks, and letting the safe primitives raise the errors. On a dumb
;;; Scheme system, this would provide speed (by eliminating the redundant
;;; error checks) at the cost of error-message clarity.
;;;
;;; See the comments preceding the hash function code for notes on tuning
;;; the default bound so that the code never overflows your implementation's
;;; fixnum size into bignum calculation.
;;;
;;; In an interpreted Scheme, some of these procedures, or the internal
;;; routines with % prefixes, are excellent candidates for being rewritten
;;; in C. Consider @VECTOR-HASH, %@VECTOR-COMPARE, the 
;;; %@VECTOR-{SUF,PRE}FIX-LENGTH routines, @VECTOR-COPY!, @VECTOR-INDEX &
;;; @VECTOR-SKIP (char case), SUB@VECTOR and SUB@VECTOR,
;;; %KMP-SEARCH, and %MULTISPAN-REPCOPY!.
;;;
;;; It would also be nice to have the ability to mark some of these
;;; routines as candidates for inlining/integration.
;;; 
;;; All the %-prefixed routines in this source code are written
;;; to be called internally to this library. They do *not* perform
;;; friendly error checks on the inputs; they assume everything is
;;; proper. They also do not take optional arguments. These two properties
;;; save calling overhead and enable procedure integration -- but they
;;; are not appropriate for exported routines.


;;; Copyright details
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The prefix/suffix and comparison routines in this code had (extremely
;;; distant) origins in MIT Scheme's @vector lib, and was substantially
;;; reworked by Olin Shivers (shivers@ai.mit.edu) 9/98. As such, it is
;;; covered by MIT Scheme's open source copyright. See below for details.
;;; 
;;; The KMP @vector-search code was influenced by implementations written
;;; by Stephen Bevan, Brian Dehneyer and Will Fitzgerald. However, this
;;; version was written from scratch by myself.
;;;
;;; The remainder of this code was written from scratch by myself for scsh.
;;; The scsh copyright is a BSD-style open source copyright. See below for
;;; details.
;;;     -Olin Shivers

;;; MIT Scheme copyright terms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This material was developed by the Scheme project at the Massachusetts
;;; Institute of Technology, Department of Electrical Engineering and
;;; Computer Science.  Permission to copy and modify this software, to
;;; redistribute either the original software or a modified version, and
;;; to use this software for any purpose is granted, subject to the
;;; following restrictions and understandings.
;;; 
;;; 1. Any copy made of this software must include this copyright notice
;;; in full.
;;; 
;;; 2. Users of this software agree to make their best efforts (a) to
;;; return to the MIT Scheme project any improvements or extensions that
;;; they make, so that these may be included in future releases; and (b)
;;; to inform MIT of noteworthy uses of this software.
;;; 
;;; 3. All materials developed as a consequence of the use of this
;;; software shall duly acknowledge such use, in accordance with the usual
;;; standards of acknowledging credit in academic research.
;;; 
;;; 4. MIT has made no warrantee or representation that the operation of
;;; this software will be error-free, and MIT is under no obligation to
;;; provide any services, by way of maintenance, update, or otherwise.
;;; 
;;; 5. In conjunction with products arising from the use of this material,
;;; there shall be no use of the name of the Massachusetts Institute of
;;; Technology nor of any adaptation thereof in any advertising,
;;; promotional, or sales literature without prior written consent from
;;; MIT in each case.

;;; Scsh copyright terms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; All rights reserved.
;;; 
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 2. Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in the
;;;    documentation and/or other materials provided with the distribution.
;;; 3. The name of the authors may not be used to endorse or promote products
;;;    derived from this software without specific prior written permission.
;;; 
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS OR
;;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;;; IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
