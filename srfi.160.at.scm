(module (srfi 160 @) ()
  (import (scheme))
  (import (only (chicken base)
    open-input-string include define-record-type case-lambda
    when unless let-values))
  (import (only (chicken module) export))
  (import (srfi 128))
  (import (srfi 160 base))

  ;; Constructors 
  (export make-@vector @vector
          @vector-unfold @vector-unfold-right
          @vector-copy @vector-reverse-copy 
          @vector-append @vector-concatenate
          @vector-append-subvectors)
  ;; Predicates 
  (export @? @vector? @vector-empty? @vector=)
  ;; Selectors
  (export @vector-ref @vector-length)
  ;; Iteration 
  (export @vector-take @vector-take-right
          @vector-drop @vector-drop-right
          @vector-segment
          @vector-fold @vector-fold-right
          @vector-map @vector-map! @vector-for-each
          @vector-count @vector-cumulate)
  ;; Searching 
  (export @vector-take-while @vector-take-while-right
          @vector-drop-while @vector-drop-while-right
          @vector-index @vector-index-right @vector-skip @vector-skip-right 
          @vector-any @vector-every @vector-partition
          @vector-filter @vector-remove)
  ;; Mutators 
  (export @vector-set! @vector-swap! @vector-fill! @vector-reverse!
          @vector-copy! @vector-reverse-copy!
          @vector-unfold! @vector-unfold-right!)
  ;; Conversion 
  (export @vector->list reverse-@vector->list reverse-list->@vector
          @vector->vector vector->@vector)
  ;; Misc
  (export make-@vector-generator @vector-comparator write-@vector)

  (include "srfi/160/@-impl.scm")

  (define (eof-object)
     (let* ((p (open-input-string "")) (e (read p)))
      (close-input-port p) e))
)
