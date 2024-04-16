#lang racket

(define current-reaction #f)
(define current-gets #f)
(define current-gets-index 0)

(define cache-clean 0)
(define cache-check 1)
(define cache-dirty 2)

(struct reactive (
  [value #:mutable]
  [fn #:mutable]
  [observers #:mutable]
  [sources #:mutable]
  [state #:mutable]
  equals))

(define (get-value this)
  (when current-reaction
    (if (and (not current-gets) (eq? (list-ref (reactive-sources current-reaction) current-gets-index) this))
        (set! current-gets-index (+ current-gets-index 1))
        (set! current-gets (cons this current-gets))))
  (when (reactive-fn this)
      (update-if-necessary this))
    (reactive-value this))
(define (set-value this v)
  (cond
    [(procedure? v)
      (set-reactive-fn! this v)
      (stale this cache-dirty)]
    [else
      (when (reactive-fn this)
        (remove-parent-observers this 0)
        (set-reactive-sources! this #f)
        (set-reactive-fn! this #f))
      (unless ((reactive-equals this) (reactive-value this) v)
        (when (reactive-observers this)
          (for ([observer (reactive-observers this)])
            (stale observer cache-dirty)))
        (set-reactive-value! this v))]))
(define (stale this state)
  (when (< (reactive-state this) state)
    (set-reactive-state! this state)
    (when (reactive-observers this)
      (for ([observer (reactive-observers this)])
        (stale observer cache-check)))))
(define (update this)
  (define old-value (reactive-value this))
  (let ([prev-reaction current-reaction]
        [prev-gets current-gets]
        [prev-index current-gets-index])
    (set! current-reaction this)
    (set! current-gets '())
    (set! current-gets-index 0)

    (set-reactive-value! this ((reactive-fn this)))
    (if current-gets
        (when true 
          (remove-parent-observers this current-gets-index)
          (define stable-sources (take (reactive-sources this) current-gets-index))
          (set-reactive-sources! this (append stable-sources current-gets))
          (for ([i (range current-gets-index (length (reactive-sources this)))])
            (define source (list-ref (reactive-sources this) i))
          (set-reactive-observers! source (cons this (reactive-observers source)))))
      (when (and (reactive-sources this)
                (< current-gets-index (length (reactive-sources this))))
        (remove-parent-observers this current-gets-index)
        (set-reactive-sources! this (take (reactive-sources this) current-gets-index))))
  
    (set! current-gets prev-gets)
    (set! current-reaction prev-reaction)
    (set! current-gets-index prev-index))

    (unless ((reactive-equals this) old-value (reactive-value this))
      (for ([observer (reactive-observers this)])
        (set-reactive-state! observer cache-dirty)))
    (set-reactive-state! this cache-clean))

(define (find-first-time-dirty this sources)
  (if (null? sources)
      #f
      (let ([source (car sources)])
        (update-if-necessary source)
        (if (= (reactive-state source) cache-dirty)
            source
            (find-first-time-dirty this (cdr sources))))))

(define (update-if-necessary this)
  (when (= (reactive-state this) cache-check)
    (find-first-time-dirty this (reactive-sources this)))
  (when (= (reactive-state this) cache-dirty)
    (update this))
  (set-reactive-state! this cache-clean))
(define (remove-parent-observers this index)
  (when (reactive-sources this)
    (for ([i (range index (length (reactive-sources this)))])
      (define source (list-ref (reactive-sources this) i))
      (when (reactive-observers source)
        (let ([swap (index-of (reactive-observers source) this)])
          (set-reactive-observers! source (list-set (reactive-observers source) swap
                        (last (reactive-observers source)))))
        (set-reactive-observers! source (drop-right (reactive-observers source) 1))))))


(define (reactively fn-or-value)
  (if (procedure? fn-or-value)
      (reactive #f fn-or-value '() '() cache-dirty equal?)
      (reactive fn-or-value #f '() '() cache-clean equal?)))

; example

(define a (reactively 0))
(define b (reactively 0))
(define c (reactively (lambda () 
                      (let ([av (get-value a)]
                            [bv (get-value b)])
                        (display "computing c\n")
                        (+ av bv)))))

(display (get-value c))
(display "\n")

(set-value a 1)
(set-value b 2)

(display (get-value c))
(display "\n")
