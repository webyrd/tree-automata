;;; Assumptions:
;;;
;;; * automata names are unique
;;; * any user-defined automata should be passed to 'compute-non-empty*' before use


(define-record-type automaton
  (fields
   name ;; set-of symbol?
   (mutable non-empty) ;; boolean?
   (mutable productions) ;; list-of production?
   )
  (protocol
   (lambda (make)
     (define f
       (case-lambda
         [(names->automaton name) (f names->automaton name '())]
         [(names->automaton name productions) (f names->automaton name #t productions #t)]
         [(names->automaton name non-empty productions use-cache?)
          (let ([a (make name non-empty productions)]) ;; TODO: we might not want to assume newly created automata are non-empty
            (if use-cache?
                (begin
                  (assert (not (assoc name (box-value names->automaton))))
                  (box-value-set! names->automaton
                                  (cons (cons name a) (box-value names->automaton)))))
            a)]))
     f)))

(define (factor-productions productions)
  (define factored-productions '())
  ;; productions ::
  (define (f p) (make-production (car p) (cdr p)))
  (define (rec productions)
    (cond
      [(null? productions) (map f (reverse factored-productions))]
      [(assq (caar productions) factored-productions)
       => (lambda (entry)
            (set-cdr! entry (cons (cdar productions) (cdr entry)))
            (rec (cdr productions)))]
      [else (set! factored-productions
                  (cons (list (caar productions) (cdar productions)) factored-productions))
            (rec (cdr productions))]))
  (rec productions))

;; (define-automata
;;   [def-name (string-name ...) non-empty use-cache (ctor1 a2) (ctor2)]
;;   [a2 ...]
;;   ...)
(define-syntax define-automata-internal
  (lambda (stx)
    (syntax-case stx ()
      [(_) #`(begin)]
      [(_ [def-name string-names non-empty use-cache [ctor children ...] ...] . rest)
       #`(begin
           (define def-name (make-automaton names->automaton string-names non-empty '() use-cache))
           (define-automata-internal . rest)
           (define dummy (automaton-productions-set!
                          def-name
                          (factor-productions (list [list 'ctor children ...] ...)))))])))

(define-syntax define-automata
  (lambda (stx)
    (syntax-case stx ()
      [(_ [name productions ...] ...)
       (with-syntax ([(string-names ...) (map symbol->string (syntax->datum #'(name ...)))])
         #`(define-automata-internal [name '(string-names) #t #t productions ...] ...))])))

;; returns true if the automataon 'a' contains a production clause
;; for which all the children are non-empty
(define (compute-automaton-non-empty a)
  (exists
      (lambda (p)
        (exists
            (lambda (cs) (for-all (lambda (c) (automaton-non-empty c)) cs))
          (production-children p)))
    (automaton-productions a)))

(define-record-type production
  (fields
   ;; NOTE: we factor out the constructor so we can iterate over every production that uses this constructor
   constructor ;; symbol? ;; TODO: primitives are constructor names
   (mutable children) ;; set-of (list-of automaton?)
   ))

(define-record-type box (fields (mutable value)))
(define names->automaton (make-box '()))
;; names->automaton :: alist (set-of symbol?) automaton?

;; ll computes a tree comprehension.  It also filters out any results
;; that are #f and each binding in the comprehension is in the scope
;; of the RHS of each of the following bindings.
(define-syntax ll
  (syntax-rules ()
    [(_ ([l1 list1]) body0 . body)
     (filter (lambda (x) x) (map (lambda (l1) body0 . body) list1))]
    [(_ ([l1 list1] . rest) body0 . body)
     (apply append (map (lambda (l1) (ll rest body0 . body)) list1))]))

;; NOTE: names are sets of symbols so we have a sensible name for intersects and take advantage of the associativity, communitivity and idempotency of intersect
;; NOTE: the name '() thus is the name of the universe
(define (combine-names n1 n2)
  (define (f l) ;; Eliminate duplicates from the list
    (cond
      [(or (null? l) (null? (cdr l))) l]
      [(string=? (car l) (cadr l)) (f (cdr l))]
      [else (cons (car l) (f (cdr l)))]))
  (f (list-sort string<? (append n1 n2))))

(define (clear-caches) (box-value-set! names->automaton '()))

(define new-automaton
  (case-lambda
   [(name) (make-automaton names->automaton name)]
   [(name productions) (make-automaton names->automaton name productions)]))

(define (intersect a1 a2)
  (define name (combine-names (automaton-name a1) (automaton-name a2)))
  (define ps1 (automaton-productions a1))
  (define ps2 (automaton-productions a2))
  (cond
    [(assoc name (box-value names->automaton)) => cdr]
    [else
     ;; We have to allocate the automaton in advance in case there is
     ;; a recursive loop.  We set the automaton's productions after the recursion.
     (let ([a (new-automaton name)])
       (let ([ps
              ;; TODO: if we keep productions sorted by constructor name
              ;; then this outer loop could be done more efficiently by
              ;; traversing the lists in parallel
              (ll ([p1 ps1] [p2 ps2]) ;; iterate over each production/constructor
                  ;; filter for when the constructors are equal
                  (and (eq? (production-constructor p1)
                            (production-constructor p2))
                       (make-production
                        (production-constructor p1)
                        ;; iterate over each list of children
                        (ll ([cs1 (production-children p1)]
                             [cs2 (production-children p2)])
                            ;; iterate over each child
                            (map intersect cs1 cs2)))))])
         (automaton-productions-set! a ps)
         a))]))

;; construct the "inverse map".  This maps automata names to the
;; productions of the automata that contain them.
;;
;; We could make this more efficient if we stored this inside the
;; automaton instead of as an external assoc list
;;
;; Also note that duplicate entries could appear if
;; there are multiple ways to be a child.
;; We don't optimize this as the cost of de-duplication
;; is high, and the algorithm using this inverse-map
;; costs only a little extra due to the duplication.
(define (build-inverse-map as)
  (define inverse-map '()) ;; multi-map names=(list-of sym) (cons parent=automaton children=(list automaton))
  (ll ([a as]
       [p (automaton-productions a)]
       [cs (production-children p)]
       [c cs])
      (define entry (cons a cs))
      (define cell (assoc (automaton-name c) inverse-map))
      (if cell
          ;; Add to whatever was there before
          (set-cdr! cell (cons entry (cdr cell)))
          ;; Add a new entry
          (set! inverse-map (cons (cons (automaton-name c) (list entry)) inverse-map))))
  inverse-map)

;; Return the part of new not in old assuming old is a suffix of new.
;; It is an error if old is not a suffix of new.
(define (prefix new old)
  (cond
    [(eq? new old) '()]
    [else (cons (car new) (prefix (cdr new) old))]))

(define (compute-non-empty* as)
  ;; ----------------
  ;; This queue contains automata that should be marked as non-empty
  (define queue '())
  (define (dequeue) (define x (car queue)) (set! queue (cdr queue)) x)
  (define (enqueue a)
    (or (automaton-non-empty a) ;; don't enqueue if already non-empty
        (begin (automaton-non-empty-set! a #t) (set! queue (cons a queue)))))

  ;; if the automaton is
  (define (check a)
    (let ([v (assoc (automaton-name a) inverse-map)])
        (and v
             (ll ([entry (cdr v)])
               (or (automaton-non-empty (car entry))
                   (and (for-all automaton-non-empty (cdr entry))
                        (enqueue (car entry))))))))
  ;; Repeatedly run "check" on elements of the queue until it is empty
  (define (run)
    (cond
      [(null? queue) (values)]
      [else (check (dequeue)) (run)]))

  (define inverse-map (build-inverse-map as))
  ;; NOTE: we assume the non-empty flag is correct on any child automata
  ;; NOTE: currently our primitives are technically empty, but they have the non-empty bit set
  ;; Start with the pessimistic assumption about non-emptiness
  (ll ([a as]) (automaton-non-empty-set! a #f))
  ;; put in the queue only those that are known to be non-empty
  (ll ([a as]) (and (compute-automaton-non-empty a) (enqueue a)))
  (run)
  )

;; eliminate production clauses calling non-non-empty automata
(define (filter-empty-clauses a)
  (ll ([p (automaton-productions a)])
      (production-children-set!
         p (filter (lambda (cs) (not (exists (lambda (c) (not (automaton-non-empty c))) cs)))
                   (production-children p)))))

;; eliminate productions with no production clauses
(define (filter-empty-productions a)
  (automaton-productions-set! a (filter (lambda (p) (not (null? (production-children p))))
                                        (automaton-productions a))))

;; This is intersection but we also filter out empty automata and productions
(define (intersect-driver a1 a2)
  ;; ** First, we compute the intersection while keeping track of
  ;; newly created automata **
  ;;
  ;; We remember which automata are newly created
  ;; by seeing which automata are added onto the old list of automata
  (define old-names->automaton (box-value names->automaton))
  ;; Do the actual intersection
  (define a (intersect a1 a2))
  ;; Get the list of automata created while intersecting a1 and a2
  (define new (map cdr (prefix (box-value names->automaton) old-names->automaton)))

  ;; ** Next, we have to compute the non-emptiness 
  (compute-non-empty* new)
  ;; NOTE: this code is side-effecting the automata in 'new'.
  ;; A nicer way to write this would be in terms of a 'map-graph'
  ;; that applies a particular function to every object in a graph
  (for-each filter-empty-clauses new)
  (for-each filter-empty-productions new)  
  a)


;; TODO: when reifying find tree automaton that are equal and give them
;; the same name (or at least for tree automaton that are equal to the intersection
;; of some subset of automata that were interesected to make them)

(define (find-first p l)
  (cond
    [(null? l) #f]
    [(p (car l)) (car l)]
    [else (find-first p (cdr l))]))

(define (automaton-has-constructor? a c)
  (find-first (lambda (p) (eq? c (production-constructor p))) (automaton-productions a)))

;; 'a' is a tree automaton
;; 't' is a miniKanren term
;;
;; returns a list of mappings
;; a mapping is a list of bindings
;; a binding is a pair of a var? and an automaton
;;
;; returns the empty list on failure
;;
;; NOTE: there may be multiple bindings for each variable in the
;; result.  The tree automata for each of these need to be intersected
;; with each other.
(define (make-unfold var? walk S)
  (letrec
    ([unfold
       (lambda (a t)
         (let ([t (walk t S)])
           (cond
             [(var? t) (list (list (cons t a)))]

             [(symbol? t) (and (automaton-has-constructor? a 'symbol?) '(()))]
             [(number? t) (and (automaton-has-constructor? a 'number?) '(()))]
             [(eq? t #t)  (and (automaton-has-constructor? a 'true?)   '(()))]
             [(eq? t #f)  (and (automaton-has-constructor? a 'false?)  '(()))]
             [(null? t)   (and (automaton-has-constructor? a 'null?)   '(()))]
             [(pair? t)   (let ([p (automaton-has-constructor? a 'pair?)])
                            (and p
                                 ;; NOTE: this assumes all "pair?" constructors
                                 ;; have exactly two children
                                 (ll ([cs (production-children p)]
                                      [m (unfold (car cs) (car t))]
                                      [n (unfold (cadr cs) (cdr t))])
                                     (append m n))))])))])
    unfold))

;;; MINIKANREN INTEGRATION COMMENTS and TODO
;;;
;;; Define a wrapper for unfold that performs the intersection of the
;;; tree automata when there are multiple bindings for a variable in
;;; the map.  This is the helper we would want to use for unification.

;;; If the list of mappings retuned by the unfold wrapper is empty, unification should fail.

;;; 'unfold' doesn't call 'walk'.  Need to either call walk* on 't', or interleave calls to 'walk' and 'unfold' until fresh variables are reached.  Could parameterize 'unfold' with 'walk'.

;;; From a constant factor standpoint, MDA says intersections are expensive, unfolds are cheap.  However, unfold can produce branching, while intersection cannot.

;;; Might want an 'apply automaton to term' helper function.


;;; The less we change unification the better.  Unification can still be deterministic, but == will produce branching like conde.


;;; Looks like we want to use 'intersect-driver' to perform intersection in unification.  Then check if the 'non-empty' field is set to true--if so, unification fails.

;;; Need a tree automata constraint operator that associates a tree automaton with a term.

;;; Probably want (clear-caches) to be called at the beginning/end of 'run'.

;;; Would be nice to be able to reify automata in 'define-automata' notation, so that the output could easily be used for another miniKanren program.
