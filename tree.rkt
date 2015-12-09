#lang racket

(require racket/serialize)

(provide
  make-unfold
  intersect!
  clear-caches!
  make-automaton!
  production
  set-automaton-productions!
  define-automata)

(module+ test
  (require rackunit))

;;; Assumptions:
;;;
;;; * automata names are unique
;;; * any user-defined automata should be passed to 'compute-non-empty*' before use

(struct automaton [name [non-empty #:mutable] [productions #:mutable]] #:prefab)

(define (make-automaton! name [productions (set)] [non-empty #t] [use-cache #t])
  (unless (set? productions)
    (error 'make-automaton! "productions must be a set, but is: ~s" productions))
  (let ([a (automaton name non-empty productions)])
    (when use-cache
      (when (hash-has-key? name-to-automaton-map name)
        (error 'make-automaton! "automata names should be unique, but this automaton '~s' is already registered" name))
      (set! name-to-automaton-map (hash-set name-to-automaton-map name a)))
    a))

(define (factor-productions prods)
  (define factored
    (for/fold ([acc (hash)])
              ([prod prods])
      (match-let ([(cons ctor children) prod])
        (hash-update acc ctor (lambda (s) (set-add s children)) (set)))))
  (for/set ([(ctor children-set) factored])
    (production ctor children-set)))

(module+ test
  (check-equal?
    (factor-productions '((cons 1 2) (cons 2 3) (cons 4 5) (foo 1) (foo 2) (nil) (string)))
    (set (production 'cons (set '(4 5) '(2 3) '(1 2)))
         (production 'foo (set '(2) '(1)))
         (production 'nil (set '()))
         (production 'string (set '())))))

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
           (define def-name (make-automaton! string-names (set) non-empty use-cache))
           (define-automata-internal . rest)
           (define dummy (set-automaton-productions!
                          def-name
                          (factor-productions (list [list 'ctor children ...] ...)))))])))

(define-syntax define-automata
  (lambda (stx)
    (syntax-case stx ()
      [(_ [name productions ...] ...)
       (with-syntax ([(string-names ...) (map symbol->string (syntax->datum #'(name ...)))])
         #`(begin
             (define-automata-internal [name '(string-names) #t #t productions ...] ...)
             (compute-non-empty* (list name ...))
             (for ([a (list name ...)])
               (unless (automaton-non-empty a)
                 (error 'define-automata "automaton ~s is non-empty" (automaton-name a))))))])))




;; returns true if the automataon 'a' contains a production clause
;; for which all the children are non-empty
(define (compute-automaton-non-empty a)
  (ormap
      (lambda (p)
        (ormap
            (lambda (cs) (andmap (lambda (c) (automaton-non-empty c)) cs))
          (set->list (production-children p))))
    (set->list (automaton-productions a))))

(struct production [constructor [children #:mutable]] #:prefab)

(define name-to-automaton-map (hash))
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
  (f (sort (append n1 n2) string<?)))

(module+ test
  (check-equal?
    (combine-names '("a" "c" "d") '("b" "d" "e"))
    '("a" "b" "c" "d" "e")))

(define (clear-caches!) (set! name-to-automaton-map (hash)))

(define (intersect-internal a1 a2)
  (define name (combine-names (automaton-name a1) (automaton-name a2)))
  (define ps1 (set->list (automaton-productions a1)))
  (define ps2 (set->list (automaton-productions a2)))
  (cond
    [(hash-ref name-to-automaton-map name #f)]
    [else
     ;; We have to allocate the automaton in advance in case there is
     ;; a recursive loop.  We set the automaton's productions after the recursion.
     (let ([a (make-automaton! name)])
       (let ([ps
              ;; TODO: if we keep productions sorted by constructor name
              ;; then this outer loop could be done more efficiently by
              ;; traversing the lists in parallel
              (ll ([p1 ps1] [p2 ps2]) ;; iterate over each production/constructor
                  ;; filter for when the constructors are equal
                  (and (eq? (production-constructor p1)
                            (production-constructor p2))
                       (production
                        (production-constructor p1)
                        ;; iterate over each list of children
                        (list->set
                          (ll ([cs1 (set->list (production-children p1))]
                               [cs2 (set->list (production-children p2))])
                              ;; iterate over each child
                              (map intersect-internal cs1 cs2))))))])
         (set-automaton-productions! a (list->set ps))
         a))]))

(module+ test
  (check-equal?
    (intersect-internal
      (make-automaton! '("a1")) (make-automaton! '("a2")))
    (make-automaton! '("a1" "a2") (set) #t #f))

  (define-syntax define-test-automata
    (lambda (stx)
      (syntax-case stx ()
        [(_ [name string-names non-empty productions ...] ...)
         #`(define-automata-internal [name 'string-names non-empty #f productions ...] ...)])))

  (let ()
    (clear-caches!)

    (define sym (make-automaton! '("sym") (set (production 'sym (set '())))))
    (define num (make-automaton! '("num") (set (production 'num (set '())))))
    (define nil (make-automaton! '("nil") (set (production 'nil (set '())))))
    (define term1 (make-automaton! '("term1")))
    (set-automaton-productions!
      term1
      (set (production 'cons (set (list term1 term1)))
           (production 'sym (set '()))
           (production 'num (set '()))))
    (define term2 (make-automaton! '("term2")))
    (set-automaton-productions!
      term2
      (set (production 'cons (set (list term2 term2)))
           (production 'nil (set '()))
           (production 'sym (set '()))
           (production 'num (set '()))))
    (define binding (make-automaton! '("binding") (set (production 'cons (set (list sym term1))))
                                     ))

    (define env (make-automaton! '("env")))
    (set-automaton-productions!
      env
      (set (production 'cons
                        (set (list binding env)))
            (production 'nil (set '()))))

    (clear-caches!)

    (define other-env
      (let ()
        (define-automata
          [env [nil] [cons binding env]]
          [binding [cons sym term1]]
          [term1 [cons term1 term1] [sym] [num]]
          [term2 [cons term2 term2] [nil] [sym] [num]]
          [sym [sym]]
          [num [num]]
          [nil [nil]])
        env))

    (check-equal?
      other-env
      env)

    (check-equal?
      (intersect-internal env nil)
      (automaton '("env" "nil") #t (set (production 'nil (set '())))))

    (check-equal?
      (intersect-internal env env)
      env)

    (check-equal?
      (intersect-internal env term1)
      (let ()
        (define-test-automata
          [a0 ("env" "term1") #t [cons a2 a0]]
          [a2 ("binding" "term1") #t [cons a3 a1]]
          [a1 ("term1") #t [cons a1 a1] [sym] [num]]
          [a3 ("sym" "term1") #t [sym]])
        a0))
    ))

;; construct the "inverse map". This maps automata names to information
;; about productions from other automata that reference them. The information
;; is a cons pair where the car is the other automata and the cdr is the
;; particular list of children which references this automata.
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
       [p (set->list (automaton-productions a))]
       [cs (set->list (production-children p))]
       [c cs])
      (define entry (cons a cs))
      (define cell (assoc (automaton-name c) inverse-map))
      (if cell
          ;; Add to whatever was there before
          (set-box! (cdr cell) (cons entry (unbox (cdr cell))))
          ;; Add a new entry
          (set! inverse-map (cons (cons (automaton-name c) (box (list entry))) inverse-map))))
  inverse-map)

(module+ test
  (let ()
    (clear-caches!)

    (define-automata
      [env [null?] [pair? binding env]]
      [binding [pair? sym term1]]
      [term1 [pair? term1 term1] [symbol?] [number?]]
      [term2 [pair? term2 term2] [null?] [symbol?] [number?]]
      [sym [symbol?]]
      [num [number?]]
      [nil [null?]])

    (check-equal? (intersect-internal term1 env)
                  (let ()
                    (define-test-automata
                      [a0 ("env" "term1") #t [pair? a2 a0]]
                      [a2 ("binding" "term1") #t [pair? a3 a1]]
                      [a1 ("term1") #t [pair? a1 a1] [symbol?] [number?]]
                      [a3 ("sym" "term1") #t [symbol?]])
                    a0))

    (check-equal? (intersect-internal env term2)
                  (let ()
                    (define-test-automata
                      [a0 ("env" "term2") #t [null?] [pair? a2 a0]]
                      [a2 ("binding" "term2") #t [pair? a3 a1]]
                      [a3 ("sym" "term2") #t [symbol?]]
                      [a1 ("term1" "term2") #t [pair? a1 a1] [symbol?] [number?]])
                    a0))

    (define inverse-map (build-inverse-map (list env term1 term2 sym num nil binding)))

    (check-equal?
      (list->set (unbox (cdr (assoc (automaton-name sym) inverse-map))))
      (set (cons binding (first (set->list (production-children (first (set->list (automaton-productions binding)))))))))

    (check-true (compute-automaton-non-empty env))
    (check-true (compute-automaton-non-empty sym))))

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
        (begin (set-automaton-non-empty! a #t) (set! queue (cons a queue)))))

  ;; if the automaton is
  (define (check a)
    (let ([v (assoc (automaton-name a) inverse-map)])
        (and v
             (ll ([entry (unbox (cdr v))])
               (or (automaton-non-empty (car entry))
                   (and (andmap automaton-non-empty (cdr entry))
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
  (ll ([a as]) (set-automaton-non-empty! a #f))
  ;; put in the queue only those that are known to be non-empty
  (ll ([a as]) (and (compute-automaton-non-empty a) (enqueue a)))
  (run)
  )

;; eliminate production clauses calling non-non-empty automata
(define (filter-empty-clauses a)
  (ll ([p (automaton-productions a)])
      (set-production-children!
         p (list->set (filter (lambda (cs) (not (ormap (lambda (c) (not (automaton-non-empty c))) cs)))
                   (set->list (production-children p)))))))

;; eliminate productions with no production clauses
(define (filter-empty-productions a)
  (set-automaton-productions! a (filter (lambda (p) (not (null? (set->list (production-children p)))))
                                        (automaton-productions a))))

(define (hash-key-subtract h1 h2)
  (define key-difference
    (set-subtract
      (hash-keys h1)
      (hash-keys h2)))
  (for/fold ([acc h1])
            ([key key-difference])
    (hash-remove acc key)))

;; This is intersection but we also filter out empty automata and productions
(define (intersect! a1 a2)
  ;; ** First, we compute the intersection while keeping track of
  ;; newly created automata **
  ;;
  ;; We remember which automata are newly created
  ;; by seeing which automata are added onto the old list of automata
  (define old-name-to-automaton-map name-to-automaton-map)
  ;; Do the actual intersection
  (define a (intersect-internal a1 a2))
  ;; Get the list of automata created while intersecting a1 and a2
  (define new (hash-values (hash-key-subtract name-to-automaton-map old-name-to-automaton-map)))

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

             [(symbol? t) (or (and (automaton-has-constructor? a 'symbol?) '(())) '())]
             [(number? t) (or (and (automaton-has-constructor? a 'number?) '(())) '())]
             [(eq? t #t)  (or (and (automaton-has-constructor? a 'true?)   '(())) '())]
             [(eq? t #f)  (or (and (automaton-has-constructor? a 'false?)  '(())) '())]
             [(null? t)   (or (and (automaton-has-constructor? a 'null?)   '(())) '())]
             [(pair? t)   (let ([p (automaton-has-constructor? a 'pair?)])
                            (or (and p
                                 ;; NOTE: this assumes all "pair?" constructors
                                 ;; have exactly two children
                                 (ll ([cs (set->list (production-children p))]
                                      [m (unfold (car cs) (car t))]
                                      [n (unfold (cadr cs) (cdr t))])
                                     (append m n)))
                                '()))])))])
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
