#lang racket/base

; Note that the hash-procs we define for automaton and production are awful (constant),
;    so these objects should not currently be used as keys in hash maps or in sets
;    anywhere that performance is at all relevant. The equal? we define probably
;    suffers in performance because of this, but it's only used for tests so we don't care.

; We tried using racket's sets for the automaton-productions and production-children, but
;    because we needed to construct cyclic terms we were mutating elements after adding them to the sets,
;    and thus getting bad behavior. Don't make that mistake again!

; is it actually necessary to have the initial automata in the cache, or just the intersected automata?
; If so, logic from make-automaton could move to intersect!

; Assumptions:
; - automata names are unique
; - any user-defined automata should be passed to 'compute-non-empty*' before use

(require
  (only-in racket/match match-let)
  (only-in racket/set list->set set)
  (only-in racket/list remove-duplicates)
  (for-syntax racket/base))

(provide
  define-automata
  make-unfold
  intersect!
  clear-caches!
  automaton-non-empty
  automaton-name)

(module+ test
  (require rackunit))

(struct automaton [name [productions #:mutable] [non-empty #:mutable]]
        #:transparent
        #:methods gen:equal+hash
        [(define (equal-proc a1 a2 rec)
           (match-let ([(automaton name1 productions1 non-empty1) a1]
                       [(automaton name2 productions2 non-empty2) a2])
             (and (rec name1 name2)
                  (rec (list->set productions1) (list->set productions2))
                  (rec non-empty1 non-empty2))))
         (define (hash-proc a rec) 1)
         (define (hash2-proc a rec) 2)])

(define (make-automaton! name [productions '()] [non-empty #t] [use-cache #t])
  (let ([a (automaton name productions non-empty)])
    (when use-cache
      (when (assoc name name-to-automaton-map)
        (error 'make-automaton! "automata names should be unique, but this automaton '~s' is already registered" name))
      (set! name-to-automaton-map (cons (cons name a) name-to-automaton-map)))
    a))

; Given a list of production descriptions like '(pair? a1 a2) and lifts the productions
; with common constructors and returns a list of production objects, one per constructor.
(define (factor-productions prods)
  (define factored
    (for/fold ([acc (hash)])
              ([prod prods])
      (match-let ([(cons ctor children) prod])
        (hash-update acc ctor (lambda (s) (cons children s)) '()))))

  (for/list ([(ctor children-set) factored])
    (production ctor children-set)))

(module+ test
  (check-equal?
    (list->set (factor-productions '((cons 1 2) (cons 2 3) (cons 4 5) (foo 1) (foo 2) (nil) (string))))
    (set (production 'cons (list '(4 5) '(2 3) '(1 2)))
         (production 'foo (list '(2) '(1)))
         (production 'nil (list '()))
         (production 'string (list '())))))

; Defines automata without checking emptiness and allowing explicit specification of
; the automata name. Useful for defining automata to compare to the results of intersection
; in tests.
(define-syntax define-automata-internal
  (lambda (stx)
    (syntax-case stx ()
      [(_ [def-name string-names non-empty use-cache [ctor children ...] ...] ...)
       #`(begin
           (define def-name (make-automaton! string-names '() non-empty use-cache))
           ...
           (set-automaton-productions!
             def-name
             (factor-productions (list [list 'ctor children ...] ...)))
           ...
           )])))

; Defines automata named the same as the variable into which they are defined.
; Throws an error if any of the automata are empty.
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

; Assuming the non-empty flag of all children is correct, return whether this
; automata should be considered empty.
(define (compute-automaton-non-empty a)
  (for*/or ([p (automaton-productions a)]
            [cs (production-children p)])
    (for/and ([c cs])
      (automaton-non-empty c))))

(struct production [constructor [children #:mutable]]
        #:transparent
        #:methods gen:equal+hash
        [(define (equal-proc p1 p2 rec)
           (match-let ([(production ctor1 children1) p1]
                       [(production ctor2 children2) p2])
             (and (rec ctor1 ctor2)
                  (rec (list->set children1) (list->set children2)))))
         (define (hash-proc p rec) 1)
         (define (hash2-proc p rec) 2)])


;; name-to-automaton-map :: alist (set-of symbol?) automaton?
(define name-to-automaton-map '())

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
  (remove-duplicates (sort (append n1 n2) string<?)))

(module+ test
  (check-equal?
    (combine-names '("a" "c" "d") '("b" "d" "e"))
    '("a" "b" "c" "d" "e")))

(define (clear-caches!)
  (set! name-to-automaton-map '()))

(define (intersect-internal a1 a2)
  (define name (combine-names (automaton-name a1) (automaton-name a2)))
  (define ps1 (automaton-productions a1))
  (define ps2 (automaton-productions a2))
  (cond
    [(assoc name name-to-automaton-map) => cdr]
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
                        (ll ([cs1 (production-children p1)]
                             [cs2 (production-children p2)])
                            ;; iterate over each child
                            (map intersect-internal cs1 cs2)))))])
         (set-automaton-productions! a ps)
         a))]))

(module+ test
  (check-equal?
    (intersect-internal
      (make-automaton! '("a1")) (make-automaton! '("a2")))
    (make-automaton! '("a1" "a2") '() #t #f))

  (define-syntax define-test-automata
    (lambda (stx)
      (syntax-case stx ()
        [(_ [name string-names non-empty productions ...] ...)
         #`(define-automata-internal [name 'string-names non-empty #f productions ...] ...)])))

  (let ()
    (clear-caches!)

    (define sym (make-automaton! '("sym") (list (production 'symbol? (list '())))))
    (define num (make-automaton! '("num") (list (production 'number? (list '())))))
    (define nil (make-automaton! '("nil") (list (production 'null? (list '())))))
    (define term1 (make-automaton! '("term1")))
    (set-automaton-productions!
      term1
      (list (production 'pair? (list (list term1 term1)))
           (production 'symbol? (list '()))
           (production 'number? (list '()))))
    (define term2 (make-automaton! '("term2")))
    (set-automaton-productions!
      term2
      (list (production 'pair? (list (list term2 term2)))
           (production 'null? (list '()))
           (production 'symbol? (list '()))
           (production 'number? (list '()))))
    (define binding (make-automaton! '("binding") (list (production 'pair? (list (list sym term1))))))

    (define env (make-automaton! '("env")))
    (set-automaton-productions!
      env
      (list (production 'pair?
                        (list (list binding env)))
            (production 'null? (list '()))))

    (clear-caches!)


    (define other-env
      (let ()
        (define-automata
          [env [null?] [pair? binding env]]
          [binding [pair? sym term1]]
          [term1 [pair? term1 term1] [symbol?] [number?]]
          [term2 [pair? term2 term2] [null?] [symbol?] [number?]]
          [sym [symbol?]]
          [num [number?]]
          [nil [null?]])
        env))


    (check-equal?
      other-env
      env)

    (check-equal?
      (intersect-internal env nil)
      (automaton '("env" "nil") (list (production 'null? (list '()))) #t))

    (check-equal?
      (intersect-internal env env)
      env)

    (check-equal?
      (intersect-internal env term1)
      (let ()
        (define-test-automata
          [a0 ("env" "term1") #t [pair? a2 a0]]
          [a2 ("binding" "term1") #t [pair? a3 a1]]
          [a1 ("term1") #t [pair? a1 a1] [symbol?] [number?]]
          [a3 ("sym" "term1") #t [symbol?]])
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
       [p (automaton-productions a)]
       [cs (production-children p)]
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
      (unbox (cdr (assoc (automaton-name sym) inverse-map)))
      (list (cons binding (car (production-children (car (automaton-productions binding))))))
      "The inverse association for the sym automata should consist of a set containing one value:
         a pair of the binding automata and the one list of children for its pair constructor.")

    (check-true (compute-automaton-non-empty env))
    (check-true (compute-automaton-non-empty sym))))

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
  (run))

;; eliminate production clauses calling non-non-empty automata
(define (filter-empty-clauses a)
  (ll ([p (automaton-productions a)])
      (set-production-children!
         p (filter (lambda (cs) (not (ormap (lambda (c) (not (automaton-non-empty c))) cs)))
                   (production-children p)))))

;; eliminate productions with no production clauses
(define (filter-empty-productions a)
  (set-automaton-productions! a (filter (lambda (p) (not (null? (production-children p))))
                                                   (automaton-productions a))))


(module+ test
  (let ()
    (clear-caches!)
    (define sym (make-automaton! '("sym") (list (production 'symbol? (list '())))))
    (define num (make-automaton! '("num") (list (production 'number? (list '())))))
    (define nil (make-automaton! '("nil") (list (production 'null? (list '())))))
    (define term1 (make-automaton! '("term1")))
    (set-automaton-productions!
      term1
      (list (production 'pair? (list (list term1 term1)))
           (production 'symbol? (list '()))
           (production 'number? (list '()))))
    (define term2 (make-automaton! '("term2")))
    (set-automaton-productions!
      term2
      (list (production 'pair? (list (list term2 term2)))
           (production 'null? (list '()))
           (production 'symbol? (list '()))
           (production 'number? (list '()))))
    (define binding (make-automaton! '("binding") (list (production 'pair? (list (list sym term1))))))

    (define env (make-automaton! '("env")))
    (set-automaton-productions!
      env
      (list (production 'pair?
                        (list (list binding env)))
            (production 'null? (list '()))))

    ; Self-recursive, with no base case. Should be empty.
    (define foo (make-automaton! '("foo-empty")))
    (set-automaton-productions!
      foo (list (production 'pair? (list (list env foo)))))

    ; Mutually recursive, with a base case. Should be non-empty.
    (define bar1 (make-automaton! '("bar1-empty")))
    (define bar2 (make-automaton! '("bar2-empty")))
    (set-automaton-productions!
      bar1 (list (production 'pair? (list (list env bar2)))))
    (set-automaton-productions!
      bar2 (list (production 'pair? (list (list env bar1) (list env env)))))

    ; Mutually recursive with no base case. Should be empty.
    (define im-so-empty (make-automaton! '("im-so-empty")))
    (define oh-so-empty (make-automaton! '("oh-so-empty")))
    (set-automaton-productions!
      im-so-empty (list (production 'pair? (list (list env oh-so-empty)))))
    (set-automaton-productions!
      oh-so-empty (list (production 'pair? (list (list env im-so-empty)))))

    (define type-confusion! (intersect! sym num))

    (define as (list sym num nil term1 term2 binding env foo bar1 bar2 im-so-empty oh-so-empty type-confusion!))
    (compute-non-empty* as)
    (check-equal? (map automaton-non-empty as)
                  '(#t #t #t #t #t #t #t #f #t #t #f #f #f))))

(define (prefix new old)
  (cond
    [(eq? new old) '()]
    [else (cons (car new) (prefix (cdr new) old))]))

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
  (define new (map cdr (prefix name-to-automaton-map old-name-to-automaton-map)))

  ;; ** Next, we have to compute the non-emptiness
  (compute-non-empty* new)
  ;; NOTE: this code is side-effecting the automata in 'new'.
  ;; A nicer way to write this would be in terms of a 'map-graph'
  ;; that applies a particular function to every object in a graph
  (for-each filter-empty-clauses new)
  (for-each filter-empty-productions new)
  a)

(module+ test
  (let ()
    (clear-caches!)

    (define-automata
      [env [null?] [pair? binding env]]
      [binding [pair? num num]]
      [term2 [pair? term2 term2] [null?] [number?]]
      [num [number?]]
      [nil [null?]])

    (check-equal? (intersect! env term2)
                  (let () (define-test-automata
                            [a0 ("env" "term2") #t [pair? a2 a0] [null?]]
                            [a1 ("num" "term2") #t [number?]]
                            [a2 ("binding" "term2") #t [pair? a1 a1]]
                            )
                    a0)))

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

    (define a1 (make-automaton! '("a1")))
    (define a2 (make-automaton! '("a2")))
    (compute-non-empty* (list a1 a2))

    (check-true (not (automaton-non-empty a1)))
    (check-true (not (automaton-non-empty a2)))

    (check-true (not (automaton-non-empty (intersect! a1 a2))))

    ;(check-true (not (automaton-non-empty (intersect! (make-automaton! '("a1")) (make-automaton! '("a2"))))))

    (check-equal? (intersect! env nil)
                  (automaton '("env" "nil") (list (production 'null? (list '()))) #t))

    (check-equal? (intersect! env env) env)

    (check-equal? (intersect! env term1) (automaton (list "env" "term1") '() #f))

    (check-equal? (intersect! env term2)
                  (let () (define-test-automata
                            [a0 ("env" "term2") #t [null?] [pair? a2 a0]]
                            [a1 ("term1" "term2") #t [pair? a1 a1] [symbol?] [number?]]
                            [a2 ("binding" "term2") #t [pair? a3 a1]]
                            [a3 ("sym" "term2") #t [symbol?]])
                    a0))))


;; TODO: when reifying find tree automaton that are equal and give them
;; the same name (or at least for tree automaton that are equal to the intersection
;; of some subset of automata that were interesected to make them)

(define (automaton-has-constructor? a c)
  (findf (lambda (p) (eq? c (production-constructor p))) (automaton-productions a)))

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
                                 (ll ([cs (production-children p)]
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

