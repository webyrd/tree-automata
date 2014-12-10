;; Need to implement unfolding and intersection

(define-record-type automaton
  (fields
   name ;; set-of symbol?
   (mutable productions) ;; list-of production?
   )
  (protocol
   (lambda (make)
     (define f
       (case-lambda
         [(names->automaton name) (f names->automaton name '())]
         [(names->automaton name productions)
          (let ([a (make name productions)])
            (assert (not (assoc name (box-value names->automaton))))
            (box-value-set! names->automaton
                            (cons (cons name a) (box-value names->automaton)))
            a)]))
     f)))

(define-record-type production
  (fields
   ;; NOTE: we factor out the constructor so we can iterate over every production that uses this constructor
   constructor ;; symbol? ;; TODO: primitives are constructor names
   children ;; set-of (list-of automaton?)
   ))

(define-record-type box (fields (mutable value)))
(define names->automaton (make-box '()))
;; names->automaton :: alist (set-of symbol?) automaton?

;; ll computes a tree comprehension.  It also filters out any results
;; that are #f and each binding in the comprehension is in the scope
;; of the RHS of each of the following bindings.
(define-syntax ll
  (syntax-rules ()
    [(_ ([l1 list1]) body)
     (filter (lambda (x) x) (map (lambda (l1) body) list1))]
    [(_ ([l1 list1] . rest) body)
     (apply append (map (lambda (l1) (ll rest body)) list1))]))

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

#;(define (intersect-driver a1 a2)
  ;; We remember which automata are newly created
  ;; by seeing which automata are added onto the old list of automata
  (define old-names->automaton (box-value names->automaton))
  (define for-each-new ;; applies 'f' to each new automaton
    (case-lambda
      [(f) (for-each-new f names->automaton)]
      [(f n)
       (cond
         [(eq? (box-value n) old-names->automaton) (values)]
         [else (f (car (box-value n))) (for-each-new f (cdr (box-value n)))])]))
  (define a (intersect a1 a2)) ;; The top-level new automaton
  ;; Start with the pessimistic assumption about non-emptiness
  (for-each-new (lambda (a) (automaton-non-empty-set! a #f)))
  ;; construct the "inverse map".  This maps automata names to the productions of the automata that contain them.
  ;; We could make this more efficient if we stored this inside the automaton instead of as an external assoc list
  (define inverse-map '()) ;; multi-map names=(list-of sym) (cons parent=automaton children=(list automaton))
  (for-each-new (lambda (a)
                  (for-each
                    (lambda (p)
                      (for-each
                        (lambda (cs)
                          (for-each
                            (lambda (c)
                              (define entry (cons a cs))
                              (define cell (assoc (automata-name c) inverse-map))
                              (if cell
                                  ;; Add to whatever was there before
                                  (set-cdr! cell (cons (entry (cdr cell))))
                                  ;; Add a new entry
                                  (set! inverse-map (cons (cons (automata-name c) (list entry)) inverse-map))))
                            cs))
                        (production-children p)))
                    (automata-productions a))))
  (define queue '())
  (define (pop-queue) (define x (car queue)) (set! queue (cdr queue)) x)
  (for-each-new (lambda (a)
                  ;; put in only those that are known to be non-empty
                  (and (exists (lambda (p) (exists (lambda (cs) (for-all (lambda (c) (automata-non-empty c)) cs)) (production-children))) (automata-productions a))
                       (set! queue (cons a queue)))))
  (define (run)
    (cond
      [(null? queue) (values)]
      [else (check (pop-queue)) (run)]))
  (define (check a)
    (or (automaton-non-empty a)
        (for-each (lambda (entry)
                    (or (automaton-non-empty (car entry))
                        (and (for-all automaton-non-empty (cdr entry))
                             (begin (automaton-non-empty-set! (car entry) #t)
                                    (set! queue (cons (car entry) queue))))))
                  (assoc (automaton-name a) inverse-map))))
  (run)
  ;; eliminate production clauses calling non-non-empty automata
  (for-each-new (lambda (a)
                  (for-each (lambda (p)
                              (production-children-set! p
                                                        (filter (lambda (cs) (exists (lambda (c) (not (automaton-non-empty c))) cs)) (production-children p))))
                            (automaton-productions a))))
  ;; eliminate productions with no production clauses
  (for-each-new (lambda (a)
                  (automata-productions-set! a
                    (filter (lambda (p) (null? (production-children p)))))))
  ;; set automata with no productions to be not-non-empty
  (for-each-new (lambda (a) (automata-non-empty-set! a (not (null? (automata-productions a))))))
  a
  )

;; TODO: when reifying find tree automaton that are equal and give them
;; the same name (or at least for tree automaton that are equal to the intersection
;; of some subset of automata that were interesected to make them)

(define (unfold a t) ???)
(define (non-empty? a) ???)

