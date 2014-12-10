;; Need to implement unfolding and intersection

(define-record-type automaton
  (fields
   name ;; set-of symbol?
   (mutable productions) ;; list-of production?
   )
  (protocol
   (lambda (make)
     (lambda (name productions)
       (make name productions)))))

(define-record-type production
  (fields
   ;; NOTE: we factor out the constructor so we can iterate over every production that uses this constructor
   constructor ;; symbol? ;; TODO: primitives are constructor names
   children ;; set-of (list-of automaton?)
   ))

(define names->automaton '())
;; names->automaton :: alist (set-of symbol?) automaton?

(define-syntax ll
  (syntax-rules ()
    [(_ (l1 list1) (l2 list2) body)
     (filter (lambda (x) x)
             (apply append (map (lambda (l1) (map (lambda (l2) body) list2)) list1)))]))

;; NOTE: names are sets of symbols so we have a sensible name for intersects and take advantage of the associativity, communitivity and idempotency of intersect
;; NOTE: the name '() thus is the name of the universe
(define (combine-names n1 n2)
  (define (f l) ;; Eliminate duplicates from the list
    (cond
      [(or (null? l) (null? (cdr l))) l]
      [(string=? (car l) (cadr l)) (f (cdr l))]
      [else (cons (car l) (f (cdr l)))]))
  (f (list-sort string<? (append n1 n2))))

(define (clear-caches) (set! names->automaton '()))

(define new-automaton
  (case-lambda
   [(name) (new-automaton name '())]
   [(name productions)
    (let ([a (make-automaton name productions)])
      (assert (not (assoc name names->automaton)))
      (set! names->automaton (cons (cons name a) names->automaton))
      a)]))

(define (intersect a1 a2)
  (define name (combine-names (automaton-name a1) (automaton-name a2)))
  (define ps1 (automaton-productions a1))
  (define ps2 (automaton-productions a2))
  (cond
    [(assoc name names->automaton) => cdr]
    [else
     ;; We have to allocate the automaton in advance in case there is
     ;; a recursive loop
     (let ([a (new-automaton name)])
       (let ([ps
              ;; TODO: if we keep productions sorted by constructor name
              ;; then this outer loop could be done more efficiently by
              ;; traversing the lists in parallel
              (ll (p1 ps1) (p2 ps2) ;; iterate over each production/constructor
                  ;; filter for when the constructors are equal
                  (and (eq? (production-constructor p1)
                            (production-constructor p2))
                       (make-production
                        (production-constructor p1)
                        ;; iterate over each list of children
                        (ll (cs1 (production-children p1))
                            (cs2 (production-children p2))
                            ;; iterate over each child
                            (map intersect cs1 cs2)))))])
         (automaton-productions-set! a ps)
         a))]))

(define (intersect-driver a1 a2)
  ;; init cache of new automatons (we can get this by remembering pointer of names->automaton)
  (define old-names->automaton names->automaton)
  (define for-each-new ;; applies 'f' to each new automaton
    (case-lambda
      [(f) (for-each-new f names->automaton)]
      [(f n)
       (cond
         [(eq? n old-names->automaton) (values)]
         [else (f (car n)) (for-each-new f (cdr n))])]))
  (define a (intersect a1 a2))
  ;; Start with the pessimistic assumption about non-emptiness
  (for-each-new (lambda (a) (automaton-non-empty-set! a #f)))
  ;; construct the "inverse map"
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

