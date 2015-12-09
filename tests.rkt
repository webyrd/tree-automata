#lang racket

(require "tree.rkt")

(require rackunit)

(define unfold (make-unfold vector? (lambda (a b) a) '()))

;(define-syntax define-test-automata
  ;(lambda (stx)
    ;(syntax-case stx ()
      ;[(_ [name string-names non-empty productions ...] ...)
       ;#`(define-automata-internal [name 'string-names non-empty #f productions ...] ...)])))


;(check-equal? (combine-names '("a" "c" "d") '("b" "d" "e"))
  ;'("a" "b" "c" "d" "e"))

;(check-equal? (factor-productions '((cons 1 2) (cons 2 3) (cons 4 5) (foo 1) (foo 2) (nil) (string)))
  ;(list (production 'cons '((4 5) (2 3) (1 2)))
        ;(production 'foo '((2) (1)))
        ;(production 'nil '(()))
        ;(production 'string '(()))))

;(check-equal?
  ;(intersect-internal
    ;(make-automaton! '("a1")) (make-automaton! '("a2")))
  ;(make-automaton! '("a1" "a2") '() #t))

;(let ()
  ;(clear-caches!)

  ;(define sym (make-automaton! '("sym") (list (production 'sym '(())))))
  ;(define num (make-automaton! '("num") (list (production 'num '(())))))
  ;(define nil (make-automaton! '("nil") (list (production 'nil '(())))))
  ;(define term1 (make-automaton! '("term1")))
  ;(set-automaton-productions!
    ;term1
    ;(list (production 'cons (list (list term1 term1)))
          ;(production 'sym '(()))
          ;(production 'num '(()))))
  ;(define term2 (make-automaton! '("term2") #t))
  ;(set-automaton-productions!
    ;term2
    ;(list (production 'cons (list (list term2 term2)))
          ;(production 'nil '(()))
          ;(production 'sym '(()))
          ;(production 'num '(()))))
  ;(define binding (make-automaton! '("binding") #t
                                  ;(list (production 'cons (list (list sym term1))))))

  ;(define env (make-automaton! '("env") #t))
  ;(set-automaton-productions!
    ;env
    ;(list (production 'nil '(()))
          ;(production 'cons
                           ;(list (list binding env)))))


  ;(check-equal? (let ()
                  ;(define-automata
                    ;[env [nil] [cons binding env]]
                    ;[binding [cons sym term1]]
                    ;[term1 [cons term1 term1] [sym] [num]]
                    ;[term2 [cons term2 term2] [nil] [sym] [num]]
                    ;[sym [sym]]
                    ;[num [num]]
                    ;[nil [nil]])
                  ;env)
    ;env)

  ;(check-equal?
    ;(intersect env nil)
    ;(automaton '("env" "nil") #t (list (production 'nil '(())))))

  ;(check-equal?
    ;(intersect env env)
    ;env)

  ;(check-equal?
    ;(intersect env term1)
    ;(let ()
      ;(define-test-automata
        ;[a0 ("env" "term1") #t [cons a2 a0]]
        ;[a2 ("binding" "term1") #t [cons a3 a1]]
        ;[a1 ("term1") #t [cons a1 a1] [sym] [num]]
        ;[a3 ("sym" "term1") #t [sym]])
      ;a0))
  ;)

;(let ()
  ;(clear-caches!)

  ;(check-equal? (intersect term1 env)
    ;(let () (define-test-automata
              ;[a0 ("env" "term1") #t [cons a2 a0]]
              ;[a2 ("binding" "term1") #t [cons a3 a1]]
              ;[a1 ("term1") #t [cons a1 a1] [sym] [num]]
              ;[a3 ("sym" "term1") #t [sym]])
      ;a0))

  ;(check-equal? (intersect env term2)
    ;(let () (define-test-automata
              ;[a0 ("env" "term2") #t [nil] [cons a2 a0]]
              ;[a2 ("binding" "term2") #t [cons a3 a1]]
              ;[a3 ("sym" "term2") #t [sym]]
              ;[a1 ("term1" "term2") #t [cons a1 a1] [sym] [num]])
      ;a0))

  ;(build-inverse-map (list env term1 term2 sym num nil binding))
  ;[>
  ;((("sym")
    ;(#[automaton ("binding") (#[production cons ((#[automaton ("sym") (#[production sym ()])]
                                                  ;#0=#[automaton ("term1") (#[production cons ((#0#
                                                                                                ;#0#))]
                                                                            ;#[production sym (())]
                                                                            ;#[production num (())])]))])]
     ;#[automaton ("sym") (#[production sym ()])] #0#))
   ;(("term2")
    ;(#1=#[automaton ("term2") (#[production cons ((#1# #1#))]
                               ;#[production nil (())]
                               ;#[production sym (())]
                               ;#[production num (())])]
     ;#1# #1#)
    ;(#1# #1# #1#))
   ;(("term1")
    ;(#[automaton ("binding") (#[production cons ((#[automaton ("sym") (#[production sym ()])]
                                                  ;#0#))])]
     ;#[automaton ("sym") (#[production sym ()])] #0#)
    ;(#0# #0# #0#) (#0# #0# #0#))
   ;(("env")
    ;(#2=#[automaton ("env") (#[production nil (())]
                             ;#[production cons ((#[automaton ("binding") (#[production cons ((#[automaton ("sym") (#[production sym ()])]
                                                                                              ;#0#))])]
                                                 ;#2#))])]
     ;#[automaton ("binding") (#[production cons ((#[automaton ("sym") (#[production sym ()])]
                                                  ;#0#))])]
     ;#2#))
   ;(("binding")
    ;(#2#
     ;#[automaton ("binding") (#[production cons ((#[automaton ("sym") (#[production sym ()])]
                                                  ;#0#))])]
     ;#2#)))
  ;|#

  ;(assert (compute-automaton-non-empty env))
  ;(assert (compute-automaton-non-empty sym))

  ;(define a0 (make-automaton! '("a0") (list (production 'foo '()))))
  ;(define a1 (make-automaton! '("a1") (list (production 'foo '()))))
  ;(automaton-non-empty-set! a0 #f)
  ;(automaton-non-empty-set! a1 #f)
  ;(define a2 (make-automaton! '("a2") (list (production 'bar (list (list a0 a1))))))

  ;(assert (equal? '(1 2 3) (let ([x '(4 5 6)]) (prefix (append '(1 2 3) x) x))))

  ;)

;;; ---------

(let ()
  (clear-caches!)
  (define sym (make-automaton! '("sym-empty") (list (production 'sym '()))))
  (define num (make-automaton! '("num-empty") (list (production 'num '()))))
  (define nil (make-automaton! '("nil-empty") (list (production 'nil '(())))))
  (define term1 (make-automaton! '("term1-empty")))
  (set-automaton-productions!
    term1
    (list (production 'cons (list (list term1 term1)))
          (production 'sym '(()))
          (production 'num '(()))))
  (define term2 (make-automaton! '("term2-empty")))
  (set-automaton-productions!
    term2
    (list (production 'cons (list (list term2 term2)))
          (production 'nil '(()))
          (production 'sym '(()))
          (production 'num '(()))))
  (define binding (make-automaton! '("binding-empty")
                                 (list (production 'cons (list (list sym term1))))))

  (define env (make-automaton! '("env-empty")))
  (set-automaton-productions!
    env
    (list (production 'nil '(()))
          (production 'cons
                           (list (list binding env)))))
  (define foo (make-automaton! '("foo-empty")))
  (set-automaton-productions!
    foo (list (production 'cons (list (list env foo)))))
  (define bar1 (make-automaton! '("bar1-empty")))
  (define bar2 (make-automaton! '("bar2-empty")))
  (set-automaton-productions!
    bar1 (list (production 'cons (list (list env bar2)))))
  (set-automaton-productions!
    bar2 (list (production 'cons (list (list env bar1) (list env env)))))
  (define as (list sym num nil term1 term2 binding env foo bar1 bar2))
  (compute-non-empty* as)
  (assert (equal? '(#f #f #t #t #t #f #t #f #t #t)
                  (map automaton-non-empty as)))

  (for-each filter-empty-clauses as)
  ;; as ==> (#[automaton ("sym-empty") #f (#[production sym ()])] #[automaton ("num-empty") #f (#[production num ()])] #[automaton ("nil-empty") #f (#[production nil ()])] #0=#[automaton ("term1-empty") #t (#[production cons ((#0# #0#))] #[production sym (())] #[production num (())])] #1=#[automaton ("term2-empty") #t (#[production cons ((#1# #1#))] #[production nil (())] #[production sym (())] #[production num (())])] #[automaton ("binding-empty") #f (#[production cons ()])] #[automaton ("env-empty") #t (#[production nil (())] #[production cons ()])] #[automaton ("foo-empty") #f (#[production cons ()])] #2=#[automaton ("bar1-empty") #t (#[production cons ((#[automaton ("env-empty") #t (#[production nil (())] #[production cons ()])] #[automaton ("bar2-empty") #t (#[production cons ((#[automaton ("env-empty") #t (#[production nil (())] #[production cons ()])] #2#) (#[automaton ("env-empty") #t (#[production nil (())] #[production cons ()])] #[automaton ("env-empty") #t (#[production nil (())] #[production cons ()])]))])]))])] #[automaton ("bar2-empty") #t (#[production cons ((#[automaton ("env-empty") #t (#[production nil (())] #[production cons ()])] #2#) (#[automaton ("env-empty") #t (#[production nil (())] #[production cons ()])] #[automaton ("env-empty") #t (#[production nil (())] #[production cons ()])]))])])

  (for-each filter-empty-productions as)
  ;; as => (#[automaton ("sym-empty") #f ()] #[automaton ("num-empty") #f ()] #[automaton ("nil-empty") #f ()] #0=#[automaton ("term1-empty") #t (#[production cons ((#0# #0#))] #[production sym (())] #[production num (())])] #1=#[automaton ("term2-empty") #t (#[production cons ((#1# #1#))] #[production nil (())] #[production sym (())] #[production num (())])] #[automaton ("binding-empty") #f ()] #[automaton ("env-empty") #t (#[production nil (())])] #[automaton ("foo-empty") #f ()] #2=#[automaton ("bar1-empty") #t (#[production cons ((#[automaton ("env-empty") #t (#[production nil (())])] #[automaton ("bar2-empty") #t (#[production cons ((#[automaton ("env-empty") #t (#[production nil (())])] #2#) (#[automaton ("env-empty") #t (#[production nil (())])] #[automaton ("env-empty") #t (#[production nil (())])]))])]))])] #[automaton ("bar2-empty") #t (#[production cons ((#[automaton ("env-empty") #t (#[production nil (())])] #2#) (#[automaton ("env-empty") #t (#[production nil (())])] #[automaton ("env-empty") #t (#[production nil (())])]))])]))

  ;;;
  )

(let ()
  (clear-caches!)
  (define sym (make-automaton! '("sym-empty") (list (production 'sym '(())))))
  (define num (make-automaton! '("num-empty") (list (production 'num '(())))))
  (define nil (make-automaton! '("nil-empty") (list (production 'nil '(())))))
  (define term1 (make-automaton! '("term1-empty")))
  (set-automaton-productions!
    term1
    (list (production 'cons (list (list term1 term1)))
          (production 'sym '(()))
          (production 'num '(()))))
  (define term2 (make-automaton! '("term2-empty")))
  (set-automaton-productions!
    term2
    (list (production 'cons (list (list term2 term2)))
          (production 'nil '(()))
          (production 'sym '(()))
          (production 'num '(()))))
  (define binding (make-automaton! '("binding-empty")
                                 (list (production 'cons (list (list sym term1))))))

  (define env (make-automaton! '("env-empty")))
  (set-automaton-productions!
    env
    (list (production 'nil '(()))
          (production 'cons
                           (list (list binding env)))))
  (define foo (make-automaton! '("foo-empty")))
  (set-automaton-productions!
    foo (list (production 'cons (list (list env foo)))))
  (define bar1 (make-automaton! '("bar1-empty")))
  (define bar2 (make-automaton! '("bar2-empty")))
  (set-automaton-productions!
    bar1 (list (production 'cons (list (list env bar2)))))
  (set-automaton-productions!
    bar2 (list (production 'cons (list (list env bar1) (list env env)))))
  (define as (list sym num nil term1 term2 binding env foo bar1 bar2))
  (compute-non-empty* as)


  (assert (not (automaton-non-empty (intersect! (make-automaton! '("a1")) (make-automaton! '("a2"))))))

  (check-equal? (intersect! env nil)
    (automaton '("env-empty" "nil-empty") #t (list (production 'nil '(())))))

  (check-equal? (intersect! env env) env)

  (check-equal? (intersect! env term1) (automaton (list "env-empty" "term1-empty") #f '()))

  (check-equal? (intersect! env term2)
    (let () (define-test-automata
              [a0 ("env-empty" "term2-empty") #t [nil] [cons a2 a0]]
              [a1 ("term1-empty" "term2-empty") #t [cons a1 a1] [sym] [num]]
              [a2 ("binding-empty" "term2-empty") #t [cons a3 a1]]
              [a3 ("sym-empty" "term2-empty") #t [sym]])
      a0))

  ;(intersect! term1 env)
  ;(intersect! env term2)


  ;;;
  )

(let ()
  (clear-caches!)
  (define sym (make-automaton! '("sym-empty") (list (production 'symbol? '(())))))
  (define num (make-automaton! '("num-empty") (list (production 'number? '(())))))
  (define nil (make-automaton! '("nil-empty") (list (production 'null? '(())))))
  (define term1 (make-automaton! '("term1-empty")))
  (set-automaton-productions!
    term1
    (list (production 'pair? (list (list term1 term1)))
          (production 'symbol? '(()))
          (production 'number? '(()))))
  (define term2 (make-automaton! '("term2-empty")))
  (set-automaton-productions!
    term2
    (list (production 'pair? (list (list term2 term2)))
          (production 'null? '(()))
          (production 'symbol? '(()))
          (production 'number? '(()))))
  (define binding (make-automaton! '("binding-empty")
                                 (list (production 'pair? (list (list sym term1))))))

  (define env (make-automaton! '("env-empty")))
  (set-automaton-productions!
    env
    (list (production 'null? '(()))
          (production 'pair?
                           (list (list binding env)))))
  (define foo (make-automaton! '("foo-empty")))
  (set-automaton-productions!
    foo (list (production 'pair? (list (list env foo)))))
  (define bar1 (make-automaton! '("bar1-empty")))
  (define bar2 (make-automaton! '("bar2-empty")))
  (set-automaton-productions!
    bar1 (list (production 'pair? (list (list env bar2)))))
  (set-automaton-productions!
    bar2 (list (production 'pair? (list (list env bar1) (list env env)))))
  (define as (list sym num nil term1 term2 binding env foo bar1 bar2))
  (compute-non-empty* as)


  (check-equal? (unfold env '#(a)) `(((#(a) . ,env))))

  (check-equal? (unfold env '(#(a) . #(b))) `(((#(a) . ,binding)
                                       (#(b) . ,env))))

  (check-equal? (unfold env '((#(a) . #(b)) . #(c))) `(((#(a) . ,sym)
                                                (#(b) . ,term1)
                                                (#(c) . ,env))))

  )

(display 'done)
(display "\n")

;; TODO: tree-automaton equality at reification time


#|

;;; How we might use tree automata in miniKanren

(define-automata
  [env [nil] [cons binding env]]
  [binding [cons sym term]]
  [term [cons term term] [sym] [num]]
  [sym [sym]]
  [num [num]]
  [nil [nil]])

;;; nicer syntax:

(define-automata
  [env () [[symbol? . term] . env]]
  [term () symbol? number? [term . term]])

;;; or maybe

(define-automata
  [env [[symbol? . term] ...]]
  [term () symbol? number? [term . term]])


(define lookupo
  (lambda (x env val)
    (fresh (y v rest)
      (== `((,y . ,v) . ,rest) env)
      (conde
        ((== x y) (== v val))
        ((lookupo x rest val))))))

(define lookupoA
  (lambda (x env val)
    (fresh ()
      (automata-constraint sym x)
      (automata-constraint env environment)
      (automata-constraint term val)
      (fresh (y v rest)
        (automata-constraint sym y)
        (automata-constraint env rest)
        (automata-constraint term v)
        (== `((,y . ,v) . ,rest) env)
        (conde
          ((== x y) (== v val))
          ((lookupoA x rest val)))))))

(define lookupoA
  (lambda (x env val)
    (fresh (y v rest)
      (automata-constraints (sym x) (env environment) (term val))
      (== `((,y . ,v) . ,rest) env)
      (conde
        ((== x y) (== v val))
        ((lookupoA x rest val))))))

(define lookupoA
  (lambdaA ((sym x) (env environment) (term val))
    (freshA ((sym y) (env rest) (term v))
      (== `((,y . ,v) . ,rest) env)
      (conde
        ((== x y) (== v val))
        ((lookupoA x rest val))))))

(define lookupoR ;; maybe more Rackety
  (lambdaR (x:sym environment:env val:term)
    (freshA (y:sym rest:env v:term)
      (== `((,y . ,v) . ,rest) env)
      (conde
        ((== x y) (== v val))
        ((lookupoR x rest val))))))


(run* (environment value)
  (automata-constraint env environment)
  (== '((x . 5) (y . ()) (z . foo)) environment)
  (lookupo 'z environment value))

(run* (environment value)
  (== '((x . 5) (y . ()) (z . foo)) environment)
  (automata-constraint env environment)
  (lookupo 'z environment value))

(run* (environment value)
  (== '((x . 5) (y . ()) (z . foo)) environment)
  (lookupo 'z environment value)
  (automata-constraint env environment))

(run 10 (environment value)
  (automata-constraint env environment)
  (lookupo 'z environment value))

(run 10 (environment value)
  (lookupo 'z environment value)
  (automata-constraint env environment))

|#
