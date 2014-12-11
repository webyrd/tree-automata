(assert (equal? (combine-names '("a" "c" "d") '("b" "d" "e"))
                '("a" "b" "c" "d" "e")))

(intersect (new-automaton '("a1")) (new-automaton '("a2")))
;; ==> #[automaton ("a1" "a2") ()]

(clear-caches)
(define sym (new-automaton '("sym") (list (make-production 'sym '(())))))
(define num (new-automaton '("num") (list (make-production 'num '(())))))
(define nil (new-automaton '("nil") (list (make-production 'nil '(())))))
(define term1 (new-automaton '("term1")))
(automaton-productions-set!
 term1
 (list (make-production 'cons (list (list term1 term1)))
       (make-production 'sym '(()))
       (make-production 'num '(()))))
(define term2 (new-automaton '("term2")))
(automaton-productions-set!
 term2
 (list (make-production 'cons (list (list term2 term2)))
       (make-production 'nil '(()))
       (make-production 'sym '(()))
       (make-production 'num '(()))))
(define binding (new-automaton '("binding")
                               (list (make-production 'cons (list (list sym term1))))))

(define env (new-automaton '("env")))
(automaton-productions-set!
 env
 (list (make-production 'nil '(()))
       (make-production 'cons
                        (list (list binding env)))))
(intersect env nil)
;; ==> #[automaton ("env" "nil") (#[production nil ()])]

(intersect env env)
;; ==> env

(intersect env term1)
#|
#0=#[automaton ("env" "term1") (#[production cons ((#[automaton ("binding"
                                                                  "term1") (#[production cons ((#[automaton ("sym"
                                                                                                              "term1") (#[production sym ()])]
                                                                                                 #1=#[automaton ("term1") (#[production cons ((#1#
                                                                                                                                                #1#))]
                                                                                                                            #[production sym (())]
                                                                                                                            #[production num (())])]))])]
                                                     #0#))])]
|#

(clear-caches)
(intersect term1 env)
;; ==> same as with (intersect env term1)

(intersect env term2)
#|
#0=#[automaton ("env" "term2") (#[production nil (())]
                                 #[production cons ((#[automaton ("binding"
                                                                   "term2") (#[production cons ((#[automaton ("sym"
                                                                                                               "term2") (#[production sym ()])]
                                                                                                  #1=#[automaton ("term1"
                                                                                                                   "term2") (#[production cons ((#1#
                                                                                                                                                  #1#))]
                                                                                                                              #[production sym (())]
                                                                                                                              #[production num (())])]))])]
                                                      #0#))])]
|#

(build-inverse-map (list env term1 term2 sym num nil binding))
#|
((("sym")
   (#[automaton ("binding") (#[production cons ((#[automaton ("sym") (#[production sym ()])]
                                                  #0=#[automaton ("term1") (#[production cons ((#0#
                                                                                                 #0#))]
                                                                             #[production sym (())]
                                                                             #[production num (())])]))])]
     #[automaton ("sym") (#[production sym ()])] #0#))
  (("term2")
    (#1=#[automaton ("term2") (#[production cons ((#1# #1#))]
                                #[production nil (())]
                                #[production sym (())]
                                #[production num (())])]
      #1# #1#)
    (#1# #1# #1#))
  (("term1")
    (#[automaton ("binding") (#[production cons ((#[automaton ("sym") (#[production sym ()])]
                                                   #0#))])]
      #[automaton ("sym") (#[production sym ()])] #0#)
    (#0# #0# #0#) (#0# #0# #0#))
  (("env")
    (#2=#[automaton ("env") (#[production nil (())]
                              #[production cons ((#[automaton ("binding") (#[production cons ((#[automaton ("sym") (#[production sym ()])]
                                                                                                #0#))])]
                                                   #2#))])]
      #[automaton ("binding") (#[production cons ((#[automaton ("sym") (#[production sym ()])]
                                                    #0#))])]
      #2#))
  (("binding")
    (#2#
      #[automaton ("binding") (#[production cons ((#[automaton ("sym") (#[production sym ()])]
                                                    #0#))])]
      #2#)))
|#

(assert (compute-automaton-non-empty env))
(assert (compute-automaton-non-empty sym))

(define a0 (new-automaton '("a0") (list (make-production 'foo '()))))
(define a1 (new-automaton '("a1") (list (make-production 'foo '()))))
(automaton-non-empty-set! a0 #f)
(automaton-non-empty-set! a1 #f)
(define a2 (new-automaton '("a2") (list (make-production 'bar (list (list a0 a1))))))

(assert (equal? '(1 2 3) (let ([x '(4 5 6)]) (prefix (cons* 1 2 3 x) x))))

;;; ---------

(clear-caches)
(define sym (new-automaton '("sym-empty") (list (make-production 'sym '()))))
(define num (new-automaton '("num-empty") (list (make-production 'num '()))))
(define nil (new-automaton '("nil-empty") (list (make-production 'nil '(())))))
(define term1 (new-automaton '("term1-empty")))
(automaton-productions-set!
 term1
 (list (make-production 'cons (list (list term1 term1)))
       (make-production 'sym '(()))
       (make-production 'num '(()))))
(define term2 (new-automaton '("term2-empty")))
(automaton-productions-set!
 term2
 (list (make-production 'cons (list (list term2 term2)))
       (make-production 'nil '(()))
       (make-production 'sym '(()))
       (make-production 'num '(()))))
(define binding (new-automaton '("binding-empty")
                               (list (make-production 'cons (list (list sym term1))))))

(define env (new-automaton '("env-empty")))
(automaton-productions-set!
 env
 (list (make-production 'nil '(()))
       (make-production 'cons
                        (list (list binding env)))))
(define foo (new-automaton '("foo-empty")))
(automaton-productions-set!
 foo (list (make-production 'cons (list (list env foo)))))
(define bar1 (new-automaton '("bar1-empty")))
(define bar2 (new-automaton '("bar2-empty")))
(automaton-productions-set!
 bar1 (list (make-production 'cons (list (list env bar2)))))
(automaton-productions-set!
 bar2 (list (make-production 'cons (list (list env bar1) (list env env)))))
(define as (list sym num nil term1 term2 binding env foo bar1 bar2))
(compute-non-empty* as)
(assert (equal? '(#f #f #t #t #t #f #t #f #t #t)
                (map automaton-non-empty as)))

(for-each filter-empty-clauses as)
;; as ==> (#[automaton ("sym-empty") #f (#[production sym ()])] #[automaton ("num-empty") #f (#[production num ()])] #[automaton ("nil-empty") #f (#[production nil ()])] #0=#[automaton ("term1-empty") #t (#[production cons ((#0# #0#))] #[production sym (())] #[production num (())])] #1=#[automaton ("term2-empty") #t (#[production cons ((#1# #1#))] #[production nil (())] #[production sym (())] #[production num (())])] #[automaton ("binding-empty") #f (#[production cons ()])] #[automaton ("env-empty") #t (#[production nil (())] #[production cons ()])] #[automaton ("foo-empty") #f (#[production cons ()])] #2=#[automaton ("bar1-empty") #t (#[production cons ((#[automaton ("env-empty") #t (#[production nil (())] #[production cons ()])] #[automaton ("bar2-empty") #t (#[production cons ((#[automaton ("env-empty") #t (#[production nil (())] #[production cons ()])] #2#) (#[automaton ("env-empty") #t (#[production nil (())] #[production cons ()])] #[automaton ("env-empty") #t (#[production nil (())] #[production cons ()])]))])]))])] #[automaton ("bar2-empty") #t (#[production cons ((#[automaton ("env-empty") #t (#[production nil (())] #[production cons ()])] #2#) (#[automaton ("env-empty") #t (#[production nil (())] #[production cons ()])] #[automaton ("env-empty") #t (#[production nil (())] #[production cons ()])]))])])

(for-each filter-empty-productions as)
;; as => (#[automaton ("sym-empty") #f ()] #[automaton ("num-empty") #f ()] #[automaton ("nil-empty") #f ()] #0=#[automaton ("term1-empty") #t (#[production cons ((#0# #0#))] #[production sym (())] #[production num (())])] #1=#[automaton ("term2-empty") #t (#[production cons ((#1# #1#))] #[production nil (())] #[production sym (())] #[production num (())])] #[automaton ("binding-empty") #f ()] #[automaton ("env-empty") #t (#[production nil (())])] #[automaton ("foo-empty") #f ()] #2=#[automaton ("bar1-empty") #t (#[production cons ((#[automaton ("env-empty") #t (#[production nil (())])] #[automaton ("bar2-empty") #t (#[production cons ((#[automaton ("env-empty") #t (#[production nil (())])] #2#) (#[automaton ("env-empty") #t (#[production nil (())])] #[automaton ("env-empty") #t (#[production nil (())])]))])]))])] #[automaton ("bar2-empty") #t (#[production cons ((#[automaton ("env-empty") #t (#[production nil (())])] #2#) (#[automaton ("env-empty") #t (#[production nil (())])] #[automaton ("env-empty") #t (#[production nil (())])]))])]))

;;;

(clear-caches)
(define sym (new-automaton '("sym-empty") (list (make-production 'sym '(())))))
(define num (new-automaton '("num-empty") (list (make-production 'num '(())))))
(define nil (new-automaton '("nil-empty") (list (make-production 'nil '(())))))
(define term1 (new-automaton '("term1-empty")))
(automaton-productions-set!
 term1
 (list (make-production 'cons (list (list term1 term1)))
       (make-production 'sym '(()))
       (make-production 'num '(()))))
(define term2 (new-automaton '("term2-empty")))
(automaton-productions-set!
 term2
 (list (make-production 'cons (list (list term2 term2)))
       (make-production 'nil '(()))
       (make-production 'sym '(()))
       (make-production 'num '(()))))
(define binding (new-automaton '("binding-empty")
                               (list (make-production 'cons (list (list sym term1))))))

(define env (new-automaton '("env-empty")))
(automaton-productions-set!
 env
 (list (make-production 'nil '(()))
       (make-production 'cons
                        (list (list binding env)))))
(define foo (new-automaton '("foo-empty")))
(automaton-productions-set!
 foo (list (make-production 'cons (list (list env foo)))))
(define bar1 (new-automaton '("bar1-empty")))
(define bar2 (new-automaton '("bar2-empty")))
(automaton-productions-set!
 bar1 (list (make-production 'cons (list (list env bar2)))))
(automaton-productions-set!
 bar2 (list (make-production 'cons (list (list env bar1) (list env env)))))
(define as (list sym num nil term1 term2 binding env foo bar1 bar2))
(compute-non-empty* as)


(assert (not (automaton-non-empty (intersect-driver (new-automaton '("a1")) (new-automaton '("a2"))))))

(intersect-driver env nil)
;; ==> #[automaton ("env-empty" "nil-empty") #t (#[production nil (())])]

(intersect-driver env env)
;; ==> env
(intersect-driver env term1)
;; ==> #[automaton ("env-empty" "term1-empty") #f ()]
(intersect-driver env term2)
#|
==> #0=#[automaton ("env-empty" "term2-empty") #t (#[production nil (())]
                                                #[production cons ((#[automaton ("binding-empty"
                                                                                  "term2-empty") #t (#[production cons ((#[automaton ("sym-empty"
                                                                                                                                       "term2-empty") #t (#[production sym (())])]
                                                                                                                          #1=#[automaton ("term1-empty"
                                                                                                                                           "term2-empty") #t (#[production cons ((#1#
                                                                                                                                                                                   #1#))]
                                                                                                                                                               #[production sym (())]
                                                                                                                                                               #[production num (())])]))])]
                                                                     #0#))])]
|#

;(intersect-driver term1 env)
;(intersect-driver env term2)


;;;

(clear-caches)
(define sym (new-automaton '("sym-empty") (list (make-production 'symbol? '(())))))
(define num (new-automaton '("num-empty") (list (make-production 'number? '(())))))
(define nil (new-automaton '("nil-empty") (list (make-production 'null? '(())))))
(define term1 (new-automaton '("term1-empty")))
(automaton-productions-set!
 term1
 (list (make-production 'pair? (list (list term1 term1)))
       (make-production 'symbol? '(()))
       (make-production 'number? '(()))))
(define term2 (new-automaton '("term2-empty")))
(automaton-productions-set!
 term2
 (list (make-production 'pair? (list (list term2 term2)))
       (make-production 'null? '(()))
       (make-production 'symbol? '(()))
       (make-production 'number? '(()))))
(define binding (new-automaton '("binding-empty")
                               (list (make-production 'pair? (list (list sym term1))))))

(define env (new-automaton '("env-empty")))
(automaton-productions-set!
 env
 (list (make-production 'null? '(()))
       (make-production 'pair?
                        (list (list binding env)))))
(define foo (new-automaton '("foo-empty")))
(automaton-productions-set!
 foo (list (make-production 'pair? (list (list env foo)))))
(define bar1 (new-automaton '("bar1-empty")))
(define bar2 (new-automaton '("bar2-empty")))
(automaton-productions-set!
 bar1 (list (make-production 'pair? (list (list env bar2)))))
(automaton-productions-set!
 bar2 (list (make-production 'pair? (list (list env bar1) (list env env)))))
(define as (list sym num nil term1 term2 binding env foo bar1 bar2))
(compute-non-empty* as)


(unfold env '#(a))
#|
(((#(a)
   . #0=#[automaton ("env-empty") #t (#[production nil (())]
                                       #[production cons ((#[automaton ("binding-empty") #t (#[production cons ((#[automaton ("sym-empty") #t (#[production sym (())])]
                                                                                                                  #1=#[automaton ("term1-empty") #t (#[production cons ((#1#
                                                                                                                                                                          #1#))]
                                                                                                                                                      #[production sym (())]
                                                                                                                                                      #[production num (())])]))])]
                                                            #0#))])])))
|#

(unfold env '(#(a) . #(b)))
#| ==>
(((#(a)
   . #[automaton ("binding-empty") #t (#[production pair? ((#[automaton ("sym-empty") #t (#[production symbol? (())])]
                                                             #0=#[automaton ("term1-empty") #t (#[production pair? ((#0#
                                                                                                                      #0#))]
                                                                                                 #[production symbol? (())]
                                                                                                 #[production number? (())])]))])])
   (#(b)
    . #1=#[automaton ("env-empty") #t (#[production null? (())]
                                        #[production pair? ((#[automaton ("binding-empty") #t (#[production pair? ((#[automaton ("sym-empty") #t (#[production symbol? (())])]
                                                                                                                     #0#))])]
                                                              #1#))])])))
|#

(unfold env '((#(a) . #(b)) . #(c)))
#| ==>
(((#(a)
   . #[automaton ("sym-empty") #t (#[production symbol? (())])])
   (#(b)
    . #0=#[automaton ("term1-empty") #t (#[production pair? ((#0#
                                                               #0#))]
                                          #[production symbol? (())]
                                          #[production number? (())])])
   (#(c)
    . #1=#[automaton ("env-empty") #t (#[production null? (())]
                                        #[production pair? ((#[automaton ("binding-empty") #t (#[production pair? ((#[automaton ("sym-empty") #t (#[production symbol? (())])]
                                                                                                                     #0#))])]
                                                              #1#))])])))
|#


;; TODO: tree-automaton equality at reification time
