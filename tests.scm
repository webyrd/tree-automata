(assert (equal? (combine-names '("a" "c" "d") '("b" "d" "e"))
                '("a" "b" "c" "d" "e")))

(intersect (new-automaton '("a1")) (new-automaton '("a2")))
;; ==> #[automaton ("a1" "a2") ()]

(clear-caches)
(define sym (new-automaton '("sym") (list (make-production 'sym '()))))
(define num (new-automaton '("num") (list (make-production 'num '()))))
(define nil (new-automaton '("nil") (list (make-production 'nil '()))))
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


;; TODO:
;;   need to implement unfolding
;;   need to test intersection driver (maybe call this intersect-and-check-emptiness)
;;     read that code
;;     get a better syntax
;;     factor out some helpers
;;     unit test those helpers
;;     TEST IT
;;   Integrate into minikanren
