#lang racket/base

(require tree-automata)

(require rackunit)

(define unfold (make-unfold vector? (lambda (a b) a) '()))

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

  (check-equal? (unfold env '#(a)) `(((#(a) . ,env))))

  (check-equal? (unfold env '(#(a) . #(b))) `(((#(a) . ,binding)
                                       (#(b) . ,env))))

  (check-equal? (unfold env '((#(a) . #(b)) . #(c))) `(((#(a) . ,sym)
                                                (#(b) . ,term1)
                                                (#(c) . ,env)))))


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
