#lang racket


;(define sym (new-automaton '("sym")))
;(define num (new-automaton '("num")))
;(define nil (new-automaton '("nil") ))
;(define term (new-automaton '("term")))
;(define binding (new-automaton '("binding")))
;(define env (new-automaton '("env")))

;(automaton-productions-set!
  ;sym
  ;(list (make-production 'symbol? '(()))))

;(automaton-productions-set!
  ;num
  ;(list (make-production 'number? '(()))))

;(automaton-productions-set!
  ;nil
  ;(list (make-production 'null? '(()))))

;(automaton-productions-set!
 ;term
 ;(list (make-production 'pair? (list (list term term)))
       ;(make-production 'symbol? '(()))
       ;(make-production 'number? '(()))))

;(automaton-productions-set!
  ;binding
  ;(list (make-production 'pair? (list (list sym term)))))

;(automaton-productions-set!
 ;env
 ;(list (make-production 'null? '(()))
       ;(make-production 'pair?
                        ;(list (list binding env)))))

(define (alist-update alist key value)
  (let ([p (assv key alist)])
    (if p
      (cons (cons key value)
            (remq p alist))
      (cons (cons key value) alist))))

(define (alist-lookup alist key default)
  (let ([p (assv key alist)])
    (if p
      (cdr p)
      default)))

(define (factor-productions prods)
  (define factored
    (reverse
      (fold-left
        (lambda (acc prod)
          (let* ([ctor (car prod)]
                 [children (cadr prod)]
                 [set-of-lists-of-children (alist-lookup acc ctor '())])
            (alist-update acc ctor (cons children set-of-lists-of-children))))
        '()
        prods)))

  (map (lambda (p)
         (let ([ctor (car p)]
               [set-of-lists-of-children (cdr p)])
           (make-production ctor set-of-lists-of-children)))
       factored))

(define-syntax define-automata
  (lambda (stx)
    (syntax-case stx ()
                 [(_
                    [name (ctor args ...)
                          (ctor* args* ...)
                          ...]
                    ...)
                  (with-syntax
                    ([(string* ...) (map symbol->string (syntax->datum #'(name ...)))])
                    #'(begin
                        (define name (new-automaton '(string*)))
                        ...

                        (automaton-productions-set!
                          name
                          (factor-productions
                            (list
                              (list 'ctor (list args ...))
                              (list 'ctor* (list args* ...))
                              ...)))
                        ...))])))



(define-automata
  [sym (symbol?)]
  [nil (null?)]
  [term (pair? [term term])
        (symbol?)
        (number?)]
  [binding (pair? [sym term])]
  [env (null?)
       (pair? [binding env])])

