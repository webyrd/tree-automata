;;; test-equal.sps - tests for equality predicates in equal.sls

;;; Copyright (c) 2008 Michael D. Adams and R. Kent Dybvig
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation files
;;; (the "Software"), to deal in the Software without restriction,
;;; including without limitation the rights to use, copy, modify, merge,
;;; publish, distribute, sublicense, and/or sell copies of the Software,
;;; and to permit persons to whom the Software is furnished to do so,
;;; subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;;; SOFTWARE.

;;; This program contains a handful of tests for the equality predicates
;;; defined in equal.sls.  The set of tests is far from exhaustive.  We
;;; would appreciate contributions of additional tests.

;;; By default, the tests are run on the built-in equal? predicate.
;;; Comment out the appropriate lines at the end of this file to avoid
;;; this behavior on systems without a working equal? implementation.

;;; Running this program should produce the following output:
;;;
;;; running r5rs-checks on r5rs-equal? ... done
;;; running r5rs-checks on uf-equal? ... done
;;; running r5rs-checks on precheck/uf-equal? ... done
;;; running r5rs-checks on interleave-equal? ... done
;;; running r5rs-checks on precheck/interleave-equal? ... done
;;; running r5rs-checks on equal? ... done
;;; running r6rs-checks on uf-equal? ... done
;;; running r6rs-checks on precheck/uf-equal? ... done
;;; running r6rs-checks on interleave-equal? ... done
;;; running r6rs-checks on precheck/interleave-equal? ... done
;;; running r6rs-checks on equal? ... done

(import (rnrs) (rnrs mutable-pairs) (equal))

(define-syntax test
  (lambda (x)
    (syntax-case x ()
      [(_ expr result)
       #`(unless (equal? expr result)
           (raise
             (condition
               (make-message-condition "test failed")
               (make-syntax-violation #'#,x #f))))])))

(define (make-list n x)
  (if (fx=? n 0)
      '()
      (cons x (make-list (fx- n 1) x))))

(define (last-pair ls)
  (if (pair? (cdr ls))
      (last-pair (cdr ls))
      ls))


(define (record-checks equal?)
  (define-record-type foo)
  (define-record-type bar)
  (define-record-type baz (fields (mutable x) (mutable y)))
  (test (equal? (make-foo) (make-foo)) #t)
  (test (equal? (make-foo) (make-bar)) #f)
  (let ([x (make-baz 1 2)]
        [y (make-baz 1 3)])
    (test (equal? x y) #f)
    (baz-y-set! y 2)
    (test (equal? x y) #t)
    (baz-y-set! y x)
    (baz-y-set! x y)
    (test (equal? x y) #t)
    (baz-x-set! y 4)
    (test (equal? x y) #f)))


(define (r5rs-checks equal?)
  (test (equal? '(a b c) '(a b c)) #t)
  (test (equal? 'a 'a) #t)
  (test (equal? '(a b (c)) "hi") #f)
  (test (equal? '(a b (c)) (list 'a 'b '(c))) #t)
  (test (equal? '(a b (c)) '(a b (d))) #f)
  (test (equal? 123124211123 123124211123) #t)
  (test (equal? 123124211123 123124211124) #f)
  (test (equal? "hi there" (string-append "hi " "there")) #t)
  (test (equal? "hi there " "hi there") #f)
  (test (equal? (vector 1 2 (vector 3 4) 5) '#(1 2 #(3 4) 5)) #t)
  (test (equal? (vector 1 2 (vector 3 4) 5) '#(1 2 3 4 5)) #f)
  (test (equal? +inf.0 +inf.0) #t)
  (test (equal? -inf.0 -inf.0) #t)
  (test (equal? -inf.0 +inf.0) #f)
  (test (equal? +0.0 +0.0) #t)
  (test (equal? -0.0 -0.0) #t)
  (test (equal? +0.0 -0.0) #f)
  (test (equal? 3.0+0.0i 3.0+0.0i) #t)
  (test (equal? 3.0-0.0i 3.0-0.0i) #t)
  (test (equal? 3.0+0.0i 3.0-0.0i) #f)
  (test (equal? 3.0+0.0i 3.0) #f)
  (test (equal? 3.0 3) #f)
  (test (equal? 3.0+4.0i 3+4i) #f)
  (test (equal? 3 3.0) #f)
  (test (equal? 3+4i 3.0+4.0i) #f)
  (test
    (let ([ls1 (make-list 100000 'a)]
          [ls2 (make-list 100000 'a)])
      (equal? ls1 ls2))
    #t)
 ; tests from from srfi-85
  (test (equal? '() '()) #t)
  (test (equal? (vector 34.5 34.5) '#(34.5 34.5)) #t)
  (test
    (let* ([x (list 'a)] [y (list 'a)] [z (list x y)])
      (list (equal? z (list y x)) (equal? z (list x x))))
    '(#t #t))
)

(define (r6rs-checks equal?)
  (test
    (let ([leaf1 (list "As a tree, I am huge.")]
          [leaf2 (list "As a dag, I am small.")])
      (let ([tr1 (let f ([n 100])
                   (if (= n 0)
                       leaf1
                       (let ([tr (f (- n 1))]) (cons tr tr))))]
            [tr2 (let f ([n 100])
                   (if (= n 0)
                       leaf2
                       (let ([tr (f (- n 1))]) (cons tr tr))))])
        (let ([ls (list
                    (equal? tr1 tr1)
                    (equal? tr2 tr2)
                    (equal? tr1 tr2)
                    (equal? tr1 (car tr1)))])
          (set-car! leaf1 (car leaf2))
          (cons* (equal? tr1 tr1) (equal? tr2 tr2)
            (equal? tr1 tr2) (equal? tr1 (cdr tr1)) ls))))
    '(#t #t #t #f #t #t #f #f))
  (test
    (equal?
      (let ([g0 (cons* 'a 'b 'c *)])
        (set-cdr! (cdr (cdr g0)) g0)
        g0)
      (let ([g0 (cons* 'a 'b 'c *)])
        (set-cdr! (cdr (cdr g0)) g0)
        g0))
    #t)
  (test
    (equal?
      (let ([g1 (cons* 'a 'b 'c *)])
        (set-cdr! (cdr (cdr g1)) g1)
        g1)
      (let ([g2 (cons* 'a 'b *)]) (set-cdr! (cdr g2) g2) g2))
    #f)
  (test
    (equal?
      (let ([g3 (cons* 'a 'b 'c *)])
        (set-cdr! (cdr (cdr g3)) g3)
        g3)
      (let ([g4 (cons* 'a 'b 'c 'a 'b 'c *)])
        (set-cdr! (cdr (cdr (cdr (cdr (cdr g4))))) g4)
        g4))
    #t)
  (test
    (let ([g7 (cons* 'a 'b 'c *)])
      (set-cdr! (cdr (cdr g7)) g7)
      (equal? g7 (cons* 'a 'b 'c 'a 'b 'c g7)))
    #t)
  (test
    (let ([g8 (cons* 'a 'b 'c *)])
      (let ([g9 (cons* 'a 'b 'c 'a 'c *)])
        (set-cdr! (cdr (cdr g8)) g8)
        (set-cdr! (cdr (cdr (cdr (cdr g9)))) g9)
        (equal? g8 g9)))
    #f)
  (test
    (let ([g0 (cons* 'a * 'c * 'd * '(f))])
      (let ([g1 (cons* 'a (cons* 'a * 'c g0 'd * '(f)) 'c g0 'd * '(f))])
        (set-car! (cdr g0) g0)
        (set-car! (cdddr g0) g0)
        (set-car! (cdr (cddddr g0)) g1)
        (set-car! (cdadr g1) g1)
        (set-car! (cddddr (cdadr g1)) g1)
        (set-car! (cdr (cddddr g1)) g1)
        (list g0 g1)
        (list (equal? g0 g0) (equal? g1 g1) (equal? g0 g1)
          (equal? g1 g0) (equal? (cadr g0) g1)
          (equal? (cons 'g g0) g0) (equal? (append g0 '(g)) g0)
          (equal? (cdr g0) (cdddr g0))
          (equal? (cdr g0) (cdr (cadr g1))))))
    '(#t #t #t #t #t #f #f #f #t))
  (test
    (equal?
      (let ([g0 (vector 'a 'b 'c *)]) (vector-set! g0 3 g0) g0)
      (let ([g1 (vector 'a 'b 'c *)]) (vector-set! g1 3 g1) g1))
    #t)
  (test
    (equal?
      (let ([g2 (vector 'a 'b 'c *)]) (vector-set! g2 3 g2) g2)
      (let ([g3 (vector 'a 'b *)]) (vector-set! g3 2 g3) g3))
    #f)
  (test
    (equal?
      (let ([g4 (vector 'a 'b 'c *)]) (vector-set! g4 3 g4) g4)
      (let ([g5 (vector 'a 'b 'c (vector 'a 'b 'c *))])
        (vector-set! (vector-ref g5 3) 3 g5)
        g5))
    #t)
  (test
    (let ([g7 (vector 'a 'b 'c *)])
      (vector-set! g7 3 g7)
      (equal? g7 (vector 'a 'b 'c (vector 'a 'b 'c g7))))
    #t)
  (test
    (equal?
      (let ([g8 (vector 'a 'b 'c *)]) (vector-set! g8 3 g8) g8)
      (let ([g9 (vector 'a 'b 'c (vector 'a 'c *))])
        (vector-set! (vector-ref g9 3) 2 g9)
        g9))
    #f)
  (test
    (let ([g10 (vector 'a (vector 'a * 'c * 'd * 'f) 'c * 'd * 'f)])
      (let ([g11 (vector 'a * 'c * 'd g10 'f)])
        (let ([g12 (vector 'a (vector 'a * 'c g11 'd * 'f) 'c g11 'd * 'g)])
          (vector-set! (vector-ref g10 1) 1 g10)
          (vector-set! (vector-ref g10 1) 3 g11)
          (vector-set! (vector-ref g10 1) 5 g10)
          (vector-set! g10 3 g11)
          (vector-set! g10 5 g10)
          (vector-set! g11 1 g11)
          (vector-set! g11 3 g11)
          (vector-set! (vector-ref g12 1) 1 g12)
          (vector-set! (vector-ref g12 1) 5 g12)
          (vector-set! g12 5 g12)
          (list (equal? g11 g11)
                (equal? g10 g10)
                (equal? g12 g12)
                (equal? g11 g10)
                (equal? g10 g11)
                (equal? g11 g12)
                (equal? g10 g12)
                (equal? g12 g11)
                (equal? g12 g10)
                (equal? (vector-ref g11 1) g10)))))
    '(#t #t #t #t #t #f #f #f #f #t))
  (test
    (let ([leaf1 (vector "As a tree, I am huge.")]
          [leaf2 (vector "As a dag, I am small.")])
      (let ([tr1 (let f ([n 100])
                   (if (= n 0)
                       leaf1
                       (let ([tr (f (- n 1))])
                         (vector tr tr))))]
            [tr2 (let f ([n 100])
                   (if (= n 0)
                       leaf2
                       (let ([tr (f (- n 1))])
                         (vector tr tr))))])
        (let ([ls (list (equal? tr1 tr1)
                        (equal? tr2 tr2)
                        (equal? tr1 tr2)
                        (equal? tr1 (vector-ref tr1 0)))])
          (vector-set! leaf1 0 (vector-ref leaf2 0))
          (cons* (equal? tr1 tr1)
                 (equal? tr2 tr2)
                 (equal? tr1 tr2)
                 (equal? tr1 (vector-ref tr1 1))
                 ls))))
    '(#t #t #t #f #t #t #f #f))
  (test
    (let ([v1 (make-vector 10000 (make-vector 100 'a))]
          [v2 (make-vector 10000 (make-vector 100 'a))])
      (equal? v1 v2))
    #t)
  (test
    (let ()
      (define x
        (let ([x1 (vector 'h)]
              [x2 (let ([x (list #f)]) (set-car! x x) x)])
          (vector x1 (vector 'h) x1 (vector 'h) x1 x2)))
      (define y
        (let ([y1 (vector 'h)]
              [y2 (vector 'h)]
              [y3 (let ([x (list #f)]) (set-car! x x) x)])
          (vector (vector 'h) y1 y1 y2 y2 y3)))
      (equal? x y))
    #t)
  (test
    (let ()
      (define x
        (let ([x0 (vector #f #f #f)]
              [x1 (vector #f #f #f)]
              [x2 (vector #f #f #f)])
           (vector-fill! x0 x0)
           (vector-fill! x1 x1)
           (vector-fill! x2 x2)
           (vector x0 x1 x0 x2 x0)))
      (define y
        (let ([y0 (vector #f #f #f)]
              [y1 (vector #f #f #f)]
              [y2 (vector #f #f #f)])
           (vector-fill! y0 y0)
           (vector-fill! y1 y1)
           (vector-fill! y2 y2)
           (vector y0 y1 y1 y2 y2)))
      (equal? x y))
    #t)
  (test
    (let ()
      (define x
        (let ([x (cons (cons #f 'a) 'a)])
          (set-car! (car x) x)
          x))
      (define y
        (let ([y (cons (cons #f 'a) 'a)])
          (set-car! (car y) (car y))
          y))
      (equal? x y))
    #t)
  (test
    (let ()
      (define x
        (let* ([x3 (cons 'x3 'x3)]
               [x2 (cons 'x2 x3)]
               [x1 (cons x2 'x1)])
          (set-car! x3 x3)
          (set-cdr! x3 x3)
          (set-car! x2 x2)
          (set-cdr! x1 x1)
          x1))
      (define y
        (let* ([y2 (cons 'y1 'y1)]
               [y1 (cons y2 y2)])
          (set-car! y2 y1)
          (set-cdr! y2 y1)
          y1))
      (equal? x y))
    #t)
  (test
    (let ()
      (define x
        (let* ([x3 (cons 'x3 'x3)]
               [x2 (cons 'x2 x3)]
               [x1 (cons x2 'x1)])
          (set-car! x3 x3)
          (set-cdr! x3 x3)
          (set-car! x2 x2)
          (set-cdr! x1 x1)
          x1))
      (define y
        (let* ([y2 (cons 'y1 'y1)]
               [y1 (cons y2 y2)])
          (set-car! y2 y1)
          (set-cdr! y2 y1)
          y1))
      (equal? x y))
    #t)
  (test
    (let ()
      (define (make-x k)
        (let ([x1 (cons
                    (let f ([n k])
                      (if (= n 0)
                          (let ([x0 (cons #f #f)])
                            (set-car! x0 x0)
                            (set-cdr! x0 x0)
                            x0)
                          (let ([xi (cons #f (f (- n 1)))])
                            (set-car! xi xi)
                            xi)))
                    #f)])
          (set-cdr! x1 x1)
          x1))
      (define y
        (let* ([y2 (cons #f #f)] [y1 (cons y2 y2)])
          (set-car! y2 y1)
          (set-cdr! y2 y1)
          y1))
      (equal? (make-x 100) y))
    #t)
  (test
    (let ([v1 (let ([g13 (list *)]) (set-car! g13 g13) g13)]
          [v2 (let ([g14 (list *)]) (set-car! g14 g14) g14)])
      (equal? v1 v2))
    #t)
  (test
    (let ([v1 (make-vector 95000 (make-vector 95000 0))]
          [v2 (make-vector 95000 (make-vector 95000 0))])
      (equal? v1 v2))
    #t)
  (test
    (let ([n 100000])
      (let ([f (lambda (n)
                 (let ([ls (make-list n 0)])
                   (set-cdr! (last-pair ls) ls)
                   ls))])
         (let ([v1 (f n)] [v2 (f (- n 1))])
           (equal? v1 v2))))
    #t)
 ; tests from srfi-85
  (test
    (let ([x (list 'a 'b 'c 'a)]
          [y (list 'a 'b 'c 'a 'b 'c 'a)])
      (set-cdr! (list-tail x 2) x)
      (set-cdr! (list-tail y 5) y)
      (list
        (equal? x x)
        (equal? x y)
        (equal? (list x y 'a) (list y x 'b))))
    '(#t #t #f))
)

(define-syntax check-one
  (syntax-rules ()
    [(_ checker equal?)
     (begin
       (display "running ")
       (write 'checker)
       (display " on ")
       (write 'equal?)
       (display " ...")
       (flush-output-port (current-output-port))
       (checker equal?)
       (display " done\n"))]))

(check-one r5rs-checks r5rs-equal?)
(check-one r5rs-checks uf-equal?)
(check-one r5rs-checks precheck/uf-equal?)
(check-one r5rs-checks interleave-equal?)
(check-one r5rs-checks precheck/interleave-equal?)
(check-one r5rs-checks equal?) ; comment out to not test built-in equal?

(check-one r6rs-checks uf-equal?)
(check-one r6rs-checks precheck/uf-equal?)
(check-one r6rs-checks interleave-equal?)
(check-one r6rs-checks precheck/interleave-equal?)
(check-one r6rs-checks equal?) ; comment out to not test built-in equal?

(check-one record-checks uf-equal?)
