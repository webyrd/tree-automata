;;; equal.sls - Library implementing the equality-checking algorithms
;;; described in "Efficient nondestructive equality checking for trees
;;; and graphs" by Michael D. Adams and R. Kent Dybvig, ICFP 2008.

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

;;; We would appreciate contributions of improvements to the code.

;;; The equality-checking algorithms implemented here are described in:
;;;
;;; @inproceedings{Adams:equality,
;;;  author = {Michael D. Adams and R. Kent Dybvig},
;;;  title = {Efficient nondestructive equality checking for trees and graphs},
;;;  booktitle = {ICFP '08: Proceeding of the 13th ACM SIGPLAN international conference on Functional programming},
;;;  year = {2008},
;;;  isbn = {978-1-59593-919-7},
;;;  pages = {179--188},
;;;  location = {Victoria, BC, Canada},
;;;  doi = {http://doi.acm.org/10.1145/1411204.1411230},
;;;  publisher = {ACM},
;;;  address = {New York, NY, USA},
;;;  }

;;; The union-find algorithm used by the all but the possibly
;;; nonterminating equality check is the splitting algorithm described
;;; and analyzed in:
;;;
;;; @article{Tarjan:setunion,
;;;  author = {Robert Endre Tarjan},
;;;  title = {Efficiency of a Good But Not Linear Set Union Algorithm},
;;;  journal = {J. ACM},
;;;  volume = {22},
;;;  number = {2},
;;;  year = {1975},
;;;  issn = {0004-5411},
;;;  pages = {215--225},
;;;  doi = {http://doi.acm.org/10.1145/321879.321884},
;;;  publisher = {ACM},
;;;  address = {New York, NY, USA},
;;; }
;;;
;;; @article{Tarjan:worst-case-analysis,
;;;  author = {Robert E. Tarjan and Jan van Leeuwen},
;;;  title = {Worst-case Analysis of Set Union Algorithms},
;;;  journal = {J. ACM},
;;;  volume = {31},
;;;  number = {2},
;;;  year = {1984},
;;;  issn = {0004-5411},
;;;  pages = {245--281},
;;;  doi = {http://doi.acm.org/10.1145/62.2160},
;;;  publisher = {ACM},
;;;  address = {New York, NY, USA},
;;;  }
;;;
;;; The union-find procedure might be useful separately.

;;; Implementation notes:
;;;
;;;   - A working random procedure should be provided---see the note
;;;     above the stub version of random below.
;;;
;;;   - The box datatype is implemented via define-record-type.  It can
;;;     be implemented using pairs, since boxes are used in contexts
;;;     where pairs are never expected, or replaced with a built-in box
;;;     type, if available.
;;;
;;;   - If vector lengths and indicies are guaranteed to be fixnums in
;;;     your implementation of Scheme, you can replace each reference of
;;;     = with fx=?, replace each reference of + with fx+, and eliminate
;;;     + and = from the imports.
;;; 
;;;   - hashtable-set! and hashtable-ref can be replaced with variants
;;;     that are specialized to eq hashtables.
;;;
;;;   - Unchecked versions of all imported procedures can be used.

;;; To use the recommended precheck/interleave-equal? as an implementation
;;; of the R6RS equal? procedure, simply:
;;;
;;;   - change the name of precheck/interleave-equal? to equal?
;;;   - change the export form to read, simply, (export equal?)
;;;
;;; You can also comment out or remove the definitions of r5rs-equal?,
;;; uf-equal?, precheck/uf-equal?, and interleave-equal?.

(library (equal (1))
  (export r5rs-equal?
          uf-equal?
          precheck/uf-equal?
          interleave-equal?
          precheck/interleave-equal?)

  (import
    (only (rnrs)
          define let and or cond else if not unless begin set!

          define-record-type fields mutable

          pair? vector? string? bytevector? record?
          eq? eqv? string=? bytevector=?
          car cdr vector-length vector-ref
          record-rtd record-type-field-names record-accessor
          = + fx<=? fx>? fx=? fx+ fx-
          fxarithmetic-shift-right fxarithmetic-shift-left

          make-eq-hashtable hashtable-set! hashtable-ref))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Random-number generation procedure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The random procedure used by the interleaving code is always passed a
;; positive fixnum and is expected to return a fixnum between zero
;; (inclusive) and n (exclusive).  Since R6RS does not provide a random
;; procedure, we define a stub version that always returns a value near
;; the mid-point of the range.  If your Scheme implementation provides a
;; random procedure (most do), use it instead.

(define (random n) (fxarithmetic-shift-right n 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Boxes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type (box-record box box?)
  (fields (mutable contents unbox set-box!)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tuning constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; k0 * 2 and kb are assumed to be fixnums.

(define k0 400)
(define kb -40)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Exported equality predicates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Possibly nonterminating R5RS-compliant equal? w/bytevector support

(define (r5rs-equal? x y)
  (cond
    [(eq? x y) #t]
    [(pair? x)
     (and (pair? y)
          (and (r5rs-equal? (car x) (car y))
               (r5rs-equal? (cdr x) (cdr y))))]
    [(vector? x)
     (and (vector? y)
          (let ([n (vector-length x)])
            (and (= (vector-length y) n)
                 (let f ([i 0])
                   (or (= i n)
                       (and (r5rs-equal? (vector-ref x i) (vector-ref y i))
                            (f (+ i 1))))))))]
    [(string? x) (and (string? y) (string=? x y))]
    [(bytevector? x) (and (bytevector? y) (bytevector=? x y))]
    [else (eqv? x y)]))

;; Straight union-find based implementation

(define (uf-equal? x y)
  (let ([ht (make-eq-hashtable)])
    (define (e? x y)
      (cond
        [(eq? x y) #t]
        [(pair? x)
         (and (pair? y)
           (or (union-find ht x y)
               (and (e? (car x) (car y))
                    (e? (cdr x) (cdr y)))))]
        [(vector? x)
         (and (vector? y)
           (let ([n (vector-length x)])
             (and (= (vector-length y) n)
               (or (union-find ht x y)
                   (let f ([i 0])
                     (or (= i n)
                         (and (e? (vector-ref x i) (vector-ref y i))
                              (f (+ i 1)))))))))]
        [(string? x) (and (string? y) (string=? x y))]
        [(bytevector? x) (and (bytevector? y) (bytevector=? x y))]
        [(record? x)
         (and (record? y)
              (let ([rtd (record-rtd x)])
                (and (eq? rtd (record-rtd y))
                     (or (union-find ht x y)
                         (let ([n (vector-length (record-type-field-names rtd))])
                           (let f ([i 0])
                             (or (= i n)
                                 (and (let ([accessor (record-accessor rtd i)])
                                        (e? (accessor x) (accessor y)))
                                      (f (+ i 1))))))))))]
        [else (eqv? x y)]))
    (e? x y)))

;; Union-find based implementation with precheck

(define (precheck/uf-equal? x y)
  (let ([k (precheck? x y k0)])
    (and k (or (fx>? k 0) (uf-equal? x y)))))

;; Interleave based implementation

(define (interleave-equal? x y)
  (interleave? x y k0))

;; Interleave based implementation with precheck
;; (recommended in paper for general purpose equal)

(define (precheck/interleave-equal? x y)
  (let ([k (precheck? x y k0)])
    (and k (or (fx>? k 0) (interleave? x y 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unexported helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Per a suggestion from Aziz Ghuloum, union-find creates at most one pair
;; per node in smallest graph, which effectively limits equivalence class
;; sizes to the number of nodes in the smallest graph.  This doesn't improve
;; the asymptotic bound because the hashtable presumably allocates at least
;; one entry per node anyway, but it does reduce overall allocation and
;; traversal overhead.

(define (union-find ht x y)
  (define (find b) ; splitting
    (let ([n (unbox b)]) ; next or census
      (if (box? n)
          (let loop ([b b] [n n])
            (let ([nn (unbox n)])
              (if (box? nn)
                  (begin (set-box! b nn) (loop n nn))
                  n)))
          b)))
  (let ([bx (hashtable-ref ht x #f)]
        [by (hashtable-ref ht y #f)])
    (if (not bx)
        (if (not by)
            (let ([b (box 1)])
              (hashtable-set! ht x b)
              (hashtable-set! ht y b)
              #f)
            (let ([ry (find by)])
              (hashtable-set! ht x ry)
              #f))
        (if (not by)
            (let ([rx (find bx)])
              (hashtable-set! ht y rx)
              #f)
            (let ([rx (find bx)] [ry (find by)])
              (or (eq? rx ry)
                  (let ([nx (unbox rx)] [ny (unbox ry)])
                    (if (fx>? nx ny)
                        (begin
                          (set-box! ry rx)
                          (set-box! rx (fx+ nx ny))
                          #f)
                        (begin
                          (set-box! rx ry)
                          (set-box! ry (fx+ ny nx))
                          #f)))))))))

(define (precheck? x y k) ; called pre? in paper
  (cond
    [(eq? x y) k]
    [(pair? x)
     (and (pair? y)
       (if (fx<=? k 0)
           k
           (let ([k (precheck? (car x) (car y) (fx- k 1))])
             (and k (precheck? (cdr x) (cdr y) k)))))]
    [(vector? x)
     (and (vector? y)
       (let ([n (vector-length x)])
         (and (= (vector-length y) n)
           (let f ([i 0] [k k])
             (if (or (= i n) (fx<=? k 0))
                 k
                 (let ([k (precheck?
                            (vector-ref x i)
                            (vector-ref y i)
                            (fx- k 1))])
                   (and k (f (+ i 1) k))))))))]
    [(string? x) (and (string? y) (string=? x y) k)]
    [(bytevector? x) (and (bytevector? y) (bytevector=? x y) k)]
    [else (and (eqv? x y) k)]))

(define (interleave? x y k)
  (let ([ht #f])
    (define (call-union-find x y)
      (unless ht (set! ht (make-eq-hashtable)))
      (union-find ht x y))
    (define (e? x y k)
      (if (fx<=? k 0)
          (if (fx=? k kb)
              (fast? x y (random (fxarithmetic-shift-left k0 1)))
              (slow? x y k))
          (fast? x y k)))
    (define (slow? x y k)
      (cond
        [(eq? x y) k]
        [(pair? x)
         (and (pair? y)
           (if (call-union-find x y)
               0
               (let ([k (e? (car x) (car y) (fx- k 1))])
                 (and k (e? (cdr x) (cdr y) k)))))]
        [(vector? x)
         (and (vector? y)
           (let ([n (vector-length x)])
             (and (= (vector-length y) n)
               (if (call-union-find x y)
                   0
                   (let f ([i 0] [k (fx- k 1)])
                     (if (= i n)
                         k
                         (let ([k (e? (vector-ref x i) (vector-ref y i) k)])
                            (and k (f (+ i 1) k)))))))))]
        [(string? x) (and (string? y) (string=? x y) k)]
        [(bytevector? x) (and (bytevector? y) (bytevector=? x y) k)]
        [else (and (eqv? x y) k)]))
    (define (fast? x y k)
      (let ([k (fx- k 1)])
        (cond
          [(eq? x y) k]
          [(pair? x)
           (and (pair? y)
             (let ([k (e? (car x) (car y) k)])
               (and k (e? (cdr x) (cdr y) k))))]
          [(vector? x)
           (and (vector? y)
             (let ([n (vector-length x)])
               (and (= (vector-length y) n)
                 (let f ([i 0] [k k])
                   (if (= i n)
                       k
                       (let ([k (e? (vector-ref x i) (vector-ref y i) k)])
                          (and k (f (+ i 1) k))))))))]
          [(string? x) (and (string? y) (string=? x y) k)]
          [(bytevector? x) (and (bytevector? y) (bytevector=? x y) k)]
          [else (and (eqv? x y) k)])))
    (and (e? x y k) #t)))
)
