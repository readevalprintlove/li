#|
Copyright (C) 2013 Michael Fogus <me -at- fogus -dot- me>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
|#

;; Core functions

(define call/cc call-with-current-continuation)

(define (caar lst) (car (car lst)))
(define (cadr lst) (car (cdr lst)))
(define (cdar lst) (cdr (car lst)))
(define (cddr lst) (cdr (cdr lst)))
(define (caaar lst) (car (car (car lst))))
(define (caadr lst) (car (car (cdr lst))))
(define (cadar lst) (car (cdr (car lst))))
(define (caddr lst) (car (cdr (cdr lst))))
(define (cdaar lst) (cdr (car (car lst))))
(define (cdadr lst) (cdr (car (cdr lst))))
(define (cddar lst) (cdr (cdr (car lst))))
(define (cdddr lst) (cdr (cdr (cdr lst))))
(define (caaaar lst) (car (car (car (car lst)))))
(define (caaadr lst) (car (car (car (cdr lst)))))
(define (caadar lst) (car (car (cdr (car lst)))))
(define (caaddr lst) (car (car (cdr (cdr lst)))))
(define (cadaar lst) (car (cdr (car (car lst)))))
(define (cadadr lst) (car (cdr (car (cdr lst)))))
(define (caddar lst) (car (cdr (cdr (car lst)))))
(define (cadddr lst) (car (cdr (cdr (cdr lst)))))
(define (cdaaar lst) (cdr (car (car (car lst)))))
(define (cdaadr lst) (cdr (car (car (cdr lst)))))
(define (cdadar lst) (cdr (car (cdr (car lst)))))
(define (cdaddr lst) (cdr (car (cdr (cdr lst)))))
(define (cddaar lst) (cdr (cdr (car (car lst)))))
(define (cddadr lst) (cdr (cdr (car (cdr lst)))))
(define (cdddar lst) (cdr (cdr (cdr (car lst)))))
(define (cddddr lst) (cdr (cdr (cdr (cdr lst)))))

(define (list-tail lst k)
  (if (zero? k)
    lst
    (list-tail (cdr lst) (- k 1))))

(define (list . elems) elems)

(define (list-ref lst k) (car (list-tail lst k)))

;; Tests

(define x 0)

x
;;=> 0

(begin
  (set! x 5)
  (+ x 1))
;;=> 6

x
;;=> 5

'(1 . 2)
;;=> (1 . 2)

(pair? '(1 . 2))
;;= #t

(pair? 1)
;;=> #f

(pair? '(1 2 3))
;;=> #f

(pair? '())
;;=> #f

(pair? '(1 2 3))
;;=> #t

(cons 1 2)
;;=> (1 . 2)

(pair? (cons 1 2))
;;=> #t

(cons '(a b) 'c)
;;=> ((a b) . c)

(car '(1 . 2))
;;=> 1

(car (cons '(a b) 'c))
;;=> (a b)

(cdr '(1 . 2))
;;=> 2

(cdr (cons '(a b) 'c))
;;=> c

(append '() 'a)
;;=> a

(append '(a b) '(c . d))
;;=> (a b c . d)

(append '(x) '(y))
;;=> (x y)

(append '(x) '(b c d))
;;=> (x b c d)

(append '(a (b)) '((c)))
;;=> (a (b) (c))

(define (add x y) (+ x y))

(add 1 2)
;;=> 3

(define f (lambda (a b . m) m))

(f 1 2)
;;=> ()

(f 1 2 3 4)
;;=> (3 4)

(define (more a b . c) c)

(more 1 2)
;;=> ()

(more 1 2 3 4)
;;=> (3 4)

(+ 3 4)
;;=> 7

(+ 1 2 3)
;;=> 6

(- 3 4 5)
;;=> -6

(- 3 4)
;;=> -1

(null? '())
;;=> #t

(list? '())
;;=> #t

(list? '(1 2 3))
;;=> #t

(list? '(a . b))
;;=> #f

(make-list 3 3)
;;=> (3 3 3)

(make-list 10 'a)
;;=> (a a a a a a a a a a)

(list? (make-list 3 3))
;;=> #t

(make-list 3)
;;=> (0 0 0)

(length '(1 2 3))
;;=> 3

(length '(a (b) (c d e)))
;;=> 3

(length '())
;;=> 0

(reverse '(1 2 3))
;;=> (3 2 1)

(reverse '(a (b c) d (e (f))))
;;=> ((e (f)) d (b c) a)

(zero? 0)
;;=> #t

(zero? 1)
;;=> #f

(zero? '())
;;=> #f

(list-tail '(1 2 3 4 5) 2)
;;=> (3 4 5)

(list 1 2 3)
;;=> (1 2 3)

(list 'a (+ 3 4) 'c)
;;=> (a 7 c)

(list)
;;=> ()

(list-ref '(a b c d) 2)
;;=> c

(symbol->string 'a)
;;=> "a"

(string->symbol "A")
;;=> A

(symbol->string (string->symbol "A"))
;;=> "A"


(char? #\()
;;=> #t

#f
#t

