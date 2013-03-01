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

(define call/cc call-with-current-continuation)

(define return #f)
(+ 1 (call-with-current-continuation (λ (k) (set! return k) 1)))

(define null? { s | (= [] s) })

(define foldl
  (λ (f init coll)
    (if (null? coll)
      init
      (foldl f (f init (head coll)) (tail coll)))))

(define sum
  (λ (coll)
    (foldl {acc n | (+ acc n)} 0 coll)))

(define foldr
  (λ (f init coll)
    (if (null? coll)
      init
      (f (foldr f init (tail coll)) (head coll)))))

(define map 
  (λ (f coll)
    (define m 
      (λ (coll acc)
        (if (null? coll)
          acc
          (m (tail coll) (cat acc [(f (head coll))])))))
    (m coll [])))

-- combinators

(define always { c | { _ | c}}) 

-- alists

(define alist/assq
  (λ (elem alist)
    (if (null? alist)
      #f
      (if (= (head (head alist)) elem)
        (head alist)
        (alist/assq elem (tail alist))))))

#t
