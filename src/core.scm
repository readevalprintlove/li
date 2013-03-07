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

;; this is a test

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

#f
#t

