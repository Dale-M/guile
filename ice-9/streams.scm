;;;; streams.scm --- general lazy streams
;;;; -*- Scheme -*-

;;;; Copyright (C) 1999 Free Software Foundation, Inc.
;;;; 
;;;; This program is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 2, or (at your option)
;;;; any later version.
;;;; 
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU General Public License
;;;; along with this software; see the file COPYING.  If not, write to
;;;; the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;; Boston, MA 02111-1307 USA

;; the basic stream operations are inspired by
;; (i.e. ripped off) Scheme48's `stream' package,
;; modulo stream-empty? -> stream-null? renaming.

(define-module (ice-9 streams))

(export make-stream
        stream-car stream-cdr stream-null?
        list->stream vector->stream port->stream
        stream->list stream->reversed-list
        stream->list&length stream->reversed-list&length
        stream->vector
        stream-fold stream-for-each stream-map)

;; Use:
;;
;; (make-stream producer initial-state)
;;  - PRODUCER is a function of one argument, the current state.
;;    it should return either a pair or an atom (i.e. anything that
;;    is not a pair).  if PRODUCER returns a pair, then the car of the pair
;;    is the stream's head value, and the cdr is the state to be fed
;;    to PRODUCER later.  if PRODUCER returns an atom, then the stream is
;;    considered depleted.
;;
;; (stream-car stream)
;; (stream-cdr stream)
;; (stream-null? stream)
;;  - yes.
;;
;; (list->stream list)
;; (vector->stream vector)
;;  - make a stream with the same contents as LIST/VECTOR.
;;
;; (port->stream port read)
;;  - makes a stream of values which are obtained by READing from PORT.
;;
;; (stream->list stream)
;;  - returns a list with the same contents as STREAM.
;;
;; (stream->reversed-list stream)
;;  - as above, except the contents are in reversed order.
;;
;; (stream->list&length stream)
;; (stream->reversed-list&length stream)
;;  - multiple-valued versions of the above two, the second value is the
;;    length of the resulting list (so you get it for free).
;;
;; (stream->vector stream)
;;  - yes.
;;
;; (stream-fold proc init stream0 ...)
;;  - PROC must take (+ 1 <number-of-stream-arguments>) arguments, like this:
;;    (PROC car0 ... init).  *NOTE*: the INIT argument is last, not first.
;;    I don't have any preference either way, but it's consistent with
;;    `fold[lr]' procedures from SRFI-1.  PROC is applied to successive
;;    elements of the given STREAM(s) and to the value of the previous
;;    invocation (INIT on the first invocation).  the last result from PROC
;;    is returned.
;;
;; (stream-for-each proc stream0 ...)
;;  - like `for-each' we all know and love.
;;
;; (stream-map proc stream0 ...)
;;  - like `map', except returns a stream of results, and not a list.

;; Code:

(define (make-stream m state)
  (delay
    (let ((o (m state)))
      (if (pair? o)
	  (cons (car o)
		(make-stream m (cdr o)))
          '()))))

(define (stream-car stream)
  (car (force stream)))

(define (stream-cdr stream)
  (cdr (force stream)))

(define (stream-null? stream)
  (null? (force stream)))

(define (list->stream l)
  (make-stream
   (lambda (l) l)
   l))

(define (vector->stream v)
  (make-stream
   (let ((len (vector-length v)))
     (lambda (i)
       (or (= i len)
           (cons (vector-ref v i) (+ 1 i)))))
   0))

(define (stream->reversed-list&length stream)
  (let loop ((s stream) (acc '()) (len 0))
    (if (stream-null? s)
        (values acc len)
        (loop (stream-cdr s) (cons (stream-car s) acc) (+ 1 len)))))

(define (stream->reversed-list stream)
  (call-with-values
   (lambda () (stream->reversed-list&length stream))
   (lambda (l len) l)))

(define (stream->list&length stream)
  (call-with-values
   (lambda () (stream->reversed-list&length stream))
   (lambda (l len) (values (reverse! l) len))))

(define (stream->list stream)
  (reverse! (stream->reversed-list stream)))

(define (stream->vector stream)
  (call-with-values
   (lambda () (stream->reversed-list&length stream))
   (lambda (l len)
     (let ((v (make-vector len)))
       (let loop ((i 0) (l l))
         (if (not (null? l))
             (begin
               (vector-set! v (- len i 1) (car l))
               (loop (+ 1 i) (cdr l)))))
       v))))

(define (stream-fold f init stream . rest)
  (if (null? rest) ;fast path
      (let loop ((stream stream) (r init))
        (if (stream-null? stream)
            r
            (loop (stream-cdr stream) (f (stream-car stream) r))))
      (let loop ((streams (cons stream rest)) (r init))
        (if (or-map stream-null? streams)
            r
            (loop (map stream-cdr streams)
                  (apply f (let recur ((cars (map stream-car streams)))
                             (if (null? cars)
                                 (list r)
                                 (cons (car cars)
                                       (recur (cdr cars)))))))))))

(define (stream-for-each f stream . rest)
  (apply stream-fold
         (lambda (elt _) (f elt))
         #f
         stream rest))

(define (stream-map f stream . rest)
  (if (null? rest) ;fast path
      (make-stream (lambda (s)
                     (or (stream-null? s)
                         (cons (f (stream-car s)) (stream-cdr s))))
                   stream)
      (make-stream (lambda (streams)
                     (or (or-map stream-null? streams)
                         (cons (apply f (map stream-car streams))
                               (map stream-cdr streams))))
                   (cons stream rest))))

(define (port->stream port read)
  (make-stream (lambda (p)
                 (let ((o (read p)))
                   (or (eof-object? o)
                       (cons o p))))
               port))

;;; streams.scm ends here
