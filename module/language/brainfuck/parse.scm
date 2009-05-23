;;; Brainfuck for GNU Guile.

;; Copyright (C) 2009 Free Software Foundation, Inc.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(define-module (language brainfuck parse)
  #:export (read-brainfuck))

; Purpose of the parse module is to read in brainfuck in text form and produce
; the corresponding tree representing the brainfuck code.
;
; Each object (representing basically a single instruction) is structured like:
; (<instruction> [arguments])
; where <instruction> is a symbolic name representing the type of instruction
; and the optional arguments represent further data (for instance, the body of
; a [...] loop as a number of nested instructions).
;
; A full brainfuck program is represented by the (<brainfuck> instructions)
; object.


; Read a brainfuck program from an input port.  We construct the <brainfuck>
; program and read in the instructions using (read-body).

(define (read-brainfuck p)
  `(<brainfuck> ,@(read-body p)))


; While reading a number of instructions in sequence, all of them are cons'ed
; onto a list of instructions; thus this list gets out in reverse order.
; Additionally, for "comment characters" (everything not an instruction) we
; generate <bf-nop> NOP instructions.
;
; This routine reverses a list of instructions and removes all <bf-nop>'s on the
; way to fix these two issues for a read-in list.

(define (reverse-without-nops lst)
  (let iterate ((cur lst)
                (result '()))
    (if (null? cur)
      result
      (let ((head (car cur))
            (tail (cdr cur)))
        (if (eq? (car head) '<bf-nop>)
          (iterate tail result)
          (iterate tail (cons head result)))))))


; Read in a set of instructions until a terminating ] character is found (or
; end of file is reached).  This is used both for loop bodies and whole
; programs, so that a program has to be either terminated by EOF or an
; additional ], too.
;
; For instance, the basic program so just echo one character would be:
; ,.]

(define (read-body p)
  (let iterate ((parsed '()))
    (let ((chr (read-char p)))
      (if (or (eof-object? chr) (eq? #\] chr))
        (reverse-without-nops parsed)
        (iterate (cons (process-input-char chr p) parsed))))))


; This routine processes a single character of input and builds the
; corresponding instruction.  Loop bodies are read by recursively calling
; back (read-body).
;
; For the poiner movement commands >< and the cell increment/decrement +-
; commands, we only use one instruction form each and specify the direction of
; the pointer/value increment using an argument to the instruction form.

(define (process-input-char chr p)
  (case chr
    ((#\>) '(<bf-move> 1))
    ((#\<) '(<bf-move> -1))
    ((#\+) '(<bf-increment> 1))
    ((#\-) '(<bf-increment> -1))
    ((#\.) '(<bf-print>))
    ((#\,) '(<bf-read>))
    ((#\[) `(<bf-loop> ,@(read-body p)))
    (else '(<bf-nop>))))
