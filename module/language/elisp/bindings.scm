;;; Guile Emac Lisp

;; Copyright (C) 2001 Free Software Foundation, Inc.

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

(define-module (language elisp bindings)
  #:export (make-bindings
            mark-fluid-needed! map-fluids-needed
            with-lexical-bindings with-dynamic-bindings
            get-lexical-binding))

; This module defines routines to handle analysis of symbol bindings used
; during elisp compilation.  This data allows to collect the symbols, for
; which fluids need to be created, or mark certain symbols as lexically bound.

; Needed fluids are stored in an association-list that stores a list of fluids
; for each module they are needed in.

; The lexical bindings of symbols are stored in a hash-table that associates
; symbols to fluids; those fluids are used in the with-lexical-binding and
; with-dynamic-binding routines to associate symbols to different bindings
; over a dynamic extent.


; Record type used to hold the data necessary.

(define bindings-type
  (make-record-type 'bindings
                    '(needed-fluids lexical-bindings)))


; Construct an 'empty' instance of the bindings data structure to be used
; at the start of a fresh compilation.

(define (make-bindings)
  ((record-constructor bindings-type) '() (make-hash-table)))


; Mark that a given symbol is needed as fluid in the specified slot-module.

(define (mark-fluid-needed! bindings sym module)
  (let* ((old-needed ((record-accessor bindings-type 'needed-fluids) bindings))
         (old-in-module (or (assoc-ref old-needed module) '()))
         (new-in-module (if (memq sym old-in-module)
                          old-in-module
                          (cons sym old-in-module)))
         (new-needed (assoc-set! old-needed module new-in-module)))
    ((record-modifier bindings-type 'needed-fluids) bindings new-needed)))


; Cycle through all fluids needed in order to generate the code for their
; creation or some other analysis.

(define (map-fluids-needed bindings proc)
  (let ((needed ((record-accessor bindings-type 'needed-fluids) bindings)))
    (let iterate-modules ((mod-tail needed)
                          (mod-result '()))
      (if (null? mod-tail)
        mod-result
        (iterate-modules
          (cdr mod-tail)
          (let* ((aentry (car mod-tail))
                 (module (car aentry))
                 (symbols (cdr aentry)))
            (let iterate-symbols ((sym-tail symbols)
                                  (sym-result mod-result))
              (if (null? sym-tail)
                sym-result
                (iterate-symbols (cdr sym-tail)
                                 (cons (proc module (car sym-tail))
                                       sym-result))))))))))


; Get the current lexical binding (gensym it should refer to in the current
; scope) for a symbol or #f if it is dynamically bound.

(define (get-lexical-binding bindings sym)
  (let* ((lex ((record-accessor bindings-type 'lexical-bindings) bindings))
         (slot (hash-ref lex sym #f)))
    (if slot
      (fluid-ref slot)
      #f)))


; Establish a binding or mark a symbol as dynamically bound for the extent of
; calling proc.

(define (with-symbol-bindings bindings syms targets proc)
  (if (or (not (list? syms))
          (not (and-map symbol? syms)))
    (error "can't bind non-symbols" syms))
  (let ((lex ((record-accessor bindings-type 'lexical-bindings) bindings)))
    (for-each (lambda (sym)
                (if (not (hash-ref lex sym))
                  (hash-set! lex sym (make-fluid))))
              syms)
    (with-fluids* (map (lambda (sym)
                         (hash-ref lex sym))
                       syms)
                  targets
                  proc)))

(define (with-lexical-bindings bindings syms targets proc)
  (if (or (not (list? targets))
          (not (and-map symbol? targets)))
    (error "invalid targets for lexical binding" targets)
    (with-symbol-bindings bindings syms targets proc)))

(define (with-dynamic-bindings bindings syms proc)
  (with-symbol-bindings bindings
                        syms (map (lambda (el) #f) syms)
                        proc))
