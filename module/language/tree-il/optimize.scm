;;; Tree-il optimizer

;; Copyright (C) 2009, 2010-2015, 2018-2020 Free Software Foundation, Inc.

;;;; This library is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public
;;;; License as published by the Free Software Foundation; either
;;;; version 3 of the License, or (at your option) any later version.
;;;; 
;;;; This library is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this library; if not, write to the Free Software
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA

;;; Code:

(define-module (language tree-il optimize)
  #:use-module (language tree-il)
  #:use-module (language tree-il debug)
  #:use-module (language tree-il eta-expand)
  #:use-module (language tree-il fix-letrec)
  #:use-module (language tree-il letrectify)
  #:use-module (language tree-il peval)
  #:use-module (language tree-il primitives)
  #:use-module (ice-9 match)
  #:use-module (system base optimize)
  #:export (optimize
            make-lowerer
            tree-il-optimizations))

(define (kw-arg-ref args kw default)
  (match (memq kw args)
    ((_ val . _) val)
    (_ default)))

(define *debug?* #f)

(define (maybe-verify x)
  (if *debug?*
      (verify-tree-il x)
      x))

(define (optimize x env opts)
  (define-syntax-rule (run-pass pass kw default)
    (when (kw-arg-ref opts kw default)
      (set! x (maybe-verify (pass x)))))
  (define (resolve* x) (resolve-primitives x env))
  (define (peval* x) (peval x env))
  (define (letrectify* x)
    (let ((seal? (kw-arg-ref opts #:seal-private-bindings? #f)))
      (letrectify x #:seal-private-bindings? seal?)))
  (maybe-verify x)
  (run-pass resolve*           #:resolve-primitives? #t)
  (run-pass expand-primitives  #:expand-primitives?  #t)
  (run-pass letrectify*        #:letrectify?         #t)
  (set! x (fix-letrec x))
  (run-pass peval*             #:partial-eval?       #t)
  (run-pass eta-expand         #:eta-expand?         #t)
  x)

(define (tree-il-optimizations)
  (available-optimizations 'tree-il))

(define (make-lowerer optimization-level opts)
  (define (enabled-for-level? level) (<= level optimization-level))
  (let ((opts (let lp ((all-opts (tree-il-optimizations)))
                (match all-opts
                  (() '())
                  (((kw level) . all-opts)
                   (acons kw (kw-arg-ref opts kw (enabled-for-level? level))
                          (lp all-opts)))))))
    (lambda (exp env)
      (optimize exp env opts))))
