;;; Guile Emacs Lisp -*- lexical-binding: t -*-

;;; Copyright (C) 2011 Free Software Foundation, Inc.

;;; This library is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License as
;;; published by the Free Software Foundation; either version 3 of the
;;; License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;;; Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this library; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;;; 02110-1301 USA

;;; Code:

(defmacro @ (module symbol)
  `(guile-ref ,module ,symbol))

(defmacro eval-and-compile (&rest body)
  `(progn
     (eval-when-compile ,@body)
     (progn ,@body)))

(eval-and-compile
  (defun null (object)
    (if object nil t))
  (defun consp (object)
    (%funcall (@ (guile) pair?) object))
  (defun listp (object)
    (if object (consp object) t))
  (defun car (list)
    (if list (%funcall (@ (guile) car) list) nil))
  (defun cdr (list)
    (if list (%funcall (@ (guile) cdr) list) nil))
  (defun make-symbol (name)
    (%funcall (@ (guile) make-symbol) name))
  (defun signal (error-symbol data)
    (%funcall (@ (guile) throw) 'elisp-condition error-symbol data)))

(defmacro lambda (&rest cdr)
  `#'(lambda ,@cdr))

(defmacro prog1 (first &rest body)
  (let ((temp (make-symbol "prog1-temp")))
    `(let ((,temp ,first))
       (declare (lexical ,temp))
       ,@body
       ,temp)))

(defmacro prog2 (form1 form2 &rest body)
  `(progn ,form1 (prog1 ,form2 ,@body)))

(defmacro cond (&rest clauses)
  (if (null clauses)
      nil
    (let ((first (car clauses))
          (rest (cdr clauses)))
     (if (listp first)
         (let ((condition (car first))
               (body (cdr first)))
           (if (null body)
               (let ((temp (make-symbol "cond-temp")))
                 `(let ((,temp ,condition))
                    (declare (lexical ,temp))
                    (if ,temp
                        ,temp
                      (cond ,@rest))))
             `(if ,condition
                  (progn ,@body)
                (cond ,@rest))))
       (signal 'wrong-type-argument `(listp ,first))))))

(defmacro and (&rest conditions)
  (cond ((null conditions) t)
        ((null (cdr conditions)) (car conditions))
        (t `(if ,(car conditions)
                (and ,@(cdr conditions))
              nil))))

(defmacro or (&rest conditions)
  (cond ((null conditions) nil)
        ((null (cdr conditions)) (car conditions))
        (t (let ((temp (make-symbol "or-temp")))
             `(let ((,temp ,(car conditions)))
                (declare (lexical ,temp))
                (if ,temp
                    ,temp
                  (or ,@(cdr conditions))))))))

(defmacro lexical-let (bindings &rest body)
  (labels ((loop (list vars)
             (if (null list)
                 `(let ,bindings
                    (declare (lexical ,@vars))
                    ,@body)
               (loop (cdr list)
                     (if (consp (car list))
                         `(,(car (car list)) ,@vars)
                       `(,(car list) ,@vars))))))
    (loop bindings '())))

(defmacro lexical-let* (bindings &rest body)
  (labels ((loop (list vars)
             (if (null list)
                 `(let* ,bindings
                    (declare (lexical ,@vars))
                    ,@body)
               (loop (cdr list)
                     (if (consp (car list))
                         (cons (car (car list)) vars)
                       (cons (car list) vars))))))
    (loop bindings '())))

(defmacro while (test &rest body)
  (let ((loop (make-symbol "loop")))
    `(labels ((,loop ()
                 (if ,test
                     (progn ,@body (,loop))
                   nil)))
       (,loop))))

(defmacro unwind-protect (bodyform &rest unwindforms)
  `(funcall (@ (guile) dynamic-wind)
            #'(lambda () nil)
            #'(lambda () ,bodyform)
            #'(lambda () ,@unwindforms)))

(defun symbolp (object)
  (%funcall (@ (guile) symbol?) object))

(defun functionp (object)
  (%funcall (@ (guile) procedure?) object))

(defun symbol-function (symbol)
  (let ((f (%funcall (@ (language elisp runtime) symbol-function)
                     symbol)))
    (if (%funcall (@ (language elisp falias) falias?) f)
        (%funcall (@ (language elisp falias) falias-object) f)
      f)))

(defun eval (form)
  (%funcall (@ (system base compile) compile)
            form
            (%funcall (@ (guile) symbol->keyword) 'from)
            'elisp
            (%funcall (@ (guile) symbol->keyword) 'to)
            'value))

(defun %indirect-function (object)
  (cond
   ((functionp object)
    object)
   ((symbolp object)                    ;++ cycle detection
    (%indirect-function (symbol-function object)))
   ((listp object)
    (eval `(function ,object)))
   (t
    (signal 'invalid-function `(,object)))))

(defun apply (function &rest arguments)
  (%funcall (@ (guile) apply)
            (@ (guile) apply)
            (%indirect-function function)
            arguments))

(defun funcall (function &rest arguments)
  (%funcall (@ (guile) apply)
            (%indirect-function function)
            arguments))

(defun fset (symbol definition)
  (funcall (@ (language elisp runtime) set-symbol-function!)
           symbol
           (if (functionp definition)
               definition
             (funcall (@ (language elisp falias) make-falias)
                      #'(lambda (&rest args) (apply definition args))
                      definition)))
  definition)

(defun load (file)
  (funcall (@ (system base compile) compile-file)
           file
           (funcall (@ (guile) symbol->keyword) 'from)
           'elisp
           (funcall (@ (guile) symbol->keyword) 'to)
           'value)
  t)

;;; Equality predicates

(defun eq (obj1 obj2)
  (if obj1
      (funcall (@ (guile) eq?) obj1 obj2)
    (null obj2)))

(defun eql (obj1 obj2)
  (if obj1
      (funcall (@ (guile) eqv?) obj1 obj2)
    (null obj2)))

(defun equal (obj1 obj2)
  (if obj1
      (funcall (@ (guile) equal?) obj1 obj2)
    (null obj2)))

;;; Symbols

;;; `symbolp' and `symbol-function' are defined above.

(fset 'symbol-value (@ (language elisp runtime) symbol-value))
(fset 'set (@ (language elisp runtime) set-symbol-value!))
(fset 'makunbound (@ (language elisp runtime) makunbound!))
(fset 'fmakunbound (@ (language elisp runtime) fmakunbound!))
(fset 'boundp (@ (language elisp runtime) symbol-bound?))
(fset 'fboundp (@ (language elisp runtime) symbol-fbound?))

(defun defvaralias (new-alias base-variable &optional docstring)
  (let ((fluid (funcall (@ (language elisp runtime) symbol-fluid)
                        base-variable)))
    (funcall (@ (language elisp runtime) set-symbol-fluid!)
             new-alias
             fluid)
    base-variable))

;;; Numerical type predicates

(defun floatp (object)
  (and (funcall (@ (guile) real?) object)
       (or (funcall (@ (guile) inexact?) object)
           (null (funcall (@ (guile) integer?) object)))))

(defun integerp (object)
  (and (funcall (@ (guile) exact?) object)
       (funcall (@ (guile) integer?) object)))

(defun numberp (object)
  (funcall (@ (guile) real?) object))

(defun wholenump (object)
  (and (funcall (@ (guile) exact?) object)
       (funcall (@ (guile) integer?) object)
       (>= object 0)))

(defun zerop (object)
  (= object 0))

;;; Numerical comparisons

(fset '= (@ (guile) =))

(defun /= (num1 num2)
  (null (= num1 num2)))

(fset '< (@ (guile) <))
(fset '<= (@ (guile) <=))
(fset '> (@ (guile) >))
(fset '>= (@ (guile) >=))

(defun max (&rest numbers)
  (apply (@ (guile) max) numbers))

(defun min (&rest numbers)
  (apply (@ (guile) min) numbers))

;;; Arithmetic functions

(fset '1+ (@ (guile) 1+))
(fset '1- (@ (guile) 1-))
(fset '+ (@ (guile) +))
(fset '- (@ (guile) -))
(fset '* (@ (guile) *))
(fset '% (@ (guile) modulo))
(fset 'abs (@ (guile) abs))

;;; Floating-point rounding

(fset 'ffloor (@ (guile) floor))
(fset 'fceiling (@ (guile) ceiling))
(fset 'ftruncate (@ (guile) truncate))
(fset 'fround (@ (guile) round))

;;; Numeric conversion

(defun float (arg)
  (if (numberp arg)
      (funcall (@ (guile) exact->inexact) arg)
    (signal 'wrong-type-argument `(numberp ,arg))))

;;; List predicates

(fset 'not #'null)

(defun atom (object)
  (null (consp object)))

(defun nlistp (object)
  (null (listp object)))

;;; Lists

(fset 'cons (@ (guile) cons))
(fset 'list (@ (guile) list))
(fset 'make-list (@ (guile) make-list))
(fset 'append (@ (guile) append))
(fset 'reverse (@ (guile) reverse))

(defun car-safe (object)
  (if (consp object)
      (car object)
    nil))

(defun cdr-safe (object)
  (if (consp object)
      (cdr object)
    nil))

(defun setcar (cell newcar)
  (if (consp cell)
      (progn
        (funcall (@ (guile) set-car!) cell newcar)
        newcar)
    (signal 'wrong-type-argument `(consp ,cell))))

(defun setcdr (cell newcdr)
  (if (consp cell)
      (progn
        (funcall (@ (guile) set-cdr!) cell newcdr)
        newcdr)
    (signal 'wrong-type-argument `(consp ,cell))))

(defun nthcdr (n list)
  (let ((i 0))
    (while (< i n)
      (setq list (cdr list)
            i (+ i 1)))
    list))

(defun nth (n list)
  (car (nthcdr n list)))

(defun %member (elt list test)
  (cond
   ((null list) nil)
   ((consp list)
    (if (funcall test elt (car list))
        list
      (%member elt (cdr list) test)))
   (t (signal 'wrong-type-argument `(listp ,list)))))

(defun member (elt list)
  (%member elt list #'equal))

(defun memql (elt list)
  (%member elt list #'eql))

(defun memq (elt list)
  (%member elt list #'eq))

;;; Strings

(defun string (&rest characters)
  (funcall (@ (guile) list->string)
           (mapcar (@ (guile) integer->char) characters)))

;;; Sequences

(fset 'length (@ (guile) length))

(defun mapcar (function sequence)
  (funcall (@ (guile) map) function sequence))

;;; Property lists

(defun %plist-member (plist property test)
  (cond
   ((null plist) nil)
   ((consp plist)
    (if (funcall test (car plist) property)
        (cdr plist)
      (%plist-member (cdr (cdr plist)) property test)))
   (t (signal 'wrong-type-argument `(listp ,plist)))))

(defun %plist-get (plist property test)
  (car (%plist-member plist property test)))

(defun %plist-put (plist property value test)
  (let ((x (%plist-member plist property test)))
    (if x
        (progn (setcar x value) plist)
      (cons property (cons value plist)))))

(defun plist-get (plist property)
  (%plist-get plist property #'eq))

(defun plist-put (plist property value)
  (%plist-put plist property value #'eq))

(defun plist-member (plist property)
  (%plist-member plist property #'eq))

(defun lax-plist-get (plist property)
  (%plist-get plist property #'equal))

(defun lax-plist-put (plist property value)
  (%plist-put plist property value #'equal))

(defvar plist-function (funcall (@ (guile) make-object-property)))

(defun symbol-plist (symbol)
  (funcall plist-function symbol))

(defun setplist (symbol plist)
  (funcall (funcall (@ (guile) setter) plist-function) symbol plist))

(defun get (symbol propname)
  (plist-get (symbol-plist symbol) propname))

(defun put (symbol propname value)
  (setplist symbol (plist-put (symbol-plist symbol) propname value)))

;;; Nonlocal exits

(defmacro condition-case (var bodyform &rest handlers)
  (let ((key (make-symbol "key"))
        (error-symbol (make-symbol "error-symbol"))
        (data (make-symbol "data"))
        (conditions (make-symbol "conditions")))
    (flet ((handler->cond-clause (handler)
             `((or ,@(mapcar #'(lambda (c) `(memq ',c ,conditions))
                             (if (consp (car handler))
                                 (car handler)
                               (list (car handler)))))
               ,@(cdr handler))))
      `(funcall (@ (guile) catch)
                'elisp-condition
                #'(lambda () ,bodyform)
                #'(lambda (,key ,error-symbol ,data)
                    (let ((,conditions
                           (get ,error-symbol 'error-conditions))
                          ,@(if var
                                `((,var (cons ,error-symbol ,data)))
                              '()))
                      (cond ,@(mapcar #'handler->cond-clause handlers)
                            (t (signal ,error-symbol ,data)))))))))

(put 'error 'error-conditions '(error))
(put 'wrong-type-argument 'error-conditions '(wrong-type-argument error))
(put 'invalid-function 'error-conditions '(invalid-function error))
(put 'no-catch 'error-conditions '(no-catch error))
(put 'throw 'error-conditions '(throw))

(defmacro catch (tag &rest body)
  (let ((tag-value (make-symbol "tag-value"))
        (c (make-symbol "c"))
        (data (make-symbol "data")))
    `(let ((,tag-value ,tag))
       (condition-case ,c
           (progn ,@body)
         (throw
          (let ((,data (cdr ,c)))
            (if (eq (car ,data) ,tag-value)
                (car (cdr ,data))
              (signal 'throw ,data))))))))

(defun throw (tag value)
  (signal 'throw (list tag value)))
