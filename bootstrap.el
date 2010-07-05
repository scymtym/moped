;;; bootstrap.el --- Bootstrapping of the meta object system
;;
;; Copyright (C) 2010 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: oop, clos, mop. bootstrap
;; X-RCS: $Id:$
;;
;; This Program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This Program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses>.


;;; Commentary:
;;
;; This file provides then function `moped-bootstrap-object-systen',
;; which is used to bootstrap the object system. Namely, it creates
;; and stores the metaobjects
;; + `standard-class'
;; + `forward-referenced-class'


;;; History:
;;
;; 0.3 - Bootstrap version of certain functions
;;     - Macros for binding bootstrap functions
;;
;; 0.2 - Changed prefix to moped
;;
;; 0.1 - Initial version


;;; Code:
;;

(eval-when-compile
  (require 'cl))

(require 'moped/impl)
(require 'moped/naming)
(require 'moped/macros)

(require 'moped/metaobjects/standard-class)


;;; Bootstrap Functions
;;
;; These functions are poor man's versions of important functions and
;; generic functions of the meta object system. They are used during
;; bootstrapping when particular functions are needed but not yet
;; available.

(defun moped-bootstrap-make-instance-standard-direct-slot-definition (class &rest initargs)
  "Allocate, initialize and return a `standard-direct-slot-definition' instance."
  (let ((instance (make-vector 10 nil)))
    (aset instance 0 'object)
    (aset instance 1 class)
    (aset instance 4 (plist-get initargs :name))
    (aset instance 5 (list (plist-get initargs :initarg)))
    (aset instance 6 (plist-get initargs :type))
    (aset instance 7 (or (plist-get initargs :allocation)
			 'instance))
    (aset instance 8 (plist-get initargs :initform))
    (aset instance 9 (plist-get initargs :initfunction))
    (aset instance 2 (list (plist-get initargs :reader)))
    (aset instance 3 (list (plist-get initargs :writer)))
    instance))

(defun moped-bootstrap-slot-definition-location-standard-direct-slot-definition (name class)
  (let ((slot-index (position
		     name
		     (moped-slot-value-using-class-standard-class class nil :slots)
		     :test #'memq
		     :key  (lambda (slot)
			     (cons (aref slot 4) ;; slot name
				   (aref slot 5) ;; list of initargs
				   )))))
    (when slot-index
      (+ 2 slot-index))))

(defun moped-bootstrap-moped-class-of (instance)
  "Bootstrap version of `class-of'"
  (if (listp instance)
      (moped-class-of (funcallable-instance-data instance))
    (aref instance moped-standard-class-class)))

(defun moped-bootstrap-moped-make-instance (class &rest initargs)
  "Bootstrap version of `make-instance'"
  (cond
   ((or (eq class moped-standard-class-metaobject)
	(eq class (moped-find-class 'funcallable-standard-class)))
    (apply #'moped-make-instance-standard-class class initargs))

   ((eq class (moped-find-class 'standard-direct-slot-definition))
    (apply #'moped-make-instance-standard-direct-slot-definition
	   class initargs))

   ((eq class (moped-find-class 'standard-generic-function))
    (apply #'moped-make-instance-funcallable-standard-object
	   class initargs))

   (t
    (apply #'moped-make-instance-standard-object class initargs)))
  )

(defun moped-bootstrap-ensure-class-using-class-existing (class name direct-superclasses direct-slots options)
  "Bootstrap version of `ensure-class-using-class'"
  (moped-initialize-instance-standard-class
   class
   :name                name
   :direct-superclasses direct-superclasses
   :direct-slots        direct-slots))

(defun moped-bootstrap-moped-slot-value (instance slot-name)
  "Bootstrap version of `slot-value'"
  (cond
   ((eq (moped-class-of instance) moped-standard-class-metaobject)
    (moped-slot-value-using-class-standard-class
     instance (moped-class-of instance) slot-name))

   ((moped-funcallable-standard-object-instance-p instance)
    (moped-slot-value
     (funcallable-instance-data instance) slot-name))

   (t
    (moped-slot-value-using-class-standard-object
     instance (moped-class-of instance) slot-name)))
  )

(defun moped-bootstrap-moped-set-slot-value (instance slot-name value)
  "Bootstrap version of `set-slot-value'"
  (cond
   ((moped-funcallable-standard-object-instance-p instance)
    (moped-oset
     (funcallable-instance-data instance) slot-name value))

   (t
    (moped-set-slot-value-using-class-standard-object
     instance (moped-class-of instance) slot-name value)))
  )

(defun moped-bootstrap-add-method (generic-function method)
  "Bootstrap version of `add-method'"
  (let ((name     (moped-oref generic-function :name))
	(function (moped-oref method           :function)))
    (push method (moped-oref generic-function :methods))
    (moped-oset method :generic-function generic-function)
    (set-funcallable-instance-function
     generic-function `(lambda (&rest args)
			 (call-method ,function args nil nil)))))

(defun moped-bootstrap-make-method-lambda (generic-function method lambda)
  "Bootstrap version of `make-method-lambda'"
  (list `(lambda (args &rest ignored) (apply ,lambda args)) nil))


;;; Actual Bootstrap Sequence
;;

(defun moped-bootstrap-object-system ()
  "Bootstrap the Moped object system."
  ;; Clear all classes
  (moped-naming-clear-classes)
  (moped-naming-clear-generic-functions)

  ;; Bootstrap Preparation
  ;;
  ;; Create and store preliminary versions of metaobjects:
  ;; + `standard-class'
  ;; + `standard-direct-slot-definition'
  (puthash 'standard-class (moped-make-standard-class-metaobject)
           moped-naming-classes)
  (puthash 'standard-direct-slot-definition
	   (moped-make-instance-standard-class
	    (moped-find-class 'standard-class))
	   moped-naming-classes)

  ;; Bootstrap Stage 1
  ;;
  ;; Create class metaobjects
  (moped-defclass forward-referenced-class () ())

  (moped-defclass standard-object () ())

  (moped-defclass standard-generic-function-10 (standard-object)
    ((name         :initarg :name
		   :type    (or symbol list))
     (methods      :initarg  :methods
		   :type     standard-method
		   :initform nil)
     (method-class :initarg method-class
		   :type    standard-class)))

  (moped-defclass standard-method (standard-object)
    ((specializers :initarg :specializers
		   :type    list)))

  nil)


;;; Utility Functions
;;

(defmacro moped-without-functions (functions &rest body)
  "Execute BODY with certain functions replaced with `ignore'."
  (declare (indent 1))

  (let ((bindings))
    (dolist (function functions)
      (push
       `(,function (&rest args) nil)
       bindings))

    `(flet ,bindings
       (progn ,@body)))
  )

(defmacro moped-with-bootstrap-functions (functions &rest body)
  "Execute BODY with bootstrap versions of FUNCTIONS.
FUNCTIONS is a list of symbol specifying functions for which
bootstrap versions should be used."
  (declare (indent 1))

  (let ((bindings))
    (dolist (function functions)
      (let ((target-function
	     (intern (concat "moped-bootstrap-" (symbol-name function)))))
	(push
	 `(,function (&rest args)
		     (apply (function ,target-function) args))
	 bindings)))

    `(flet ,bindings
       (progn ,@body)))
  )

(provide 'moped/bootstrap)
;;; bootstrap.el ends here
