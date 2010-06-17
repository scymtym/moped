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
;; This file provides then function `eieio-bootstrap-object-systen',
;; which is used to bootstrap the object system. Namely, it creates
;; and stores the metaobjects
;; + `standard-class'
;; + `forward-referenced-class'


;;; History:
;;
;; 0.1 - Initial version


;;; Code:
;;

(require 'eieio/impl)
(require 'eieio/naming)
(require 'eieio/macros)

(require 'eieio/metaobjects/standard-class)



;;; Bootstrap Functions
;;

(defun eieio-make-standard-class ()
  "Allocate, initialize and return standard-class metaobject."
  (let ((object (make-vector eieio-standard-class-num-slots nil)))
    (aset object eieio-standard-class-tag                 'defclass)
    (aset object eieio-standard-class-name                'standard-class)
    (aset object eieio-standard-class-direct-superclasses nil)
    (aset object eieio-standard-class-subclasses          nil)
    (aset object eieio-standard-class-direct-slots        nil)
    (aset object eieio-standard-class-effective-slots     nil)
    object))


;;; Actual Bootstrap Sequence
;;

(defun eieio-bootstrap-object-system ()
  ""
  ;; Clear all classes
  (eieio-naming-clear-classes)

  ;; Create and store metaobject `standard-class'
  (puthash 'standard-class (eieio-make-standard-class)
           eieio-naming-classes)

  ;; Create metaobject `forward-referenced-class'
  ;; TODO superclass standard-object?
  (defclass forward-referenced-class () ())

  (defclass standard-object () ())

  (defclass standard-generic-function-10 (standard-object)
    ((name         :initarg :name
		   :type    (or symbol list))
     (methods      :initarg  :methods
		   :type     standard-method
		   :initform nil)
     (method-class :initarg method-class
		   :type    standard-class)))

  (defclass standard-method (standard-object)
    ((specializers :initarg :specializers
		   :type    list)))

  nil)

(provide 'eieio/bootstrap)
;;; bootstrap.el ends here
