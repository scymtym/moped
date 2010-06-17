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
;; 0.2 - Changed prefix to moped
;;
;; 0.1 - Initial version


;;; Code:
;;

(require 'moped/impl)
(require 'moped/naming)
(require 'moped/macros)

(require 'moped/metaobjects/standard-class)


;;; Bootstrap Functions
;;

(defun moped-make-standard-class ()
  "Allocate, initialize and return standard-class metaobject."
  (let ((object (make-vector moped-standard-class-num-slots nil)))
    (aset object moped-standard-class-tag                 'moped-defclass)
    (aset object moped-standard-class-name                'standard-class)
    (aset object moped-standard-class-direct-superclasses nil)
    (aset object moped-standard-class-subclasses          nil)
    (aset object moped-standard-class-direct-slots        nil)
    (aset object moped-standard-class-effective-slots     nil)
    object))


;;; Actual Bootstrap Sequence
;;

(defun moped-bootstrap-object-system ()
  ""
  ;; Clear all classes
  (moped-naming-clear-classes)

  ;; Create and store metaobject `standard-class'
  (puthash 'standard-class (moped-make-standard-class)
           moped-naming-classes)

  ;; Create metaobject `forward-referenced-class'
  ;; TODO superclass standard-object?
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

(provide 'moped/bootstrap)
;;; bootstrap.el ends here
