;;; naming.el ---
;;
;; Copyright (C) 2010 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: oop, clos, mop, naming
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
;; This file contains the naming layer of the object system. Named
;; objects are:
;;
;; + classes
;; + generic functions
;; + methods
;; + method combinations


;;; History:
;;
;; 0.2 - Changed prefix to moped.
;;
;; 0.1 - Initial version.


;;; Code:
;;

(require 'moped/errors)


;;; Hash-tables for named objects
;;

(defvar moped-naming-classes (make-hash-table :test 'equal)
  "Hash-table that maps names of named classes to the
corresponding class metaobjects.")

(defvar moped-naming-generic-functions (make-hash-table :test 'equal)
  "Hash-table that maps names of generic functions to the
  corresponding generic function metaobjects.")

(defvar moped-naming-methods (make-hash-table :test 'equal)
  "")


;;; Naming functions for class metaobjects
;;

;; TODO errorp should default to true
(defun moped-find-class (name &optional errorp)
  ""
  (or (gethash name moped-naming-classes)
      (when errorp
	(signal 'no-such-class (list name)))))

(defun ensure-class (name direct-superclasses direct-slots options)
  ""
  (puthash
   name
   (ensure-class-using-class
    (moped-find-class name)
    name direct-superclasses direct-slots options)
   moped-naming-classes)) ;; TODO who should do the puthashing?

;; TODO not part of naming infrastructure?
;; TODO should be generic function
(defun ensure-class-using-class (class name direct-superclasses direct-slots options)
  ""
  (let ((direct-superclasses-objects
	 (mapcar #'moped-naming-maybe-create-forward-class
		 direct-superclasses)))
    (if class
	(ensure-class-using-class-existing
	 class name direct-superclasses-objects direct-slots options)
      (ensure-class-using-class-null
       class name direct-superclasses-objects direct-slots options))))

(defun ensure-class-using-class-existing (class name direct-superclasses direct-slots options)
  ""
  (error "changing class metaobjects is not implemented"))

(defun ensure-class-using-class-null (class name direct-superclasses direct-slots options)
  ""
  (let* ((metaclass-symbol (or (plist-get options :metaclass)
			       'standard-class))
	 (metaclass       (moped-find-class metaclass-symbol))) ;; TODO Do this lookup earlier?
    (unless metaclass
      (signal 'invalid-metaclass (list metaclass-symbol)))

    (moped-make-instance
     metaclass
     :name                name
     :direct-superclasses direct-superclasses
     :direct-slots        direct-slots)))


;;; Naming functions for generic function metaobjects
;;

(defun find-generic-function (name)
  ""
  (gethash name moped-naming-generic-functions))

(defun ensure-generic-function (name args doc options)
  ""
  (puthash
   name
   (ensure-generic-function-using-class
    (find-generic-function name)
    name args doc options)
   moped-naming-generic-functions)) ;; TODO who should do the puthashing?

;; TODO not part of naming infrastructure
;; TODO should be generic
(defun ensure-generic-function-using-class (generic name args doc options)
  ""
  (if generic
      (ensure-generic-function-using-class-existing generic name args doc options)
    (ensure-generic-function-using-class-null generic name args doc options))
  )

(defun ensure-generic-function-using-class-existing (generic name args doc options)
  (warn "changing generic function metaobjects is not implemented")
  generic)

(defun ensure-generic-function-using-class-null (generic name args doc options)
  (let* ((metaclass-symbol (or (plist-get options :metaclass)
			       'standard-generic-function))
	 (metaclass        (moped-find-class metaclass-symbol)))
    (unless metaclass
      (signal 'invalid-metaclass (list metaclass-symbol)))

    (apply
     #'moped-make-instance
     metaclass
     :name        name
     :lambda-list args
     options))
  )


;;; Naming functions for method metaobjects
;;

;; TODO not in CLOS(?) or AMOP
(defun find-method (name specializers)
  ""
  (gethash (list name specializers) eieio-naming-methods))

;; TODO not in CLOS(?) or AMOP
(defun ensure-method (name specializers qualifiers args doc body)
  ""
  (let* ((generic-function (ensure-generic-function
			    name args doc nil))
	 (method           (moped-make-instance
			    (moped-find-class 'standard-method) ;; TODO use correct method class
			    :qualifiers   qualifiers
			    :lambda-list  args
			    :specializers specializers)))
    (multiple-value-bind (method-function initargs)
	(make-method-lambda
	 generic-function method
	 `(lambda ,args ,body))
      (moped-set-slot-value method :function method-function)
      (add-method generic-function method)
      method))
  )


;;; Naming functions for method-combination metaobjects
;;

(defun find-method-combination (name)
  ""
  )

(defun ensure-method-combination (name)
  ""
  (ensure-method-combination-using-class
   (find-method-combination name)
   name))


;;; Utility functions
;;

(defun moped-naming-clear-classes ()
  ""
  (setq moped-naming-classes (make-hash-table :test 'equal)))

(defun moped-naming-clear-generic-functions ()
  ""
  (setq moped-naming-generic-functions (make-hash-table :test 'equal)))

(defun moped-naming-maybe-find-class (name-or-class)
  ""
  (if (symbolp name-or-class)  ;; TODO ugly and maybe wrong
      (moped-find-class name-or-class)
    name-or-class))

(defun moped-naming-maybe-create-forward-class (name-or-class)
  ""
  (or (moped-naming-maybe-find-class name-or-class)
      (moped-naming-create-forward-class name-or-class)))

(defun moped-naming-create-forward-class (name)
  ""
  (moped-make-instance 'forward-referenced-class
		       :name name)) ;; TODO

(provide 'moped/naming)
;;; naming.el ends here
