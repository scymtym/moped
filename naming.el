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
;; + method combination


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
  "")

(defvar moped-naming-generic-functions (make-hash-table :test 'equal)
  "")

(defvar moped-naming-methods (make-hash-table)
  "")


;;; Naming functions for class metaobjects
;;

(defun moped-find-class (name &optional error-p)
  ""
  (or (gethash name moped-naming-classes)
      (when error-p
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
  (let ((metaclass (moped-find-class (or (plist-get options :metaclass)
				   'standard-class)))) ;; TODO Do this lookup earlier?
    (unless metaclass
      (error "invalid metaclass" ))
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
(defun ensure-generic-function-using-class (generic name args options)
  ""
  (if generic
      (ensure-generic-function-using-class-existing generic name args options)
    (ensure-generic-function-using-class-null generic name args options))
  )

(defun ensure-generic-function-using-class-existing (generic name args options)
  (error "changing generic function metaobjects is not implemented"))

(defun ensure-generic-function-using-class-null (generic name args options)
  (let ((metaclass (moped-find-class (or (plist-get options :metaclass)
				   'standard-generic-function))))
    (unless metaclass
      (error "invalid metaclass"))

    (moped-make-instance
     metaclass
     :name name
     :args args)))


;;; Naming functions for method metaobjects
;;

(defun find-method (name specializers)
  ""
  )

(defun ensure-method (name qualifiers args doc body)
  ""
  (ensure-method-using-class
   (find-method name)
   name qualifiers args doc body))

;; TODO should be generic
;; TODO not part of naming infrastructure
(defun ensure-method-using-class (method name qualifiers args doc body)
  ""
  (if method
      (ensure-method-using-class-existing method name qualifiers args)
    (ensure-method-using-class-null class method qualifiers args)))

(defun ensure-method-using-class-existing (method name qualifiers args doc body)
  ""
  (error "changing method metaobjects is not supported"))

(defun ensure-method-using-class-null (method name qualifiers args doc body)
  ""
  (let ((metaclass (moped-find-class (or (plist-get options :metaclass)
				   'standard-method))))
    (unless metaclass
      (error "invalid metaclass"))

    (moped-make-instance
     metaclass
     :name       name
     :qualifiers qualifiers
     :args       args)))


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

(defun moped-naming-maybe-moped-find-class (name-or-class)
  ""
  (if (symbolp name-or-class)  ;; TODO ugly and maybe wrong
      (moped-find-class name-or-class)
    name-or-class))

(defun moped-naming-maybe-create-forward-class (name-or-class)
  ""
  (or (moped-naming-maybe-moped-find-class name-or-class)
      (moped-naming-create-forward-class name-or-class)))

(defun moped-naming-create-forward-class (name)
  ""
  (moped-make-instance 'forward-referenced-class
		       :name name)) ;; TODO

(provide 'moped/naming)
;;; naming.el ends here
