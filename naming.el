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
;; 0.1 - Initial version


;;; Code:
;;

(require 'eieio/errors)


;;; Hash-tables for named objects
;;

(defvar eieio-naming-classes (make-hash-table :test 'equal)
  "")

(defvar eieio-naming-generic-functions (make-hash-table :test 'equal)
  "")

(defvar eieio-naming-methods (make-hash-table)
  "")


;;; Naming functions for class metaobjects
;;

(defun find-class (name &optional error-p)
  ""
  (or (gethash name eieio-naming-classes)
      (when error-p
	(signal 'no-such-class (list name)))))

(defun ensure-class (name direct-superclasses direct-slots options)
  ""
  (puthash
   name
   (ensure-class-using-class
    (find-class name)
    name direct-superclasses direct-slots options)
   eieio-naming-classes)) ;; TODO who should do the puthashing?

;; TODO not part of naming infrastructure?
;; TODO should be generic function
(defun ensure-class-using-class (class name direct-superclasses direct-slots options)
  ""
  (let ((direct-superclasses-objects
	 (mapcar #'eieio-naming-maybe-create-forward-class
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
  (let ((metaclass (find-class (or (plist-get options :metaclass)
				   'standard-class)))) ;; TODO Do this lookup earlier?
    (unless metaclass
      (error "invalid metaclass" ))
    (make-instance
     metaclass
     :name                name
     :direct-superclasses direct-superclasses
     :direct-slots        direct-slots)))


;;; Naming functions for generic function metaobjects
;;

(defun find-generic-function (name)
  ""
  (gethash name eieio-naming-generic-functions))

(defun ensure-generic-function (name args doc options)
  ""
  (puthash
   name
   (ensure-generic-function-using-class
    (find-generic-function name)
    name args doc options)
   eieio-naming-generic-functions)) ;; TODO who should do the puthashing?

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
  (let ((metaclass (find-class (or (plist-get options :metaclass)
				   'standard-generic-function))))
    (unless metaclass
      (error "invalid metaclass"))

    (make-instance
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
  (let ((metaclass (find-class (or (plist-get options :metaclass)
				   'standard-method))))
    (unless metaclass
      (error "invalid metaclass"))

    (make-instance
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

(defun eieio-naming-clear-classes ()
  ""
  (setq eieio-naming-classes (make-hash-table :test 'equal)))

(defun eieio-naming-maybe-find-class (name-or-class)
  ""
  (if (symbolp name-or-class)  ;; TODO ugly and maybe wrong
      (find-class name-or-class)
    name-or-class))

(defun eieio-naming-maybe-create-forward-class (name-or-class)
  ""
  (or (eieio-naming-maybe-find-class name-or-class)
      (eieio-naming-create-forward-class name-or-class)))

(defun eieio-naming-create-forward-class (name)
  ""
  (make-instance 'forward-referenced-class
		 :name name)) ;; TODO

(provide 'eieio/naming)
;;; naming.el ends here
