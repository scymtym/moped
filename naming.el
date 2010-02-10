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


;;; History:
;;
;; 0.1 - Initial version


;;; Code:
;;

(defvar eieio-naming-classes (make-hash-table :test 'eq)
  "")

(defvar eieio-naming-generics (make-hash-table)
  "")

(defvar eieio-naming-methods (make-hash-table)
  "")


;;; Naming functions for class meta objects
;;

(defun find-class (name)
  ""
  (gethash name eieio-naming-classes))

(defun ensure-class (name direct-superclasses direct-slots options)
  ""
  (puthash
   name
   (ensure-class-using-class
    (find-class name)
    name direct-superclasses direct-slots options)
   eieio-naming-classes))

;; TODO should be generic
(defun ensure-class-using-class (class name direct-superclasses direct-slots options)
  ""
  (let ((direct-superclasses-objects (mapcar #'eieio-naming-maybe-create-forward-class
					     direct-superclasses)))
    (if class
	(ensure-class-using-class-existing
	 class name direct-superclasses-objects direct-slots options)
      (ensure-class-using-class-null
       class name direct-superclasses-objects direct-slots options))))

(defun ensure-class-using-class-existing (class name direct-superclasses direct-slots options)
  ""
  (error "changing classes is not implemented"))

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


;;; Naming functions for generic function meta objects
;;

(defun find-generic-function (name)
  ""
  (gethash name eieio-naming-generics))

(defun ensure-generic-function (name)
  ""
  )


;;; Naming functions for method meta objects
;;

(defun find-method (name)
  ""
  )

(defun find-method-combination (name)
  ""
  )


;;; Utility functions
;;

(defun eieio-naming-clear-classes ()
  ""
  (setq eieio-naming-classes (make-hash-table :test 'eq)))

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
  (make-instance 'forward-referenced-class)) ;; TODO

(provide 'naming)
;;; naming.el ends here
