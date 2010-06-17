;;; standard-class.el ---
;;
;; Copyright (C) 2010 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;; Keywords: oop, clos, mop, standard class
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
;; 1.0 - Initial version.


;;; Code:
;;


;;; Storage layout of standard-class
;;

(defconst eieio-standard-class-num-slots 26
  "Number of slots in the class definition object.")

(defconst eieio-standard-class-tag 0
  "Class's type indicator tag.")

(defconst eieio-standard-class-class 1
  "Class's type indicator tag.")

(defconst eieio-standard-class-name 2
  "Class's symbol (self-referencing.).")

(defconst eieio-standard-class-direct-superclasses 3
  "Class direct superclasses parent slot.")

(defconst eieio-standard-class-subclasses 4
  "Class subclasses class slot.")

(defconst eieio-standard-class-direct-slots 5
  "Class direct superclasses parent slot.")

(defconst eieio-standard-class-effective-slots 6
  "Class subclasses class slot.")


;;;
;;

(defun eieio-make-standard-class-metaobject ()
  "Allocate, initialize and return standard-class metaobject."
  (let ((instance (make-vector eieio-standard-class-num-slots nil)))
    (aset instance eieio-standard-class-tag   'object)
    (aset instance eieio-standard-class-class instance)
    (eieio-initialize-instance-standard-class
     instance
     :name 'standard-class)))


;;; "Methods" of the metaobject standard-class
;;

(defun eieio-make-instance-standard-class (class &rest initargs)
  (let ((instance)) ;; TODO errors

    ;; Allocate the instance
    (setq instance (apply #'eieio-allocate-instance-standard-class class initargs))

    ;; Initialize the instance
    (apply #'eieio-initialize-instance-standard-class instance initargs)

    instance))

(defun eieio-allocate-instance-standard-class (class &rest initargs)
  (let ((instance (make-vector eieio-standard-class-num-slots nil))
	(class    (moped-find-class 'standard-class)))
    (aset instance eieio-standard-class-tag  'object)
    (aset instance eieio-standard-class-class class)
    instance))

(defun eieio-initialize-instance-standard-class (instance &rest initargs)
  (let* ((name                (plist-get initargs :name))
	 (direct-superclasses (plist-get initargs :direct-superclasses))
	 (direct-slot-specs   (plist-get initargs :direct-slots))
	 (direct-slots        (mapcar
			       (lambda (spec)
				 (apply
				  #'moped-make-instance
				  (moped-find-class 'standard-direct-slot-definition)
				  (cons :name spec)))
			       direct-slot-specs))
	 (effective-slots     (apply #'append
				     direct-slots
				     (mapcar
				      (lambda (class)
					;;(moped-slot-value class :direct-slots)
					(aref class eieio-standard-class-direct-slots))
				      direct-superclasses)))) ;; TODO recurse
    (aset instance eieio-standard-class-name                name)
    (aset instance eieio-standard-class-direct-superclasses direct-superclasses)
    (aset instance eieio-standard-class-subclasses          nil)
    (aset instance eieio-standard-class-direct-slots        direct-slots)
    (aset instance eieio-standard-class-effective-slots     effective-slots)
    instance))

(defun eieio-slot-value-using-class-standard-class (instance class slot-name)
  (case slot-name
    ((:name 'name)
     (aref instance eieio-standard-class-name))

    ((:direct-superclasses 'direct-superclasses)
     (aref instance eieio-standard-class-direct-superclasses))

    ((:subclasses 'subclasses)
     (aref instance eieio-standard-class-subclasses))

    ((:direct-slots 'direct-slots)
     (aref instance eieio-standard-class-direct-slots))

    ((:effective-slots 'effective-slots)
     (aref instance eieio-standard-class-effective-slots))

    (t
     (moped-slot-missing class instance slot-name 'slot-value))))

(provide 'eieio/standard-class)
;;; standard-class.el ends here
