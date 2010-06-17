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
;; 0.2 - Changed prefix to moped.
;;
;; 1.0 - Initial version.


;;; Code:
;;


;;; Storage layout of standard-class
;;

(defconst moped-standard-class-num-slots 26
  "Number of slots in the class definition object.")

(defconst moped-standard-class-tag 0
  "Class's type indicator tag.")

(defconst moped-standard-class-class 1
  "Class's type indicator tag.")

(defconst moped-standard-class-name 2
  "Class's symbol (self-referencing.).")

(defconst moped-standard-class-direct-superclasses 3
  "Class direct superclasses parent slot.")

(defconst moped-standard-class-subclasses 4
  "Class subclasses class slot.")

(defconst moped-standard-class-direct-slots 5
  "Class direct superclasses parent slot.")

(defconst moped-standard-class-effective-slots 6
  "Class subclasses class slot.")


;;;
;;

(defun moped-make-standard-class-metaobject ()
  "Allocate, initialize and return standard-class metaobject."
  (let ((instance (make-vector moped-standard-class-num-slots nil)))
    (aset instance moped-standard-class-tag   'object)
    (aset instance moped-standard-class-class instance)
    (moped-initialize-instance-standard-class
     instance
     :name 'standard-class)))


;;; "Methods" of the metaobject standard-class
;;

(defun moped-make-instance-standard-class (class &rest initargs)
  (let ((instance)) ;; TODO errors

    ;; Allocate the instance
    (setq instance (apply #'moped-allocate-instance-standard-class class initargs))

    ;; Initialize the instance
    (apply #'moped-initialize-instance-standard-class instance initargs)

    instance))

(defun moped-allocate-instance-standard-class (class &rest initargs)
  (let ((instance (make-vector moped-standard-class-num-slots nil))
	(class    (moped-moped-find-class 'standard-class)))
    (aset instance moped-standard-class-tag  'object)
    (aset instance moped-standard-class-class class)
    instance))

(defun moped-initialize-instance-standard-class (instance &rest initargs)
  (let* ((name                (plist-get initargs :name))
	 (direct-superclasses (plist-get initargs :direct-superclasses))
	 (direct-slot-specs   (plist-get initargs :direct-slots))
	 (direct-slots        (mapcar
			       (lambda (spec)
				 (apply
				  #'moped-make-instance
				  (moped-moped-find-class 'standard-direct-slot-definition)
				  (cons :name spec)))
			       direct-slot-specs))
	 (effective-slots     (apply #'append
				     direct-slots
				     (mapcar
				      (lambda (class)
					;;(moped-slot-value class :direct-slots)
					(aref class moped-standard-class-direct-slots))
				      direct-superclasses)))) ;; TODO recurse
    (aset instance moped-standard-class-name                name)
    (aset instance moped-standard-class-direct-superclasses direct-superclasses)
    (aset instance moped-standard-class-subclasses          nil)
    (aset instance moped-standard-class-direct-slots        direct-slots)
    (aset instance moped-standard-class-effective-slots     effective-slots)
    instance))

(defun moped-slot-value-using-class-standard-class (instance class slot-name)
  (case slot-name
    ((:name 'name)
     (aref instance moped-standard-class-name))

    ((:direct-superclasses 'direct-superclasses)
     (aref instance moped-standard-class-direct-superclasses))

    ((:subclasses 'subclasses)
     (aref instance moped-standard-class-subclasses))

    ((:direct-slots 'direct-slots)
     (aref instance moped-standard-class-direct-slots))

    ((:effective-slots 'effective-slots)
     (aref instance moped-standard-class-effective-slots))

    (t
     (moped-slot-missing class instance slot-name 'slot-value))))

(provide 'moped/standard-class)
;;; standard-class.el ends here
