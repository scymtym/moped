;;; standard-object.el --- metaobject standard-object
;;
;; Copyright (C) 2010 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;; Keywords: mop, oop
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
;; 0.1 - Initial version.


;;; Code:
;;


;;; Metaobject `standard-object'
;;

(moped-defclass standard-object () ()) ;; TODO direct superclass T

(moped-defmethod moped-make-instance ((class standard-object) &rest initargs) ;; TODO usually specialized on class
  ""
  (apply #'moped-make-instance-standard-object class initargs))

(moped-defmethod moped-initialize-instance ((instance standard-object) &rest initargs)
  ""
  instance)

;; TODO these methods should receive a slot metaobject not just the slot name?
(moped-defmethod slot-value-using-class ((instance standard-object) class slot-name)
  ""
  (moped-slot-value-using-class-standard-object
   instance class slot-name))

(moped-defmethod set-slot-value-using-class ((instance standard-object) class slot-name new-value)
  ""
  (moped-set-slot-value-using-class-standard-object
   instance class slot-name new-value))

(moped-defmethod slot-boundp-using-class ((instance standard-object) class slot-name)
  ""
  (moped-slot-boundp-using-class-standard-object
   instance class slot-name))

(moped-defmethod slot-makunbound-using-class ((instance standard-object) class slot-name)
  ""
  (moped-slot-makunbound-using-class-standard-object
   instance class slot-name))


;;; Metaobject `metaobject'
;;

(moped-defclass metaobject (standard-object) ()) ;; abstract


;;;
;;

(defconst moped-standard-object-slot-unbound-marker
  (make-symbol "secret-unbound-marker")
  "")

(defun moped-make-instance-standard-object (class &rest initargs)
  ""
  (let ((instance (apply #'moped-allocate-instance-standard-object
			 class initargs)))
    (apply #'moped-initialize-instance-standard-object
	   instance initargs)))

(defun moped-allocate-instance-standard-object (class &rest initargs)
  ""
  (let* ((num-slots (length (moped-slot-value class :slots)))
	 (instance  (make-vector (+ num-slots 2) nil))) ;;moped-standard-object-slot-unbound-marker)))
    (aset instance 0 'object)
    (aset instance 1 class)
    instance)
  )

(defun moped-initialize-instance-standard-object (instance &rest initargs)
  ""
  (let* ((class (aref instance 1)) ;;(moped-class-of instance))
	 (slots (moped-slot-value class :slots)) ;; TODO can be done fast as well
	 (args  initargs))
    ;; TODO use SLOTS to set instance's slot values (when not covered
    ;; by INITARGS)
    (while args
      (let ((slot-index (moped-slot-definition-location-standard-direct-slot-definition
			 (car args) class)))
	(aset instance slot-index (cadr args)))
      (setq args (cddr args))))
  ;; Return the initialized instance.
  instance)

(defun moped-slot-value-using-class-standard-object (instance class slot-name)
  ""
  (let* ((index (moped-slot-definition-location-standard-direct-slot-definition
		 slot-name class))
	 (value (if index
		    (aref instance index)
		  (moped-slot-missing class instance slot-name 'oref))))
    (if (eq value moped-standard-object-slot-unbound-marker)
	(moped-slot-unbound class instance slot-name)
      value)))

(defun moped-set-slot-value-using-class-standard-object (instance class slot-name new-value)
  (let ((index (moped-slot-definition-location-standard-direct-slot-definition
		slot-name class)))
    (if index
	(aset instance index value)
      (moped-slot-missing
       class instance slot-name 'oset new-value))))

(defun moped-slot-boundp-using-class-standard-object (instance class slot-name)
  ""
  (let ((index (moped-slot-definition-location-standard-direct-slot-definition
		slot-name class)))
    (if index
	(not (eq (aref instance index)
		 moped-standard-object-slot-unbound-marker))
      (moped-slot-missing class instance slot-name 'boundp))))

(defun moped-slot-makunbound-using-class-standard-object (instance class slot-name)
  ""
  (let ((index (moped-slot-definition-location-standard-direct-slot-definition
		slot-name class)))
    (if index
	(aset instance index
	      moped-standard-object-slot-unbound-marker)
      (moped-slot-missing class instance slot-name 'makunbound))))

(provide 'moped/metaobjects/standard-object)
;;; standard-object.el ends here
