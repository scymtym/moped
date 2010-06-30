;;; funcallable-standard-object.el --- funcallable-standard-object metaobject
;;
;; Copyright (C) 2010 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;; Keywords: oop, mop
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
;; This file contains the `funcallable-standard-object' metaobject.


;;; History:
;;
;; 0.1 - Initial version.


;;; Code:
;;

(require 'moped/macros)


;;; Metaobject `funcallable-standard-object'
;;

(moped-defclass funcallable-standard-object (standard-object)
  ())

(moped-defmethod moped-initialize-instance ((instance funcallable-standard-object) &rest initargs)
  ""
  (apply #'moped-initialize-instance-standard-object
	 (funcallable-instance-data instance) initargs)
  instance)

(moped-defmethod slot-value-using-class ((instance funcallable-standard-object)
					 class slot-name)
  (moped-slot-value-using-class-standard-object
   (funcallable-instance-data instance)
   class
   slot-name))

(moped-defmethod set-slot-value-using-class ((instance funcallable-standard-object)
					     class slot-name value)
  (moped-set-slot-value-using-class-standard-object
   (funcallable-instance-data instance)
   class
   slot-name value))

(moped-defmethod slot-boundp-using-class ((instance funcallable-standard-object)
					  class slot-name)
  ""
  (moped-slot-boundp-using-class-standard-object
   (funcallable-instance-data instance)
   class
   slot-name))

(moped-defmethod slot-makunbound-using-class ((instance funcallable-standard-object)
					      class slot-name)
  ""
  (moped-slot-makunbound-using-class-standard-object
   (funcallable-instance-data instance)
   class
   slot-name))


;;;
;;

(defun moped-make-instance-funcallable-standard-object (class &rest initargs)
  ""
  (let ((instance (apply #'moped-allocate-instance-funcallable-standard-object
			 class initargs)))
    (apply #'moped-initialize-instance-standard-object
	   (funcallable-instance-data instance) initargs)
    instance))

(defun moped-allocate-instance-funcallable-standard-object (class &rest initargs)
  ""
  (let ((instance-data (apply #'moped-allocate-instance-standard-object class initargs)))
    `(lambda (&rest args)
       (if moped-funcallable-standard-object-always-false
	   ,instance-data
	 ,(copy-tree '(apply nil args)))))
  )


;;; Utility Functions
;;
;; Setf methods for these are defined in ../macros.el

(defvar moped-funcallable-standard-object-always-false nil
  "Note: I think, this has to be a variable (not a constant) to
keep the byte-compiler from optimizing.")

(defun funcallable-instance-data (funcallable)
  "Return the instance object stored in FUNCALLABLE."
  (nth 2 (nth 2 funcallable))
  )

(defun funcallable-instance-function (funcallable)
  "Return the function stored in FUNCALLABLE."
  (nth 1 (nth 3 (nth 2 funcallable))))

(defun set-funcallable-instance-function (funcallable function)
  "Set FUNCTION as function of FUNCALLABLE."
  (setf (nth 1 (nth 3 (nth 2 funcallable))) function))
;; TODO we could also set (byte-compile function) at this point

(provide 'moped/metaobjects/funcallable-standard-object)
;;; funcallable-standard-object.el ends here
