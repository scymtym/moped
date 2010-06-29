;;; slot-definition.el --- slot-definition metaobject
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

(require 'moped/macros)


;;; Metaobject `slot-definition'
;;

(moped-defclass slot-definition (metaobject)
  ((name         :initarg :name
		 :type    symbol
		 :reader  slot-definition-name)
   (initargs     :initarg :initargs
		 :type    list
		 :reader  slot-definition-initargs)
   (type         :initarg :type
		 :type    symbol
		 :reader  slot-definition-type)
   (allocation   :initarg :allocation
		 :type    symbol
		 :reader  slot-definition-allocation)
   (initform     :initarg :initform
		 :type    list
		 :reader  slot-definition-initform)
   (initfunction :initarg :initfunction
		 :type    function
		 :reader  slot-definition-initfunction))) ;; abstract

(moped-defmethod moped-object-print ((slot slot-definition) &rest strings)
  "Return an unreadable string representation of CLASS."
  (format "#<%s %s %s %s %s>"
	  (moped-slot-value (moped-class-of slot) :name)
	  (moped-slot-value slot :name)
	  (moped-slot-value slot :type)
	  (moped-slot-value slot :allocation)
	  (mapconcat #'indentity strings " ")))


;;; Metaobject `direct-slot-definition'
;;

(moped-defclass direct-slot-definition (slot-definition)
  ((readers :initarg :readers
	    :type    list
	    :reader  slot-definition-readers)
   (writers :initarg :writers
	    :type    list
	    :reader  slot-definition-writers))) ;; abstract


;;; Metaobject `effective-slot-definition'
;;

(moped-defclass effective-slot-definition (slot-definition)
  ((location :initarg :location
	     :type    (integer 0)
	     :reader  slot-definition-location))) ;; abstract


;;; Metaobject `standard-slot-definition'
;;

(moped-defclass standard-slot-definition (slot-definition) ()) ;; abstract


;;; Metaobject `standard-direct-slot-definition'
;;

(moped-defclass standard-direct-slot-definition
  (standard-slot-definition direct-slot-definition)
  ())


;;; Metaobject `standard-effective-slot-definition'
;;

(moped-defclass standard-effective-slot-definition
  (standard-slot-definition effective-slot-definition)
  ())

(provide 'moped/metaobjects/slot-definition)
;;; slot-definition.el ends here
