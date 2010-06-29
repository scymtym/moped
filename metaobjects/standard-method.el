;;; standard-method.el --- standard-method metaobject
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
;; This file contains the metaobject `standard-method' which is the
;; default class of methods.


;;; History:
;;
;; 0.1 - Initial version.


;;; Code:
;;

(require 'moped/macros)


;;; Metaobject `generic-function'
;;

(moped-defclass generic-function
  (metaobject funcallable-standard-object)
  ()) ;; abstract


;;; Metaobject `method'
;;

(moped-defclass method (metaobject) ()) ;; abstract


;;; Metaobject `standard-method'
;;

(moped-defclass standard-method (method)
  ((generic-function :initarg :generic-function
		     :type    generic-function
		     :reader  method-generic-function)
   (qualifiers       :initarg :qualifiers
		     :type    list
		     :reader  method-qualifiers)
   (lambda-list      :initarg :lambda-list
		     :type    list
		     :reader  method-lambda-list)
   (specializers     :initarg :specializers
		     :type    list
		     :reader  method-specializers)
   (function         :initarg :function
		     :type    nil
		     :reader  method-function)))

;; TODO non CLOS temp impl
(moped-defmethod moped-object-print ((method standard-method) &rest strings)
  (let ((class            (moped-class-of method))
	(generic-function (moped-slot-value method :generic-function))
	(specializers     (moped-slot-value method :specializers)))
    (format "#<%s %s %s %s %s>"
	    (symbol-name (moped-slot-value class :name))
	    (symbol-name (moped-slot-value generic-function :name))
	    (mapconcat #'symbol-name (moped-slot-value method :qualifiers) " ")
	    (mapcar (lambda (spec) (symbol-name (moped-slot-value spec :name))) specializers)
	    (mapconcat #'identity strings " "))))

(provide 'moped/metaobjects/standard-method)
;;; standard-method.el ends here
