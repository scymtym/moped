;;; class.el ---
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

(eval-when-compile
  (require 'cl))

(require 'moped/macros)


;;; Metaobject `class'
;;

(moped-defclass class (specializer)
  ((name                    :initarg :name
			    :type    (or symbol list)
			    :reader  class-name)
   (direct-superclasses     :initarg :direct-superclasses
			    :type    list
			    :reader  class-direct-superclasses)
   (direct-subclasses       :initarg :direct-subclasses
			    :type    list
			    :reader  class-direct-subclasses)
   (direct-slots            :initarg :direct-slots
			    :type    list
			    :reader  class-direct-slots)
   (slots                   :initarg :slots
			    :type    list
			    :reader  class-slots)
   ;; (direct-default-initargs :initarg direct-:default-initarg
   ;;			    :type    list
   ;;			    :reader  class-direct-default-initargs)
   ;; (default-initargs        :initarg :default-initarg
   ;;                          :type    list
   ;;			    :reader  class-default-initargs)
   (class-precedence-list   :initarg :class-precedence-list
			    :type    list
			    :reader  class-precedence-list)
   (finalized-p             :initarg :finalized-p
			    :type    boolean
			    :reader  class-finalized-p))) ;; abstract

(moped-defmethod add-direct-subclass ((class class) (subclass class))
  ""
  ;; TODO use push
  (moped-set-slot-value
   class :direct-subclasses
   (cons subclass (moped-slot-value class :direct-subclasses))))

(moped-defmethod remove-direct-subclass ((class class) (subclass class))
  ""
  (moped-set-slot-value
   class :direct-subclasses
   (remove* subclass (moped-slot-value class :direct-subclasses)
	    :test #'eq)))

(moped-defmethod finalize-inheritance ((class class))
  ""
  (moped-set-slot-value class :class-precedence-list (compute-class-precedence-list class))
  (moped-set-slot-value class :slots (moped-compute-slots class))
  (moped-set-slot-value class :finalized-p t))

(moped-defmethod compute-class-precedence-list ((class class))
  ""
  (list class (find-class 'standard-object) t))

(moped-defmethod compute-slots ((class class))
  ""
  (error "compute-slots not implemented"))

(provide 'moped/metaobjects/class)
;;; class.el ends here
