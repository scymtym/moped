;;; impl.el ---
;;
;; Copyright (C) 2010 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;; Keywords: oop, clos
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

;; TODO this is a generic function
(defun make-instance (name-or-class &rest initargs) ;; TODO part of the user interface; not impl
  ""
  (cond
   ((symbolp name-or-class)
    (apply #'make-instance-symbol name-or-class initargs))

   ((eq name-or-class (find-class 'standard-class))
    (apply #'eieio-make-instance-standard-class name-or-class initargs))

   (t
    (error "not implemented")
    (apply (find-generic-function 'make-instance) name-or-class initargs))))

(defun make-instance-symbol (name &rest initargs)
  (let ((class (find-class name)))
    (unless class
      (signal 'no-such-class (list name)))

    (apply #'make-instance class initargs)))

(defun allocate-instance (class &rest initargs)
  ""
  ;; (unless (class-finalized-p class)
  ;;   (finalize-inheritance class))

  (if (eq class (find-class 'standard-class))
      (apply #'eieio-allocate-instance-standard-class class initargs)
    (apply (find-generic-function 'allocate-instance) class initargs)))

(defun initialize-instance (instance &rest initargs)
  ""
  (if (eq (aref instance eieio-standard-class-name) (find-class 'standard-class)) ;; TODO
      (apply #'eieio-initialize-instance-standard-class instance initargs)
    (warn "not implemented: initialize-instance %s %s" instance initargs)))

(defun object-class (instance)
  (if (eq instance (find-class 'standard-class))
      (aref instance eieio-standard-class-name)
    (invoke-generic-function
     (find-generic-function 'object-class)
     instance)))

(defun slot-value (instance slot-name)
  (if (eq (object-class instance) (find-class 'standard-class))
      (let ((index (case slot-name
		     (name
		      eieio-standard-class-name)
		     (direct-superclasses
		      eieio-standard-class-direct-superclasses))))
	(aref instance index))
    (invoke-generic-function
     (find-generic-function 'slot-value)
     slot-name)))

(defun slot-value-using-class (instance class slot-name)
  "TODO"
  (error "not implemented yet"))

(defun invoke-generic-function (function &rest args)
  "Invoke generic function metaobject FUNCTION"
  (let ((discriminating-function (oref function :discriminating-function)))
    (apply discriminating-function args)))

(provide 'eieio/impl)
;;; impl.el ends here