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
;; 0.2 - Changed prefix to moped.
;;
;; 0.1 - Initial version.


;;; Code:
;;

(eval-when-compile
  (require 'cl))


;;; Predicates
;;

(defun moped-standard-object-instance-p (object)
  ""
  (and (vectorp object)
       (eq (aref object 0) 'object)))

(defun moped-funcallable-standard-object-instance-p (object)
  ""
  (and (listp object)
       (= (safe-length object) 3)
       (eq (nth 0 object) 'lambda)
       (eq (nth 1 (nth 2 object))
	   'moped-funcallable-standard-object-always-false)))

(defun moped-object-p (object)
  ""
  (or (moped-standard-object-instance-p object)
      (moped-funcallable-standard-object-instance-p object)))


;;;
;;

(defun allocate-instance (class &rest initargs)
  ""
  ;; (unless (class-finalized-p class)
  ;;   (finalize-inheritance class))

  (if (eq class moped-standard-class-metaobject) ;;(moped-find-class 'standard-class)
      (apply #'moped-allocate-instance-standard-class class initargs)
    (apply (find-generic-function 'allocate-instance) class initargs)))

(defun moped-initialize-instance (instance &rest initargs)
  ""
  (if (eq (aref instance moped-standard-class-name) moped-standard-class-metaobject) ;;(moped-find-class 'standard-class) ;; TODO
      (apply #'moped-initialize-instance-standard-class instance initargs)
    (warn "not implemented: initialize-instance %s %s" instance initargs)))

(defun moped-class-of (object)
  (cond
   ;; TODO temp?
   ((moped-standard-object-instance-p object)
    (aref object 1))

   ((moped-funcallable-standard-object-instance-p object)
    (aref (funcallable-instance-data object) 1))
    ;;(moped-class-of (funcallable-instance-data object)))

   (t
    (type-of object)))
  )


;;; Slot Access Functions
;;

(defun moped-slot-value (instance slot-name)
  (if (eq instance moped-standard-class-metaobject)
      (moped-slot-value-using-class-standard-class
       instance (moped-class-of instance) slot-name)

    (slot-value-using-class
     instance (moped-class-of instance) slot-name)))

(defun moped-set-slot-value (instance slot-name value)
  (set-slot-value-using-class
   instance (moped-class-of instance) slot-name value))

(defun moped-slot-boundp (instance slot-name)
  (slot-boundp-using-class
   instance (moped-class-of instance) slot-name))

(defun moped-slot-makunbound (instance slot-name)
  (slot-makunbound-using-class
   instance (moped-class-of instance) slot-name))


;;; Functions Related to Generic Functions
;;

(defun call-method (function args &rest more-args) ;; TODO macro?
  ""
  (apply function args more-args))

(provide 'moped/impl)
;;; impl.el ends here
