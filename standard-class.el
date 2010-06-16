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

(defconst eieio-standard-class-name 1
  "Class's symbol (self-referencing.).")

(defconst eieio-standard-class-direct-superclasses 2
  "Class direct superclasses parent slot.")

(defconst eieio-standard-class-subclasses 3
  "Class subclasses class slot.")

(defconst eieio-standard-class-direct-slots 4
  "Class direct superclasses parent slot.")

(defconst eieio-standard-class-effective-slots 5
  "Class subclasses class slot.")


;;; "Methods" of the metaobject standard-class
;;

(defun eieio-make-instance-standard-class (class &rest initargs)
  (let ((instance)) ;; TODO errors

    ;; Allocate the instance
    (setq instance (apply #'allocate-instance class initargs))

    ;; Initialize the instance
    (apply #'initialize-instance instance initargs)

    instance))

(defun eieio-allocate-instance-standard-class (class &rest initargs)
  (let ((name     (plist-get initargs :name)) ;; TODO can we avoid setting slots here?
	(instance (make-vector eieio-standard-class-num-slots nil)))
    (aset instance eieio-standard-class-tag  'defclass)
    (aset instance eieio-standard-class-name name)
    instance))

(defun eieio-initialize-instance-standard-class (instance &rest initargs)
  (let ((name                (plist-get initargs :name))
	(direct-superclasses (plist-get initargs :direct-superclasses))
	(direct-slots        (plist-get initargs :direct-slots))
	(effective-slots     (apply #'append
				    (mapcar
				     (lambda (class)
				       (aref class eieio-standard-class-direct-slots))
				     direct-superclasses)
				    (list direct-slots))))
    (aset instance eieio-standard-class-tag                 'defclass)
    (aset instance eieio-standard-class-name                name)
    (aset instance eieio-standard-class-direct-superclasses direct-superclasses)
    (aset instance eieio-standard-class-subclasses          nil)
    (aset instance eieio-standard-class-direct-slots        direct-slots)
    (aset instance eieio-standard-class-effective-slots     effective-slots)
    instance))

(provide 'eieio/standard-class)
;;; standard-class.el ends here
