;;; bootstrap.el --- Bootstrapping of the meta object system
;;
;; Copyright (C) 2010 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: oop, clos, mop. bootstrap
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

(require 'eieio/naming)

;; TODO move to standard.el or something
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


(defun eieio-make-standard-class ()
  ""
  (let ((object (make-vector eieio-standard-class-num-slots nil)))
    (aset object eieio-standard-class-tag                 'defclass)
    (aset object eieio-standard-class-name                'standard-class)
    (aset object eieio-standard-class-direct-superclasses nil)
    (aset object eieio-standard-class-subclasses          nil)
    (aset object eieio-standard-class-direct-slots        nil)
    (aset object eieio-standard-class-effective-slots     nil)
    object))


;;; Actual Bootstrap Sequence
;;

(defun eieio-bootstrap-object-system ()
  ""
  ;; Clear all classes
  (eieio-naming-clear-classes)

  ;; Create meta object `standard-class'
  (puthash 'standard-class (eieio-make-standard-class) eieio-naming-classes)

  ;; Create meta object `forward-referenced-class'
  ;; TODO superclass standard-object?
  (defclass forward-referenced-class () ())

  nil)

(provide 'eieio/bootstrap)
;;; bootstrap.el ends here
