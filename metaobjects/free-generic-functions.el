;;; free-generic-functions.el --- Generic functions with metaobjects
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

(require 'moped/errors)
(require 'moped/macros)

;; slot-missing

(moped-defgeneric moped-slot-missing (class instance slot-name operation &optional new-value)
  "This generic function is called when a non-existent slot is accessed.")

(moped-defmethod moped-slot-missing (class instance slot-name operation &optional new-value)
  ""
  (signal 'slot-missing (list slot-name))) ;;(list class instance slot-name operation new-value)))

;; slot-unbound

(moped-defgeneric moped-slot-unbound (class instance slot-name)
  "This generic function is called a non-existent slot is accessed.")

(moped-defmethod moped-slot-unbound (class instance slot-name)
  ""
  (signal 'slot-unbound (list slot-name))) ;;(list class instance slot-name)))

;; no-applicable-method

(moped-defgeneric moped-no-applicable-method (generic-function &rest args)
  "This generic function is called a non-existent slot is accessed.")

(moped-defmethod moped-no-applicable-method (generic-function &rest args)
  ""
  (signal 'no-applicable-method (list generic-function args)))

;; no-next-method

(moped-defgeneric moped-no-next-method (generic-function method &rest args)
  "This generic function is called when `call-next-method' is used in the absence of a next method.")

(moped-defmethod moped-no-next-method (generic-function method &rest args)
  ""
  (signal 'no-next-method (list generic-function method args)))

(provide 'moped/metaobjects/free-generic-functions)
;;; free-generic-functions.el ends here
