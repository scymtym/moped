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


;;; Utility Functions
;;

(defvar moped-funcallable-standard-object-always-false nil
  "")

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
