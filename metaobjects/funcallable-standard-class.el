;;; funcallable-standard-class.el ---
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


;;; Metaobject `funcallable-standard-class'
;;

(moped-defclass funcallable-standard-class (class)
  ())

(moped-defmethod moped-make-instance ((class funcallable-standard-class) &rest initargs)
  ""
  (let ((instance (apply #'moped-allocate-instance class initargs)))
    (apply #'moped-initialize-instance instance initargs)))
;; TODO AMOP says, we have this, but why? Only the allocation is
;; different

(moped-defmethod moped-allocate-instance ((class funcallable-standard-class) &rest initargs)
  ""
  (apply #'moped-allocate-instance-funcallable-standard-object class initargs))

(provide 'moped/metaobjects/funcallable-standard-class)
;;; funcallable-standard-class.el ends here
