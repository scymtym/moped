;;; specializer.el --- specializer metaobject
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


;;; Metaobject `specializer'
;;

(moped-defclass specializer (metaobject)
  ((direct-methods :initarg  :direct-methods
		   :type     list
		   :initform nil)
   (type           :initarg  :type))) ;; abstract


;;; Metaobject `eql-specializer'
;;

(moped-defclass eql-specializer (specializer)
  ((object :initarg :object)))

(moped-defmethod specializer-direct-methods ((specializer eql-specializer))
  (error "specializer-direct-methods not implemented"))

(moped-defmethod add-direct-method ((specializer eql-specializer) (method method))
  (error "add-direct-method not implemented"))

(moped-defmethod remove-direct-method ((specializer eql-specializer) (method method))
  (error "remove-direct-method not implemented"))

(moped-defmethod specializer-direct-generic-functions ((specializer eql-specializer))
  (error "specializer-direct-generic-functions not implemented"))

(provide 'moped/metaobjects/specializer)
;;; specializer.el ends here
