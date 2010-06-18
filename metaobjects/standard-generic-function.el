;;; standard-generic-function.el --- standard-generic-function metaobject
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
;; This file contains the `standard-generic-function' metaobject.


;;; History:
;;
;; 0.1 - Initial version.


;;; Code:
;;

(moped-defclass standard-generic-function (generic-function)
  ((name                      :initarg  :name
			      :type     (or symbol list)
			      :reader   generic-function-name)
   (lambda-list               :initarg  :lambda-list
			      :type     list
			      :reader   generic-function-lambda-list)
   (argument-precedence-order :initarg  :argument-precedence-order
			      :reader   generic-function-argument-precedence-order)
   (declarations              :initarg  declarations
			      :type     list
			      :reader   generic-function-declarations)
   (method-combination        :initarg  :method-combination
			      :type     standard-method-combination
			      :reader   generic-function-method-combination)
   (method-class              :initarg  method-class
			      :type     standard-class
			      :reader   generic-function-method-class)
   (methods                   :initarg  :methods
			      :type     standard-method
			      :initform nil
			      :reader   generic-function-methods)))

(provide 'moped/metaobjects/standard-generic-function)
;;; standard-generic-function.el ends here
