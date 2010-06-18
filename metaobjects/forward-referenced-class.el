;;; forward-referenced-class.el ---
;;
;; Copyright (C) 2010 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;; Keywords: mop. oop
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


;;; Metaobject `forward-referenced-class'
;;

(moped-defclass forward-referenced-class (class)
  ())

(moped-defmethod finalize-inheritance ((class forward-referenced-class))
  (signal 'finalizing-foward-refenrece-class (list class)))

(provide 'moped/metaobjects/forward-referenced-class)
;;; forward-referenced-class.el ends here
