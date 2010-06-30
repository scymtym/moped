;;; builtin.el ---
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
;; This file contains the definition of the `builtin-class' metaobject
;; and the following instances:
;;
;; + `t'
;;   + `symbol'


;;; History:
;;
;; 0.1 - Initial version.


;;; Code:
;;

(require 'moped/macros)


;;; Metaobject `builtin-class'
;;

(moped-defclass builtin-class (class)
  ())


;;; Metaobject `t'
;;

(moped-defclass t ()
  ()
  (:metaclass builtin-class))
;; TODO this fails in stage 1 but is required for stage 2
;; (moped-set-slot-value (moped-find-class 't) :type 't)


;;; Metaobject `symbol'
;;

;; same comments as for t
(moped-defclass symbol (t)
  ()
  (:metaclass builtin-class))

(provide 'moped/metaobjects/builtin)
;;; builtin.el ends here
