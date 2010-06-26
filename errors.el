;;; errors.el ---
;;
;; Copyright (C) 2010 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;; Keywords: oop, clos, conditions, errors
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
;; This file contains condition symbols.


;;; History:
;;
;; 0.2 - Changed prefix to moped.
;;
;; 0.1 - Initial version.


;;; Code:
;;

;; no-such-class

(intern "no-such-class")

(put 'no-such-class 'error-conditions
     '(error no-such-class))

(put 'no-such-class 'error-message
     "No such class")

;; invalid-metaclass

(intern "invalid-metaclass")

(put 'invalid-metaclass 'error-conditions
     '(error invalid-metaclass))

(put 'invalid-metaclass 'error-message
     "Invalid metaclass")

;; slot-unbound

(intern "slot-unbound")

(put 'slot-unbound 'error-conditions
     '(error slot-unbound))

(put 'slot-unbound 'error-message
     "Slot unbound")

;; slot-missing

(intern "slot-missing")

(put 'slot-missing 'error-conditions
     '(error slot-missing))

(put 'slot-missing 'error-message
     "Slot missing")

;; class-not-finalized

(intern "class-not-finalized")

(put 'class-not-finalized 'error-conditions
     '(error class-not-finalized))

(put 'class-not-finalized 'error-message
     "Class is not finalized")

;; no-applicable-method

(intern "no-applicable-method")

(put 'no-applicable-method 'error-conditions
     '(error no-applicable-method))

(put 'no-applicable-method 'error-message
     "No applicable method")

;; no-next-method

(intern "no-next-method")

(put 'no-next-method 'error-conditions
     '(error no-next-method))

(put 'no-next-method 'error-message
     "No next method")

(provide 'moped/errors)
;;; errors.el ends here
