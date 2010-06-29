;;; types.el ---
;;
;; Copyright (C) 2010 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;; Keywords: mop, oop, types
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
;; This file contains the implementation of the interaction with the
;; common lisp type system. This is required for determining
;; applicable methods based on the types of the actual arguments.


;;; History:
;;
;; 0.1 - Initial version.


;;; Code:
;;

(eval-when-compile
  (require 'cl))


;;; Classes as Types
;;

;; TODO This is a temporary helper function
(defun moped-class-precedence-list (class)
  ""
  (remove-duplicates
   (cons
    class
    (apply
     #'append
     (mapcar
      #'moped-class-precedence-list
      (moped-slot-value-using-class-standard-class
       class nil :direct-superclasses))))))

(defun moped-types-make-instance-test (class)
  ""
  `(lambda (query)
     (and (moped-object-p query)
	  (memq (moped-naming-maybe-find-class (quote ,class))
		(moped-class-precedence-list
		 (moped-class-of query))))))

(defun moped-types-class-handler (class)
  ""
  `(satisfies ,(moped-types-make-instance-test class)))

(put (intern "class")
     'cl-deftype-handler #'moped-types-class-handler)


;;; Eql Specializers as Types
;;

(defun moped-types-make-eql-test (object)
  ""
  `(lambda (query)
     (eq query ,object)))

(defun moped-types-eql-handler (object)
  ""
  `(satisfies ,(moped-types-make-eql-test object)))

(put (intern "eql")
     'cl-deftype-handler #'moped-types-eql-handler)

(provide 'moped/types)
;;; types.el ends here
