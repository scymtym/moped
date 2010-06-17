;;; macros.el --- MOPED user interface macros
;;
;; Copyright (C) 2010 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: oop, clos, mop, macros, frontend
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
;; 0.2 - Changed prefix to moped
;;
;; 0.1 - Initial version.


;;; Code:
;;

(eval-when-compile
  (require 'cl))


;;; Macros
;;

(defmacro moped-defclass (name direct-superclasses direct-slots &rest options)
  ""
  ;; TODO Should the lower layers check this?
  (unless (symbolp name)
    (signal 'wrong-type-argument (type-of name)))
  (unless (and (listp direct-superclasses)
	       (every #'symbolp direct-superclasses))
    (signal 'wrong-type-argument (type-of direct-superclasses))) ;; TODO use (every #'moped-find-class or #'ensure-class ?
  (unless (and (listp direct-slots)
	       (every #'listp direct-slots))
    (signal 'wrong-type-argument (type-of direct-slots)))
  (unless (every #'listp options)
    (signal 'wrong-type-argument (type-of options)))
  ;; Further errors:
  ;; duplicate slot names are forbidden
  ;; duplicate slot options (for some options) are forbidden
  ;; duplicate default-initargs are forbidden

  `(ensure-class
    (quote ,name)
    (quote ,direct-superclasses)
    (quote ,(mapcar #'moped-macros-normalize-slot-definition
		    direct-slots))
    ,options))

(defmacro moped-defgeneric (name args &rest doc-and-options)
  ""
  (multiple-value-bind (doc options)
      (moped-macros-parse-moped-defgeneric-doc-and-options doc-and-options)
    `(ensure-generic-function
      (quote ,name)
      (quote ,args)
      ,doc
      (quote ,options))))

(defmacro moped-defmethod (name &rest qualifiers-args-doc-body)
  ""
  (cond
   ((symbolp name)
    )
   ;; Names that are lists are not yet supported
   ((listp name)
    (signal 'wrong-type-argument (list 'list))) ;; TODO invalid method name?
   ;;
   (t
    (signal 'wrong-type-argument (list (type-of name)))))

  (multiple-value-bind (qualifiers args doc body)
      (moped-macros-parse-moped-defmethod-qualifiers-args-doc-body
       qualifiers-args-doc-body)
    (let ((specializers (mapcar
			 ;; TODO should we really generate the lookup code here?
			 (lambda (name)
			   `(moped-find-class (quote ,name))) ;; TODO equal-specializer
			 (remove-if
			  #'null
			  (mapcar #'moped-macros-extract-specializer
				  args))))
	  (arg-names    (mapcar #'moped-macros-remove-specializer
				args)))
      `(ensure-method
	(quote ,name)
	(list ,@specializers)
	(quote ,qualifiers)
	,arg-names
	,doc
	(quote ,body))))
  )


;;; Utility Functions
;;

(defun moped-macros-normalize-slot-definition (slot-definition)
  "TODO"
  slot-definition)

(defun moped-macros-parse-moped-defgeneric-doc-and-options (doc-and-options)
  "TODO"
  (if (stringp (car doc-and-options))
      (list (car doc-and-options) (cdr doc-and-options))
    (list nil doc-and-options)))

(defun moped-macros-extract-specializer (arg)
  (if (listp arg) (second arg) nil))

(defun moped-macros-remove-specializer (arg)
  (if (listp arg) (first arg) arg))

(defun moped-macros-parse-moped-defmethod-qualifiers-args-doc-body (qualifiers-args-doc-body)
  ""
  (let* ((args-start   (position-if #'listp qualifiers-args-doc-body)) ;; TODO can be nil
	 (qualifiers   (subseq qualifiers-args-doc-body 0 args-start))
	 (args         (nth args-start qualifiers-args-doc-body))
	 (doc-and-body (nthcdr (+ args-start 1) qualifiers-args-doc-body))
	 (doc          (when (stringp (car doc-and-body))
			 (car doc-and-body)))
	 (body         (if doc
			   (cdr doc-and-body)
			 doc-and-body)))
    (list qualifiers args doc body)))

(provide 'moped/macros)
;;; macros.el ends here
