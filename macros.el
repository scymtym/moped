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
;; This file contains the interface macros
;;
;; + Defining metaobjects
;;   + `moped-defclass'
;;   + `moped-defgeneric'
;;   + `moped-defmethod'
;; + Slot access macros
;;   + `moped-oref'
;;   + `moped-oset'
;;   + `moped-with-slots'
;; + setf methods


;;; History:
;;
;; 0.2 - Changed prefix to moped.
;;
;; 0.1 - Initial version.


;;; Code:
;;

(eval-when-compile
  (require 'cl))


;;; Structure Definition Macros
;;

(defmacro moped-defclass (name direct-superclasses direct-slots
			  &rest options)
  "Define a class named NAME with given superclasses and slots."
  (declare (indent 1))

  ;; Check arguments.
  ;; TODO Should the lower layers check this?
  (unless (symbolp name)
    (signal 'wrong-type-argument (list (type-of name))))
  (unless (and (listp direct-superclasses)
	       (every #'symbolp direct-superclasses))
    (signal 'wrong-type-argument (list (type-of direct-superclasses))))
  (unless (and (listp direct-slots)
	       (every #'listp direct-slots))
    (signal 'wrong-type-argument (list (type-of direct-slots))))
  (unless (every #'listp options)
    (signal 'wrong-type-argument (list (type-of options))))
  ;; Further errors:
  ;; duplicate slot names are forbidden
  ;; duplicate slot options (for some options) are forbidden
  ;; duplicate default-initargs are forbidden

  (let ((normalized-direct-slots
	 (mapcar #'moped-macros-normalize-slot-definition
		 direct-slots)))
    `(ensure-class (quote ,name)
		   ,(when direct-superclasses
		      (list 'quote direct-superclasses))
		   ,(when normalized-direct-slots
		      (list 'quote normalized-direct-slots))
		   ,(when options
		      (list 'quote options))))
  )

(defmacro moped-defgeneric (name args &rest doc-and-options)
  "Define a generic function NAME with parameters ARGS."
  (declare (indent 2))

  (multiple-value-bind (doc options)
      (moped-macros-parse-defgeneric-doc-and-options doc-and-options)
    `(ensure-generic-function (quote ,name)
			      ,(when args
				(list 'quote args))
			      ,doc
			      ,(when options
				 (list 'quote options))))
  )

(defmacro moped-defmethod (name &rest qualifiers-args-doc-body)
  "Define a method NAME with given qualifiers, parameters and body."
  (declare (indent 2))

  ;; Check arguments.
  (unless (symbolp name) ;; (listp name) Names that are lists are not yet supported
    (signal 'wrong-type-argument (list (type-of name)))) ;; TODO invalid method name?

  ;;
  (multiple-value-bind (qualifiers args doc body)
      (moped-macros-parse-defmethod-qualifiers-args-doc-body
       qualifiers-args-doc-body)
    (let* ((arg-names           (mapcar #'moped-macros-remove-specializer
					args))
	   (specialization-args (moped-macros-extract-specializtion-args
				 args))
	   (arg-specializers    (mapcar #'moped-macros-extract-specializer
					specialization-args))
	   (specializers        (mapcar #'moped-macros-generate-specializer
					arg-specializers)))
      `(ensure-method (quote ,name)
		      ,(when specializers
			 (cons 'list specializers))
		      ,(when qualifiers
			 (list 'quote qualifiers))
		      ,(when arg-names
			 (list 'quote arg-names))
		      ,doc
		      (quote (progn ,@body))))) ;; TODO test for options
  )


;;; Slot Access Macros
;;

(defmacro moped-oref (instance slot-name)
  "Return the value of SLOT-NAME in INSTANCE."
  `(moped-slot-value ,instance (quote ,slot-name)))

(defmacro moped-oset (instance slot-name value)
  "Set SLOT-NAME to VALUE in INSTANCE."
  `(moped-set-slot-value ,instance (quote ,slot-name) ,value))

(defmacro moped-with-slots (specs instance &rest body)
  "Execute BODY with names bound to slots of INSTANCE."
  (declare (indent 2))

  (let ((instance-var (if (symbolp instance)
			  instance
			(make-symbol "instance")))
	(bindings))
    (dolist (spec specs)
      (let ((var-name  (if (listp spec) (nth 0 spec) spec))
	    (slot-name (if (listp spec) (nth 1 spec) spec)))
	(push
	 `(,var-name (moped-slot-value ,instance-var (quote ,slot-name)))
	 bindings)))
    (if (symbolp instance)
	`(symbol-macrolet ,bindings
	   (progn ,@body))
      `(let ((,instance-var ,instance))
	 (symbol-macrolet ,bindings
	   (progn ,@body)))))
  )

(define-setf-method moped-slot-value (instance slot-name)
  (let ((instance-temp  (make-symbol "instance"))
	(slot-name-temp (make-symbol "slot-name"))
	(store-temp     (make-symbol "store")))
    `((,instance-temp ,slot-name-temp)
      (,instance      ,slot-name)
      (,store-temp)
      (moped-set-slot-value
       ,instance-temp ,slot-name-temp ,store-temp)
      (moped-slot-value ,instance-temp ,slot-name-temp)))
  )


;;; Utility Functions
;;

(defun moped-macros-normalize-slot-definition (slot-definition)
  "TODO"
  slot-definition)

(defun moped-macros-parse-defgeneric-doc-and-options (doc-and-options)
  "TODO"
  (if (stringp (car doc-and-options))
      (list (car doc-and-options) (cdr doc-and-options))
    (list nil doc-and-options)))

(defun moped-macros-extract-specializtion-args (args)
  ""
  (flet ((position-of (keyword)
           (or (position keyword args :test #'eq)
	       most-positive-fixnum)))
    (let ((index (apply #'min
			(length args)
			(mapcar #'position-of '(&optional &rest)))))
      (subseq args 0 index)))
  )

(defun moped-macros-extract-specializer (arg)
  ""
  (if (listp arg) (second arg) t))

(defun moped-macros-remove-specializer (arg)
  ""
  (if (listp arg) (first arg) arg))

(defun moped-macros-parse-defmethod-qualifiers-args-doc-body (qualifiers-args-doc-body)
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

(defun moped-macros-generate-specializer (spec)
  ""
  (cond
   ((eq spec t)
    `(moped-find-class (quote t)))

   ((listp spec)
    `(TODO-eql-specializer (nth 1 spec)))

   (t
    `(progn
       (unless (moped-find-class (quote ,spec))
	 (warn "when defining `%s' could not find specializer `%s'" (quote ,name) (quote ,spec)))
       (moped-find-class (quote ,spec)))))  ;; TODO should probably use ensure-class
  )

(provide 'moped/macros)
;;; macros.el ends here
