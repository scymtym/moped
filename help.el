;;; help.el ---
;;
;; Copyright (C) 2010 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;; Keywords: mop, oop, help, describe
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
;; This file contains integration with Emacs' help system. Namely
;;
;; + `moped-describe-class'
;; + `moped-describe-generic-function'
;; + `moped-describe-method'


;;; History:
;;
;; 0.1 - Initial version.


;;; Code:
;;

(require 'help-fns)
(require 'help-mode)


;;; A Family of describe- Functions for Moped
;;

(defun moped-describe-class (class)
  ""
  (interactive
   (list (moped-read-class
	  "Describe class: " t)))

  ;; Setup backward link.
  (help-setup-xref (list #'moped-describe-class class)
		   (called-interactively-p 'interactive))

  ;; Add description
  (save-excursion
    (with-help-window (help-buffer)

      (moped-with-slots (name class-precedence-list finalized-p slots documentation) class
	;; Summary
	(princ (format "%s is a class in %s.\n" name "TODO"))
	(moped-help-print-metaclass class)
	(princ "\n\n")

	;; Class Precedence List
	(with-current-buffer standard-output
	  (insert (propertize "Class Precedence List:" 'face 'bold) "\n")
	  (if finalized-p
	      (insert (moped-help-print-list class-precedence-list #'moped-help-print-class))
	    (insert (propertize "inheritance not finalized" 'face 'italic)))
	  (princ "\n\n"))

	;; Specializer
	(with-current-buffer standard-output
	  (insert (propertize "Specializer in:" 'face 'bold) "\n")
	  (insert (moped-help-print-list (specializer-direct-methods class) #'moped-help-print-method)))
	(princ "\n\n")

	;; Slots
	(with-current-buffer standard-output
	  (insert (propertize "Slots:" 'face 'bold) "\n")
	  (dolist (slot slots)
	    (insert "  ")
	    (help-insert-xref-button
	     ;;(moped-object-print slot)
	     (symbol-name (aref slot 9))
	     'help-moped-method)
	    (insert "\n")))
	(insert "\n")

	;; Inspect link
	(moped-help-print-inspect-button class "class" name)

	)))

  )

(defun moped-describe-generic-function (generic-function)
  ""
  (interactive
   (list (moped-read-generic-function
	  "Describe generic function: " t)))

  ;; Setup backward link.
  (help-setup-xref (list #'moped-describe-generic-function
			 generic-function)
		   (called-interactively-p 'interactive))

  ;; Add description
  (save-excursion
    (with-help-window (help-buffer)
      (moped-with-slots (name lambda-list methods documentation) generic-function
	;; Summary
	(princ (format "%s is a generic function in %s.\n" name "TODO"))
	(moped-help-print-metaclass generic-function)
	(princ "\n\n")

	;; Usage
	(let ((high (help-highlight-arguments (format "%S" (help-make-usage name lambda-list)) "documentation")))
	  (with-current-buffer standard-output
	    (insert (car high) "\n\n" (cdr high) "\n\n")))

	;; Methods
	(with-current-buffer standard-output
	  (insert (propertize "Methods:" 'face 'bold) "\n")
	  (insert (moped-help-print-list methods #'moped-help-print-method "\n")))
	(princ "\n\n")

	;; Argument Precedence Order
	;; Method Combination

	;; Inspect link
	(moped-help-print-inspect-button
	 generic-function "generic function" name)

	)))
  )

(defun moped-describe-method (method)
  ""
  (interactive
   (list
    (let ((generic-function (moped-read-generic-function
			     "Method on generic function: " t)))
      (moped-read-method
       "Method: " (moped-oref generic-function :methods)))))

  ;; Setup backward link.
  (help-setup-xref (list #'moped-describe-method method)
		   (called-interactively-p 'interactive))

  ;;
  (save-excursion
    (with-help-window (help-buffer)
      (moped-with-slots (generic-function
			 qualifiers
			 lambda-list
			 specializers documentation) method
	(let ((name (moped-oref generic-function :name))
	      (args (flet ((process-args (names specializers1)
			     (when names
			       (cons
				(if (and names specializers1)
				    (list
				     (first names)
				     (moped-oref (first specializers1) :name))
				  (first names))
				(process-args
				 (rest names) (rest specializers1))))))
		      (process-args lambda-list specializers))))

	  ;; Summary
	  (princ (format "%s %s is a method in %s\n"
			 name
			 (mapconcat #'symbol-name qualifiers " ")
			 "TODO"))
	  (moped-help-print-metaclass method)
	  (princ "\n")
	  (princ "It is defined on the generic function ")
	  (moped-help-print-generic-function generic-function)
	  (princ ".")
	  (princ "\n\n")

	  ;; Usage
	  (let ((high (help-highlight-arguments (format "%S" (help-make-usage name args)) "documentation")))
	    (with-current-buffer standard-output
	      (insert (car high) "\n\n" (cdr high) "\n\n")))

	  ;; Qualifiers
	  (with-current-buffer standard-output
	    (insert (propertize "Qualifiers:" 'face 'bold) "\n")
	    (insert (mapconcat #'symbol-name qualifiers " ")))
	  (princ "\n\n")

	  ;; Specializer
	  (with-current-buffer standard-output
	    (insert (propertize "Specialized on:" 'face 'bold) "\n")
	    (insert (moped-help-print-list specializers #'moped-help-print-class)))
	  (princ "\n\n")

	  ;; Inspect link
	  (moped-help-print-inspect-button method "method" name)

	  ))))
  )

(defun moped-format-arg (arg)
  ""
  (propertize (upcase (symbol-name arg)) 'face 'italic)
  )


;;; Utility functions
;;

(defun moped-help-print-list (objects printer &optional separator)
  ""
  (mapconcat
   (lambda (object)
     (with-output-to-string
       (funcall printer object)))
   objects
   (or separator " "))
  )

(defun moped-help-print-metaclass (metaobject)
  ""
  (let ((metaclass (moped-class-of metaobject)))
    (princ "Its metaclass is ")
    (moped-help-print-class metaclass)
    (princ ".")))

(defun moped-help-print-class (class)
  ""
  (with-current-buffer standard-output
    (help-insert-xref-button
     (symbol-name (moped-oref class :name)) ;; TODO more compact
     'help-moped-class class)))

(defun moped-help-print-generic-function (generic-function)
  ""
  (with-current-buffer standard-output
    (help-insert-xref-button
     (symbol-name (moped-oref generic-function :name))
     'help-moped-generic-function generic-function)))

(defun moped-help-print-method (method)
  ""
  (with-current-buffer standard-output
    (help-insert-xref-button
     (moped-help-method-string method)
     'help-moped-method method)))

(defun moped-help-print-inspect-button (object type name)
  ""
  (with-current-buffer standard-output
    (insert-text-button
     (format "Inspect this %s" type)
     'action (lexical-let ((object1 object)
			   (type1   type)
			   (name1   name))
	       (lambda (&rest args)
		 (data-debug-show-stuff
		  object1
		  (format "%s %s" (capitalize type1) name1))))))
  )

(defun moped-help-method-string (method)
  ""
  (moped-with-slots (generic-function qualifiers specializers) method
    (concat
     (symbol-name (moped-oref generic-function :name)) ;; TODO more compact
     (when qualifiers
       " ")
     (mapconcat #'symbol-name qualifiers " ")
     " ("
     (mapconcat
      (lambda (specializer)
	(symbol-name (moped-oref specializer :name)))
      specializers
      " ")
     ")")))


;;; User Input Functions
;;

(defvar moped-read-class-history nil
  "")

(defvar moped-read-generic-function-history nil
  "")

(defun moped-read-generic-thing (table prompt history &optional objectp)
  ""
  (let ((generic-objects))
    (maphash
     (lambda (symbol function)
       (push (symbol-name symbol) generic-objects))
     table)
    (let ((name (completing-read prompt generic-objects
				 nil nil nil history)))
      (if objectp
	  (gethash (intern-soft name) table)
	name)))
  )

(defun moped-read-class (prompt &optional objectp)
  ""
  (moped-read-generic-thing
   moped-naming-classes prompt
   'moped-read-class-history objectp))

(defun moped-read-generic-function (prompt &optional objectp)
  ""
  (moped-read-generic-thing
   moped-naming-generic-functions prompt
   'moped-read-generic-function-history objectp))

(defun moped-read-method (prompt methods &optional objectp)
  ""
  (let* ((names (mapcar #'moped-help-method-string methods))
	 (name  (completing-read prompt names)))
    (find name methods :key #'moped-help-method-string :test #'string=)))


;;; Register functions for help cross referencing
;;

(define-button-type 'help-moped-instance
  :supertype 'help-xref
  'help-function 'moped-describe-instance
  'help-echo "mouse-2, RET: describe this instance")

(define-button-type 'help-moped-class
  :supertype 'help-xref
  'help-function 'moped-describe-class
  'help-echo "mouse-2, RET: describe this class")

(define-button-type 'help-moped-generic-function
  :supertype 'help-xref
  'help-function 'moped-describe-generic-function
  'help-echo "mouse-2, RET: describe this generic function")

(define-button-type 'help-moped-method
  :supertype 'help-xref
  'help-function 'moped-describe-method
  'help-echo "mouse-2, RET: describe this method")

(define-button-type 'help-moped-method-combination
  :supertype 'help-xref
  'help-function 'moped-describe-method-combination
  'help-echo "mouse-2, RET: describe this method combination")

(provide 'moped/help)
;;; help.el ends here
