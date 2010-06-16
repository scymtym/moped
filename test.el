;;; test.el ---
;;
;; Copyright (C) 2010 Jan Moringen
;;
;; Author: Jan Moringen <scymtym@users.sourceforge.net>
;; Keywords: oop, clos, mop, unit test
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

(progn
  (add-to-list 'load-path (file-name-as-directory
			   (expand-file-name
			    (concat (file-name-directory
				     (or load-file-name
					 (buffer-file-name)))
				    ".."))))
  (require 'eieio/naming)
  (require 'eieio/macros)
  (require 'eieio/bootstrap))

(defmacro with-fresh-object-system (&rest forms)
  ""
  `(progn
     (eieio-naming-clear-classes)
     (eieio-bootstrap-object-system)
     ,@forms))

(defmacro without-object-system-modification (&rest forms)
  ""
  `(let ((eieio-naming-classes
	  (copy-hash-table eieio-naming-classes))
	 (eieio-naming-generic-functions
	  (copy-hash-table eieio-naming-generic-functions))
	 (eieio-naming-methods
	  (copy-hash-table eieio-naming-methods)))
     ,@forms))

(provide 'eieio/test)
;;; test.el ends here
