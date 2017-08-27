;;; gradle-completion.el ---   -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 27 August 2017
;;

;; Author: Sébastien Le Maguer <slemaguer@coli.uni-saarland.de>

;; Package-Requires: ((emacs "25.2"))
;; Keywords:
;; Homepage:

;; gradle-completion is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; gradle-completion is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with gradle-completion.  If not, see http://www.gnu.org/licenses.

;;; Commentary:


;;; Code:

(require 'cl-lib)
(require 'gradle-core)

;;(require 'ivy)

(defcustom gradle-cache-dir "~/.gradle/completion/"
  "cache dir for the completion"
  :group 'gradle
  :type 'string)


(defvar gradle-ivy-hash-tasks nil)

(defun gradle-ivy-transformer (cmd)
  "Return CMD appended with the corresponding binding in the current window."
  (let ((desc (gethash cmd gradle-ivy-hash-tasks)))
    (if desc
      (format "%s (%s)"
              cmd (propertize (car desc) 'face 'font-lock-keyword-face))
      (format "%s" cmd))))

(ivy-set-display-transformer 'gradle-execute 'gradle-ivy-transformer)

(defun gradle-init-cache-dir ()
  "Initialize the cache dir by creating it"
  (mkdir gradle-cache-dir t))


(defun gradle-list-tasks ()
  "List the available tasks for the current project"
  (let ((default-directory  (gradle-run-from-dir (if gradle-use-gradlew
						     'gradle-is-gradlew-dir
						   'gradle-is-project-dir)))
	(root-file (concat gradle-cache-dir
			   (replace-regexp-in-string "[^[:alnum:]]" "_" (expand-file-name (concat default-directory "/build.gradle")))
			   ".md5"))
	md5-filename list-tasks)
    (if (file-exists-p root-file)
	(progn
	  (setq md5-filename (concat gradle-cache-dir
				     (with-temp-buffer
				       (insert-file-contents root-file)
				       (replace-regexp-in-string "\n$" "" (buffer-string)))))
	  (if (file-exists-p md5-filename)
	      (progn
		(setq gradle-ivy-hash-tasks (make-hash-table))
		(setq list-tasks (with-temp-buffer
				   (insert-file-contents md5-filename)
				   (split-string (buffer-string) "\n" t)))
		(cl-loop for task in list-tasks
			 collect  (let ((cur-task (split-string
						   (replace-regexp-in-string "[\\][:]" ":"
									     (replace-regexp-in-string "\\([^:]\\):\\([^:]*\\)$" "\\1\t\\2" task))
						   "\t" t)))
				    (puthash (car cur-task) (cdr cur-task) gradle-ivy-hash-tasks)))
		gradle-ivy-hash-tasks)
	    (error (format-message "%s doesn't exist, something went wrong" md5-filename))))
      (display-warning 'gradle-mode (format-message "%s doesn't exist, run init in your shell" root-file))))
  )


(provide 'gradle-completion)
;;; gradle-completion.el ends here
