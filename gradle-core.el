;;; gradle-core.el ---   -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 27 August 2017
;;

;; Author: Sébastien Le Maguer <slemaguer@coli.uni-saarland.de>

;; Package-Requires: ((emacs "25.2"))
;; Keywords:
;; Homepage:

;; gradle-core is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; gradle-core is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with gradle-core.  If not, see http://www.gnu.org/licenses.

;;; Commentary:


;;; Code:

(require 'cl-lib)


;; error processing if no executable??
(defcustom gradle-executable-path (executable-find "gradle")
  "String representation of the Gradle executable location.
Absolute path, usually found with `executable-find'."
  :group 'gradle
  :type 'string)

(defcustom gradle-gradlew-executable "gradlew"
  "String representation of the gradlew executable."
  :group 'gradle
  :type 'string)


(defcustom gradle-use-gradlew nil
  "Use gradlew or `gradle-executable-path'.
If true, run gradlew from its location in the project file system.
If false, will find project build file and run `gradle-executable-path' from there."
  :group 'gradle
  :type 'boolean)


;;; --------------------------
;; gradle-mode private functions
;;; --------------------------

(defun gradle-is-project-dir (dir)
  "Is this DIR a gradle project directory with an extra convention.
A project dir is always considered if there is a 'build.gradle' there.
A project dir is also considered if there is a '{dirname}.gradle'.  This
is a convention for multi-build projects, where dirname is under some
'rootDir/dirname/dirname.gradle'."
  (let ((dirname (file-name-nondirectory
                  (directory-file-name (expand-file-name dir)))))
    (or (file-exists-p (expand-file-name "build.gradle" dir))
        (file-exists-p (expand-file-name
                        (concat dirname ".gradle") dir)))))

(defun gradle-is-gradlew-dir (dir)
  "Does this DIR contain a gradlew executable file."
  (file-exists-p (expand-file-name "gradlew" dir)))

(defun gradle-run-from-dir (is-dir)
  "Find the closest dir to execute the gradle command under via IS-DIR function.
If there is a folder you care to run from higher than this level, you need to move out to that level (eg. through dired etc.)."
  (locate-dominating-file default-directory is-dir))

(defun gradle-kill-compilation-buffer ()
  "Kill compilation buffer if present."
  (progn
    (if (get-buffer "*compilation*")
	(progn
	  (delete-windows-on (get-buffer "*compilation*"))
	  (kill-buffer "*compilation*")))))

(defun gradle-run (gradle-tasks)
  "Run gradle command with `GRADLE-TASKS' and options supplied."
  (gradle-kill-compilation-buffer)
  (let ((default-directory
          (gradle-run-from-dir (if gradle-use-gradlew
                                   'gradle-is-gradlew-dir
                                 'gradle-is-project-dir))))
    (compile (gradle-make-command gradle-tasks))))

(defun gradle-make-command (gradle-tasks)
  "Make the gradle command, using some executable path and GRADLE-TASKS."
  (let ((gradle-executable (if gradle-use-gradlew
                               gradle-gradlew-executable
                             gradle-executable-path))
        (gradle-cmd '(gradle-executable)))
    (progn
      (when gradle-quiet-activation
        (add-to-list 'gradle-cmd gradle-quiet-option))
      (when gradle-continuous-activation
        (add-to-list 'gradle-cmd gradle-continuous-option))
      (add-to-list 'gradle-cmd gradle-tasks)
      (s-join " " gradle-cmd))))


(provide 'gradle-core)
;;; gradle-core.el ends here
