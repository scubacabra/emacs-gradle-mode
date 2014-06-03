;;; gradle-mode.el --- Gradle integration with Emacs' compile

;; Copyright (C) 2014 by Daniel Mijares

;; Author: Daniel Mijares <daniel.j.mijares@gmail.com>
;; Maintainer: Daniel Mijares <daniel.j.mijares@gmail.com>
;; URL: http://github.com/jacobono/emacs-gradle-mode
;; Version: 0.5.3
;; Keywords: gradle
;; Package-Requires: ((s "1.8.0"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Gradle integration into Emacs, through compile-mode.
;; see documentation on https://github.com/jacobono/emacs-gradle-mode

;;; Code:

;;; --------------------------
;; gradle-mode dependencies
;;; --------------------------

(require 's)
(require 'compile)

;;; --------------------------
;; gradle-mode variables
;;; --------------------------

;; error processing if no executable??
(defcustom gradle-executable-path (executable-find "gradle")
  "String representation of the Gradle executable location.
Absolute path, usually found with executable-find."
  :type '(string)
  :group 'gradle-executable)

;;; --------------------------
;; gradle-mode private functions
;;; --------------------------

(defun gradle-find-project-dir ()
  "Finds the closest project dir to execute the gradle command on.
This directory is found by checking up every directory from
'default-directory' for two files:
* 'build.gradle' -- default name for a gradle build file.
* 'some-folder.gradle' (assuming the file is located under
rootDir/some-folder/)
The latter is a common naming convention to rename the 'build.gradle'
to 'some-folder.gradle' assuming the file is located under
rootDir/some-folder/.  This is usually a convention for a
multi-project-build.
The directory to run the gradle command in is returned as the
directory that contains either of these two files.
As a side note, you can edit files in a sub-project and any command you
desire to run is run from the sub-project dir -- to run from the root
dir, you would have to move the default-directory value there somehow.
For example, switching to magit buffer, dired buffer on that folder
etc."
  (locate-dominating-file
   default-directory
   '(lambda (dir)
      (let ((dirname (file-name-nondirectory
		      (directory-file-name (expand-file-name dir)))))
        (or (file-exists-p (expand-file-name "build.gradle" dir))
            (file-exists-p (expand-file-name
			    (concat dirname ".gradle") dir)))))))

(defun gradle-kill-compilation-buffer ()
  "Kills compilation buffer is present."
  (progn
    (if (get-buffer "*compilation*")
	(progn
	  (delete-windows-on (get-buffer "*compilation*"))
	  (kill-buffer "*compilation*")))))

(defun gradle-run (gradle-tasks)
  "Runs gradle command with tasks and options supplied."
  (gradle-kill-compilation-buffer)
  (let ((default-directory (gradle-find-project-dir)))
    (compile (gradle-make-command gradle-tasks))))

(defun gradle-make-command (gradle-tasks)
  "Makes the gradle command, combinding executable path and tasks."
  (s-join " " (list gradle-executable-path gradle-tasks)))

;;; --------------------------
;; gradle-mode interactive functions
;;; --------------------------

(defun gradle-execute (tasks)
  "Executes gradle command with tasks supplied by user input."
  (interactive "sType tasks to run: ")
  (gradle-run tasks))

(defun gradle-build ()
  "Executes gradle build command."
  (interactive)
  (gradle-run "build"))

(defun gradle-test ()
  "Executes gradle test command."
  (interactive)
  (gradle-run "test"))

(defun gradle-single-test (single-test-name)
  "Executes gradle test on a single file supplied by user."
  (interactive "sType single test to run: ")
  (gradle-run
   (s-concat "test -Dtest.single=" single-test-name)))

(defun gradle-execute--daemon (tasks)
  "Executes gradle command, using daemon, with tasks supplied by user
input."
  (interactive "sType tasks to run: ")
  (gradle-run
   (s-concat tasks " --daemon")))

(defun gradle-build--daemon ()
  "Executes gradle build command, using daemon."
  (interactive)
  (gradle-run "build --daemon"))

(defun gradle-test--daemon ()
  "Executes gradle test command, using daemon."
  (interactive)
  (gradle-run "test --daemon"))

(defun gradle-single-test--daemon (single-test-name)
  "Executes gradle test, using daemon, on a single file supplied by
user."
  (interactive "sType single test to run: ")
  (gradle-run
   (s-concat "test -Dtest.single=" single-test-name " --daemon")))

;;; ----------
;; gradle-mode keybindings
;;; ----------

(defvar gradle-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-g b") 'gradle-build)
    (define-key map (kbd "C-c C-g t") 'gradle-test)
    (define-key map (kbd "C-c C-g s") 'gradle-single-test)
    (define-key map (kbd "C-c C-g C-d b") 'gradle-build--daemon)
    (define-key map (kbd "C-c C-g C-d t") 'gradle-test--daemon)
    (define-key map (kbd "C-c C-g C-d s") 'gradle-single-test--daemon)
    (define-key map (kbd "C-c C-g d") 'gradle-execute--daemon)
    (define-key map (kbd "C-c C-g r") 'gradle-execute)
    map)
  "Keymap for the gradle minor mode.")

;;;###autoload
(define-minor-mode gradle-mode
  "Emacs minor mode for integrating Gradle into compile.
Run gradle tasks from any buffer, scanning up to nearest gradle
directory to run tasks."
  :lighter " Gradle"
  :keymap 'gradle-mode-map
  :global t)

(provide 'gradle-mode)

;;; gradle-mode.el ends here
