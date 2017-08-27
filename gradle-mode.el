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
(require 'gradle-completion)
(require 'gradle-core)

;;; --------------------------
;; gradle-mode variables
;;; --------------------------

(defcustom gradle-continuous-option "--continuous"
  "String representation of the continuous option"
  :group 'gradle
  :type 'string)

(defcustom gradle-quiet-option "-q"
  "String representation of the quiet option"
  :group 'gradle
  :type 'string)


(defcustom gradle-quiet-activation nil
  "Silent main part except error and warnings provided by gradle"
  :group 'gradle
  :type 'boolean)

(defcustom gradle-continuous-activation nil
  "Continuous building"
  :group 'gradle
  :type 'boolean)

;;; --------------------------
;; gradle-mode interactive functions
;;; --------------------------

(defun gradle-execute ()
  "Execute gradle command with TASKS supplied by user input."
  (interactive)
  (let ((prompt "Select a task to execute: ")
	(choices (gradle-list-tasks))
	task)
    (when (fboundp 'ivy-read)
      (progn
	(setq task
	      (ivy-read
	       prompt choices
	       :history 'gradle-tasks-history
	       ;; :initial-input initial-input
	       :caller 'gradle-execute)))
      (gradle-run task))))


(defun gradle-build ()
  "Execute gradle build command."
  (interactive)
  (gradle-run "build"))

(defun gradle-test ()
  "Execute gradle test command."
  (interactive)
  (gradle-run "test"))

(defun gradle-single-test (single-test-name)
  "Execute gradle test on file SINGLE-TEST-NAME supplied by user."
  (interactive "sType single test to run: ")
  (gradle-run
   (s-concat "test -Dtest.single=" single-test-name)))

(defun gradle-execute--daemon (tasks)
  "Execute gradle command, using daemon, with TASKS supplied by user input."
  (interactive "sType tasks to run: ")
  (gradle-run
   (s-concat tasks " --daemon")))

(defun gradle-build--daemon ()
  "Execute gradle build command, using daemon."
  (interactive)
  (gradle-run "build --daemon"))

(defun gradle-test--daemon ()
  "Execute gradle test command, using daemon."
  (interactive)
  (gradle-run "test --daemon"))

(defun gradle-single-test--daemon (single-test-name)
  "Execute gradle test, using daemon, on file SINGLE-TEST-NAME supplied by user."
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
