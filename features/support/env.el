;;; env.el --- gradle-mode: ecukes environment setup

;; Copyright (C) 2014 by Daniel Mijares

;; Author: Daniel Mijares <daniel.j.mijares@gmail.com>
;; Maintainer: Daniel Mijares <daniel.j.mijares@gmail.com>
;; URL: http://github.com/jacobono/gradle-mode

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

;;; Code:

(require 'f)

(defvar gradle-mode-support-path
  (f-dirname load-file-name))

(defvar gradle-mode-features-path
  (f-parent gradle-mode-support-path))

(defvar gradle-mode-root-path
  (f-parent gradle-mode-features-path))

(add-to-list 'load-path gradle-mode-root-path)

;; avoid emacs interrupting tests with
;; "Please answer yes or no"
;; which is think is happening because it is trying to delete the
;; compilation buffer after running one compilation?
(fset 'yes-or-no-p (lambda (_) t))

(require 'gradle-mode)
(require 'espuds)
(require 'ert)

(Setup
 ;; Before anything has run
 (f-touch "build.gradle")
 (f-touch "gradlew")
 (setq gradle-executable-path "gradle")
 (gradle-mode 1)
 )

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 (f-delete "build.gradle")
 (f-delete "gradlew")
 (gradle-mode 0)
 )

;;; env.el ends here
