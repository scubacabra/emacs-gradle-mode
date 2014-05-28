;;; test-helper.el --- gradle-mode: Test helper

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
(require 'dash)

;; for testing, always make executable path just 'gradle'
(setq gradle-executable-path "gradle")

(defvar gradle-mode-test/test-path
  (f-dirname (f-this-file))
  "Path to tests directory.")

(defvar gradle-mode-test/root-path
  (f-parent gradle-mode-test/test-path)
  "Path to root directory.")

(defconst gradle-mode-test/sandbox-path
  (f-expand "sandbox" gradle-mode-test/test-path)
  "Path to 'sandbox' directory where gradle files are placed.")

;; load up gradle-mode to test it.
(add-to-list 'load-path gradle-mode-test/root-path)
(require 'gradle-mode)

;;; test-helper.el ends here
