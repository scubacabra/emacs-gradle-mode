;;; gradle-mode-steps.el --- gradle-mode: Step definitions for Ecukes tests

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

;; Step definitions for Ecukes integration tests of gradle-mode.
;; I didn't want to actually run the compilation, so I mocked it
;; out.  I was just testing that the bindings were working and
;; making the correct calls to 'compile'.

;;; Code:

;;; --------------------------
;; Definitions
;;; --------------------------

(Then "^Compilation directory is \"\\(.+\\)\"$"
  (lambda (compile-directory)
    (should
     (equal
       compilation-directory
	(f-short
	 (f-slash
	 (symbol-value (intern compile-directory))))))))

(And "^Compilation command is \"\\(.+\\)\"$"
  (lambda (compilation-command)
    (should
     (equal
      compile-command
      compilation-command))))

;;; gradle-mode-steps.el ends here
