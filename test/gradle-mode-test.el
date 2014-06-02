;;; gradle-mode-test.el --- gradle-mode: ERT tests

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

;; make the correct gradle command, combining executable with tasks

(ert-deftest gradle-test-make-command ()
  (should
   (equal
    (gradle-make-command "test -Dtest-single=MyTest --daemon")
    "gradle test -Dtest-single=MyTest --daemon"))
  (should
   (equal
    (gradle-make-command "build --daemon")
    "gradle build --daemon"))
  (should
   (equal
    (gradle-make-command "test")
    "gradle test")))

;; find the correct gradle project directory to run commands in

(ert-deftest gradle-test-find-project-dir ()
  ;; make sandbox directories to find the correct gradle project fil
  (f-mkdir
   gradle-mode-test/sandbox-path "some-project" "src" "main")

  ;; touch gradle project files
  (f-touch
   (f-join
    gradle-mode-test/sandbox-path "build.gradle"))
  (f-touch
   (f-join
    gradle-mode-test/sandbox-path "some-project" "some-project.gradle"))

  (let* ((project-directory)) ;; directory of the gradle project file
    ;; test in the root project dir
    (setq default-directory
	  (f-long gradle-mode-test/sandbox-path))
    (should
     (equal
      (gradle-find-project-dir)
      (f-full gradle-mode-test/sandbox-path)))

    ;; test in the sub-project dir
    (setq default-directory
    	  (f-join gradle-mode-test/sandbox-path "some-project"))
    (should
     (equal
      (gradle-find-project-dir)
      (f-full (f-join
	       gradle-mode-test/sandbox-path "some-project"))))

    ;; test in a sub directory of sub-project dir
    (setq default-directory
    	  (f-join gradle-mode-test/sandbox-path "some-project" "src" "main"))
    (should
     (equal
      (gradle-find-project-dir)
      (f-full (f-join
	       gradle-mode-test/sandbox-path "some-project"))))
    
  ;; delete all sandbox directory
  (f-delete gradle-mode-test/sandbox-path t)))

;; kill compilation buffer if it is opened

(ert-deftest gradle-test-close-compilation-buffer ()
  ;; make compilation buffer
  (get-buffer-create "*compilation*")
  (gradle-kill-compilation-buffer)
  (should
   (equal
    (get-buffer "*compilation*")
    nil)))

;;; gradle-mode-test.el ends here

