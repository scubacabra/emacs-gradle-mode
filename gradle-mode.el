;;; gradle-mode.el --- gradle integration with Emacs

;; This is free and unencumbered software released into the public domain.

;;; Install:

;; Put this file somewhere on your load path (like in .emacs.d), and
;; require it. That's it!

;;    (require 'gradle-mode)

;;; Code:

(defcustom gradle-executable "gradle"
  "If gradle is not in `exec-path', this should contain the full
path to gradle executable."
  :type 'string
  :group 'gradle)

(defun gradle--executable-path ()
  (executable-find gradle-executable))

(defun gradle-run (tasks)
  ;; (interactive (list (gradle--input-commandline)))
  (interactive)
  ;; (gradle--with-project-root
   (print (concat (gradle--executable-path)
                    (when tasks
                      (mapconcat 'identity (cons "" tasks) " ")))))

(defvar gradle-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-g c") (gradle-run (list "compile")))
    (define-key map (kbd "C-c C-g b") (gradle-run (list "build")))
    (define-key map (kbd "C-c C-g t") (gradle-run (list "test")))
    (define-key map (kbd "C-c C-g C-d c") (gradle-run (list "compile" "--daemon")))
    (define-key map (kbd "C-c C-g C-d b") (gradle-run (list "build"   "--daemon")))
    (define-key map (kbd "C-c C-g C-d t") (gradle-run (list "test"    "--daemon")))
    (define-key map (kbd "C-c C-g d") (print "Run something as daemon"))
    (define-key map (kbd "C-c C-g r") (print "Run something as normal"))
    map)
  "Keymap for the gradle minor mode.")

;;;###autoload
(define-minor-mode gradle-mode
  "Gradle Mode -- run gradle from any buffer, will scan up for nearest gradle directory and run the command."
  :lighter " gra"
  :keymap 'gradle-mode-map)

(provide 'gradle-mode)

;; groovy-mode-plus.el ends here
