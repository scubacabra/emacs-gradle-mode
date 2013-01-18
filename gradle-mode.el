;;; gradle-mode.el --- Groovy and gradle-mode Emacs features

;; This is free and unencumbered software released into the public domain.

;;; Install:

;; Put this file somewhere on your load path (like in .emacs.d), and
;; require it. That's it!

;;    (require 'gradle-mode)

;;; Code:

(defvar gradle-mode-map (make-sparse-keymap)
  "Keymap for the gradle minor mode.")

;;;###autoload
(define-minor-mode gradle-mode
  "Extensions to groovy-mode for further support with standard Groovy tools."
  :lighter " gra"
  :keymap 'gradle-mode-map)

;; Enable the minor mode wherever groovy-mode is used.
;;;###autoload
(add-hook 'groovy-mode-hook 'gradle-mode)

(provide 'gradle-mode)

;; groovy-mode-plus.el ends here
