emacs-gradle-mode
=================

minor mode for emacs to run gradle from emacs and not have to go to a terminal!

# To Install #

```lisp
(require 'gradle-mode)
```

# Add this to a mode hook #
```lisp
(add-hook 'groovy-mode-hook 'gradle-mode)
```

etc. etc. for `java-mode` or `scala-mode` or whatever. 
