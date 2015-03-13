emacs-gradle-mode
=================
[![Build Status](https://travis-ci.org/jacobono/emacs-gradle-mode.svg?branch=master)]
(https://travis-ci.org/jacobono/emacs-gradle-mode)

Minor mode for emacs to run gradle from emacs and not have to go to a terminal!

# Installation #

Available on [MELPA](http://melpa.milkbox.net/):

    M-x package-install gradle-mode

Or you can just dump `gradle-mode.el` in your load path somewhere.

After installation, you can confiugre this mode to always be on with:

```lisp
(require 'gradle-mode)

(gradle-mode 1)
```

Or just

    M-x gradle-mode

when you are ready to use it.

## Keybindings ##

* `C-c C-g b`
  - run `gradle build`
* `C-c C-g t`
  - run `gradle test`
* `C-c C-g s`
  - run `gradle test -Dsingle.test="user-supplied"`
  - User supplies test to run from prompt
* `C-c C-g C-d b`
  - run `gradle build --daemon`
* `C-c C-g C-d t`
  - run `gradle test --daemon`
* `C-c C-g C-d s`
  - run `gradle -Dsingle.test="user-supplied" --daemon`
  - User supplies test to run from prompt
* `C-c C-g d`
  - run `gradle "user-supplied" --daemon`
  - User supplies tasks to run from prompt
* `C-c C-g r`
  - run `gradle "user-supplied"`
  - User supplies tasks to run from prompt

The prefix `C-d` runs the command with gradle's daemon, or creates one
if it is not already present.

# Limitations #
**Currently**, the only versions of emacs that are passing are _24.3_.
I plan on trying to look into it more, but both Travis and drone are
barfing when I try and run the tests with <24.3.

# Contribute #
If you want to contribute, please fork and submit a pull request!

# Changlog #
## 0.5.5
- gradlew support added.  Will find nearest gradlew file and run if
  `gradle-use-gradlew` is turned on

## 0.5.4
- CI is breaking from 0.5.4, only version of Emacs that tests execute
  appropriately is **24.3**

## 0.5.3
- losing dependencies on `f` and `dash`
- `gradle-project-find-dir` uses `locate-dominating-file` instead of `f-traverse-upwards`

## 0.5.2
- mode lighter string changed from _gra_ to _Gradle_

## 0.5
- First Release
