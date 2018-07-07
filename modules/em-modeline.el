;;; em-modeline.el --- Module file for configuring the modeline.
;;
;; Copyright (C) 2018 Wojciech Kozlowski
;;
;; Author: Wojciech Kozlowski <wk@wojciechkozlowski.eu>
;; Created: 11 Feb 2018
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This module is used for configuring the modeline.
;;
;;; License: GPLv3

;;; Required packages:

;;; Code:

(defvar emodule/em-modeline-packages

  '(doom-modeline)

  )

;;; Configuration:

(defun emodule/em-modeline-init ()
  "Initialise the `em-modeline' module."

  ;; Note that doom-modeline requires all-the-icons which in turn require the
  ;; user to manually install the fonts with the command `M-x
  ;; all-the-icons-install-fonts'.
  (use-package doom-modeline
    :defer t
    :config
    (setq-default doom-modeline-height 23)
    :hook
    (after-init . doom-modeline-init))

  )

(provide 'em-modeline)
;;; em-modeline.el ends here
