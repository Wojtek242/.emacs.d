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
    :hook
    (after-init . doom-modeline-init)
    :config
    (declare-function persp-mode-line "perspective")
    (declare-function perspectives-hash "perspective")

    (setq-default doom-modeline-height 23)

    ;; Add perspective to modeline
    (doom-modeline-def-segment perspectives
      "Perspectives list and selection. Requires `persp-mode' to be enabled."
      (if (and (bound-and-true-p persp-mode)
               (< 1 (hash-table-count (perspectives-hash))))
          (persp-mode-line)
        ""))
    (declare-function doom-modeline-segment--perspectives "em-modeline")

    ;; Set the modeline
    (doom-modeline-def-modeline main

                                (perspectives
                                 workspace-number
                                 bar
                                 matches
                                 " "
                                 buffer-info
                                 "  %l:%c %p  "
                                 selection-info)

                                (buffer-encoding
                                 major-mode
                                 vcs
                                 flycheck))
    )
  )

(provide 'em-modeline)
;;; em-modeline.el ends here
