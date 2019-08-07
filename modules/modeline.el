;;; modeline.el --- Module file for configuring the modeline.
;;
;; Copyright (C) 2018-2019 Wojciech Kozlowski
;;
;; Author: Wojciech Kozlowski <wk@wojciechkozlowski.eu>
;; Created: 2018-02-11
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

(defvar emodule/modeline-packages

  '(anzu
    doom-modeline)

  )

;;; Configuration:

(defun emodule/modeline-init ()
  "Initialise the `modeline' module."

  ;; --------------------------------------------------------------------------
  ;; `anzu'
  ;; --------------------------------------------------------------------------

  (use-package anzu
    :config
    (global-anzu-mode 1))

  ;; --------------------------------------------------------------------------
  ;; `doom-modeline' - note that doom-modeline requires all-the-icons which in
  ;; turn require the user to manually install the fonts with the command `M-x
  ;; all-the-icons-install-fonts'.
  ;; --------------------------------------------------------------------------

  (use-package doom-modeline
    :hook
    (after-init . doom-modeline-mode)
    :config
    (setq column-number-mode t
          doom-modeline-height 23
          doom-modeline-checker-simple-format nil
          doom-modeline-env-python-executable "python3")

    ;; Custom perspective display - display only the active perspective.
    (doom-modeline-def-segment perspective-name
      "Perspectives list and selection. Requires `persp-mode' to be enabled."
      (if (bound-and-true-p persp-mode)
          (persp-format-name (persp-name (persp-curr)))
        ""))

    ;; Display active python virtualenv.
    (defface pyvenv-active-face
      '((t (:inherit persp-selected-face)))
      "The face used to highlight the active virtualenv on the modeline.")

    (doom-modeline-def-segment pyvenv-venv
      "Active Python virtualenv. Requires `pyvenv' to be enabled."
      (if (bound-and-true-p pyvenv-virtual-env-name)
          (propertize pyvenv-virtual-env-name 'face 'pyvenv-active-face)
        ""))

    ;; Necessary to play nice with Helm.
    (add-hook 'helm-minibuffer-set-up-hook
              (lambda ()
                (advice-add #'doom-modeline--active :override (lambda () t))))
    (add-hook 'helm-cleanup-hook
              (lambda ()
                (advice-remove #'doom-modeline--active (lambda () t))))

    ;; Define custom modeline.
    (doom-modeline-def-modeline 'my-line
      '(bar "[" perspective-name "]" window-number matches buffer-info remote-host buffer-position selection-info)
      '(lsp debug pyvenv-venv major-mode vcs checker bar))

    (add-hook 'doom-modeline-mode-hook
              (lambda () (doom-modeline-set-modeline 'my-line 'default))))

  )

(provide 'emodule/modeline)
;;; modeline.el ends here
