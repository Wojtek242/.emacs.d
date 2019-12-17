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
          doom-modeline-height 25
          doom-modeline-checker-simple-format nil
          doom-modeline-env-python-executable "python3")

    ;; Convenience function for correct active/inactive handling.
    (defun x-doom-active-switch (str)
      "Correctly apply active/inactive mode line face to STR."
      (if (doom-modeline--active)
          str
        (propertize str 'face 'mode-line-inactive)))

    ;; Custom perspective display - display only the active perspective.
    (doom-modeline-def-segment perspective-name
      "Perspectives list and selection. Requires `persp-mode' to be enabled."
      (if (bound-and-true-p persp-mode)
          (x-doom-active-switch
           (persp-format-name (persp-name (persp-curr))))
        ""))

    ;; Display active python virtualenv.
    (defface pyvenv-active-face
      '((t (:inherit persp-selected-face)))
      "The face used to highlight the active virtualenv on the modeline.")

    (doom-modeline-def-segment pyvenv-venv
      "Active Python virtualenv. Requires `pyvenv' to be enabled."
      (if (bound-and-true-p pyvenv-virtual-env-name)
          (x-doom-active-switch
           (propertize pyvenv-virtual-env-name 'face 'pyvenv-active-face))
        ""))

    (doom-modeline-def-segment workspace-number
      "The current workspace name or number.

       Requires `eyebrowse-mode' to be enabled."

      (if (bound-and-true-p eyebrowse-mode)
          (let* ((num (eyebrowse--get 'current-slot))
                 (tag (when num
                        (nth 2 (assoc num (eyebrowse--get 'window-configs)))))
                 (str (if (and tag (< 0 (length tag)))
                          tag
                        (when num (int-to-string num)))))
            (x-doom-active-switch
             (propertize str 'face 'eyebrowse-mode-line-active)))
        ""))

    (doom-modeline-def-segment left-bracket (x-doom-active-switch "["))
    (doom-modeline-def-segment right-bracket (x-doom-active-switch "]"))
    (doom-modeline-def-segment colon (x-doom-active-switch ":"))

    ;; Define custom modeline.
    (doom-modeline-def-modeline 'my-line
      '(bar
        left-bracket perspective-name colon workspace-number right-bracket
        window-number matches buffer-info remote-host
        buffer-position selection-info)
      '(lsp debug pyvenv-venv major-mode vcs checker))

    (add-hook 'doom-modeline-mode-hook
              (lambda () (doom-modeline-set-modeline 'my-line 'default))))

  )

(provide 'emodule/modeline)
;;; modeline.el ends here
