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
    (defun emodule/modeline-inactive-switch (str)
      "Apply inactive mode line face to STR if necessary."
      (if (doom-modeline--active)
          str
        (propertize str 'face 'mode-line-inactive)))

    ;; Custom perspective display - display only the active perspective.
    (defun emodule/modeline-persp ()
      "Return the formatted perspective name."
      (if (bound-and-true-p persp-mode)
          (persp-format-name (persp-name (persp-curr)))
        ""))

    (doom-modeline-def-segment emodule/modeline-persp-segment
      "Currently chose perspective."
      (emodule/modeline-inactive-switch
       (concat "[" (emodule/modeline-persp) "]")))

    ;; The current eyebrowse workspace number.
    (defun emodule/modeline-eyebrowse ()
      "Return the currently active eyebrowse workspace."
      (if (bound-and-true-p eyebrowse-mode)
          (let* ((cur (eyebrowse--get 'current-slot))
                 (all (--map (car it) (eyebrowse--get 'window-configs)))
                 (str (--map (if (= cur it)
                                 (propertize (int-to-string it)
                                             'face
                                             'eyebrowse-mode-line-active)
                               (int-to-string it))
                             all)))
            (apply 'concat str))
        ""))

    (doom-modeline-def-segment emodule/modeline-eyebrowse-segment
      "The current workspace name or number."
      (emodule/modeline-inactive-switch (emodule/modeline-eyebrowse)))

    ;; A combined segment.
    (doom-modeline-def-segment emodule/modeline-persp-eyebrowse-segment
      (emodule/modeline-inactive-switch
       (concat "[" (emodule/modeline-persp)
               ":" (emodule/modeline-eyebrowse)
               "]")))

    ;; Display active python virtualenv.
    (defface pyvenv-active-face
      '((t (:inherit persp-selected-face)))
      "The face used to highlight the active virtualenv on the modeline."
      :group 'emodule/modeline-faces)

    (defun emodule/modeline-pyvenv ()
      "Return the formatted virtualenv name."
      (if (bound-and-true-p pyvenv-virtual-env-name)
          (propertize pyvenv-virtual-env-name 'face 'pyvenv-active-face)
        ""))

    (doom-modeline-def-segment emodule/modeline-pyvenv-segment
      "Active Python virtualenv."
      (emodule/modeline-inactive-switch (emodule/modeline-pyvenv)))

    ;; Define custom modeline.
    (doom-modeline-def-modeline 'my-line
      '(bar
        emodule/modeline-persp-eyebrowse-segment
        window-number matches buffer-info remote-host
        buffer-position selection-info)
      '(lsp debug emodule/modeline-pyvenv-segment major-mode vcs checker))

    ;; Project modeline (used in Dired).
    (doom-modeline-def-modeline 'project
      '(bar
        emodule/modeline-persp-eyebrowse-segment
        window-number buffer-default-directory)
      '(debug major-mode process))

    ;; VCS modeline (used in magit).
    (doom-modeline-def-modeline 'vcs
      '(bar
        emodule/modeline-persp-eyebrowse-segment
        window-number matches buffer-info buffer-position selection-info)
      '(debug minor-modes buffer-encoding major-mode process))

    (add-hook 'doom-modeline-mode-hook
              (lambda () (doom-modeline-set-modeline 'my-line 'default))))

  )

(provide 'emodule/modeline)
;;; modeline.el ends here
