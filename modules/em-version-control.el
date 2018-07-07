;;; em-version-control.el --- Module file for version control configuration.
;;
;; Copyright (C) 2017 Wojciech Kozlowski
;;
;; Author: Wojciech Kozlowski <wk@wojciechkozlowski.eu>
;; Created: 25 Aug 2017
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This module sets up configuration for version control packages such as
;; `magit'.
;;
;;; License: GPLv3

;;; Required packages:

;;; Code:

(defvar emodule/em-version-control-packages

  '(magit
    diff-hl)

  )

;;; Configuration:

(defun emodule/em-version-control-init ()
  "Initialise the `em-version-control' module."

  ;; --------------------------------------------------------------------------
  ;; Load and configure `magit'.
  ;; --------------------------------------------------------------------------

  (use-package magit
    :bind
    ("C-x g l" . magit-log-head)
    ("C-x g f" . magit-log-buffer-file)
    ("C-x g b" . magit-blame)
    ("C-x g m" . magit-show-refs-popup)
    ("C-x g c" . magit-branch-and-checkout)
    ("C-x g s" . magit-status)
    ("C-x g r" . magit-reflog)
    ("C-x g t" . magit-tag)
    :config
    (add-hook 'magit-mode-hook 'magit-load-config-extensions)

    ;; unbind C-x g
    (unbind-key "C-x g" magit-file-mode-map))

  ;; --------------------------------------------------------------------------
  ;; Ediff.
  ;; --------------------------------------------------------------------------

  (defvar ediff-diff-options)
  (defvar ediff-split-window-function)
  (defvar ediff-window-setup-function)
  (setq ediff-diff-options "-w"
        ediff-split-window-function 'split-window-horizontally
        ediff-window-setup-function 'ediff-setup-windows-plain)


  ;; --------------------------------------------------------------------------
  ;; Diff highlight mode.
  ;; --------------------------------------------------------------------------

  (use-package diff-hl
    :hook
    ((dired-mode . diff-hl-dired-mode)
     (magit-post-refresh . diff-hl-magit-post-refresh))
    :config
    (global-diff-hl-mode))

  ;; --------------------------------------------------------------------------
  ;; Diff mode settings.
  ;; --------------------------------------------------------------------------

  (use-package diff-mode
    :init

    ;; Diff mode hook - whitespace mode settings and set read-only mode.
    (defvar whitespace-style)
    (add-hook 'diff-mode-hook (lambda ()
                                (setq-local whitespace-style
                                            '(face
                                              tabs
                                              tab-mark
                                              spaces
                                              space-mark
                                              trailing
                                              indentation::space
                                              indentation::tab
                                              newline
                                              newline-mark))
                                (read-only-mode 1)))

    :config
    (declare-function diff-find-source-location "diff-mode")
    (declare-function diff-hunk-status-msg "diff-mode")

    ;; Extra functions ----------------------------------------------------------

    ;; Display source in other window whilst keeping point in the diff file.
    ;; Based on the code for `diff-goto-source.
    (defun x-diff-display-source (&optional other-file event)
      "Display the corresponding source line in another window.
       `diff-jump-to-old-file' (or its opposite if the OTHER-FILE
       prefix arg is given) determines whether to jump to the old
       or the new file.  If the prefix arg is bigger than 8 (for
       example with \\[universal-argument]
       \\[universal-argument]) then `diff-jump-to-old-file' is
       also set, for the next invocations."
      (interactive (list current-prefix-arg last-input-event))
      ;; When pointing at a removal line, we probably want to jump to
      ;; the old location, and else to the new (i.e. as if reverting).
      ;; This is a convenient detail when using smerge-diff.
      (if event (posn-set-point (event-end event)))
      (let ((rev (not (save-excursion (beginning-of-line) (looking-at "[-<]")))))
        (pcase-let ((`(,buf ,line-offset ,pos ,src ,_dst ,switched)
                     (diff-find-source-location other-file rev)))
          (let ((window (display-buffer buf t)))
            (save-selected-window
              (select-window window)
              (goto-char (+ (car pos) (cdr src)))
              (diff-hunk-status-msg line-offset (diff-xor rev switched) t))))))

    ;; Key-bindings -------------------------------------------------------------

    ;; This shadows new global key-binding for other-window.
    (define-key diff-mode-map (kbd "M-o") nil)

    ;; This copies behaviour from other modes where C-o displays the relevant
    ;; (setq )ource in another window.
    (define-key diff-mode-map (kbd "C-o") 'x-diff-display-source))

  )

(provide 'em-version-control)
;;; em-version-control.el ends here
