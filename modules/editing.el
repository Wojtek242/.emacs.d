;;; editing.el --- Module file for editing configuration.
;;
;; Copyright (C) 2017 Wojciech Kozlowski
;;
;; Author: Wojciech Kozlowski <wojciech.kozlowski@vivaldi.net>
;; Created: 25 Aug 2017
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This module sets up packages and configuration for editing text and code.
;;
;;; License: GPLv3

;;; Required packages:

(setq init-packages/editing-packages

      '(rainbow-delimiters
        highlight-parentheses
        whole-line-or-region)

      )

;; Configuration:

(defun init-packages/init-editing ()

  ;; --------------------------------------------------------------------------
  ;; Parentheses highlighting.
  ;; --------------------------------------------------------------------------
  (use-package rainbow-delimiters)
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

  (use-package highlight-parentheses)
  (add-hook 'prog-mode-hook 'show-paren-mode)
  (add-hook 'prog-mode-hook 'highlight-parentheses-mode)
  (setq hl-paren-colors '("#86DC2F"
                          "IndianRed1"
                          "IndianRed3"
                          "IndianRed4"))

  ;; --------------------------------------------------------------------------
  ;; Use UTF-8.
  ;; --------------------------------------------------------------------------
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-language-environment "UTF-8")
  (prefer-coding-system 'utf-8)

  ;; --------------------------------------------------------------------------
  ;; Convenient editing settings.
  ;; --------------------------------------------------------------------------

  ;; Kill whole line when point at beginning of line.
  (setq kill-whole-line t)

  ;; Replace selected rather than inserting text at point.
  (delete-selection-mode)

  ;; Kill line when calling kill-region without a selected region.
  (whole-line-or-region-global-mode t)

  ;; --------------------------------------------------------------------------
  ;; Formatting
  ;; --------------------------------------------------------------------------

  (setq-default tab-width 8)                         ;; Tab width
  (setq-default indent-tabs-mode nil)                ;; No tabs
  (setq-default fill-column 79)                      ;; Line width
  (setq-default whitespace-line-column fill-column)  ;; For whitespace mode

  ;; --------------------------------------------------------------------------
  ;; Commands.
  ;; --------------------------------------------------------------------------

  (defun toggle-indent-tabs-mode ()
    "Toggle a indent-tabs-mode between a defined and undefined state."
    (interactive)
    (setq indent-tabs-mode (not indent-tabs-mode))
    (setq-default indent-tabs-mode indent-tabs-mode))

  (defun x-move-beginning-of-line (arg)
    "Move point back to indentation of beginning of line.

    Move point to the first non-whitespace character on this
    line.  If point is already there, move to the beginning of
    the line.  Effectively toggle between the first
    non-whitespace character and the beginning of the line.

    If ARG is not nil or 1, move forward ARG - 1 lines first. If
    point reaches the beginning or end of the buffer, stop
    there."

    (interactive "^p")
    (setq arg (or arg 1))

    ;; Move lines first
    (when (/= arg 1)
      (let ((line-move-visual nil))
        (forward-line (1- arg))))

    (let ((orig-point (point)))
      (back-to-indentation)
      (when (= orig-point (point))
        (move-beginning-of-line 1))))

  (defun indent-buffer ()
    "Indent the currently visited buffer."
    (interactive)
    (indent-region (point-min) (point-max)))

  (defcustom indent-sensitive-modes
    '(coffee-mode python-mode slim-mode haml-mode yaml-mode)
    "Modes for which auto-indenting is suppressed."
    :type 'list)

  (defun indent-region-or-buffer ()
    "Indent a region if selected, otherwise the whole buffer."
    (interactive)
    (unless (member major-mode indent-sensitive-modes)
      (save-excursion
        (if (region-active-p)
            (indent-region (region-beginning) (region-end))
          (indent-buffer)
          (whitespace-cleanup)))))

  ;; Key-bindings -------------------------------------------------------------

  ;; Override the beginning of line key-binding.
  (global-set-key (kbd "C-a") 'x-move-beginning-of-line)

  ;; Override the indent-region key-binding
  (global-set-key (kbd "C-M-\\") 'indent-region-or-buffer)

  ;; --------------------------------------------------------------------------
  ;; Automatically indent yanked text in programming mode.
  ;; --------------------------------------------------------------------------

  (defvar yank-indent-modes
    '(LaTeX-mode TeX-mode)
    "Modes in which to indent regions that are yanked (or yank-popped).
    Only modes that don't derive from `prog-mode' should be
    listed here.")

  (defvar yank-indent-blacklisted-modes
    '(python-mode slim-mode haml-mode)
    "Modes for which auto-indenting is suppressed.")

  (defvar yank-advised-indent-threshold 1000
    "Threshold (# chars) over which indentation does not
    automatically occur.")

  (defun yank-advised-indent-function (beg end)
    "Do indentation, as long as the region isn't too large."
    (if (<= (- end beg) yank-advised-indent-threshold)
        (indent-region beg end nil)))

  (defadvice yank (after yank-indent activate)
    "If current mode is one of 'yank-indent-modes,
    indent yanked text (with prefix arg don't indent)."
    (if (and (not (ad-get-arg 0))
             (not (member major-mode yank-indent-blacklisted-modes))
             (or (derived-mode-p 'prog-mode)
                 (member major-mode yank-indent-modes)))
        (let ((transient-mark-mode nil))
          (yank-advised-indent-function (region-beginning) (region-end)))))

  (defadvice yank-pop (after yank-pop-indent activate)
    "If current mode is one of `yank-indent-modes',
    indent yanked text (with prefix arg don't indent)."
    (when (and (not (ad-get-arg 0))
               (not (member major-mode yank-indent-blacklisted-modes))
               (or (derived-mode-p 'prog-mode)
                   (member major-mode yank-indent-modes)))
      (let ((transient-mark-mode nil))
        (yank-advised-indent-function (region-beginning) (region-end)))))

  )
