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

      '(duplicate-thing
        expand-region
        fill-column-indicator
        rainbow-mode
        undo-tree
        volatile-highlights
        whole-line-or-region
        ws-butler)

      )

;; Configuration:

(defun init-packages/init-editing ()

  ;; --------------------------------------------------------------------------
  ;; Duplicate things.
  ;; --------------------------------------------------------------------------

  (use-package duplicate-thing
    :defer t
    :bind (("M-C" . duplicate-thing)))

  ;; --------------------------------------------------------------------------
  ;; Expand region - intelligent select.
  ;; --------------------------------------------------------------------------

  (use-package expand-region
    :defer t
    :bind (("C-'" . er/expand-region)))

  ;; --------------------------------------------------------------------------
  ;; Column indicator.
  ;; --------------------------------------------------------------------------

  (defun fci-mode-unless-large-file ()
    "Enable fci-mode unless the file is too large."
    (interactive)
    (unless (> (count-lines (point-min) (point-max)) 9999)
      (fci-mode)))


  (use-package fill-column-indicator
    :defer t
    :bind
    (("C-x t f" . fci-mode))
    :init
    ;; (add-hook 'prog-mode-hook 'fci-mode-unless-large-file)
    ;; (add-hook 'text-mode-hook 'fci-mode-unless-large-file)
    )

  ;; --------------------------------------------------------------------------
  ;; Rainbow mode.
  ;; --------------------------------------------------------------------------

  (use-package rainbow-mode
    :defer t)

  ;; --------------------------------------------------------------------------
  ;; Undo tree.  To undo "C-\", to redo "C-_", undo tree "C-x u".
  ;; --------------------------------------------------------------------------

  (use-package undo-tree
    :init
    (global-undo-tree-mode))

  ;; --------------------------------------------------------------------------
  ;; Volatile highlights - highlight changes caused by undo, yank, etc.
  ;; --------------------------------------------------------------------------

  (use-package volatile-highlights
    :init
    (volatile-highlights-mode t))

  ;; --------------------------------------------------------------------------
  ;; Kill line when calling kill-region without a selected region.
  ;; --------------------------------------------------------------------------

  (use-package whole-line-or-region
    :init
    (whole-line-or-region-global-mode t))

  ;; --------------------------------------------------------------------------
  ;; `ws-butler' will cleanup whitespace on all modified files on save.
  ;; --------------------------------------------------------------------------

  (use-package ws-butler
    :init
    (ws-butler-global-mode))

  ;; --------------------------------------------------------------------------
  ;; Non-package related editing settings.
  ;; --------------------------------------------------------------------------

  ;; Kill whole line when point at beginning of line.
  (setq-default kill-whole-line t)

  ;; Replace selected rather than inserting text at point.
  (delete-selection-mode)

  ;; --------------------------------------------------------------------------
  ;; Use UTF-8.
  ;; --------------------------------------------------------------------------
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-language-environment "UTF-8")
  (prefer-coding-system 'utf-8)

  ;; --------------------------------------------------------------------------
  ;; Formatting
  ;; --------------------------------------------------------------------------

  (setq-default
   ;; Indentation size - applies even when indent-tabs-mode is nil.
   tab-width 8
   ;; Do not use tab characters for indentation.
   indent-tabs-mode nil
   ;; Standard fill-column width - last character is for end of line glyph.
   fill-column 79
   ;; Highlight lines that are too long in whitespace mode.
   whitespace-line-column fill-column)

  ;; --------------------------------------------------------------------------
  ;; Completion help.
  ;; --------------------------------------------------------------------------

  ;; hippie-expand is a better version of dabbrev-expand.
  (global-set-key (kbd "M-/") 'hippie-expand)

  (setq-default
   hippie-expand-try-functions-list
   '(try-expand-dabbrev ;; Search current buffer.
     try-expand-dabbrev-all-buffers ;; Search all other buffers.
     try-expand-dabbrev-from-kill ;; Search the kill ring.
     try-complete-file-name-partially ;; Complete text partially as file name.
     try-complete-file-name ;; Complete text as file name.
     try-expand-all-abbrevs ;; Expand according to all abbrev tables.
     try-expand-list ;; Complete the current list to a list in the buffer.
     try-expand-line ;; Complete the current line to a line in the buffer.
     try-complete-lisp-symbol-partially ;; Complete partially as Elisp symbol.
     try-complete-lisp-symbol) ;; Complete as Elisp symbol.
   )

  ;; --------------------------------------------------------------------------
  ;; Check spelling.
  ;; --------------------------------------------------------------------------

  (use-package flyspell
    :init
    (add-hook 'text-mode-hook 'flyspell-mode)
    (add-hook 'org-mode-hook 'flyspell-mode)
    (add-hook 'prog-mode-hook 'flyspell-prog-mode)
    :config
    (if (executable-find "aspell")
        (progn
          (setq-default ispell-program-name "aspell")
          (setq-default ispell-extra-args '("--sug-mode=ultra")))
      (setq-default ispell-program-name "ispell")))

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
  ;; Additional key-bindings.
  ;; --------------------------------------------------------------------------

  ;; Toggle whitespace mode.
  (global-set-key (kbd "C-c w") 'whitespace-mode)

  ;; Occur. More convenient than "M-s o"
  (global-set-key (kbd "M-s M-o") 'occur)

  )
