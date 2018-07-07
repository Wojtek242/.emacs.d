;;; em-terminal.el --- Module file for terminal configuration.
;;
;; Copyright (C) 2017 Wojciech Kozlowski
;;
;; Author: Wojciech Kozlowski <wk@wojciechkozlowski.eu>
;; Created: 28 Aug 2017
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This module sets up packages and configuration for working with a terminal
;; in Emacs.
;;
;;; License: GPLv3

;;; Required packages:

;;; Code:

(defvar emodule/em-terminal-packages

  '()

  )

;; Configuration:

(defun emodule/em-terminal-init ()
  "Initialise the `em-terminal' module."

  ;; --------------------------------------------------------------------------
  ;; Configure term.
  ;; --------------------------------------------------------------------------

  (use-package term
    :config
    (declare-function term-send-raw-string "term")
    (declare-function term-check-proc "term")

    (defun x-term-setup ()
      (interactive)
      (define-key term-raw-map (kbd "C-y") 'term-send-raw)
      (define-key term-raw-map (kbd "C-p") 'term-send-raw)
      (define-key term-raw-map (kbd "C-n") 'term-send-raw)
      (define-key term-raw-map (kbd "C-s") 'term-send-raw)
      (define-key term-raw-map (kbd "C-r") 'term-send-raw)
      (define-key term-raw-map (kbd "M-o") 'other-window)
      (define-key term-raw-map (kbd "M-w") 'kill-ring-save)
      (define-key term-raw-map (kbd "M-y") 'helm-show-kill-ring)
      (define-key term-raw-map (kbd "M-d") (lambda () (interactive) (term-send-raw-string "\ed")))
      (define-key term-raw-map (kbd "<C-backspace>") (lambda () (interactive) (term-send-raw-string "\e\C-?")))
      (define-key term-raw-map (kbd "M-p") (lambda () (interactive) (term-send-raw-string "\ep")))
      (define-key term-raw-map (kbd "M-n") (lambda () (interactive) (term-send-raw-string "\en")))
      (define-key term-raw-map (kbd "C-S-v") 'term-paste)
      (define-key term-raw-map (kbd "C-h") nil) ; unbind C-h
      (define-key term-raw-map (kbd "M-x") nil) ; unbind M-x
      (define-key term-raw-map (kbd "C-1") 'delete-other-windows)
      (define-key term-raw-map (kbd "C-2") 'split-window-below)
      (define-key term-raw-map (kbd "C-3") 'split-window-right)
      (define-key term-raw-map (kbd "C-0") 'delete-window))
    (add-hook 'term-mode-hook 'x-term-setup t)
    (setq term-buffer-maximum-size 0)

    (defun ansi-term-pop (term-cmd)
      "Launch terminal in (preferably) other window."
      (let ((ansi-buf nil)
            (cur-buf (current-buffer)))
        (setq ansi-buf (ansi-term term-cmd))
        (switch-to-buffer cur-buf)
        (switch-to-buffer-other-window ansi-buf)))
    (declare-function ansi-term-pop "terminal")

    (defun ansi-term-recycle (term-cmd)
      "Kill current buffer and start an *ansi-term* in it."
      (kill-buffer (current-buffer))
      (ansi-term term-cmd))
    (declare-function ansi-term-recycle "terminal")

    (defun first-matching-buffer (regex)
      "Find first buffer whose name matches REGEXP."
      (declare-function remove-if-not "cl-seq")
      (car (remove-if-not
            (apply-partially #'string-match-p regex)
            (mapcar 'buffer-name (buffer-list)))))
    (declare-function first-matching-buffer "terminal")

    (defun visit-ansi-term ()
      "Open or switch to active ansi-term.
     If current buffer is a term:
       If it is running
         Open a new ansi-term in a new window
       If it is not running
         Recycle (kill buffer, restart term)
     If current buffer is not a term:
       If a *ansi-term*<x> buffer exists
         Switch to that ansi-term in other window
         Recycle if necessary
       If it does not exist
         Open a new ansi-term in a new window"
      (interactive)
      (let ((is-term (string= "term-mode" major-mode))
            (is-running (term-check-proc (buffer-name)))
            (term-cmd "/bin/bash")
            (anon-term (first-matching-buffer "^*ansi-term*")))
        (if is-term
            (if is-running
                (ansi-term-pop term-cmd)
              (ansi-term-recycle term-cmd))
          (if anon-term
              (progn
                (switch-to-buffer-other-window anon-term)
                (unless (term-check-proc (buffer-name))
                  (ansi-term-recycle term-cmd)))
            (ansi-term-pop term-cmd)))))

    (global-set-key (kbd "C-x '") 'visit-ansi-term))

  ;; --------------------------------------------------------------------------
  ;; Configure eshell.
  ;; --------------------------------------------------------------------------

  (use-package eshell
    :config
    (declare-function eshell "eshell")
    (declare-function eshell-send-input "esh-mode")

    (defun eshell-pop (name)
      "Launch terminal in (preferably) other window."
      (let ((esh-buf nil)
            (cur-buf (current-buffer)))
        (setq esh-buf (eshell name))
        (switch-to-buffer cur-buf)
        (switch-to-buffer-other-window esh-buf)))
    (declare-function eshell-pop "terminal")

    (defun eshell-here ()
      (interactive)
      (let* ((parent default-directory)
             (name   (car
                      (last
                       (split-string parent "/" t))))
             (esh-buf-name (concat "*eshell: " name "*"))
             (is-esh (string= "eshell-mode" major-mode))
             (esh-buf (first-matching-buffer esh-buf-name)))

        (unless is-esh
          (if esh-buf
              (switch-to-buffer-other-window esh-buf)
            (progn
              (eshell-pop "new")
              (rename-buffer esh-buf-name))))

        (insert (concat "ls"))
        (eshell-send-input)))

    (defun delete-single-window (&optional window)
      "Remove WINDOW from the display.  Default is `selected-window'.
       If WINDOW is the only one in its frame, then `delete-frame' too."
      (interactive)
      (save-current-buffer
        (setq window (or window (selected-window)))
        (select-window window)
        (kill-buffer)
        (if (one-window-p t)
            (delete-frame)
          (delete-window (selected-window)))))
    (declare-function delete-single-window "terminal")

    (defun eshell/x (&rest args)
      (delete-single-window))

    (defun eshell-setup ()
      (interactive)
      (define-key eshell-mode-map (kbd "C-0") 'delete-window))

    (add-hook 'eshell-mode-hook 'eshell-setup t)

    (global-set-key (kbd "C-x /") 'eshell-here))

  (use-package em-smart
    :config
    (setq eshell-where-to-jump 'begin)
    (setq eshell-review-quick-commands nil)
    (setq eshell-smart-space-goes-to-end t))

  (use-package em-term
    :config
    (setq eshell-visual-commands (nconc eshell-visual-commands '("htop"
                                                                 "tmux"))))

  )

(provide 'em-terminal)
;;; em-terminal.el ends here
