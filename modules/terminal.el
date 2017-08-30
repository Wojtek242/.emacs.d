;;; terminal.el --- Module file for terminal configuration.
;;
;; Copyright (C) 2017 Wojciech Kozlowski
;;
;; Author: Wojciech Kozlowski <wojciech.kozlowski@vivaldi.net>
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

(setq init-packages/terminal-packages

      '()

      )

;; Configuration:

(defun init-packages/init-terminal ()

  (use-package term
    :init
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

  :config
  (defun ansi-term-pop (term-cmd)
    "Launch terminal in (preferably) other window."
    (let ((ansi-buf nil)
          (cur-buf (current-buffer)))
      (setq ansi-buf (ansi-term term-cmd))
      (switch-to-buffer cur-buf)
      (switch-to-buffer-other-window ansi-buf)))

  (defun ansi-term-recycle (term-cmd)
    "Kill current buffer and start an *ansi-term* in it."
    (kill-buffer (current-buffer))
    (ansi-term term-cmd))

  (defun first-matching-buffer (regex)
    "Find first buffer whose name matches REGEXP."
    (car (remove-if-not
          (apply-partially #'string-match-p regex)
          (mapcar 'buffer-name (buffer-list)))))

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
          (term-cmd "/bin/zsh")
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

  )
