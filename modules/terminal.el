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

      '(shell-pop)

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
  (defun ansi-term-pop (buf)
    "Launch terminal in BUF, preferably in other window.
    This function will keep the default directory of the current
    buffer"
    (let ((term-cmd "/bin/zsh")
          (dir default-directory))
      (pop-to-buffer buf)
      (let ((default-directory dir))
        (unless (term-check-proc buf)
          (when (string= "term-mode" major-mode)
            (kill-buffer buf))
          (ansi-term term-cmd)))))

  (defun visit-ansi-term ()
    "If the current buffer is:
     1) a running ansi-term named *ansi-term*, run a new one.
     2) a stopped ansi-term, kill it and create a new one.
     3) a non ansi-term, go to an already running ansi-term
        or start a new one while killing a defunct one"
    (interactive)
    (let* ((is-term (string= "term-mode" major-mode))
           (is-running (term-check-proc (buffer-name)))
           (term-cmd "/bin/zsh")
           (buf-name "*ansi-term*")
           (anon-term (get-buffer buf-name)))
      (if is-term
          (if is-running
              (if (string-match buf-name (buffer-name))
                  (ansi-term-pop (other-buffer (current-buffer) 'visible-ok))
                (if anon-term
                    (switch-to-buffer buf-name)
                  (ansi-term term-cmd)))
            (kill-buffer (buffer-name))
            (ansi-term term-cmd))
        (if anon-term
            (ansi-term-pop buf-name)
          (ansi-term-pop (other-buffer (current-buffer) 'visible-ok))))))

  (global-set-key (kbd "C-x '") 'visit-ansi-term))

  )
