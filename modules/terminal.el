;;; terminal.el --- Module file for terminal configuration.
;;
;; Copyright (C) 2017-2019 Wojciech Kozlowski
;;
;; Author: Wojciech Kozlowski <wk@wojciechkozlowski.eu>
;; Created: 2017-08-28
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

(defvar emodule/terminal-packages

  '(vterm)

  )

;; Configuration:

(defun emodule/terminal-init ()
  "Initialise the `terminal' module."

  ;; -----------------------------------------------------------------------------------------------
  ;; `eshell'
  ;; -----------------------------------------------------------------------------------------------

  (use-package eshell
    :config
    (defun eshell-pop (name)
      "Launch terminal in (preferably) other window."
      (let ((esh-buf nil)
            (cur-buf (current-buffer)))
        (setq esh-buf (eshell name))
        (switch-to-buffer cur-buf)
        (switch-to-buffer-other-window esh-buf)))

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

  ;; -----------------------------------------------------------------------------------------------
  ;; `vterm'
  ;; -----------------------------------------------------------------------------------------------

  (use-package vterm
    :init
    (setq vterm-shell "/bin/zsh"
          vterm-max-scrollback 10000)

    (defun x-vterm-setup ()
      (define-key vterm-mode-map
        [remap whole-line-or-region-yank] 'vterm-yank)
      (define-key vterm-mode-map (kbd "C-S-v") 'vterm-yank)

      (define-key vterm-mode-map
        [remap scroll-up-command] 'vterm--self-insert)
      (define-key vterm-mode-map
        [remap scroll-down-command] 'vterm--self-insert))

    (defun x-vterm-recycle ()
      "Kill current buffer and start a vterm in it."
      (let ((working-directory default-directory))
        (kill-buffer (current-buffer))
        (let ((default-directory working-directory))
          (vterm))))

    (defun x-vterm-other-window ()
      "Start vterm in other window unless in initial buffer."
      (interactive)
      (if (equal major-mode 'init-buffer-mode)
          (vterm)
        (vterm-other-window)))

    (defun visit-vterm ()
      "Open or switch to active vterm.
     If current buffer is a vterm:
       If it is running
         Open a new vterm in a new window
       If it is not running
         Recycle (kill buffer, restart vterm)
     If current buffer is not a vterm:
       If a buffer in vterm-mode exists
         Switch to that buffer in other window
         Recycle if necessary
       If it does not exist
         Open a new vterm in a new window"
      (interactive)
      (if (string= "vterm-mode" major-mode)
          (if (term-check-proc (buffer-name))
              (vterm-other-window)
            (x-vterm-recycle))
        (let ((anon-term (seq-find (lambda (buffer)
                                     (with-current-buffer buffer
                                       (string= "vterm-mode" major-mode)))
                                   (buffer-list))))
          (if anon-term
              (progn
                (if (equal major-mode 'init-buffer-mode)
                    (switch-to-buffer anon-term)
                  (switch-to-buffer-other-window anon-term))
                (unless (term-check-proc (buffer-name))
                  (x-vterm-recycle)))
            (x-vterm-other-window)))))

    :hook
    (vterm-mode . x-vterm-setup)
    :bind
    (("C-x C-'" . vterm)
     ("C-x '" . visit-vterm)))

  )

(provide 'emodule/terminal)
;;; terminal.el ends here
