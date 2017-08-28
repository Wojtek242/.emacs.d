;;; emacs.el --- Module file for configuring Emacs itself.
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
;; This module is used for generic Emacs configuration.
;;
;;; License: GPLv3

;;; Required packages:

(setq init-packages/emacs-packages

      '(use-package)

      )

;;; Configuration:

(defun init-packages/init-emacs ()

  ;; --------------------------------------------------------------------------
  ;; Keep point in same position on the screen when scrolling.
  ;; --------------------------------------------------------------------------

  (setq-default scroll-preserve-screen-position 1)

  ;; --------------------------------------------------------------------------
  ;; Convenience functions.
  ;; --------------------------------------------------------------------------

  (defun quit-other-window ()
    "Quit the next window in cyclic order"
    (interactive)
    (quit-window t (next-window (selected-window))))

  (defun kill-default-buffer ()
    "Kill the currently active buffer with no confirmation."
    (interactive)
    (let (kill-buffer-query-functions) (kill-buffer)))

  (defun refresh-non-face-colours ()
    "Restart modes that use colours not set with face variables.
    This has to be called whenever the active theme changes to
    refresh these colours."

    (when (and (fboundp 'fci-mode)
               (fci-mode))
      (fci-mode 1))

    (when (and (fboundp 'highlight-parentheses-mode)
               (highlight-parentheses-mode))
      (highlight-parentheses-mode 1)))

  ;; Key-bindings -------------------------------------------------------------

  ;; Kill other window (cyclic order).
  (global-set-key (kbd "C-x C-q") 'quit-other-window)

  ;; Kill current buffer without prompting.
  (global-set-key (kbd "C-x k") 'kill-default-buffer)

  ;; --------------------------------------------------------------------------
  ;; Additional key-bindings.
  ;; --------------------------------------------------------------------------

  ;; Change active window.  More convenient than "C-x o".
  (global-set-key (kbd "M-o") 'other-window)

  ;; Scroll up/down, but keep point in place.
  (global-set-key (kbd "C-<") (lambda() (interactive)
                                (let ((scroll-preserve-screen-position nil))
                                  (scroll-down 1))))
  (global-set-key (kbd "C->") (lambda() (interactive)
                                (let ((scroll-preserve-screen-position nil))
                                  (scroll-up 1))))

  ;; Setup key-bindings for switching between themes.
  (global-set-key (kbd "C-x t l") '(lambda () (interactive)
                                     (load-theme 'havoc-light t)
                                     (refresh-non-face-colours)))
  (global-set-key (kbd "C-x t d") '(lambda () (interactive)
                                     (load-theme 'havoc-dark t)
                                     (refresh-non-face-colours)))

  ;; --------------------------------------------------------------------------
  ;; Aliases.
  ;; --------------------------------------------------------------------------

  ;; y or n is enough.
  (defalias 'yes-or-no-p 'y-or-n-p)

  ;; Always use ibuffer.
  (defalias 'list-buffers 'ibuffer)

  ;; --------------------------------------------------------------------------
  ;; Configure garbage collection.
  ;;
  ;; Based on advice from:
  ;; http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
  ;; --------------------------------------------------------------------------

  (defun minibuffer-gc-setup-hook ()
    (setq gc-cons-threshold most-positive-fixnum))

  (defun minibuffer-gc-exit-hook ()
    (setq gc-cons-threshold 800000))

  (add-hook 'minibuffer-setup-hook #'minibuffer-gc-setup-hook)
  (add-hook 'minibuffer-exit-hook #'minibuffer-gc-exit-hook)

  ;; --------------------------------------------------------------------------
  ;; Increase recursion limits.
  ;; --------------------------------------------------------------------------
  (setq-default max-specpdl-size 20000) ;; ~15x original value
  (setq-default max-lisp-eval-depth 24000) ;; 30x orignal value

  )
