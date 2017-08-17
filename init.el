;;; init.el --- Emacs Initialization File
;;
;; Copyright (c) 2017 Wojciech Kozlowski
;;
;; Author: Wojciech Kozlowski <wojciech.kozlowski@vivaldi.net>
;; URL: https://gitlab.wojciechkozlowski.eu/config/emacs.d
;;
;;; License: GPLv3

;; ----------------------------------------------------------------------------
;; Run init without garbage collection.
;; ----------------------------------------------------------------------------

(let ((gc-cons-threshold most-positive-fixnum))

  ;; --------------------------------------------------------------------------
  ;; Configure garbage collection.
  ;;
  ;; Based on advice from:
  ;; http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
  ;; --------------------------------------------------------------------------

  (defun my-minibuffer-setup-hook ()
    (setq gc-cons-threshold most-positive-fixnum))

  (defun my-minibuffer-exit-hook ()
    (setq gc-cons-threshold 800000))

  (add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
  (add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

  ;; --------------------------------------------------------------------------
  ;; Include MELPA.
  ;; --------------------------------------------------------------------------

  (require 'package)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/") t)

  ;; --------------------------------------------------------------------------
  ;; Visual configuration.
  ;; --------------------------------------------------------------------------

  ;; Font ---------------------------------------------------------------------

  (setq default-frame-alist '((font . "Source Code Pro-10")))
  ;; Emacs does not set italic face automatically
  (set-face-attribute 'italic nil
                      :family "Source Code Pro-Italic")

  ;; Fullscreen ---------------------------------------------------------------

  (add-to-list 'default-frame-alist '(fullscreen . maximized))

  ;; Visual clutter -----------------------------------------------------------

  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)

  ;; Scrolling ----------------------------------------------------------------

  (setq-default scroll-preserve-screen-position 1)

  ;; Line number --------------------------------------------------------------

  (setq-default linum-format "%4d \u2502") ;; Line number format
  (add-hook 'prog-mode-hook 'linum-mode)   ;; Only in programming modes

  ;; Theme --------------------------------------------------------------------

  ;; This is required until this init file gets moved to .emacs.d
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name)))

  (load-theme 'havoc t) ;; Load personal theme

) ;; ((gc-cons-threshold most-positive-fixnum))
