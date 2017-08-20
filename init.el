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
  ;; Visual configuration.
  ;; --------------------------------------------------------------------------

  ;; Font ---------------------------------------------------------------------

  (let* ((font-name "Source Code Pro")
         (font-size 10)
         (font-spec (concat font-name "-" (int-to-string font-size))))

    (set-frame-font font-spec nil t)
    (add-to-list 'default-frame-alist `(font . ,font-spec))
    (set-face-attribute 'italic nil           ;; Emacs does not set italic face
                        :family (concat font-name "-Italic")))

  ;; Fullscreen ---------------------------------------------------------------

  (toggle-frame-maximized)
  (add-to-list 'default-frame-alist '(fullscreen . maximized))

  ;; Visual clutter -----------------------------------------------------------

  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (blink-cursor-mode -1)

  ;; Scrolling ----------------------------------------------------------------

  (setq-default scroll-preserve-screen-position 1)

  ;; Line number --------------------------------------------------------------

  (setq-default linum-format "%4d \u2502") ;; Line number format
  (add-hook 'prog-mode-hook 'linum-mode)   ;; Only in programming modes

  ;; Theme --------------------------------------------------------------------

  (load-theme 'havoc t) ;; Load personal theme

  ;; --------------------------------------------------------------------------
  ;; Package configuration.
  ;; --------------------------------------------------------------------------

  (require 'package)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (package-initialize)

  ;; --------------------------------------------------------------------------
  ;; Formatting
  ;; --------------------------------------------------------------------------

  (setq-default tab-width 8)                         ;; Tab width
  (setq-default indent-tabs-mode nil)                ;; No tabs
  (setq-default fill-column 79)                      ;; Line width
  (setq-default whitespace-line-column fill-column)  ;; For whitespace mode
  (setq-default c-default-style "linux")             ;; Default C style

  (defun toggle-indent-tabs-mode ()
    "Toggle a indent-tabs-mode between a defined and undefined state."
    (interactive)
    (setq indent-tabs-mode (not indent-tabs-mode))
    (setq-default indent-tabs-mode indent-tabs-mode))

  ;; --------------------------------------------------------------------------
  ;; Convenience keyboard shortcuts.
  ;; --------------------------------------------------------------------------

  (defun kill-default-buffer ()
    "Kill the currently active buffer with no confirmation."
    (interactive)
    (let (kill-buffer-query-functions) (kill-buffer)))

  (global-set-key (kbd "C-x k") 'kill-default-buffer) ;; Kill current buffer
  (global-set-key (kbd "C-c w") 'whitespace-mode)     ;; Toggle whitespace mode
  (global-set-key (kbd "C-x k") 'kill-default-buffer) ;; Kill current buffer
  (global-set-key (kbd "M-o") 'other-window)          ;; Change window
  (global-set-key (kbd "M-s M-o") 'occur)             ;; Occur

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
  ;; Increase recursion limits.
  ;; --------------------------------------------------------------------------
  (setq-default max-specpdl-size 20000) ;; ~15x original value
  (setq-default max-lisp-eval-depth 24000) ;; 30x orignal value

) ;; ((gc-cons-threshold most-positive-fixnum))
