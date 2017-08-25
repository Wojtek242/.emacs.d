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

  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
  (load-theme 'havoc t) ;; Load personal theme

  ;; --------------------------------------------------------------------------
  ;; Change file in which custom variable changes are saved.
  ;; --------------------------------------------------------------------------
  (setq custom-file "~/.emacs.d/custom.el")


  ;; *********************************************************************** ;;
  ;;                                                                         ;;
  ;;                                 MODULES                                 ;;
  ;;                                                                         ;;
  ;; ----------------------------------------------------------------------- ;;
  ;;                                                                         ;;
  ;;                                                                         ;;
  ;; Visual configuration must come before this point so that the frame can  ;;
  ;; be set up before any time consuming package management.                 ;;
  ;;                                                                         ;;
  ;;                                                                         ;;
  ;; *********************************************************************** ;;


  ;; --------------------------------------------------------------------------
  ;; Initialise and setup `package'.
  ;; --------------------------------------------------------------------------

  (require 'package)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (package-initialize)

  ;; --------------------------------------------------------------------------
  ;; Load `init-packages'.
  ;; --------------------------------------------------------------------------

  (add-to-list 'load-path "~/.emacs.d/init-packages")
  (require 'init-packages)

  ;; --------------------------------------------------------------------------
  ;; Load modules.
  ;; --------------------------------------------------------------------------

  (init-packages/init '(emacs
                        version-control
                        editing))


  ;; *********************************************************************** ;;
  ;;                                                                         ;;
  ;;                                                                         ;;
  ;; Any further non-package specific configuration should be set below this ;;
  ;; point so that it does not get overridden by package configuration.      ;;
  ;;                                                                         ;;
  ;;                                                                         ;;
  ;; ----------------------------------------------------------------------- ;;
  ;;                                                                         ;;
  ;;                               END MODULES                               ;;
  ;;                                                                         ;;
  ;; *********************************************************************** ;;


  ;; --------------------------------------------------------------------------
  ;; Load any custom variables.
  ;; --------------------------------------------------------------------------
  (when (file-exists-p custom-file)
    (load custom-file))

  ;; --------------------------------------------------------------------------
  ;; Formatting
  ;; --------------------------------------------------------------------------

  (setq-default tab-width 8)                         ;; Tab width
  (setq-default indent-tabs-mode nil)                ;; No tabs
  (setq-default fill-column 79)                      ;; Line width
  (setq-default whitespace-line-column fill-column)  ;; For whitespace mode
  (setq-default c-default-style "linux")             ;; Default C style

  ;; --------------------------------------------------------------------------
  ;; Convenience functions.
  ;; --------------------------------------------------------------------------

  (defun toggle-indent-tabs-mode ()
    "Toggle a indent-tabs-mode between a defined and undefined state."
    (interactive)
    (setq indent-tabs-mode (not indent-tabs-mode))
    (setq-default indent-tabs-mode indent-tabs-mode))

  (defun quit-other-window ()
    "Quit the next window in cyclic order"
    (interactive)
    (quit-window t (next-window (selected-window))))

  (defun kill-default-buffer ()
    "Kill the currently active buffer with no confirmation."
    (interactive)
    (let (kill-buffer-query-functions) (kill-buffer)))

  ;; --------------------------------------------------------------------------
  ;; Convenience keyboard shortcuts.
  ;; --------------------------------------------------------------------------

  (global-set-key (kbd "C-x k") 'kill-default-buffer) ;; Kill current buffer
  (global-set-key (kbd "C-x C-q") 'quit-other-window) ;; Kill other window
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

) ;; Reset garbage collection settings.
