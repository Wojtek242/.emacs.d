;;; init.el --- Emacs Initialization File
;;
;; Copyright (c) 2017 Wojciech Kozlowski
;;
;; Author: Wojciech Kozlowski <wojciech.kozlowski@vivaldi.net>
;; URL: https://gitlab.wojciechkozlowski.eu/config/emacs.d
;; Created: 17 Aug 2017
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

  (add-to-list 'load-path "~/.emacs.d/themes/")
  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
  (load-theme 'havoc-dark t) ;; Load personal theme

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
                        editing
                        workflow))


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
  (load custom-file 'noerror)

  ;; --------------------------------------------------------------------------
  ;; Programming style.
  ;; --------------------------------------------------------------------------

  (setq-default c-default-style "linux")             ;; Default C style

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

  ;; --------------------------------------------------------------------------
  ;; Convenience keyboard shortcuts.
  ;; --------------------------------------------------------------------------

  ;; Kill current buffer without prompting.
  (global-set-key (kbd "C-x k") 'kill-default-buffer)

  ;; Kill other window (cyclic order).
  (global-set-key (kbd "C-x C-q") 'quit-other-window)

  ;; Toggle whitespace mode.
  (global-set-key (kbd "C-c w") 'whitespace-mode)

  ;; Change active window.  More convenient than "C-x o".
  (global-set-key (kbd "M-o") 'other-window)

  ;; Occur. More convenient than "M-s o"
  (global-set-key (kbd "M-s M-o") 'occur)

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

) ;; Reset garbage collection settings.
(put 'erase-buffer 'disabled nil)
