;;; init.el --- Emacs Initialization File
;;
;; Copyright (c) 2017 Wojciech Kozlowski
;;
;; Author: Wojciech Kozlowski <wk@wojciechkozlowski.eu>
;; URL: https://gitlab.wojciechkozlowski.eu/config/emacs.d
;; URL: https://github.com/Wojtek242/.emacs.d
;; Created: 17 Aug 2017
;;
;;; License: GPLv3

;;; Commentary:

;;; Code:

;; ----------------------------------------------------------------------------
;; Run init without garbage collection.
;; ----------------------------------------------------------------------------

(let ((gc-cons-threshold most-positive-fixnum))

  ;; --------------------------------------------------------------------------
  ;; Initialise and setup `package'.
  ;; --------------------------------------------------------------------------

  (require 'package)
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize)

  ;; Local copies of packages no longer provided by MELPA.  See
  ;; https://github.com/melpa/melpa/pull/5008.
  (add-to-list 'load-path "~/.emacs.d/emacswiki/")

  ;; --------------------------------------------------------------------------
  ;; Load `emodule'.
  ;; --------------------------------------------------------------------------

  (add-to-list 'load-path "~/.emacs.d/emodule")
  (require 'emodule)

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

  ;; Theme --------------------------------------------------------------------

  ;; Add the necessary paths.
  (add-to-list 'load-path "~/.emacs.d/themes/")
  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

  ;; Load the dark theme by default.
  (load-theme 'havoc-dark t) ;; Load personal theme

  ;; Splash screen ------------------------------------------------------------

  ;; Add path.
  (add-to-list 'load-path "~/.emacs.d/initial-buffer")
  (require 'initial-buffer)

  ;; Set the initial buffer.
  (setq initial-buffer-choice 'initial-buffer/goto-buffer)

  ;; --------------------------------------------------------------------------
  ;; Change file in which custom variable changes are saved.
  ;; --------------------------------------------------------------------------

  (setq-default custom-file "~/.emacs.d/custom.el")


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
  ;; Load modules.
  ;; --------------------------------------------------------------------------

  (emodule/init '(
                  em-editing
                  em-emacs
                  em-files
                  em-helm
                  em-helm-gtags
                  em-modeline
                  em-org
                  em-parentheses
                  em-programming
                  em-terminal
                  em-version-control
                  em-workflow
                  ))


  ;; *********************************************************************** ;;
  ;;                                                                         ;;
  ;;                                                                         ;;
  ;; Any configuration that is not in a module or needs to override module   ;;
  ;; settings should be set below this point.                                ;;
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

  ) ;; Reset garbage collection settings.

(provide 'init)
;;; init.el ends here
