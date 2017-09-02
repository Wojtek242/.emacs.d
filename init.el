;;; init.el --- Emacs Initialization File
;;
;; Copyright (c) 2017 Wojciech Kozlowski
;;
;; Author: Wojciech Kozlowski <wojciech.kozlowski@vivaldi.net>
;; URL: https://gitlab.wojciechkozlowski.eu/config/emacs.d
;; URL: https://github.com/Wojtek242/.emacs.d
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

  ;; Modeline -----------------------------------------------------------------

  (size-indication-mode 1)
  (column-number-mode 1)

  ;; Theme --------------------------------------------------------------------

  ;; Add the necessary paths.
  (add-to-list 'load-path "~/.emacs.d/themes/")
  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

  ;; Load the dark theme by default.
  (load-theme 'havoc-dark t) ;; Load personal theme

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
  ;; Initialise and setup `package'.
  ;; --------------------------------------------------------------------------

  (require 'package)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (package-initialize)

  ;; --------------------------------------------------------------------------
  ;; Load `emodule'.
  ;; --------------------------------------------------------------------------

  (add-to-list 'load-path "~/.emacs.d/emodule")
  (require 'emodule)

  ;; --------------------------------------------------------------------------
  ;; Load modules.
  ;; --------------------------------------------------------------------------

  (emodule/init-debug '(
                  editing
                  emacs
                  files
                  helm
                  helm-gtags
                  parentheses
                  programming
                  terminal
                  version-control
                  workflow
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
(put 'narrow-to-region 'disabled nil)
