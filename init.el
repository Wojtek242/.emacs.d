;;; init.el --- Emacs Initialization File
;;
;; Copyright (c) 2017-2019 Wojciech Kozlowski
;;
;; Author: Wojciech Kozlowski <wk@wojciechkozlowski.eu>
;; Created: 2017-08-17
;;
;;; License: GPLv3

;;; Commentary:

;;; Code:

;; -------------------------------------------------------------------------------------------------
;; Run init without garbage collection.
;; -------------------------------------------------------------------------------------------------

(let ((gc-cons-threshold most-positive-fixnum))

  ;; -----------------------------------------------------------------------------------------------
  ;; Helper function to get correct configuration directory.
  ;; -----------------------------------------------------------------------------------------------

  (defun emacs-dir (rel)
    "Obtain full path to REL."
    (concat (file-name-as-directory user-emacs-directory) rel))

  ;; -----------------------------------------------------------------------------------------------
  ;; Initialise and setup `package'.
  ;; -----------------------------------------------------------------------------------------------

  (require 'package)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize)

  ;; External .el files that are not available from MELPA.
  (add-to-list 'load-path (emacs-dir "external"))

  ;; -----------------------------------------------------------------------------------------------
  ;; Load `emodule'.
  ;; -----------------------------------------------------------------------------------------------

  (add-to-list 'load-path (emacs-dir "emodule"))
  (require 'emodule)

  ;; -----------------------------------------------------------------------------------------------
  ;; Visual configuration.
  ;; -----------------------------------------------------------------------------------------------

  ;; Font ------------------------------------------------------------------------------------------

  (let* ((font-name "Source Code Pro")
         (font-size 10)
         (font-spec (concat font-name "-" (int-to-string font-size))))

    (set-frame-font font-spec nil t)
    (add-to-list 'default-frame-alist `(font . ,font-spec))
    (set-face-attribute 'italic nil           ;; Emacs does not set italic face
                        :family (concat font-name "-Italic")))

  ;; Visual clutter --------------------------------------------------------------------------------

  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (blink-cursor-mode -1)

  ;; Theme -----------------------------------------------------------------------------------------

  ;; Add the necessary paths.
  (add-to-list 'load-path (emacs-dir "themes"))
  (add-to-list 'custom-theme-load-path (emacs-dir "themes"))

  ;; Load the dark theme by default.
  (load-theme 'havoc-dark t) ;; Load personal theme

  ;; Splash screen ---------------------------------------------------------------------------------

  ;; Add path.
  (add-to-list 'load-path (emacs-dir "init-buffer"))
  (require 'init-buffer)

  ;; Set the initial buffer.
  (setq-default init-buffer-choice 'init-buffer/goto-buffer)

  ;; -----------------------------------------------------------------------------------------------
  ;; Change file in which custom variable changes are saved.
  ;; -----------------------------------------------------------------------------------------------

  (setq-default custom-file (emacs-dir "custom.el"))


  ;; ******************************************************************************************** ;;
  ;;                                                                                              ;;
  ;;                                           MODULES                                            ;;
  ;;                                                                                              ;;
  ;; -------------------------------------------------------------------------------------------- ;;
  ;;                                                                                              ;;
  ;;                                                                                              ;;
  ;; Visual configuration must come before this point so that the frame can be set up before any  ;;
  ;; time consuming package management.                                                           ;;
  ;;                                                                                              ;;
  ;;                                                                                              ;;
  ;; ******************************************************************************************** ;;


  ;; -----------------------------------------------------------------------------------------------
  ;; Load modules.
  ;; -----------------------------------------------------------------------------------------------

  (emodule/init '(
                  emacs
                  helm
                  languages
                  modeline
                  org
                  programming
                  terminal
                  vcs
                  ))


  ;; ******************************************************************************************** ;;
  ;;                                                                                              ;;
  ;;                                                                                              ;;
  ;; Any configuration that is not in a module or needs to override module settings should be set ;;
  ;; below this point.                                                                            ;;
  ;;                                                                                              ;;
  ;;                                                                                              ;;
  ;; -------------------------------------------------------------------------------------------- ;;
  ;;                                                                                              ;;
  ;;                                         END MODULES                                          ;;
  ;;                                                                                              ;;
  ;; ******************************************************************************************** ;;

  ;; -----------------------------------------------------------------------------------------------
  ;; Load any custom variables.
  ;; -----------------------------------------------------------------------------------------------

  (load custom-file 'noerror)

  ) ;; Reset garbage collection settings.

(provide 'init)
;;; init.el ends here
