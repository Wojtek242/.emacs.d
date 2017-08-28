;;; programming.el --- Module file for programming configuration.
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
;; This module sets up packages and configuration for editing source code in
;; all languages.
;;
;;; License: GPLv3

;;; Required packages:

(setq init-packages/programming-packages

      '(yasnippet)

      )

;; Configuration:

(defun init-packages/init-programming ()

  ;; --------------------------------------------------------------------------
  ;; Enable yasnippet.
  ;; --------------------------------------------------------------------------

  (use-package yasnippet)
  (yas-global-mode 1)

)
