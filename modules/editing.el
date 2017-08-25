;;; editing.el --- Module file for editing configuration.
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
;; This module sets up packages and configuration for editing text and code.
;;
;;; License: GPLv3

;;; Required packages:

(setq init-packages/editing-packages
      '(rainbow-delimiters
        highlight-parentheses))

;; Configuration:

(defun init-packages/init-editing ()
  (use-package rainbow-delimiters)
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

  (use-package highlight-parentheses)
  (add-hook 'prog-mode-hook 'show-paren-mode)
  (add-hook 'prog-mode-hook 'highlight-parentheses-mode)
  (setq hl-paren-colors '("#86DC2F"
                          "IndianRed1"
                          "IndianRed3"
                          "IndianRed4")))
