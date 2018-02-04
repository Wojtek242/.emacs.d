;;; org.el --- Module file for org-mode configuration.
;;
;; Copyright (C) 2017 Wojciech Kozlowski
;;
;; Author: Wojciech Kozlowski <wojciech.kozlowski@vivaldi.net>
;; Created: 4 Feb 2018
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This module sets up org-mode.
;;
;;; License: GPLv3

;;; Required packages:

(setq emodule/org-packages

      '(org-bullets)

      )

;; Configuration:

(defun emodule/org-init ()

  ;; --------------------------------------------------------------------------
  ;; Hide special characters for itlaics/bold/underline.
  ;; --------------------------------------------------------------------------

  (setq org-hide-emphasis-markers t)

  ;; --------------------------------------------------------------------------
  ;; Better bullet points.
  ;; --------------------------------------------------------------------------

  (font-lock-add-keywords 'org-mode
                          '(("^ +\\(*\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; --------------------------------------------------------------------------
  ;; Better header bullets
  ;; --------------------------------------------------------------------------

  (use-package org-bullets
    :defer t
    :init
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

  )
