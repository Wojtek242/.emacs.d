;;; emacs.el --- Module file for Emacs management configuration.
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
;; This module sets up packages and configuration for managing Emacs.
;;
;;; License: GPLv3

;;; Required packages:

(setq init-packages/emacs-packages
      '(use-package))

;;; Configuration:

(defun init-packages/init-emacs ())
