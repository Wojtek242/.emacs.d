;;; version-control.el --- Module file for version control configuration.
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
;; This module sets up configuration for version control packages such as
;; `magit'.
;;
;;; License: GPLv3

;;; Required packages:

(setq init-packages/version-control-packages
      '(magit))

;;; Configuration:

(defun init-packages/init-version-control ()
  (use-package magit))
