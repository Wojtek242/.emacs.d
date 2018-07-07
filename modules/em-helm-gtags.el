;;; em-helm-gtags.el --- Module file for GTAGS with Helm configuration.
;;
;; Copyright (C) 2017 Wojciech Kozlowski
;;
;; Author: Wojciech Kozlowski <wk@wojciechkozlowski.eu>
;; Created: 2 Sep 2017
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This module sets up configuration for using gtags with helm.
;;
;; To add gtags for system include paths:
;;
;; export GTAGSLIBPATH=$HOME/.gtags/
;;
;; mkdir ~/.gtags
;; cd ~/.gtags
;;
;; ln -s /usr/include usr-include
;; ln -s /usr/local/include/ usr-local-include
;;
;; gtags -c
;;
;;; License: GPLv3

;;; Required packages:

;;; Code:

(defvar emodule/em-helm-gtags-packages

  '(helm-gtags)

  )

;;; Configuration:

(defun emodule/em-helm-gtags-init ()
  "Initialise the `em-helm-gtags' module."

  (use-package helm-gtags
    :hook
    ((dired-mode eshell-mode c-mode c++-mode asm-mode-hook) . helm-gtags-mode)
    :bind
    (:map helm-gtags-mode-map
          ("C-c g a" . helm-gtags-tags-in-this-function)
          ("C-c g h" . helm-gtags-show-stack)
          ("C-j" . helm-gtags-select)
          ("M-." . helm-gtags-dwim)
          ("M-," . helm-gtags-pop-stack)
          ("C-c <" . helm-gtags-previous-history)
          ("C-c >" . helm-gtags-next-history))
    :init
    (setq helm-gtags-ignore-case t
          helm-gtags-auto-update t
          helm-gtags-use-input-at-cursor t
          helm-gtags-pulse-at-cursor t
          helm-gtags-prefix-key "\C-cg"
          helm-gtags-suggested-key-mapping t))
  )

(provide 'em-helm-gtags)
;;; em-helm-gtags.el ends here
