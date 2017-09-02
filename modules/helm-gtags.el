;;; helm-gtags.el --- Module file for GTAGS with Helm configuration.
;;
;; Copyright (C) 2017 Wojciech Kozlowski
;;
;; Author: Wojciech Kozlowski <wojciech.kozlowski@vivaldi.net>
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

(setq emodule/helm-gtags-packages

      '(helm-gtags)

      )

;;; Configuration:

(defun emodule/helm-gtags-init ()

  (use-package helm-gtags
    :defer t
    :init
    (add-hook 'dired-mode-hook 'helm-gtags-mode)
    (add-hook 'eshell-mode-hook 'helm-gtags-mode)
    (add-hook 'c-mode-hook 'helm-gtags-mode)
    (add-hook 'c++-mode-hook 'helm-gtags-mode)
    (add-hook 'asm-mode-hook 'helm-gtags-mode)

    (setq
     helm-gtags-ignore-case t
     helm-gtags-auto-update t
     helm-gtags-use-input-at-cursor t
     helm-gtags-pulse-at-cursor t
     helm-gtags-prefix-key "\C-cg"
     helm-gtags-suggested-key-mapping t
     )
    :config
    (define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
    (define-key helm-gtags-mode-map (kbd "C-c g h") 'helm-gtags-show-stack)
    (define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
    (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
    (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
    (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
    (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history))
  )