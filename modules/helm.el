;;; helm.el --- Module file for Helm configuration.
;;
;; Copyright (C) 2017 Wojciech Kozlowski
;;
;; Author: Wojciech Kozlowski <wojciech.kozlowski@vivaldi.net>
;; Created: 27 Aug 2017
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This module sets up configuration for the helm package.
;;
;;; License: GPLv3

;;; Required packages:

(setq init-packages/helm-packages

      '(helm
        helm-projectile
        helm-descbinds)

      )

;;; Configuration:

(defun init-packages/init-helm ()

  (use-package helm
    :bind
    (("C-x C-f" . helm-find-files)
     ("M-x" . helm-M-x)
     ("M-y" . helm-show-kill-ring)
     ("C-x b" . helm-mini)
     ("C-M-j" . helm-semantic-or-imenu)
     ("C-c h M-s M-o" . helm-occur)
     ("C-h SPC" . helm-all-mark-rings)
     ("C-c h x" . helm-register))
    :config
    (require 'helm-config)
    (require 'helm-descbinds)

    ;; Helm prefix ------------------------------------------------------------

    ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
    ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
    ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
    (global-set-key (kbd "C-c h") 'helm-command-prefix)
    (global-unset-key (kbd "C-x c"))

    ;; Helm settings ----------------------------------------------------------

    (setq
     ;; Open helm buffer inside current window, not occupy whole other window.
     helm-split-window-in-side-p t
     ;; Move to end or beginning of list when reaching top or bottom of list.
     helm-move-to-line-cycle-in-source t
     ;; Search for library in `require' and `declare-function' sexp.
     helm-ff-search-library-in-sexp t
     ;; Use ‘recentf-list’ instead of ‘file-name-history’.
     helm-ff-file-name-history-use-recentf t
     ;; Echo iput to helm header.
     helm-echo-input-in-header-line t
     ;; Fuzzy matching.
     helm-M-x-fuzzy-match t
     helm-buffers-fuzzy-matching t
     helm-recentf-fuzzy-match t
     helm-semantic-fuzzy-match t
     helm-imenu-fuzzy-match t
     helm-apropos-fuzzy-match t
     helm-lisp-fuzzy-completion t
     ;; Autoresize settings - by setting max = 0, these settings are used to
     ;; control the helm window size.
     helm-autoresize-max-height 0
     helm-autoresize-min-height 35)

    ;; Enable autoresize to adjust helm window size.
    (helm-autoresize-mode 1)

    ;; Hide minibuffer --------------------------------------------------------

    (defun x-helm-hide-minibuffer-maybe ()
      "Hide minibuffer in Helm session if we use the header line
      as input field."
      (when (with-helm-buffer helm-echo-input-in-header-line)
        (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
          (overlay-put ov 'window (selected-window))
          (overlay-put ov 'face
                       (let ((bg-color (face-background 'default nil)))
                         `(:background ,bg-color :foreground ,bg-color)))
          (setq-local cursor-type nil))))

    (add-hook 'helm-minibuffer-set-up-hook 'x-helm-hide-minibuffer-maybe)

    ;; Key-bindings -----------------------------------------------------------

    ;; Rebind tab to run persistent action.
    (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
    ;; List actions using C-z.
    (define-key helm-map (kbd "C-z")  'helm-select-action)

    ;; ------------------------------------------------------------------------
    ;; Activate helm-descbinds.
    ;; ------------------------------------------------------------------------

    (helm-descbinds-mode)

    ;; ------------------------------------------------------------------------
    ;; Configure projectil.
    ;; ------------------------------------------------------------------------

    (projectile-global-mode)
    (setq projectile-completion-system 'helm)
    (helm-projectile-on)

    )

  (helm-mode 1)

  )
