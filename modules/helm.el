;;; helm.el --- Module file for Helm configuration.
;;
;; Copyright (C) 2017-2019 Wojciech Kozlowski
;;
;; Author: Wojciech Kozlowski <wk@wojciechkozlowski.eu>
;; Created: 2017-08-27
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This module sets up configuration for the helm package.
;;
;;; License: GPLv3

;;; Required packages:

;;; Code:

(defvar emodule/helm-packages

  '(ace-jump-helm-line
    helm
    helm-flyspell
    helm-lsp
    helm-projectile
    swiper-helm)

  )

;;; Configuration:

(defun emodule/helm-init ()
  "Initialise the `helm' module."

  ;; -----------------------------------------------------------------------------------------------
  ;; `ace-jump-helm-line'
  ;; -----------------------------------------------------------------------------------------------

  (use-package ace-jump-helm-line
    :after helm
    :bind
    (:map helm-map
          ("C-'" . ace-jump-helm-line)))

  ;; -----------------------------------------------------------------------------------------------
  ;; `helm'
  ;; -----------------------------------------------------------------------------------------------

  (use-package helm
    :init
    (require 'helm-config)
    :bind
    (("C-x C-f" . helm-find-files)
     ("C-x C-r" . helm-resume)
     ("M-x" . helm-M-x)
     ("M-y" . helm-show-kill-ring)
     ("C-x b" . helm-mini)
     ("C-x C-b" . helm-mini)
     ("C-M-j" . helm-semantic-or-imenu)
     ("C-h SPC" . helm-all-mark-rings))
    :config
    (helm-mode 1)

    ;; Helm prefix ---------------------------------------------------------------------------------

    ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs. Changed to "C-c h". Note:
    ;; We must set "C-c h" globally, because we cannot change `helm-command-prefix-key' once
    ;; `helm-config' is loaded.
    (global-set-key (kbd "C-c h") 'helm-command-prefix)
    (global-unset-key (kbd "C-x c"))

    ;; Helm settings -------------------------------------------------------------------------------

    (setq-default
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
     ;; While auresize is disabled these variable are in use.
     helm-display-buffer-default-height 0.35)

    ;; Hide minibuffer -----------------------------------------------------------------------------

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

    ;; Key-bindings --------------------------------------------------------------------------------

    ;; Rebind tab to run persistent action.
    (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
    (define-key helm-map (kbd "TAB") 'helm-execute-persistent-action)
    ;; List actions using C-z.
    (define-key helm-map (kbd "C-z") 'helm-select-action)

    ;; Change some Helm default key-bindings. Due to the `helm-config' require these have to
    ;; overridden here rather than with other keys in `:bind'.
    (global-set-key (kbd "C-c h x") 'helm-register)
    (global-set-key (kbd "C-c h M-o") 'helm-occur))

  ;; -----------------------------------------------------------------------------------------------
  ;; `helm-flyspell'
  ;; -----------------------------------------------------------------------------------------------

  (use-package helm-flyspell
    :bind (("C-c C-'" . helm-flyspell-correct)))

  ;; -----------------------------------------------------------------------------------------------
  ;; `helm-lsp'
  ;; -----------------------------------------------------------------------------------------------

  (use-package helm-lsp
    :after (helm lsp-mode))

  ;; ---------------------------------------------------------------------------------------------
  ;; `helm-projectile'
  ;; ---------------------------------------------------------------------------------------------

  (use-package helm-projectile
    :after projectile
    :config
    (setq-default projectile-completion-system 'helm)
    (helm-projectile-on))

  ;; -----------------------------------------------------------------------------------------------
  ;; `swiper-helm'
  ;; -----------------------------------------------------------------------------------------------

  (use-package swiper-helm
    :after helm
    :bind
    (("C-s" . swiper-helm)
     ("C-c C-s" . isearch-forward))
    :config
    (setq swiper-helm-display-function 'helm-default-display-buffer))

  )

(provide 'emodule/helm)
;;; helm.el ends here
