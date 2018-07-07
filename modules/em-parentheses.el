;;; em-parentheses.el --- Module file for managing parentheses packages.
;;
;; Copyright (C) 2017 Wojciech Kozlowski
;;
;; Author: Wojciech Kozlowski <wk@wojciechkozlowski.eu>
;; Created: 28 Aug 2017
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This module sets up configuration for packages that manage parentheses in
;; code and text.
;;
;;; License: GPLv3

;;; Required packages:

;;; Code:

(defvar emodule/em-parentheses-packages

  '(highlight-parentheses
    rainbow-delimiters
    smartparens)

  )

;; Configuration:

(defun emodule/em-parentheses-init ()
  "Initialise the `em-parentheses' module."

  ;; --------------------------------------------------------------------------
  ;; Highlight parentheses - this package does not use faces for colours,
  ;; instead it uses the `hl-parens-colors' variable.  This can be set in the
  ;; theme file, but the mode has to be reloaded whenever the theme changes.
  ;; --------------------------------------------------------------------------

  (use-package highlight-parentheses
    :defer t
    :init
    (add-hook 'prog-mode-hook 'highlight-parentheses-mode))

  ;; --------------------------------------------------------------------------
  ;; Rainbow delimiters - colours are set by theme.
  ;; --------------------------------------------------------------------------

  (use-package rainbow-delimiters
    :defer t
    :init
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

  ;; --------------------------------------------------------------------------
  ;; Smartparens highlighting.
  ;; --------------------------------------------------------------------------

  (use-package smartparens
    :init
    (smartparens-global-mode t)
    (show-smartparens-global-mode t)
    :config
    (require 'smartparens-config)
    (declare-function sp-local-pair "smartparens")
    (declare-function sp-beginning-of-sexp "smartparens")

    ;; Key-bindings -----------------------------------------------------------

    (define-key smartparens-mode-map (kbd "C-M-f") 'sp-forward-sexp)
    (define-key smartparens-mode-map (kbd "C-M-b") 'sp-backward-sexp)

    (define-key smartparens-mode-map (kbd "C-M-u") 'sp-backward-up-sexp)
    (define-key smartparens-mode-map (kbd "C-M-S-u") 'sp-up-sexp)

    (define-key smartparens-mode-map (kbd "C-M-d") 'sp-down-sexp)
    (define-key smartparens-mode-map (kbd "C-M-S-d") 'sp-backward-down-sexp)

    (define-key smartparens-mode-map (kbd "C-S-a") 'sp-beginning-of-sexp)
    (define-key smartparens-mode-map (kbd "C-S-e") 'sp-end-of-sexp)

    (define-key smartparens-mode-map (kbd "C-M-t") 'sp-transpose-sexp)

    (define-key smartparens-mode-map (kbd "C-M-n") 'sp-next-sexp)
    (define-key smartparens-mode-map (kbd "C-M-p") 'sp-previous-sexp)

    (define-key smartparens-mode-map (kbd "C-M-k") 'sp-kill-sexp)
    (define-key smartparens-mode-map (kbd "C-M-w") 'sp-copy-sexp)

    (define-key smartparens-mode-map (kbd "C-<right>") 'sp-forward-slurp-sexp)
    (define-key smartparens-mode-map (kbd "C-<left>") 'sp-forward-barf-sexp)

    (define-key smartparens-mode-map (kbd "M-D") 'sp-splice-sexp)
    (define-key smartparens-mode-map (kbd "C-S-d") 'sp-splice-sexp-killing-forward)
    (define-key smartparens-mode-map (kbd "C-M-<backspace>") 'sp-splice-sexp-killing-backward)
    (define-key smartparens-mode-map (kbd "C-S-<backspace>") 'sp-splice-sexp-killing-around)

    (define-key smartparens-mode-map (kbd "C-]") 'sp-select-next-thing-exchange)
    (define-key smartparens-mode-map (kbd "C-M-]") 'sp-select-next-thing)

    (define-key smartparens-mode-map (kbd "M-F") 'sp-forward-symbol)
    (define-key smartparens-mode-map (kbd "M-B") 'sp-backward-symbol)

    (define-key smartparens-mode-map (kbd "C-c f")
      (lambda () (interactive) (sp-beginning-of-sexp 2)))
    (define-key smartparens-mode-map (kbd "C-c b")
      (lambda () (interactive) (sp-beginning-of-sexp -2)))

    ;; rst-mode
    (sp-with-modes 'rst-mode
      (sp-local-pair "`" nil :actions nil)
      (sp-local-pair "``" "``"))

    ;; Smartparens custom settings --------------------------------------------

    (setq-default
     ;; Jump to closing parenthesis when closing symbol is typed.
     sp-autoskip-closing-pair t
     ;; Do not automatically reindent anything.
     sp-navigate-reindent-after-up nil
     sp-navigate-reindent-after-up-in-string nil
     ;; Do not highlight space between parentheses.
     sp-highlight-pair-overlay nil))

  )

(provide 'em-parentheses)
;;; em-parentheses.el ends here
