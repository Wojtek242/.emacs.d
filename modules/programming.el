;;; programming.el --- Module file for programming configuration.
;;
;; Copyright (C) 2017-2019 Wojciech Kozlowski
;;
;; Author: Wojciech Kozlowski <wk@wojciechkozlowski.eu>
;; Created: 2017-08-28
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This module sets up packages and configuration for editing source code in
;; any language.  Language specific configuration is in `languages.el'.
;;
;;; License: GPLv3

;;; Required packages:

;;; Code:

(defvar emodule/programming-packages

  '(company
    company-c-headers
    company-lsp
    fic-mode
    flycheck
    flycheck-pos-tip
    highlight-numbers
    highlight-symbol
    lsp-mode
    lsp-ui
    swiper
    yasnippet
    yasnippet-snippets)

  )

;; Configuration:

(defun emodule/programming-init ()
  "Initialise the `programming' module."

  ;; --------------------------------------------------------------------------
  ;; Line numbers.
  ;; --------------------------------------------------------------------------

  (setq-default
   display-line-numbers-width-start 4
   display-line-numbers-grow-only t)

  (add-hook 'prog-mode-hook 'display-line-numbers-mode)

  ;; --------------------------------------------------------------------------
  ;; Fill-column indicator.
  ;; --------------------------------------------------------------------------

  (add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)

  ;; --------------------------------------------------------------------------
  ;; Trailing whitespace.
  ;; --------------------------------------------------------------------------

  (add-hook 'prog-mode-hook (lambda ()
                              (interactive)
                              (setq show-trailing-whitespace t)))

  ;; --------------------------------------------------------------------------
  ;; Automatically indent yanked text in programming mode.
  ;; --------------------------------------------------------------------------

  (defvar yank-indent-modes
    '(LaTeX-mode TeX-mode)
    "Modes in which to indent regions that are yanked (or yank-popped).
    Only modes that don't derive from `prog-mode' should be
    listed here.")

  (defvar yank-indent-blacklisted-modes
    '(python-mode slim-mode haml-mode)
    "Modes for which auto-indenting is suppressed.")

  (defvar yank-advised-indent-threshold 10000
    "Threshold (# chars) over which indentation does not
    automatically occur.")

  (defun yank-advised-indent-function (beg end)
    "Do indentation, as long as the region isn't too large."
    (if (<= (- end beg) yank-advised-indent-threshold)
        (indent-region beg end nil)))

  (defadvice yank (after yank-indent activate)
    "If current mode is one of 'yank-indent-modes,
    indent yanked text (with prefix arg don't indent)."
    (if (and (not (ad-get-arg 0))
             (not (member major-mode yank-indent-blacklisted-modes))
             (or (derived-mode-p 'prog-mode)
                 (member major-mode yank-indent-modes)))
        (let ((transient-mark-mode nil))
          (yank-advised-indent-function (region-beginning) (region-end)))))

  (defadvice yank-pop (after yank-pop-indent activate)
    "If current mode is one of `yank-indent-modes',
    indent yanked text (with prefix arg don't indent)."
    (when (and (not (ad-get-arg 0))
               (not (member major-mode yank-indent-blacklisted-modes))
               (or (derived-mode-p 'prog-mode)
                   (member major-mode yank-indent-modes)))
      (let ((transient-mark-mode nil))
        (yank-advised-indent-function (region-beginning) (region-end)))))

  ;; --------------------------------------------------------------------------
  ;; Box comments.
  ;; --------------------------------------------------------------------------

  (defvar box-comment-char/emacs-lisp-mode ";; ")
  (defvar box-comment-char/lisp-interaction-mode ";; ")
  (defvar box-comment-char/scheme-mode ";; ")

  (defun box-comment-char ()
    "Return the comment character for the current mode."
    (let ((box-comment-var
           (intern (format "box-comment-char/%s" major-mode))))
    (if (boundp box-comment-var)
        (eval box-comment-var)
      comment-start)))

  (defun make-box-comment ()
    (interactive)
    (let ((comm-start (box-comment-char))
          beg indent len)

      ;; ----------------------------------------------------------------------
      ;; Find beginning of comment.
      ;; ----------------------------------------------------------------------
      (end-of-line)
      (unless (search-backward comm-start nil t)
        (error "Not in comment!"))

      ;; ----------------------------------------------------------------------
      ;; Reformat into a single line.
      ;; ----------------------------------------------------------------------
      (unfill-paragraph)
      (end-of-line)
      (search-backward comm-start nil t)

      ;; ----------------------------------------------------------------------
      ;; Set variables.
      ;; ----------------------------------------------------------------------
      (setq beg (point))
      (setq indent (current-column))
      (setq len (- (- fill-column (length comm-start)) indent))

      ;; ----------------------------------------------------------------------
      ;; Reformat comment text in place.
      ;; ----------------------------------------------------------------------
      (goto-char beg)
      (insert comm-start (make-string len ?-))
      (newline)
      (indent-to-column indent)
      (end-of-line)
      (fill-paragraph)
      (unless (bolp)
        (progn
          (newline)
          (indent-to-column indent)))
      (insert comm-start (make-string len ?-))))

  (global-set-key (kbd "M-'") 'make-box-comment)

  ;; --------------------------------------------------------------------------
  ;; `company' - complete anything.
  ;; --------------------------------------------------------------------------

  (use-package company
    :hook
    (after-init . global-company-mode)
    :bind
    (:map company-active-map
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous))
    :config
    (setq company-idle-delay 0
          company-minimum-prefix-length 3
          company-tooltip-align-annotations t)
    ;; For this to correctly complete headers, need to add all include paths to
    ;; `company-c-headers-path-system'.
    (add-to-list 'company-backends 'company-c-headers)
    (setq company-backends (delete 'company-clang company-backends))
    (setq company-backends (delete 'company-dabbrev company-backends))
    (setq company-backends (delete 'company-capf company-backends)))

  (use-package company-lsp
    :commands company-lsp
    :config
    (setq company-lsp-cache-candidates 'auto))

  ;; --------------------------------------------------------------------------
  ;; `compile'
  ;; --------------------------------------------------------------------------

  (use-package compile
    :init
    (defun compilation-exit-autoclose (status code msg)
      "Close *compilation* buffer if compilation exits successfully."
      (when (and (eq status 'exit) (zerop code))
        ;; Timer is necessary otherwise message is printed into another buffer
        (run-with-timer 0.5 nil (lambda () (kill-buffer "*compilation*"))))
      (cons msg code))
    :bind
    (("C-x C-." . compile)
     ("C-x C-," . recompile))
    :config
    (setq-default
     ;; Default compile commande
     compile-command "make "
     ;; Just save before compiling.
     compilation-ask-about-save nil
     ;; Just kill old compile processes before starting the new one.
     compilation-always-kill t
     ;; Automatically scroll to first error.
     compilation-scroll-output 'first-error)

    (defun toggle-compilation-exit-autoclose ()
      "Toggle autoclose on successful compilation."
      (interactive)
      (if compilation-exit-message-function
          (setq-default compilation-exit-message-function
                        nil)
        (setq-default compilation-exit-message-function
                      'compilation-exit-autoclose)))

    ;; ansi-colors
    (ignore-errors
      (require 'ansi-color)
      (defun my-colorize-compilation-buffer ()
        (when (eq major-mode 'compilation-mode)
          (ansi-color-apply-on-region compilation-filter-start (point-max))))
      (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer)))

  ;; --------------------------------------------------------------------------
  ;; `gud' - GDB options.
  ;; --------------------------------------------------------------------------

  (use-package gud
    :defer t
    :config
    (setq gud-chdir-before-run nil))

  (setq-default
   ;; Use gdb-many-windows by default.
   gdb-many-windows t
   ;; Display source file containing main.
   gdb-show-main t)

  ;; --------------------------------------------------------------------------
  ;; `fic-mode' - highlight to-do keywords.
  ;; --------------------------------------------------------------------------
  (use-package fic-mode
    :hook
    (prog-mode . fic-mode))

  ;; --------------------------------------------------------------------------
  ;; `flycheck'
  ;; --------------------------------------------------------------------------

  (use-package flycheck
    :hook (after-init . global-flycheck-mode))

  (use-package flycheck-pos-tip
    :after flycheck
    :config (flycheck-pos-tip-mode))

  ;; --------------------------------------------------------------------------
  ;; `highlight-numbers'
  ;; --------------------------------------------------------------------------

  (use-package highlight-numbers
    :hook (prog-mode . highlight-numbers-mode))

  ;; --------------------------------------------------------------------------
  ;; `highlight-symbol'
  ;; --------------------------------------------------------------------------

  (use-package highlight-symbol
    :hook
    (prog-mode . highlight-symbol-mode)
    :bind
    (("M-n" . highlight-symbol-next)
     ("M-p" . highlight-symbol-prev))
    :config
    (highlight-symbol-nav-mode)
    (setq highlight-symbol-idle-delay 0.2
          highlight-symbol-on-navigation-p t))

  ;; --------------------------------------------------------------------------
  ;; `lsp-mode'
  ;; --------------------------------------------------------------------------

  (use-package lsp-mode
    :commands lsp
    :init
    (setq lsp-diagnostics-provider :flycheck
          lsp-signature-auto-activate t
          lsp-signature-doc-lines 1
          lsp-enable-indentation nil
          lsp-file-watch-threshold 10000))

  (use-package lsp-ui
    :commands lsp-ui-mode
    :init
    (setq lsp-ui-doc-enable nil
          lsp-ui-sideline-enable nil)
    :config
    (define-key lsp-ui-mode-map
      [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
    (define-key lsp-ui-mode-map
      [remap xref-find-references] #'lsp-ui-peek-find-references))

  ;; --------------------------------------------------------------------------
  ;; `semantic'
  ;; --------------------------------------------------------------------------

  (use-package semantic
    :hook (prog-mode . semantic-mode))

  ;; --------------------------------------------------------------------------
  ;; Enable yasnippet.
  ;; --------------------------------------------------------------------------

  (use-package yasnippet
    :config
    (yas-global-mode 1)

    (define-key yas-minor-mode-map [(tab)]        nil)
    (define-key yas-minor-mode-map (kbd "TAB")    nil)
    (define-key yas-minor-mode-map (kbd "<tab>")  nil)
    (define-key yas-minor-mode-map (kbd "<C-return>")  'yas-expand))

  )

(provide 'emodule/programming)
;;; programming.el ends here
