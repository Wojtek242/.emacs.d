;;; em-programming.el --- Module file for programming configuration.
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
;; This module sets up packages and configuration for editing source code in
;; all languages.
;;
;;; License: GPLv3

;;; Required packages:

;;; Code:

(defvar emodule/em-programming-packages

  '(company
    company-c-headers
    dockerfile-mode
    elpy
    fic-mode
    function-args
    flycheck
    flycheck-plantuml
    flycheck-pos-tip
    flycheck-rust
    highlight-numbers
    highlight-symbol
    plantuml-mode
    py-autopep8
    rust-mode
    stickyfunc-enhance
    swiper
    toml-mode
    vala-mode
    yaml-mode
    yasnippet
    yasnippet-snippets

    s
    f)

  )

;; Configuration:

(defun emodule/em-programming-init ()
  "Initialise the `em-programming' module."

  ;; --------------------------------------------------------------------------
  ;; Company - complete anything.
  ;; --------------------------------------------------------------------------

  (use-package company
    :hook
    (after-init . global-company-mode)
    :config
    ;; For this to correctly complete headers, need to add all include paths to
    ;; `company-c-headers-path-system'.
    (add-to-list 'company-backends 'company-c-headers)
    (setq company-backends (delete 'company-clang company-backends)))

  ;; Functions args -----------------------------------------------------------

  (use-package function-args
    :config
    (use-package ivy)
    (fa-config-default)

    (defun set-other-window-key ()
      ;; function-args overrides the custom "M-o" binding, this undoes it
      (define-key function-args-mode-map (kbd "M-o") nil)
      (define-key function-args-mode-map (kbd "M-O") 'moo-complete))

    (defun set-moo-jump-directory-key ()
      ;; function-args overrides the default "C-M-k" binding, this undoes it
      (define-key function-args-mode-map (kbd "C-M-k") nil)
      (define-key function-args-mode-map (kbd "C-M-;") 'moo-jump-directory))

    (defun set-fa-idx-cycle-keys ()
      ;; function-args overrides the default "M-h" and "M-p" bindings, this
      ;; undoes it
      (define-key function-args-mode-map (kbd "M-h") nil)
      (define-key function-args-mode-map (kbd "M-[") 'fa-idx-cycle-up)
      (define-key function-args-mode-map (kbd "M-n") nil)
      (define-key function-args-mode-map (kbd "M-]") 'fa-idx-cycle-down))

    (defun set-fa-abort-key ()
      ;; function-args overrides the default "C-M-k" binding, this undoes it
      (define-key function-args-mode-map (kbd "M-u") nil)
      (define-key function-args-mode-map (kbd "M-k") 'fa-abort))

    (defun set-function-args-keys ()
      ;; Collects all the function-args key overrides
      (set-other-window-key)
      (set-moo-jump-directory-key)
      (set-fa-idx-cycle-keys)
      (set-fa-abort-key))

    (add-hook 'function-args-mode-hook #'set-function-args-keys))

  ;; --------------------------------------------------------------------------
  ;; Configure dockerfile environment.
  ;; --------------------------------------------------------------------------

  (use-package dockerfile-mode
    :defer t)

  ;; --------------------------------------------------------------------------
  ;; Enable elpy.
  ;; --------------------------------------------------------------------------

  (use-package elpy
    :hook (python-mode . elpy-mode)
    :config
    (unbind-key "C-c C-f" python-mode-map)
    (unbind-key "C-c C-f" elpy-mode-map)
    (setq python-shell-interpreter "ipython"
          python-shell-interpreter-args "-i --simple-prompt"))

  (use-package py-autopep8
    ;; Note that this package require autopep8 to be installed.
    :bind (("C-c C-f" . py-autopep8-buffer)))

  ;; --------------------------------------------------------------------------
  ;; FIC mode.
  ;; --------------------------------------------------------------------------
  (use-package fic-mode
    :hook
    (prog-mode . fic-mode))

  ;; --------------------------------------------------------------------------
  ;; Flycheck mode.
  ;; --------------------------------------------------------------------------

  (use-package flycheck
    :hook
    (after-init . global-flycheck-mode))

  (use-package flycheck-pos-tip
    :after flycheck
    :config
    (flycheck-pos-tip-mode)
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

  ;; --------------------------------------------------------------------------
  ;; Highlights.
  ;; --------------------------------------------------------------------------

  (use-package highlight-numbers
    :hook
    (prog-mode . highlight-numbers-mode))

  (use-package highlight-symbol
    :hook
    ((prog-mode org-mode) . highlight-symbol-mode)
    :bind
    (("M-n" . highlight-symbol-next)
     ("M-p" . highlight-symbol-prev))
    :config
    (highlight-symbol-nav-mode)
    (setq highlight-symbol-idle-delay 0.2
          highlight-symbol-on-navigation-p t))


  ;; --------------------------------------------------------------------------
  ;; Configure Rust environment.
  ;; --------------------------------------------------------------------------

  (defun rust-new-project (project-name project-type)
    (let ((rust-cargo-bin "cargo"))
      (unless (executable-find rust-cargo-bin)
        (error "Could not locate executable \"%s\"" rust-cargo-bin))

      (let* ((tmpf (make-temp-file "*cargo-new*"))
             (err-msg "")
             (ret (call-process
                   rust-cargo-bin
                   nil tmpf t
                   "new" project-name (concat "--" project-type))))

        (with-current-buffer (get-buffer-create tmpf)
          (setq err-msg (buffer-string))
          (kill-current-buffer))

        (unless (= ret 0)
          (error err-msg)))))

  (defun rust-new-project-bin (project-name)
    (interactive "sBinary project name: ")
    (rust-new-project project-name "bin"))

  (defun rust-new-project-lib (project-name)
    (interactive "sLibrary project name: ")
    (rust-new-project project-name "lib"))

  (use-package rust-mode
    :defer t)

  ;; This requires some additional setup as the racer binary must be installed
  ;; and the Rust libstd sources must be installed.
  ;; $ rustup component add rust-src
  ;; $ cargo install racer
  (add-to-list 'load-path "~/.emacs.d/emacs-deferred")
  (add-to-list 'load-path "~/.emacs.d/emacs-racer")
  (use-package racer
    :init
    (add-hook 'rust-mode-hook #'racer-mode)
    (add-hook 'racer-mode-hook #'eldoc-mode)
    :config
    ;; For racer to work, it needs to know where to find the standard library
    ;; sources.  The easiest way to do it without having a machine dependent
    ;; setup is to set the environment variable RUST_SRC_PATH.  If that's
    ;; undesirable or not possible, set the variable below instead.
    ;; (setq-default
    ;;  racer-rust-src-path
    ;;  "~/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src/")
    (setq-default racer-use-company-backend t)
    (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
    (setq-default company-tooltip-align-annotations t))

  (use-package toml-mode
    :mode "\\.lock\\'")

  ;; --------------------------------------------------------------------------
  ;; Configure `swiper'.
  ;; --------------------------------------------------------------------------

  (use-package swiper
    :bind
    (("M-s M-s" . swiper))
    :config
    (setq ivy-count-format "%d/%d "))

  ;; --------------------------------------------------------------------------
  ;; plantuml-mode
  ;; --------------------------------------------------------------------------

  (use-package plantuml-mode
    :mode "\\.pu\\'"
    :init
    (setq-default plantuml-jar-path "~/.emacs.d/plantuml.jar")
    :config
    (require 'flycheck-plantuml))

  ;; --------------------------------------------------------------------------
  ;; Configure Vala environment.
  ;; --------------------------------------------------------------------------

  (use-package vala-mode
    :defer t
    :config
    (add-to-list 'file-coding-system-alist '("\\.vala$" . utf-8))
    (add-to-list 'file-coding-system-alist '("\\.vapi$" . utf-8)))

  ;; --------------------------------------------------------------------------
  ;; Configure HDL environment.
  ;; --------------------------------------------------------------------------

  (use-package vhdl-mode
    :mode "\\.hdl\\'")

  ;; --------------------------------------------------------------------------
  ;; Configure yaml environment.
  ;; --------------------------------------------------------------------------

  (use-package yaml-mode
    :config
    (add-hook 'yaml-mode-hook #'linum-mode))

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

  ;; --------------------------------------------------------------------------
  ;; Configure CEDET.
  ;; --------------------------------------------------------------------------

  (use-package cc-mode
    :defer t)

  ;; To add include paths for semantic to parse, add them to
  ;; `semantic-add-system-include'.  This includes any local system includes,
  ;; such as those in `/usr/local/include'.
  (use-package semantic
    :config
    (global-semanticdb-minor-mode 1)
    (global-semantic-idle-scheduler-mode 1)
    (semantic-mode 1)

    (use-package stickyfunc-enhance)
    (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode))

  ;; For this to work, need to specify project roots in the variable
  ;; `ede-cpp-root-project', e.g.
  ;; (ede-cpp-root-project "project_root"
  ;;                       :file "/dir/to/project_root/Makefile"
  ;;                       :include-path '("/include1"
  ;;                                       "/include2") ;; add more include
  ;;                       ;; paths here
  ;;                       :system-include-path '("~/linux"))
  ;; May need to run `semantic-force-refresh' afterwards.
  (use-package ede
    :config
    (global-ede-mode))

  (add-hook 'c-mode-common-hook 'hs-minor-mode)

  ;; --------------------------------------------------------------------------
  ;; Debugging options.
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
  ;; Setup compilation-mode used by `compile' command
  ;; --------------------------------------------------------------------------

  (use-package compile
    :bind
    (("C-c c" . compile)
     ("C-c r" . recompile))
    :config
    (setq-default
     ;; Just save before compiling.
     compilation-ask-about-save nil
     ;; Just kill old compile processes before starting the new one.
     compilation-always-kill t
     ;; Automatically scroll to first error.
     compilation-scroll-output 'first-error)

    ;; ansi-colors
    (ignore-errors
      (require 'ansi-color)
      (defun my-colorize-compilation-buffer ()
        (when (eq major-mode 'compilation-mode)
          (ansi-color-apply-on-region compilation-filter-start (point-max))))
      (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer)))

  ;; --------------------------------------------------------------------------
  ;; Makefile settings.
  ;; --------------------------------------------------------------------------

  (defun makefile-mode-tabs ()
    (whitespace-toggle-options '(tabs))
    (setq indent-tabs-mode t))

  (add-hook 'makefile-mode-hook 'makefile-mode-tabs)

  ;; --------------------------------------------------------------------------
  ;; Line numbers.
  ;;
  ;; Ideally, we could just use linum-format "%4d \u2502".  However, the
  ;; unicode character for the vertical line causes the screen to flicker on
  ;; some screens when typing or moving the cursor. Using `nlinum' does not
  ;; solve the problem.  A compromise is to instead use a whitespace character
  ;; of a different colour.
  ;;
  ;; Furthermore, since `linum' can struggle with large buffers, it is disabled
  ;; once the number of lines cannot fit into linum-format anymore.  `nlinum'
  ;; is meant to solve the problem, but it updates line numbers after a visible
  ;; pause if a line is inderted/deleted.
  ;; --------------------------------------------------------------------------

  (defun linum-format-func (line)
    (concat
     (propertize (format "%4d " line) 'face 'linum)
     (propertize " " 'face 'mode-line-inactive)))

  (setq-default linum-format 'linum-format-func)
  (add-hook 'prog-mode-hook (lambda ()
                              (unless (> (count-lines (point-min) (point-max))
                                         9999)
                                (linum-mode))))

  ;; --------------------------------------------------------------------------
  ;; Formatting settings.
  ;; --------------------------------------------------------------------------

  (setq-default c-default-style "linux")

  ;; --------------------------------------------------------------------------
  ;; Trailing whitespace.
  ;; --------------------------------------------------------------------------

  ;; The following setting of `show-trailing-whitespace' is incompatible with
  ;; `fci-mode'.  The only known workaround is to have whitespace mode on with
  ;; whitespace-style set such that only trailing whitespace is shown.

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

  )

(provide 'em-programming)
;;; em-programming.el ends here
