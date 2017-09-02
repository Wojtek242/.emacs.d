;;; programming.el --- Module file for programming configuration.
;;
;; Copyright (C) 2017 Wojciech Kozlowski
;;
;; Author: Wojciech Kozlowski <wojciech.kozlowski@vivaldi.net>
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

(setq emodule/programming-packages

      '(company
        company-c-headers
        function-args
        flycheck
        flycheck-pos-tip
        highlight-numbers
        highlight-symbol
        racer
        rust-mode
        sr-speedbar
        stickyfunc-enhance
        yasnippet)

      )

;; Configuration:

(defun emodule/programming-init ()

  ;; --------------------------------------------------------------------------
  ;; Company - complete anything.
  ;; --------------------------------------------------------------------------

  (use-package company
    :init
    (add-hook 'after-init-hook 'global-company-mode)
    :config
    ;; For this to correctly complete headers, need to add all include paths to
    ;; `company-c-headers-path-system'.
    (add-to-list 'company-backends 'company-c-headers)
    (setq company-backends (delete 'company-clang company-backends)))

  ;; Functions args -----------------------------------------------------------

  (use-package function-args
    :init
    (use-package ivy)
    (fa-config-default)
    :config
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
  ;; Flycheck mode.
  ;; --------------------------------------------------------------------------

  (use-package flycheck
    :defer t
    :init
    (add-hook 'after-init-hook #'global-flycheck-mode)
    :config
    (require 'flycheck-pos-tip)
    (flycheck-pos-tip-mode))

  ;; --------------------------------------------------------------------------
  ;; Highlights.
  ;; --------------------------------------------------------------------------

  (use-package highlight-numbers
    :init
    (add-hook 'prog-mode-hook 'highlight-numbers-mode))

  (use-package highlight-symbol
    :init
    (highlight-symbol-nav-mode)
    (add-hook 'prog-mode-hook (lambda () (highlight-symbol-mode)))
    (add-hook 'org-mode-hook (lambda () (highlight-symbol-mode)))
    :bind
    (("M-n" . highlight-symbol-next)
     ("M-p" . highlight-symbol-prev))
    :config
    (setq highlight-symbol-idle-delay 0.2
          highlight-symbol-on-navigation-p t))


  ;; --------------------------------------------------------------------------
  ;; Configure Rust environment.
  ;; --------------------------------------------------------------------------

  (use-package rust-mode
    :defer t)

  ;; This requires some additional setup as the racer binary must be installed
  ;; and the Rust libstd sources must be installed.
  ;; $ rustup component add rust-src
  ;; $ cargo install racer
  (use-package racer
    :init
    (add-hook 'rust-mode-hook #'racer-mode)
    (add-hook 'racer-mode-hook #'eldoc-mode)
    :config
    (setq-default
     racer-rust-src-path "~/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src/")

    (require 'rust-mode)
    (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
    (setq company-tooltip-align-annotations t))

  ;; --------------------------------------------------------------------------
  ;; Speedbar.
  ;; --------------------------------------------------------------------------

  (use-package sr-speedbar
    :defer t
    :bind
    (("C-c s" . sr-speedbar-toggle))
    :config
    (setq-default
     sr-speedbar-skip-other-window-p t
     sr-speedbar-right-side nil
     speedbar-show-unknown-files t)

    (defun x-before-save-selected-window ()
      (cons (selected-window)
            ;; We save and restore all frames' selected windows, because
            ;; `select-window' can change the frame-selected-window of
            ;; whatever frame that window is in.  Each text terminal's
            ;; top-frame is preserved by putting it last in the list.
            (apply #'append
                   (mapcar (lambda (terminal)
                             (let ((frames (frames-on-display-list terminal))
                                   (top-frame (tty-top-frame terminal))
                                   alist)
                               (if top-frame
                                   (setq frames
                                         (cons top-frame
                                               (delq top-frame frames))))
                               (dolist (f frames)
                                 (push (cons f (frame-selected-window f))
                                       alist))
                               alist))
                           (terminal-list)))))

    (defun x-after-save-selected-window (state)
      (dolist (elt (cdr state))
        (and (frame-live-p (car elt))
             (window-live-p (cdr elt))
             (set-frame-selected-window (car elt) (cdr elt) 'norecord)))
      (when (window-live-p (car state))
        (select-window (car state) 'norecord)))

    (defun goto-speedbar ()
      "Change window to speedbar's window.
      This function assumes that the speedbar is either the left-
      or right-most window"
      (interactive)
      (let ((selected-window (x-before-save-selected-window)))
        (loop
         (condition-case nil
             (if sr-speedbar-right-side
                 (windmove-right)
               (windmove-left))
           (user-error (progn
                         (unless (string= major-mode "speedbar-mode")
                           (x-after-save-selected-window selected-window))
                         (return)))))))

    (global-set-key (kbd "M-m") #'goto-speedbar))

  ;; --------------------------------------------------------------------------
  ;; Enable yasnippet.
  ;; --------------------------------------------------------------------------

  (use-package yasnippet
    :init
    (yas-global-mode 1))

  ;; --------------------------------------------------------------------------
  ;; Configure CEDET.
  ;; --------------------------------------------------------------------------

  (use-package cc-mode
    :defer t)

  ;; To add include paths for semantic to parse, add them to
  ;; `semantic-add-system-include'.  This includes any local system includes,
  ;; such as those in `/usr/local/include'.
  (use-package semantic
    :init
    (global-semanticdb-minor-mode 1)
    (global-semantic-idle-scheduler-mode 1)
    (semantic-mode 1)
    :config
    (use-package stickyfunc-enhance)
    (add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
    )

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
    :init
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
    (("C-c c c" . compile)
     ("C-c c r" . recompile))
    :config
    (setq-default
     ;; Just save before compiling.
     compilation-ask-about-save nil
     ;; Just kill old compile processes before starting the new one.
     compilation-always-kill t
     ;; Automatically scroll to first error.
     compilation-scroll-output 'first-error))

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
  (add-hook 'prog-mode-hook '(lambda ()
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

  )
