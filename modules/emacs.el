;;; emacs.el --- Module file for configuring Emacs itself.
;;
;; Copyright (C) 2017-2019 Wojciech Kozlowski
;;
;; Author: Wojciech Kozlowski <wk@wojciechkozlowski.eu>
;; Created: 2017-08-25
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This module is used for generic Emacs configuration.
;;
;;; License: GPLv3

;;; Required packages:

;;; Code:

(defvar emodule/emacs-packages

  '(deadgrep
    discover-my-major
    duplicate-thing
    expand-region
    highlight-parentheses
    ibuffer-vc
    pdf-tools
    perspective
    projectile
    rainbow-delimiters
    rainbow-mode
    recentf-ext
    smartparens
    swiper
    treemacs-icons-dired
    undo-tree
    use-package
    vlf
    volatile-highlights
    which-key
    whole-line-or-region
    ws-butler)

  )

;;; Configuration:

(defun emodule/emacs-init ()
  "Initialise the `emacs' module."

  ;; --------------------------------------------------------------------------
  ;; Basic editor settings.
  ;; --------------------------------------------------------------------------

  (setq-default
   ;; This slows down cursor scrolling.
   auto-window-vscroll nil
   ;; Standard fill-column width - last character is for end of line glyph.
   fill-column 79
   ;; Do not use tab characters for indentation.
   indent-tabs-mode nil
   ;; Kill whole line when point at beginning of line.
   kill-whole-line t
   ;; Large file warning only above 10 MB.
   large-file-warning-threshold 10485760
   ;; Keep point in same position on the screen when scrolling.
   scroll-preserve-screen-position 1
   ;; Indentation size - applies even when indent-tabs-mode is nil.
   tab-width 8
   ;; Highlight lines that are too long in whitespace mode.
   whitespace-line-column fill-column)

  ;; Backup settings.
  (defvar backup-directory "~/.emacs.d/.backups")
  (if (not (file-exists-p backup-directory))
      (make-directory backup-directory t))

  (setq-default
   ;; Backup a file the first time it is saved.
   make-backup-files t
   ;; Save backup files in ~/.emacs.d/.backups.
   backup-directory-alist `((".*" . ,backup-directory))
   ;; Copy the current file into backup directory.
   backup-by-copying t
   ;; Version numbers for backup files.
   version-control t
   ;; Delete unnecessary versions.
   delete-old-versions t
   ;; Oldest versions to keep when a new numbered backup is made.
   kept-old-versions 2
   ;; Newest versions to keep when a new numbered backup is made.
   kept-new-versions 3
   ;; Auto-save every buffer that visits a file.
   auto-save-default t
   ;; Number of seconds idle time before auto-save.
   auto-save-timeout 30
   ;; Number of keystrokes between auto-saves.
   auto-save-interval 300)

  ;; Update buffers when files change.
  (global-auto-revert-mode)
  
  ;; Replace selected rather than inserting text at point.
  (delete-selection-mode)

  ;; Treat CamelCase as separate words.
  (global-subword-mode)

  ;; Recentf.
  (recentf-mode 1)

  ;; Use UTF-8.
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-language-environment "UTF-8")
  (prefer-coding-system 'utf-8)

  ;; Address mode.
  (add-hook 'prog-mode-hook 'goto-address-mode)
  (add-hook 'text-mode-hook 'goto-address-mode)

  ;; y or n is enough.
  (defalias 'yes-or-no-p 'y-or-n-p)

  ;; Always use ibuffer.
  (defalias 'list-buffers 'ibuffer)

  ;; Increase recursion limits.
  (setq-default max-specpdl-size 20000) ;; ~15x original value
  (setq-default max-lisp-eval-depth 24000) ;; 30x orignal value

  ;; Add directories to exec-path.
  (setq exec-path (append exec-path '("/home/wojtek/.local/bin"
                                      "/home/wojtek/.cask/bin")))

  ;; --------------------------------------------------------------------------
  ;; Configure garbage collection.
  ;;
  ;; Based on advice from:
  ;; http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
  ;; --------------------------------------------------------------------------

  (add-hook 'minibuffer-setup-hook
	    (lambda () (setq gc-cons-threshold most-positive-fixnum)))

  (add-hook 'minibuffer-exit-hook
	    (lambda () (setq gc-cons-threshold 800000)))

  ;; --------------------------------------------------------------------------
  ;; Additional key-bindings.
  ;; --------------------------------------------------------------------------

  ;; Toggle whitespace mode.
  (global-set-key (kbd "C-c w") 'whitespace-mode)

  ;; Occur. More convenient than "M-s o".
  (global-set-key (kbd "M-s M-o") 'occur)

  ;; Kill other window (cyclic order).
  (global-set-key (kbd "C-z")
		  (lambda ()
		    (interactive)
		    (quit-window t (next-window (selected-window)))))

  ;; Kill current buffer without prompting.
  (global-set-key (kbd "C-x k")
		  (lambda ()
		    (interactive)
		    (let (kill-buffer-query-functions) (kill-buffer))))

  ;; Change active window.  More convenient than "C-x o".
  (global-set-key (kbd "M-o") 'other-window)
  (global-set-key (kbd "M-O") (lambda () (interactive) (other-window -1)))

  ;; Scroll up/down, but keep point in place.
  (global-set-key (kbd "C-<") (lambda()
                                (interactive)
                                (let ((scroll-preserve-screen-position nil))
                                  (scroll-down 1))))
  (global-set-key (kbd "C->") (lambda()
                                (interactive)
                                (let ((scroll-preserve-screen-position nil))
                                  (scroll-up 1))))

  ;; --------------------------------------------------------------------------
  ;; Dark/light theme switch.
  ;; --------------------------------------------------------------------------

  (defun refresh-non-face-colours ()
    "Restart modes that use colours not set with face variables.
    This has to be called whenever the active theme changes to
    refresh these colours."

    (when (and (fboundp 'highlight-parentheses-mode)
               highlight-parentheses-mode)
      (highlight-parentheses-mode 1)))

  ;; Key-bindings -------------------------------------------------------------

  (global-set-key (kbd "C-x t l") (lambda ()
                                    (interactive)
                                    (load-theme 'havoc-light t)
                                    (refresh-non-face-colours)))
  (global-set-key (kbd "C-x t d") (lambda ()
                                    (interactive)
                                    (load-theme 'havoc-dark t)
                                    (refresh-non-face-colours)))

  ;; --------------------------------------------------------------------------
  ;; Toggle indent-tabs-mode.  Useful for working with source code
  ;; that have a different tab policy.
  ;; --------------------------------------------------------------------------

  (defun toggle-indent-tabs-mode ()
    "Toggle a indent-tabs-mode between a defined and undefined state."
    (interactive)
    (setq indent-tabs-mode (not indent-tabs-mode))
    (setq-default indent-tabs-mode indent-tabs-mode))

  ;; --------------------------------------------------------------------------
  ;; Beginning of line.
  ;; --------------------------------------------------------------------------

  (defun x-move-beginning-of-line (arg)
    "Move point back to indentation of beginning of line.

    Move point to the first non-whitespace character on this
    line.  If point is already there, move to the beginning of
    the line.  Effectively toggle between the first
    non-whitespace character and the beginning of the line.

    If ARG is not nil or 1, move forward ARG - 1 lines first. If
    point reaches the beginning or end of the buffer, stop
    there."

    (interactive "^p")
    (setq arg (or arg 1))

    ;; Move lines first
    (when (/= arg 1)
      (let ((line-move-visual nil))
        (forward-line (1- arg))))

    (let ((orig-point (point)))
      (back-to-indentation)
      (when (= orig-point (point))
        (move-beginning-of-line 1))))

  ;; Key-bindings -------------------------------------------------------------

  ;; Override the beginning of line key-binding.
  (global-set-key (kbd "C-a") 'x-move-beginning-of-line)

  ;; --------------------------------------------------------------------------
  ;; Transpose lines.
  ;; --------------------------------------------------------------------------

  (defun transpose-lines-down ()
    "Transpose the current line with the one below."
    (interactive)
    (next-line)
    (transpose-lines 1)
    (previous-line))

  (defun transpose-lines-up ()
    "Transpose the current line with the one above."
    (interactive)
    (transpose-lines 1)
    (previous-line 2))

  ;; Key-bindings -------------------------------------------------------------

  (global-set-key (kbd "M-<down>") 'transpose-lines-down)
  (global-set-key (kbd "M-<up>") 'transpose-lines-up)

  ;; --------------------------------------------------------------------------
  ;; Indent buffer.
  ;; --------------------------------------------------------------------------

  (defun indent-buffer ()
    "Indent the currently visited buffer."
    (interactive)
    (indent-region (point-min) (point-max)))

  (defcustom indent-sensitive-modes
    '(coffee-mode python-mode slim-mode haml-mode yaml-mode)
    "Modes for which auto-indenting is suppressed."
    :type 'list
    :group 'emodule/emacs)

  (defun indent-region-or-buffer ()
    "Indent a region if selected, otherwise the whole buffer."
    (interactive)

    (unless (member major-mode indent-sensitive-modes)
      (save-excursion
        (if (region-active-p)
            (indent-region (region-beginning) (region-end))
          (indent-buffer)
          (whitespace-cleanup)))))

  ;; Key-bindings -------------------------------------------------------------

  ;; Override the indent-region key-binding
  (global-set-key (kbd "C-M-\\") 'indent-region-or-buffer)
  
  ;; --------------------------------------------------------------------------
  ;; Unfill paragraph - inverse of fill paragraph.
  ;; --------------------------------------------------------------------------

  (defun unfill-paragraph (&optional region)
    "Takes a multi-line paragraph and makes it into a single line of text."
    (interactive (progn (barf-if-buffer-read-only) '(t)))
    (let ((fill-column (point-max))
          ;; This would override `fill-column' if it's an integer.
          (emacs-lisp-docstring-fill-column t))
      (fill-paragraph nil region)))

  ;; Key-bindings -------------------------------------------------------------

  (global-set-key (kbd "M-Q") 'unfill-paragraph)

  ;; --------------------------------------------------------------------------
  ;; `deadgrep'
  ;; --------------------------------------------------------------------------

  (use-package deadgrep
    :init
    (setq deadgrep-project-root-function
	  (lambda () (read-directory-name "Base directory: "
					  nil default-directory t)))
    :bind
    (("C-x C-g" . deadgrep)))

  ;; Use rgrep if ripgrep not present.
  (unless (executable-find "rg")
    (global-set-key (kbd "C-x C-g") 'rgrep))

  ;; --------------------------------------------------------------------------
  ;; `dired'
  ;; --------------------------------------------------------------------------

  (use-package dired
    :config
    (setq
     ;; If another Dired buffer is visible, use it as target for Rename/Copy.
     dired-dwim-target t
     ;; "always" means no asking.
     dired-recursive-copies 'always
     ;; "top" means ask once for top level directory.
     dired-recursive-deletes 'top
     ;; Human-readable listing
     dired-listing-switches "-lha --group-directories-first")

    ;; Automatically refresh dired buffer on changes.
    (add-hook 'dired-mode-hook 'auto-revert-mode))

  (use-package dired-x
    :after dired
    :init
    (add-hook 'dired-mode-hook 'dired-omit-mode)
    :config
    (setq-default dired-omit-files "^\\.\\|^\\#"))

  (use-package wdired
    :after dired
    :config
    (setq-default wdired-allow-to-change-permissions t
                  wdired-allow-to-redirect-links t))

  (use-package treemacs-icons-dired
    :after dired
    :hook (dired-mode . treemacs-icons-dired-mode))

  ;; --------------------------------------------------------------------------
  ;; `discover-my-major'
  ;; --------------------------------------------------------------------------

  (use-package discover-my-major
    :bind
    (("C-h M" . discover-my-major)))

  ;; --------------------------------------------------------------------------
  ;; `duplicate-thing'
  ;; --------------------------------------------------------------------------

  (use-package duplicate-thing
    :bind (("M-C" . duplicate-thing)))

  ;; --------------------------------------------------------------------------
  ;; `expand-region'
  ;; --------------------------------------------------------------------------

  (use-package expand-region
    :bind (("C-'" . er/expand-region)))

  ;; --------------------------------------------------------------------------
  ;; `flyspell'
  ;; --------------------------------------------------------------------------

  (use-package flyspell
    :bind
    (("C-c C-'" . flyspell-correct-word-before-point))
    :hook
    (((text-mode org-mode) . flyspell-mode)
     (prog-mode . flyspell-prog-mode))
    :config
    (if (executable-find "aspell")
        (progn
          (setq-default ispell-program-name "aspell")
          (setq-default ispell-extra-args '("--sug-mode=ultra")))
      (setq-default ispell-program-name "ispell"))
    (unbind-key "C-M-i" flyspell-mode-map))

  ;; --------------------------------------------------------------------------
  ;; `highlight-parentheses' - this package does not use faces for colours,
  ;; instead it uses the `hl-parens-colors' variable.  This can be set in the
  ;; theme file, but the mode has to be reloaded whenever the theme changes.
  ;; --------------------------------------------------------------------------

  (use-package highlight-parentheses
    :hook
    (prog-mode . highlight-parentheses-mode))

  ;; --------------------------------------------------------------------------
  ;; `ibuffer-vc'
  ;; --------------------------------------------------------------------------

  (use-package ibuffer-vc
    :defer t
    :init
    (add-hook 'ibuffer-hook
              (lambda ()
                (ibuffer-vc-set-filter-groups-by-vc-root)
                (unless (eq ibuffer-sorting-mode 'alphabetic)
                  (ibuffer-do-sort-by-alphabetic))))
    :config
    (setq ibuffer-formats
          '((mark modified read-only vc-status-mini " "
                  (name 36 36 :left :elide)
                  " "
                  (size 9 -1 :right)
                  " "
                  (mode 16 16 :left :elide)
                  " "
                  (vc-status 16 16 :left)
                  " "
                  filename-and-process))))

  ;; --------------------------------------------------------------------------
  ;; `pdf-tools' - use instead of DocView.
  ;; --------------------------------------------------------------------------

  (use-package pdf-tools
    :config
    (pdf-tools-install))

  ;; --------------------------------------------------------------------------
  ;; `perspective'
  ;; --------------------------------------------------------------------------

  (use-package perspective
    :config
    (persp-mode))

  ;; --------------------------------------------------------------------------
  ;; `projectile'
  ;; --------------------------------------------------------------------------

  (use-package projectile
    :defer nil
    :bind
    (("C-c p" . projectile-command-map))
    :config
    (projectile-mode))

  ;; --------------------------------------------------------------------------
  ;; `rainbow-delimiters' - colours are set by theme.
  ;; --------------------------------------------------------------------------

  (use-package rainbow-delimiters
    :hook
    (prog-mode . rainbow-delimiters-mode))

  ;; --------------------------------------------------------------------------
  ;; `rainbow-mode'
  ;; --------------------------------------------------------------------------
  
  (use-package rainbow-mode
    :defer t)

  ;; --------------------------------------------------------------------------
  ;; `recentf-ext'
  ;; --------------------------------------------------------------------------

  (use-package recentf-ext)

  ;; --------------------------------------------------------------------------
  ;; `saveplace' - remember location in file.
  ;; --------------------------------------------------------------------------

  (use-package saveplace
    :init
    (save-place-mode 1))

  ;; --------------------------------------------------------------------------
  ;; `smartparens'
  ;; --------------------------------------------------------------------------

  (use-package smartparens
    :config
    (smartparens-global-mode t)
    (show-smartparens-global-mode t)

    (require 'smartparens-config)

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

  ;; --------------------------------------------------------------------------
  ;; Configure `swiper'.
  ;; --------------------------------------------------------------------------

  (use-package swiper
    :bind
    (("M-s M-s" . swiper))
    :config
    (setq ivy-count-format "%d/%d "))

  ;; --------------------------------------------------------------------------
  ;; `tramp'
  ;; --------------------------------------------------------------------------

  (use-package tramp
    :defer t
    :config
    (setq-default tramp-default-method "ssh")
    ;; This line proxies all sudo connections via an ssh connection to the
    ;; provided hostname.
    (add-to-list 'tramp-default-proxies-alist '(nil "\\`root\\'" "/ssh:%h:"))
    ;; This rule is an exception to the above so that local sudo does not proxy
    ;; via ssh.  This has to be added last so that it is the first element of
    ;; the list.
    (add-to-list 'tramp-default-proxies-alist '("localhost" "\\`root\\'" nil))

    (defun sudo ()
      "Use TRAMP to `sudo' the current buffer"
      (interactive)
      (when buffer-file-name
        (find-alternate-file
         (concat "/sudo:root@localhost:"
                 buffer-file-name)))))

  ;; --------------------------------------------------------------------------
  ;; `undo-tree' - to undo "C-\", to redo "C-_", undo tree "C-x u".
  ;; --------------------------------------------------------------------------

  (use-package undo-tree
    :config
    (global-undo-tree-mode))

  ;; --------------------------------------------------------------------------
  ;; `vlf' - view large files.
  ;; --------------------------------------------------------------------------

  (use-package vlf-integrate
    :defer t
    :init
    (setq-default vlf-application 'dont-ask))

  ;; --------------------------------------------------------------------------
  ;; `volatile-highlights' - highlight changes caused by undo, yank, etc.
  ;; --------------------------------------------------------------------------

  (use-package volatile-highlights
    :config
    (volatile-highlights-mode t))

  ;; --------------------------------------------------------------------------
  ;; `which-key'
  ;; --------------------------------------------------------------------------
  
  (use-package which-key
    :init
    (setq which-key-separator " ")
    (setq which-key-prefix-prefix "+")
    :config
    (which-key-mode 1))

  ;; --------------------------------------------------------------------------
  ;; `whole-line-or-region' - kill line when calling kill-region without a
  ;; selected region.
  ;; --------------------------------------------------------------------------

  (use-package whole-line-or-region
    :config
    (whole-line-or-region-global-mode t))

  ;; --------------------------------------------------------------------------
  ;; `ws-butler' - will cleanup whitespace on all modified files on save.
  ;; --------------------------------------------------------------------------

  (use-package ws-butler
    :config
    (ws-butler-global-mode))

  )

(provide 'emodule/emacs)
;;; emacs.el ends here
