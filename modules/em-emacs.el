;;; em-emacs.el --- Module file for configuring Emacs itself.
;;
;; Copyright (C) 2017 Wojciech Kozlowski
;;
;; Author: Wojciech Kozlowski <wk@wojciechkozlowski.eu>
;; Created: 25 Aug 2017
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

(defvar emodule/em-emacs-packages

  '(deadgrep
    discover-my-major
    ibuffer-vc
    objed
    pdf-tools
    rainbow-mode
    treemacs
    treemacs-projectile
    treemacs-icons-dired
    treemacs-magit
    which-key
    use-package)

  )

;;; Configuration:

(defun emodule/em-emacs-init ()
  "Initialise the `em-emacs' module."

  ;; --------------------------------------------------------------------------
  ;; Enable objed.
  ;; --------------------------------------------------------------------------

  (use-package objed
    :config
    (objed-mode))

  ;; --------------------------------------------------------------------------
  ;; Use deadgrep.
  ;; --------------------------------------------------------------------------

  (use-package deadgrep
    :bind
    (("C-x C-g" . deadgrep)))

  ;; --------------------------------------------------------------------------
  ;; Help extensions.
  ;; --------------------------------------------------------------------------

  (use-package info+)

  (use-package discover-my-major
    :init
    (global-unset-key (kbd "C-h h"))
    :bind
    (("C-h h m" . discover-my-major)))

  (use-package help+)

  (use-package help-fns+)

  (use-package help-mode+)

  (use-package which-key
    :init
    (setq which-key-separator " ")
    (setq which-key-prefix-prefix "+")
    :config
    (which-key-mode 1))

  ;; --------------------------------------------------------------------------
  ;; Configure `ibuffer'.
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
  ;; Rainbow mode.
  ;; --------------------------------------------------------------------------

  (use-package rainbow-mode
    :defer t)

  ;; --------------------------------------------------------------------------
  ;; Keep point in same position on the screen when scrolling.
  ;; --------------------------------------------------------------------------

  (setq-default scroll-preserve-screen-position 1)

  ;; --------------------------------------------------------------------------
  ;; Functions.
  ;; --------------------------------------------------------------------------

  (defun quit-other-window ()
    "Quit the next window in cyclic order"
    (interactive)
    (quit-window t (next-window (selected-window))))

  (defun kill-default-buffer ()
    "Kill the currently active buffer with no confirmation."
    (interactive)
    (let (kill-buffer-query-functions) (kill-buffer)))

  (defun refresh-non-face-colours ()
    "Restart modes that use colours not set with face variables.
    This has to be called whenever the active theme changes to
    refresh these colours."

    (when (and (fboundp 'fci-mode)
               (member 'fci-mode minor-mode-list))
      (fci-mode 1))

    (when (and (fboundp 'highlight-parentheses-mode)
               highlight-parentheses-mode)
      (highlight-parentheses-mode 1)))

  ;; Key-bindings -------------------------------------------------------------

  ;; Kill other window (cyclic order).
  (global-set-key (kbd "C-z") 'quit-other-window)

  ;; Kill current buffer without prompting.
  (global-set-key (kbd "C-x k") 'kill-default-buffer)

  ;; --------------------------------------------------------------------------
  ;; Additional key-bindings.
  ;; --------------------------------------------------------------------------

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

  ;; Recursive grep.  Use only if ripgrep not present.
  (unless (executable-find "rg")
    (global-set-key (kbd "C-x C-g") 'rgrep))

  ;; Setup key-bindings for switching between themes.
  (global-set-key (kbd "C-x t l") (lambda ()
                                    (interactive)
                                    (load-theme 'havoc-light t)
                                    (refresh-non-face-colours)))
  (global-set-key (kbd "C-x t d") (lambda ()
                                    (interactive)
                                    (load-theme 'havoc-dark t)
                                    (refresh-non-face-colours)))

  ;; --------------------------------------------------------------------------
  ;; Update buffers when files change.
  ;; --------------------------------------------------------------------------

  (global-auto-revert-mode)

  ;; --------------------------------------------------------------------------
  ;; Aliases.
  ;; --------------------------------------------------------------------------

  ;; y or n is enough.
  (defalias 'yes-or-no-p 'y-or-n-p)

  ;; Always use ibuffer.
  (defalias 'list-buffers 'ibuffer)

  ;; --------------------------------------------------------------------------
  ;; Address mode.
  ;; --------------------------------------------------------------------------

  (add-hook 'prog-mode-hook 'goto-address-mode)
  (add-hook 'text-mode-hook 'goto-address-mode)

  ;; --------------------------------------------------------------------------
  ;; Use PDF Tools instead of DocView.
  ;; --------------------------------------------------------------------------

  (use-package pdf-tools
    :config
    (pdf-tools-install))

  ;; --------------------------------------------------------------------------
  ;; Tramp configuration.
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
  ;; Configure garbage collection.
  ;;
  ;; Based on advice from:
  ;; http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
  ;; --------------------------------------------------------------------------

  (defun minibuffer-gc-setup-hook ()
    (setq gc-cons-threshold most-positive-fixnum))

  (defun minibuffer-gc-exit-hook ()
    (setq gc-cons-threshold 800000))

  (add-hook 'minibuffer-setup-hook #'minibuffer-gc-setup-hook)
  (add-hook 'minibuffer-exit-hook #'minibuffer-gc-exit-hook)

  ;; --------------------------------------------------------------------------
  ;; Increase recursion limits.
  ;; --------------------------------------------------------------------------
  (setq-default max-specpdl-size 20000) ;; ~15x original value
  (setq-default max-lisp-eval-depth 24000) ;; 30x orignal value

  ;; --------------------------------------------------------------------------
  ;; This slows down cursor scrolling.
  ;; --------------------------------------------------------------------------
  (setq-default auto-window-vscroll nil)

  ;; --------------------------------------------------------------------------
  ;; Add directories to exec-path.
  ;; --------------------------------------------------------------------------
  (setq exec-path (append exec-path '("/home/wojtek/.local/bin")))

  (use-package treemacs
    :defer t
    :init
    (with-eval-after-load 'winum
      (define-key winum-keymap (kbd "M-m") #'treemacs-select-window))
    :config
    (progn
      (setq treemacs-collapse-dirs                 (if (executable-find "python") 3 0)
            treemacs-deferred-git-apply-delay      0.5
            treemacs-display-in-side-window        t
            treemacs-file-event-delay              5000
            treemacs-file-follow-delay             0.2
            treemacs-follow-after-init             t
            treemacs-git-command-pipe              ""
            treemacs-goto-tag-strategy             'refetch-index
            treemacs-indentation                   2
            treemacs-indentation-string            " "
            treemacs-is-never-other-window         t
            treemacs-max-git-entries               5000
            treemacs-no-png-images                 nil
            treemacs-no-delete-other-windows       t
            treemacs-project-follow-cleanup        nil
            treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
            treemacs-recenter-distance             0.1
            treemacs-recenter-after-file-follow    nil
            treemacs-recenter-after-tag-follow     nil
            treemacs-recenter-after-project-jump   'always
            treemacs-recenter-after-project-expand 'on-distance
            treemacs-show-cursor                   nil
            treemacs-show-hidden-files             t
            treemacs-silent-filewatch              nil
            treemacs-silent-refresh                nil
            treemacs-sorting                       'alphabetic-desc
            treemacs-space-between-root-nodes      t
            treemacs-tag-follow-cleanup            t
            treemacs-tag-follow-delay              1.5
            treemacs-width                         35)

      ;; The default width and height of the icons is 22 pixels. If you are
      ;; using a Hi-DPI display, uncomment this to double the icon size.
      ;;(treemacs-resize-icons 44)

      (treemacs-follow-mode t)
      (treemacs-filewatch-mode t)
      (treemacs-fringe-indicator-mode t)
      (pcase (cons (not (null (executable-find "git")))
                   (not (null (executable-find "python3"))))
        (`(t . t)
         (treemacs-git-mode 'deferred))
        (`(t . _)
         (treemacs-git-mode 'simple))))

    :bind
    (:map global-map
          ("M-m"       . treemacs-select-window)
          ("C-x t 1"   . treemacs-delete-other-windows)
          ("C-x t t"   . treemacs)
          ("C-x t B"   . treemacs-bookmark)
          ("C-x t C-t" . treemacs-find-file)
          ("C-x t M-t" . treemacs-find-tag)))

  (use-package treemacs-projectile
    :after treemacs projectile)

  (use-package treemacs-icons-dired
    :after treemacs dired
    :config (treemacs-icons-dired-mode))

  (use-package treemacs-magit
    :after treemacs magit)

  )

(provide 'em-emacs)
;;; em-emacs.el ends here
