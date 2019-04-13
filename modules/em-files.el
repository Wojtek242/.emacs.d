;;; em-files.el --- Module file for configuring file management.
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
;; This module is used for configuring file management within Emacs.
;;
;;; License: GPLv3

;;; Required packages:

;;; Code:

(defvar emodule/em-files-packages

  '(recentf-ext
    treemacs
    treemacs-projectile
    treemacs-icons-dired
    treemacs-magit
    vlf)

  )

;;; Configuration:

(defun emodule/em-files-init ()
  "Initialise the `em-files' module."

  ;; --------------------------------------------------------------------------
  ;; View large files.
  ;; --------------------------------------------------------------------------

  (use-package vlf-integrate
    :defer t
    :init
    (setq-default vlf-application 'dont-ask))

  ;; --------------------------------------------------------------------------
  ;; Large file threshold.
  ;; --------------------------------------------------------------------------

  (setq large-file-warning-threshold 10485760) ;; 10 MB

  ;; --------------------------------------------------------------------------
  ;; Back up settings.
  ;; --------------------------------------------------------------------------

  (defvar backup-directory "~/.emacs.d/.backups")
  (if (not (file-exists-p backup-directory))
      (make-directory backup-directory t))

  (setq-default
   ;; Backup a file the first time it is saved
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

  ;; --------------------------------------------------------------------------
  ;; Dired.
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
     dired-listing-switches "-lha --group-directories-first"
     )

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

  ;; --------------------------------------------------------------------------
  ;; Recentf.
  ;; --------------------------------------------------------------------------

  (recentf-mode 1)
  (use-package recentf-ext)

  ;; --------------------------------------------------------------------------
  ;; Remember location in file.
  ;; --------------------------------------------------------------------------

  (use-package saveplace
    :init
    (save-place-mode 1))

  ;; --------------------------------------------------------------------------
  ;; Treemacs.
  ;; --------------------------------------------------------------------------

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
    :defer t)

  (use-package treemacs-magit
    :defer t)

  (use-package treemacs-icons-dired
    :hook (dired-mode . treemacs-icons-dired-mode))

  )

(provide 'em-files)
;;; em-files.el ends here
