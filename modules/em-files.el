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
  (add-hook 'dired-mode-hook 'auto-revert-mode)

  (use-package dired-x
    :init
    (add-hook 'dired-mode-hook 'dired-omit-mode)
    :config
    (setq-default dired-omit-files "^\\.\\|^\\#"))

  (use-package wdired
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

  )

(provide 'em-files)
;;; em-files.el ends here
