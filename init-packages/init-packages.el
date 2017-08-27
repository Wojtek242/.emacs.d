;;; init-packages.el --- Helpful automation functions for `package'
;;
;; Copyright (C) 2017 Wojciech Kozlowski
;;
;; Author: Wojciech Kozlowski <wojciech.kozlowski@vivaldi.net>
;; Created: 23 Aug 2017
;; Version: 0.0.1
;; Keywords: tools
;; Package-Requires:
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This package serves to help further automate package management with
;; `package'.  The aim of this package is to expose a simple API which when
;; provided with a list of desired packages will (i) install them and remove
;; any redundant packages, (ii) update them, (iii) rollback updates.
;;
;; Currently only (i) is implemented.
;;
;;; License: GPLv3

;;; Code:

(require 'package)
(require 'cl)

(defgroup init-packages nil
  "Further automate working with `package'"
  :group 'applications)

;;; Customization options

(defcustom init-packages/print-logs t
  "This variable determines if `init-packages' prints any logs."
  :type 'boolean)

(defcustom init-packages/log "*Init-Packages-Log*"
  "Buffer to which logs will be printed by `init-packages'.
This buffer will be erased whenever
\\[init-packages/isntall-packages] is called."
  :type 'string)

(defcustom init-packages/error-log "*Init-Packages-Error-Log*"
  "Buffer to which error logs will be printed by `init-packages'.
This buffer will be erased whenever
\\[init-packages/install-packages] is called."
  :type 'string)

(defcustom init-packages/install-attempts 2
  "How many times to attempt a package installation.
This only matters if for some reason a package fails to install
on the first attempt.  Sometimes re-attempting the installation
may fix the problem.  Note that subsequent attempts are only made
after attempting to install all other packages first."
  :type 'integer)

(defcustom init-packages/modules-dir "~/.emacs.d/modules/"
  "Directory in which module files are to be found."
  :type 'string)

;;; Print functions

(defun init-packages/set-logs-read-only ()
  "Set log buffer to log-view-mode."
  (when init-packages/print-logs

    (save-excursion
      (set-buffer (get-buffer-create init-packages/log))
      (log-view-mode))

    (save-excursion
      (set-buffer (get-buffer-create init-packages/error-log))
      (log-view-mode))))

(defun init-packages/erase-logs ()
  "Erase both log buffers."
  (when init-packages/print-logs

    ;; Erase `init-packages/log'.
    (save-excursion
      (set-buffer (get-buffer-create init-packages/log))
      (read-only-mode 0)
      (erase-buffer)
      (goto-char (point-min)))

    ;; Erase `init-packages/error-log'.
    (save-excursion
      (set-buffer (get-buffer-create init-packages/error-log))
      (read-only-mode 0)
      (erase-buffer)
      (goto-char (point-min)))))

(defun init-packages/print (string buffer)
  "Print STRING to BUFFER."
  (when init-packages/print-logs
    (save-excursion
      (set-buffer (get-buffer-create buffer))
      (goto-char (point-max))
      (if (not (= (point) 1))
          (newline))
      (insert string))))

(defun init-packages/print-format (fmt pkg buffer)
  "Print string of FMT about PKG to BUFFER"
  (or (stringp pkg)
      (setq pkg (symbol-name pkg)))
  (init-packages/print (format fmt pkg) buffer))

(defun init-packages/print-installing (pkg)
  "Print a log message about installing PKG."
  (init-packages/print-format "Installing: %s" pkg init-packages/log))

(defun init-packages/print-deleting (pkg)
  "Print a log message about deleting PKG."
  (init-packages/print-format "Deleting: %s" pkg init-packages/log))

(defun init-packages/print-failed (logstrbase pkg)
  "Print a log message about failed operation of PKG."
  (let* ((logstr (concat logstrbase
                         (format " (see %s for details)"
                                 init-packages/error-log)))
         (errstr (concat "*** " logstrbase " ***")))
    (init-packages/print-format logstr pkg init-packages/log)
    (init-packages/print-format errstr pkg init-packages/error-log)))

(defun init-packages/print-failed-install (pkg)
  "Print a log message about failed installation of PKG."
  (init-packages/print-failed "Failed to install: %s" pkg))

(defun init-packages/print-failed-delete (pkg)
  "Print a log message about failed deletion of PKG."
  (init-packages/print-failed "Failed to delete: %s" pkg))

(defun init-packages/print-log (string)
  "Print STRING to `init-packages/log'."
  (init-packages/print string init-packages/log))

(defun init-packages/print-error-log (string)
  "Print STRING to `init-packages/error-log'."
  (init-packages/print string init-packages/error-log))

;;; Helper functions

(defun init-packages/install-pkgs (install-pkgs)
  "Install all packages in INSTALL-PKGS.
Log errors to `init-packages/error-log'."
  (dolist (p install-pkgs nil)
    (init-packages/print-installing p)
    (condition-case err
        (package-install p)
      (error (progn
               (init-packages/print-failed-install p)
               (init-packages/print-error-log (error-message-string err)))))))

(defun init-packages/delete-pkgs (delete-pkgs)
  "Delete all packages in DELETE-PKGS.
This will attempt to delete all installed versions.  Log errors
to `init-packages/error-log'.  This assumes all DELETE-PKGS can
be removed, including packages that are dependencies as it is
assumed they would not be dependencies once all packages in
DELETE-PKGS are removed."
  (dolist (p delete-pkgs nil)
    (init-packages/print-deleting p)
    (condition-case err
        (package-delete (cadr (assq p package-alist)) t)
      (error (progn
               (init-packages/print-failed-install p)
               (init-packages/print-error-log (error-message-string err)))))))

(defun init-packages/removable-packages (pkgs)
  "Return a list of names of packages no longer needed.
These are packages which are neither contained in PKGS nor a
dependency of one that is."
  (let ((needed (cl-loop for p in pkgs
                         if (assq p package-alist)
                         ;; `p' and its dependencies are needed.
                         append (cons p (package--get-deps p)))))
    (cl-loop for p in (mapcar #'car package-alist)
             unless (memq p needed)
             collect p)))

;;; Operational functions:
;; These functions are expected to be called from the init file.

(defun init-packages/install-packages (desired-pkgs &optional no-set-selected)
  "Install DESIRED-PKGS and remove redundant packages.
First, any missing packages will be installed followed by the
deletion of all packages that are not dependencies of anything in
DESIRED-PKGS.  Information logs will be printed to the
`init-packages/log' buffer whilst error messages will be printed
to the `init-packages/error-log' buffer.  Finally the
`package-selected-packages' custom variable will be set to
DESIRED-PKGS unless NO-SET-SELECTED is non-nil"

  ;; Erase log buffers.
  (init-packages/erase-logs)

  ;; Check if `package' initialized.
  (when (not package--initialized)
    (let ((err-str "`package' not initialized"))
      (init-packages/print-error-log err-str)
      (error err-str)))

  ;; Install packages.  If any packages fail to install, re-attempt up to
  ;; `init-packages/install-attempts' total attempts.
  (let ((attempt 0))
    (while (and (< attempt init-packages/install-attempts)
                (setq install-pkgs
                      (remove-if #'package-installed-p desired-pkgs)))
      (if (= attempt 0)
          (progn
            (init-packages/print-log "*** Install packages ***")
            (init-packages/print-log "--- Refreshing package archives ---")
            (package-refresh-contents))
        (init-packages/print-log
         "--- Re-attempt installation of failed packages ---"))
      (init-packages/install-pkgs install-pkgs)
      (setq attempt (1+ attempt))))

  ;; Print an error message if not all packages were installed.
  (let ((failed-pkgs (remove-if #'package-installed-p desired-pkgs)))
    (when failed-pkgs
      (init-packages/print-error-log
       "*** WARNING: NOT ALL PACKAGES WERE INSTALLED ***")
      (init-packages/print-error-log "--- Packages not installed: ")
      (init-packages/print-error-log
       (mapconcat #'symbol-name failed-pkgs ", "))))

  ;; Remove packages.
  (let ((delete-pkgs (init-packages/removable-packages desired-pkgs)))
    (when delete-pkgs
      (init-packages/print-log "*** Delete packages ***")
      (init-packages/delete-pkgs delete-pkgs)))

  (unless no-set-selected
    (customize-save-variable 'package-selected-packages desired-pkgs)))

(defun init-packages/load-module (mod)
  "Load all definitions for module MOD.
This function expects the module to be located in a file called
MOD.el in the `init-packages/modules-dir' directory."
  (load (expand-file-name (format "%s/%s.el"
                                  init-packages/modules-dir
                                  (symbol-name mod)))))

(defun init-packages/load-module-list (modlist)
  "Load all modules in MODLIST."
  (dolist (mod modlist nil)
    (init-packages/load-module mod)))

(defun init-packages/get-module-packages (mod)
  "Get all packages required by module MOD to MODLIST.
A module's packages are expected to be found in a list called
`init-packages/MOD-packages'"
  (eval (intern (format "init-packages/%s-packages" (symbol-name mod)))))

(defun init-packages/cons-package-list (modlist)
  "Construct a list of all packages required by MODLIST."
  (let (pkglist)
    (dolist (mod modlist pkglist)
      (setq pkglist (append (init-packages/get-module-packages mod)
                            pkglist)))))

(defun init-packages/init-module (mod)
  "Call the initialisation function for module MOD.
It is expected that every module has a function called
`init-packages/init-MOD' which will be called by the expansion of
this macro."
  (funcall (intern (format "init-packages/init-%s" (symbol-name mod)))))

(defun init-packages/init-module-list (modlist)
  "Init all modules in MODLIST."
  (dolist (mod modlist nil)
    (init-packages/init-module mod)))

(defun init-packages/init (modlist)
  "Initialise all modules in MODLIST."
  (init-packages/load-module-list modlist)
  (init-packages/install-packages (init-packages/cons-package-list modlist))
  (init-packages/init-module-list modlist)
  (init-packages/set-logs-read-only))

(provide 'init-packages)

;;; init-packages.el ends here
