;;; emodule.el --- Helpful automation functions for `package'
;;
;; Copyright (C) 2017 Wojciech Kozlowski
;;
;; Author: Wojciech Kozlowski <wk@wojciechkozlowski.eu>
;; Created: 23 Aug 2017
;; Version: 1.0.0
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
;; any redundant packages, (ii) upgrade them, (iii) restore previous state.
;;
;;; License: GPLv3

;;; Code:

(require 'package)
(with-no-warnings
  (require 'cl))

(defgroup emodule nil
  "Further automate working with `package'"
  :group 'applications)

;;; Customization options

(defcustom emodule/print-logs t
  "This variable determines if `emodule' prints any logs."
  :type 'boolean)

(defcustom emodule/log "*EModule-Log*"
  "Buffer to which logs will be printed by `emodule'.
This buffer will be erased whenever
\\[emodule/isntall-packages] is called."
  :type 'string)

(defcustom emodule/error-log "*EModule-Error-Log*"
  "Buffer to which error logs will be printed by `emodule'.
This buffer will be erased whenever
\\[emodule/install-packages] is called."
  :type 'string)

(defcustom emodule/install-attempts 2
  "How many times to attempt a package installation.
This only matters if for some reason a package fails to install
on the first attempt.  Sometimes re-attempting the installation
may fix the problem.  Note that subsequent attempts are only made
after attempting to install all other packages first."
  :type 'integer)

(defcustom emodule/modules-dir "~/.emacs.d/modules/"
  "Directory in which module files are to be found."
  :type 'string)

;;; Print functions:

(defun emodule/set-logs-read-only ()
  "Set log buffer to `log-view-mode'."
  (when emodule/print-logs

    (with-current-buffer (get-buffer-create emodule/log)
      (log-view-mode))

    (with-current-buffer (get-buffer-create emodule/error-log)
      (log-view-mode))))

(defun emodule/unset-logs-read-only ()
  "Unset log buffer from `log-view-mode'."
  (when emodule/print-logs

    (with-current-buffer (get-buffer-create emodule/log)
      (read-only-mode 0))

    (with-current-buffer (get-buffer-create emodule/error-log)
      (read-only-mode 0))))

(defun emodule/erase-logs ()
  "Erase both log buffers."
  (when emodule/print-logs

    ;; Erase `emodule/log'.
    (with-current-buffer (get-buffer-create emodule/log)
      (read-only-mode 0)
      (erase-buffer)
      (goto-char (point-min)))

    ;; Erase `emodule/error-log'.
    (with-current-buffer (get-buffer-create emodule/error-log)
      (read-only-mode 0)
      (erase-buffer)
      (goto-char (point-min)))))

(defun emodule/print (string buffer)
  "Print STRING to BUFFER."
  (when emodule/print-logs
    (with-current-buffer (get-buffer-create buffer)
      (goto-char (point-max))
      (if (not (= (point) (line-beginning-position)))
          (newline))
      (insert string))))

(defun emodule/print-format (fmt pkg buffer)
  "Print string of FMT about PKG to BUFFER."
  (or (stringp pkg)
      (setq pkg (symbol-name pkg)))
  (emodule/print (format fmt pkg) buffer))

(defun emodule/print-installing (pkg)
  "Print a log message about installing PKG."
  (emodule/print-format "Installing: %s" pkg emodule/log))

(defun emodule/print-deleting (pkg)
  "Print a log message about deleting PKG."
  (emodule/print-format "Deleting: %s" pkg emodule/log))

(defun emodule/print-failed (logstrbase pkg)
  "Print a log message LOGSTRBASE about failed operation of PKG."
  (let* ((logstr (concat logstrbase
                         (format " (see %s for details)"
                                 emodule/error-log)))
         (errstr (concat "*** " logstrbase " ***")))
    (emodule/print-format logstr pkg emodule/log)
    (emodule/print-format errstr pkg emodule/error-log)))

(defun emodule/print-failed-install (pkg)
  "Print a log message about failed installation of PKG."
  (emodule/print-failed "Failed to install: %s" pkg))

(defun emodule/print-failed-delete (pkg)
  "Print a log message about failed deletion of PKG."
  (emodule/print-failed "Failed to delete: %s" pkg))

(defun emodule/print-log (string)
  "Print STRING to `emodule/log'."
  (emodule/print string emodule/log))

(defun emodule/print-error-log (string)
  "Print STRING to `emodule/error-log'."
  (emodule/print string emodule/error-log))

;;; Package management functions:

(defun emodule/install-pkgs (install-pkgs)
  "Install all packages in INSTALL-PKGS.
Log errors to `emodule/error-log'."
  (dolist (p install-pkgs nil)
    (emodule/print-installing p)
    (condition-case err
        (unless (package-installed-p p)
          (package-install p))
      (error (progn
               (emodule/print-failed-install p)
               (emodule/print-error-log (error-message-string err))
               (error-message-string err))))))

(defun emodule/delete-pkgs (delete-pkgs)
  "Delete all packages in DELETE-PKGS.
This will attempt to delete all installed versions.  Log errors
to `emodule/error-log'.  This assumes all DELETE-PKGS can
be removed, including packages that are dependencies as it is
assumed they would not be dependencies once all packages in
DELETE-PKGS are removed."
  (dolist (p delete-pkgs nil)
    (emodule/print-deleting p)
    (condition-case err
        (package-delete (cadr (assq p package-alist)) t)
      (error (progn
               (emodule/print-failed-install p)
               (emodule/print-error-log (error-message-string err))
               (error-message-string err))))))

(defun emodule/removable-packages (pkgs)
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

(defun emodule/install-packages (desired-pkgs &optional no-set-selected)
  "Install DESIRED-PKGS and remove redundant packages.
First, any missing packages will be installed followed by the
deletion of all packages that are not dependencies of anything in
DESIRED-PKGS.  Information logs will be printed to the
`emodule/log' buffer whilst error messages will be printed
to the `emodule/error-log' buffer.  Finally the
`package-selected-packages' custom variable will be set to
DESIRED-PKGS unless NO-SET-SELECTED is non-nil"

  ;; Erase log buffers.
  (emodule/erase-logs)

  ;; Check if `package' initialized.
  (when (not package--initialized)
    (let ((err-str "`package' not initialized"))
      (emodule/print-error-log err-str)
      (error err-str)))

  ;; Install packages.  If any packages fail to install, re-attempt up to
  ;; `emodule/install-attempts' total attempts.
  (let ((attempt 0)
        (install-pkgs nil))
    (while (and (< attempt emodule/install-attempts)
                (setq install-pkgs
                      (remove-if #'package-installed-p desired-pkgs)))
      (if (= attempt 0)
          (progn
            (emodule/print-log "*** Install packages ***")
            (emodule/print-log "--- Refreshing package archives ---")
            (package-refresh-contents))
        (emodule/print-log
         "--- Re-attempt installation of failed packages ---"))
      (emodule/install-pkgs install-pkgs)
      (setq attempt (1+ attempt))))

  ;; Print an error message if not all packages were installed.
  (let ((failed-pkgs (remove-if #'package-installed-p desired-pkgs)))
    (when failed-pkgs
      (emodule/print-error-log
       "*** WARNING: NOT ALL PACKAGES WERE INSTALLED ***")
      (emodule/print-error-log "--- Packages not installed: ")
      (emodule/print-error-log
       (mapconcat #'symbol-name failed-pkgs ", "))))

  ;; Remove packages.
  (let ((delete-pkgs (emodule/removable-packages desired-pkgs)))
    (when delete-pkgs
      (emodule/print-log "*** Delete packages ***")
      (emodule/delete-pkgs delete-pkgs)))

  (unless no-set-selected
    (customize-save-variable 'package-selected-packages desired-pkgs)))

;;; Init functions:

(defun emodule/load-module (mod)
  "Load all definitions for module MOD.
This function expects the module to be located in a file called
MOD.el in the `emodule/modules-dir' directory."
  (load (expand-file-name (format "%s/%s.el"
                                  emodule/modules-dir
                                  (symbol-name mod)))))

(defun emodule/load-module-list (modlist)
  "Load all modules in MODLIST."
  (dolist (mod modlist nil)
    (emodule/load-module mod)))

(defun emodule/get-module-packages (mod)
  "Get all packages required by module MOD to MODLIST.
A module's packages are expected to be found in a list called
`emodule/MOD-packages'"
  (eval (intern (format "emodule/%s-packages" (symbol-name mod)))))

(defun emodule/cons-package-list (modlist)
  "Construct a list of all packages required by MODLIST."
  (let (pkglist)
    (dolist (mod modlist pkglist)
      (setq pkglist (append (emodule/get-module-packages mod)
                            pkglist)))))

(defun emodule/init-module (mod)
  "Call the initialisation function for module MOD.
It is expected that every module has a function called
`emodule/init-MOD' which will be called by the expansion of
this macro."
  (funcall (intern (format "emodule/%s-init" (symbol-name mod)))))

(defun emodule/init-module-list (modlist)
  "Init all modules in MODLIST."
  (dolist (mod modlist nil)
    (emodule/init-module mod)))

(defun emodule/init (modlist)
  "Initialise all modules in MODLIST."
  (emodule/load-module-list modlist)
  (emodule/install-packages (emodule/cons-package-list modlist))
  (emodule/init-module-list modlist)
  (emodule/set-logs-read-only))

(defun emodule/init-debug (modlist)
  "Initialise all modules in MODLIST, but don't install/delete packages."
  (emodule/load-module-list modlist)
  (emodule/init-module-list modlist)
  (emodule/set-logs-read-only))

;;; Backup handling functions:

(defun emodule/backup ()
  "Create a backup of the elpa directory in elpa.tar.xz."
  (interactive)
  (message "Backing up elpa...")
  (let* ((default-directory "~/.emacs.d")
         (dir "elpa")
         (archive (format "%s.tar.xz" dir)))
    (emodule/unset-logs-read-only)
    (emodule/print (format "*** Backing up %s into %s ***\n" dir archive)
                   emodule/log)
    (let* ((cmd (format "XZ_OPT=-9 tar -cJf %s %s" archive dir))
           (res (shell-command cmd nil emodule/error-log)))
      (unless (zerop res)
        (progn
          (emodule/print "*** Failed to create backup ***" emodule/log)
          (delete-file archive)
          (emodule/set-logs-read-only)
          (error (format "Command: '%s' failed with code %d" cmd res)))))
    (emodule/print (format "*** Backup created in %s ***" archive) emodule/log)
    (emodule/set-logs-read-only)))

(defun emodule/clean-move-dir (dir1 dir2)
  "Move DIR1 to DIR2 after ensuring DIR2 does not exist."
  (when (file-directory-p dir2)
    (delete-directory dir2 t))
  (rename-file dir1 dir2))

(defun emodule/restore ()
  "Restore elpa directory state from backup."
  (interactive)
  (message "Restoring elpa...")
  (let* ((default-directory "~/.emacs.d")
         (dir "elpa")
         (dir-bkp (format "%s.bkp" dir))
         (archive (format "%s.tar.xz" dir)))
    (emodule/unset-logs-read-only)
    (emodule/print (format "*** Restoring %s back from %s ***\n" dir archive)
                   emodule/log)
    (when (file-directory-p dir)
      (emodule/clean-move-dir dir dir-bkp))
    (let* ((cmd (format "tar -xJf %s" archive))
           (res (shell-command cmd nil emodule/error-log)))
      (unless (zerop res)
        (progn
          (emodule/print "*** Failed to restore ***" emodule/log)
          (when (file-directory-p dir-bkp)
            (emodule/clean-move-dir dir-bkp dir))
          (emodule/set-logs-read-only)
          (error (format "Command: '%s' failed with code %d" cmd res)))))
    (emodule/print (format "*** Restored from %s ***" archive) emodule/log)
    (when (file-directory-p dir-bkp)
      (delete-directory dir-bkp t))
    (emodule/set-logs-read-only)))

;;; Upgrade functions:

(defun emodule/upgradable-packages ()
  "Return a list of names of packages that have a newer version."
  (let (upgrades)
    (dolist (entry package-alist)
      (let* ((pkg-desc (cadr entry))
             (pkg-name (package-desc-name pkg-desc))
             (pkg-version (package-desc-version pkg-desc))
             (pkg-avail (assq pkg-name package-archive-contents))
             (pkg-avail-desc (cadr pkg-avail))
             (pkg-avail-version (package-desc-version pkg-avail-desc)))
        (when (version-list-< pkg-version pkg-avail-version)
          (push pkg-name upgrades))))
    upgrades))

(defun emodule/upgrade ()
  "Upgrade all packages that have a newer version.
This is achieved by first deleting the installed version followed
by installing the newer version"
  (interactive)
  (emodule/backup)
  (package-refresh-contents)
  (emodule/unset-logs-read-only)
  (emodule/print "*** Upgrading packages ***" emodule/log)
  (condition-case err
      (let ((upgrades (emodule/upgradable-packages)))
        (emodule/delete-pkgs upgrades)
        (emodule/install-pkgs upgrades)
        (emodule/print "*** Upgrade complete ***" emodule/log)
        (emodule/set-logs-read-only))
    (error (let ((err-str "*** Upgrade failed ***"))
             (emodule/print err-str emodule/log)
             (emodule/print err-str emodule/error-log)
             (emodule/print-error-log (error-message-string err))
             (emodule/set-logs-read-only)
             (emodule/restore)
             err-str))))

(provide 'emodule)

;;; emodule.el ends here
