;;; languages.el --- Module file for programming language configuration.
;;
;; Copyright (C) 2019 Wojciech Kozlowski
;;
;; Author: Wojciech Kozlowski <wk@wojciechkozlowski.eu>
;; Created: 2019-07-20
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This module sets up language-specific configuration for editing source code.
;; Language-agnostic settings are in `programming.el'.
;;
;;; License: GPLv3

;;; Required packages:

;;; Code:

(defvar emodule/languages-packages

  '(
    ;; C/C++
    ccls
    ;; Dockerfile
    dockerfile-mode
    ;; Python
    py-autopep8
    ;; Rust
    cargo
    flycheck-rust
    rust-mode
    toml-mode
    ;; YAML
    yaml-mode)

  )

;; Configuration:

(defun emodule/languages-init ()
  "Initialise the `languages' module."

  ;; --------------------------------------------------------------------------
  ;; C/C++.
  ;; --------------------------------------------------------------------------

  (use-package ccls
    :hook ((c-mode c++-mode objc-mode) .
           (lambda () (require 'ccls) (lsp))))

  (setq-default
   c-default-style "linux"
   c-basic-offset 4)

  ;; --------------------------------------------------------------------------
  ;; Dockerfile.
  ;; --------------------------------------------------------------------------

  (use-package dockerfile-mode
    :defer t)

  ;; --------------------------------------------------------------------------
  ;; Makefile settings.
  ;; --------------------------------------------------------------------------

  (add-hook 'makefile-mode-hook (lambda () (setq indent-tabs-mode t)))

  ;; --------------------------------------------------------------------------
  ;; Python.
  ;; --------------------------------------------------------------------------

  (use-package python
    :init
    (setq python-shell-interpreter "python3")
    :hook
    (python-mode . lsp))

  (use-package py-autopep8
    ;; Note that this package require autopep8 to be installed.
    :bind (("C-c C-f" . py-autopep8-buffer)))

  ;; --------------------------------------------------------------------------
  ;; Rust.
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
          (kill-buffer))

        (unless (= ret 0)
          (error err-msg)))))

  (defun rust-new-project-bin (project-name)
    (interactive "sBinary project name: ")
    (rust-new-project project-name "bin"))

  (defun rust-new-project-lib (project-name)
    (interactive "sLibrary project name: ")
    (rust-new-project project-name "lib"))

  (use-package rust-mode
    :hook (rust-mode . lsp)
    :config
    (setq exec-path (append exec-path '("/home/wojtek/.cargo/bin"))))

  (use-package flycheck-rust
    :hook
    ((rust-mode . flycheck-mode)
     (flycheck-mode . flycheck-rust-setup)))

  ;; Add keybindings for interacting with Cargo
  (use-package cargo
    :hook (rust-mode . cargo-minor-mode))

  (use-package toml-mode
    :mode "\\.lock\\'")

  ;; --------------------------------------------------------------------------
  ;; YAML.
  ;; --------------------------------------------------------------------------

  (use-package yaml-mode
    :config
    (add-hook 'yaml-mode-hook 'display-line-numbers-mode))

  )

(provide 'emodule/languages)
;;; languages.el ends here