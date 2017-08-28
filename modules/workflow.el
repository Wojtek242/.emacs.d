;;; workflow.el --- Module file for setting up workflows.
;;
;; Copyright (C) 2017 Wojciech Kozlowski
;;
;; Author: Wojciech Kozlowski <wojciech.kozlowski@vivaldi.net>
;; Created: 25 Aug 2017
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This module sets up configuration for a workflow and is expected to be
;; loaded last.
;;
;;; License: GPLv3

;;; Required packages:

(setq init-packages/workflow-packages

      '(workgroups2)

      )

;;; Configuration:

(defun init-packages/init-workflow ()

  ;; --------------------------------------------------------------------------
  ;; Enable `workgroups'.
  ;; --------------------------------------------------------------------------

  (use-package workgroups2
    :init
    (workgroups-mode 1)
    :config
    ;; Don't save.  Workgroups are transient to the session.
    (setq wg-emacs-exit-save-behavior           nil)
    (setq wg-workgroups-mode-exit-save-behavior nil))

  )
