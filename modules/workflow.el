;;; workflow.el --- Module file for setting up workflows.
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
;; This module sets up configuration for a workflow and is expected to be
;; loaded last.
;;
;;; License: GPLv3

;;; Required packages:

(setq emodule/workflow-packages

      '(workgroups2)

      )

;;; Configuration:

(defun emodule/workflow-init ()

  ;; --------------------------------------------------------------------------
  ;; Enable `workgroups'.
  ;; --------------------------------------------------------------------------

  (use-package workgroups2
    :defer t
    :bind
    (("C-c z z" . (lambda () (interactive) (workgroups-mode)))))


  )
