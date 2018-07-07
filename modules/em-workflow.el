;;; em-workflow.el --- Module file for setting up workflows.
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

;;; Code:

(defvar emodule/em-workflow-packages

      '(workgroups2)

      )

;;; Configuration:

(defun emodule/em-workflow-init ()
  "Initialise the `em-workflow' module."

  ;; --------------------------------------------------------------------------
  ;; Enable `workgroups'.
  ;; --------------------------------------------------------------------------

  (use-package workgroups2
    :bind
    (("C-c z z" . (lambda () (interactive) (workgroups-mode)))))

  )

(provide 'em-workflow)
;;; em-workflow.el ends here
