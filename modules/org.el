;;; org.el --- Module file for org-mode configuration.
;;
;; Copyright (C) 2017-2019 Wojciech Kozlowski
;;
;; Author: Wojciech Kozlowski <wk@wojciechkozlowski.eu>
;; Created: 2018-02-04
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This module sets up org-mode.
;;
;;; License: GPLv3

;;; Required packages:

;;; Code:


(defvar emodule/org-packages

  '(org-bullets
    org-noter)

  )

;; Configuration:

(defun emodule/org-init ()
  "Initialise the `org' module."

  (use-package org
    :hook
    (org-mode . auto-fill-mode)
    :bind
    (("C-c a" . org-agenda)
     ("C-c b" . org-switchb)
     ("C-c c" . org-capture)
     ("C-c l" . org-store-link))
    :config
    ;; ------------------------------------------------------------------------
    ;; Set variables.
    ;; ------------------------------------------------------------------------

    (setq
     ;; Hide special characters for italics/bold/underline.
     org-hide-emphasis-markers t
     ;; Add timestamp when tasks are marked as done.
     org-log-done t
     ;; Open org files unfolded
     org-startup-folded nil
     ;; Catch edits in invisible areas (space after the ellipsis ...)
     org-catch-invisible-edits 'error)

    ;; ------------------------------------------------------------------------
    ;; Set workflow states.
    ;; ------------------------------------------------------------------------

    (setq org-todo-keywords
          (quote ((sequence "TODO(t)"
                            "NEXT(n)"
                            "|"
                            "DONE(d)")
                  (sequence "WAIT(w@/!)"
                            "HOLD(h@/!)"
                            "|"
                            "UNPLANNED(c@/!)"))))

    (setq org-todo-keyword-faces
          (quote (("NEXT" :foreground "#96DEFA" :weight bold))))

    ;; ------------------------------------------------------------------------
    ;; Better bullet points.
    ;; ------------------------------------------------------------------------

    (font-lock-add-keywords 'org-mode
                            '(("^ +\\(*\\) "
                               (0 (prog1 ()
                                    (compose-region (match-beginning 1)
                                                    (match-end 1)
                                                    "•"))))))

    ;; ------------------------------------------------------------------------
    ;; LaTeX font size.
    ;; ------------------------------------------------------------------------

    (plist-put org-format-latex-options :scale 2.0)

    ;; ------------------------------------------------------------------------
    ;; Load agenda-files.
    ;; ------------------------------------------------------------------------
    (let* ((org-dir "~/Workspace/org/")
           (file-list (concat org-dir "agenda-files.el")))
      (when (file-exists-p file-list)
        (load file-list))))

  ;; ------------------------------------------------------------------------
  ;; Better header bullets
  ;; ------------------------------------------------------------------------

  (use-package org-bullets
    :hook (org-mode . org-bullets-mode))

  ;; --------------------------------------------------------------------------
  ;; Org-noter.
  ;; --------------------------------------------------------------------------

  (use-package org-noter
    :defer t)

  )

(provide 'emodule/org)
;;; org.el ends here
