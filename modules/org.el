;;; org.el --- Module file for org-mode and org-like package configuration.
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

  '(elfeed
    org-bullets
    org-noter)

  )

;; Configuration:

(defun emodule/org-init ()
  "Initialise the `org' module."

  ;; -----------------------------------------------------------------------------------------------
  ;; `elfeed'
  ;; -----------------------------------------------------------------------------------------------

  (use-package elfeed
    :bind
    (("C-x w" . elfeed))
    :config
    (let ((elfeed-feeds-file "~/Workspace/rss.xml"))
      (when (file-exists-p elfeed-feeds-file)
        (elfeed-load-opml elfeed-feeds-file)
        (run-at-time nil 3600 'elfeed-update))))

  ;; -----------------------------------------------------------------------------------------------
  ;; `org'
  ;; -----------------------------------------------------------------------------------------------

  (use-package org
    :hook
    (org-mode . auto-fill-mode)
    :bind
    (("C-c a" . org-agenda)
     ("C-c b" . org-switchb)
     ("C-c c" . org-capture)
     ("C-c l" . org-store-link))
    :config
    ;; ---------------------------------------------------------------------------------------------
    ;; Set variables.
    ;; ---------------------------------------------------------------------------------------------

    (setq
     ;; Do not hide special characters for italics/bold/underline.
     org-hide-emphasis-markers nil
     ;; Open org files unfolded
     org-startup-folded nil
     ;; Catch edits in invisible areas (space after the ellipsis ...)
     org-catch-invisible-edits 'error
     ;; Don't warn about deadlines - they're pretty visible as is
     org-deadline-warning-days 0)

    (setq org-agenda-prefix-format '((agenda . " %i %?-12t% s %b")
                                     (todo . " %i %-48b")
                                     (tags . " %i %b")
                                     (search . " %i %b")))

    ;; ---------------------------------------------------------------------------------------------
    ;; Set workflow states.
    ;; ---------------------------------------------------------------------------------------------

    (setq org-todo-keywords
          (quote ((sequence "TODO(t)"
                            "NEXT(n)"
                            "|"
                            "DONE(d)")
                  (sequence "WAIT(w@/!)"
                            "HOLD(h@/!)"
                            "|"
                            "UNPLANNED(c@/!)"))))

    (setq org-todo-keyword-faces (quote (("NEXT" :foreground "#96DEFA" :weight bold)
                                         ("WAIT" :foreground "#798A9B" :weight bold)
                                         ("HOLD" :foreground "#798A9B" :weight bold))))

    ;; ---------------------------------------------------------------------------------------------
    ;; Better bullet points.
    ;; ---------------------------------------------------------------------------------------------

    (font-lock-add-keywords 'org-mode
                            '(("^ +\\(*\\) "
                               (0 (prog1 ()
                                    (compose-region (match-beginning 1)
                                                    (match-end 1)
                                                    "•"))))))

    ;; ---------------------------------------------------------------------------------------------
    ;; LaTeX font size.
    ;; ---------------------------------------------------------------------------------------------

    (plist-put org-format-latex-options :scale 2.0)

    ;; ---------------------------------------------------------------------------------------------
    ;; Setup agenda and capture files.
    ;; ---------------------------------------------------------------------------------------------

    (setq
     ;; File for org-capture
     org-default-notes-file "~/Workspace/notes.org"
     ;; Org-agenda files
     org-agenda-files '("~/Workspace/agenda.org")))

  ;; -----------------------------------------------------------------------------------------------
  ;; Better header bullets
  ;; -----------------------------------------------------------------------------------------------

  (use-package org-bullets
    :hook (org-mode . org-bullets-mode))

  ;; -----------------------------------------------------------------------------------------------
  ;; Org-noter.
  ;; -----------------------------------------------------------------------------------------------

  (use-package org-noter
    :defer t)

  )

(provide 'emodule/org)
;;; org.el ends here
