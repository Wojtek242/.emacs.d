;;; havoc-common.el --- definition of `Havoc' themes.

;; Copyright (C) 2017-2019 Wojciech Kozlowski

;; Author: Wojciech Kozlowski <wk@wojciechkozlowski.eu>
;; Keywords: faces
;; Compatibility: 24.1
;; Version: 1.0.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; havoc-dark - an adaptation of the underwater theme, a gentle deep blue
;; theme, by Jon-Michael Deldin.

;; havoc-light - an adaptation of the default Emacs theme.

;; Code structure and some other colours taken from the spacemacs theme.

;;; Code:

(defun create-havoc-theme (variant theme-name) ;;    ** DARK **       ** LIGHT **
  (let (
        ;; Common background colours.
        (*bg-1*               (if (eq variant 'dark) "#282c34"        "#FFFFFF"))
        (*bg-2*               (if (eq variant 'dark) "#30343C"        "#ECF6FF"))
        (*bg-3*               (if (eq variant 'dark) "#373B43"        "#D9E2EB"))
        (*bg-4*               (if (eq variant 'dark) "#3F434B"        "#C6CFD6"))
        (*header-bg*          (if (eq variant 'dark) "#495765"        "#E2DAEF"))
        (*region*             (if (eq variant 'dark) "#2D3745"        "#8AC6F2"))

        ;; Common foreground colours.
        (*active-line*        (if (eq variant 'dark) "#3F444A"        "#90AECE"))
        (*normal*             (if (eq variant 'dark) "#DFEFF6"        "#000000"))
        (*normal-standout*    (if (eq variant 'dark) "#8AC6F2"        "#A020F0"))
        (*cursor-block*       (if (eq variant 'dark) "#6785C5"        "#000000"))

        ;; Common heading/highlight colours.
        (*head-1*             (if (eq variant 'dark) "#4F97D7"        "#3A81C3"))
        (*head-1-bg*          (if (eq variant 'dark) "#333377"        "#90AECE"))
        (*head-2*             (if (eq variant 'dark) "#2D9574"        "#2D9574"))
        (*head-3*             (if (eq variant 'dark) "#67B11D"        "#67B11D"))
        (*head-4*             (if (eq variant 'dark) "#AF81F4"        "#AF81F4"))

        ;; Common base colours.
        (*green*              (if (eq variant 'dark) "#00FF00"        "#00FF00"))
        (*cyan*               (if (eq variant 'dark) "#00FFFF"        "#00FFFF"))
        (*red*                (if (eq variant 'dark) "#FF0000"        "#FF0000"))
        (*magenta*            (if (eq variant 'dark) "#FF00FF"        "#FF00FF"))

        ;; Other common colours
        (*constant*           (if (eq variant 'dark) "#A45BAD"        "#5F9EA0"))
        (*highlight-1*        (if (eq variant 'dark) "#3E71A1"        "#DA70D6"))
        (*highlight-2*        (if (eq variant 'dark) "#5BA0EB"        "#228B22"))
        (*info*               (if (eq variant 'dark) "#0D98BA"        "#BC8F8F"))
        (*string*             (if (eq variant 'dark) "#89E14B"        "#BC8F8F"))
        (*name*               (if (eq variant 'dark) "#AF81F4"        "#0000FF"))
        (*tooltip*            (if (eq variant 'dark) "#E1E1E0"        "#000000"))
        (*tooltip-bg*         (if (eq variant 'dark) "#495765"        "#E2DAEF"))

        ;; Common error/success colours.
        (*success*            (if (eq variant 'dark) "#86DC2F"        "#42AE2C"))
        (*warning*            (if (eq variant 'dark) "#FFC0CB"        "#FFC0CB"))
        (*error*              (if (eq variant 'dark) "#C62626"        "#C62626"))

        ;; Common diff colours.
        (*diff-added-bg*      (if (eq variant 'dark) "#336622"        "#DDFFDD"))
        (*diff-added-fg*      (if (eq variant 'dark) "#DDFFDD"        "#22AA22"))
        (*diff-rmvd-bg*       (if (eq variant 'dark) "#663333"        "#FFDDDD"))
        (*diff-rmvd-fg*       (if (eq variant 'dark) "#FFDDDD"        "#AA2222")))

    ;; ------------------------------------------------------------------------
    ;; Base group.
    ;; ------------------------------------------------------------------------
    (let ((*base-bg* *bg-1*)
          (*base-fg* *normal*)

          (*base-cursor-block*       *cursor-block*)
          (*base-fci*                *bg-3*)
          (*base-highlight*          *active-line*)
          (*base-highlight-symbol*   *head-1-bg*)
          (*base-highlight-numbers*  *normal-standout*)
          (*base-info*               *normal-standout*)
          (*base-region*             *region*)
          (*base-volatile-highlight* *bg-2*)

          (*base-line-number*     (if (eq variant 'dark) "#46577C" "#B3B3B3"))
          (*base-line-number-bg*  (if (eq variant 'dark) "#22252c" "#FAFDFF"))
          (*base-mb-prompt*       (if (eq variant 'dark) "#8AC6F2" "#0000FF"))
          (*base-vertical-border* (if (eq variant 'dark) "#0A1721" "#C2C2C2")))

      (custom-theme-set-faces
       theme-name

       ;; ---------------------------------------------------------------------
       ;; Core - background, foreground, and cursor.
       ;; ---------------------------------------------------------------------
       `(default ((t (:background ,*base-bg* :foreground ,*base-fg*))))
       `(cursor ((t (:background ,*base-cursor-block*))))

       ;; ---------------------------------------------------------------------
       ;; Peripherals.
       ;; ---------------------------------------------------------------------
       `(fringe ((t (:background ,*base-bg* :foreground ,*base-fg*))))
       `(line-number ((t (:background ,*base-line-number-bg* :foreground ,*base-line-number*))))
       `(line-number-current-line ((t (:background ,*base-line-number-bg* :foreground ,*base-cursor-block*
                                                   :weight bold))))
       `(minibuffer-prompt ((t (:foreground ,*base-mb-prompt* :weight bold))))
       `(vertical-border ((t (:foreground ,*base-vertical-border*))))

       ;; ---------------------------------------------------------------------
       ;; Region highlighting.
       ;; ---------------------------------------------------------------------
       `(region ((t (:background ,*base-region*))))
       `(vhl/default-face ((t (:background ,*base-volatile-highlight*))))

       ;; ---------------------------------------------------------------------
       ;; Highlights.
       ;; ---------------------------------------------------------------------
       `(match ((t (:background ,*base-highlight*))))
       `(highlight ((t (:background ,*base-highlight*))))
       `(highlight-symbol-face ((t (:background ,*base-highlight-symbol*))))
       `(highlight-numbers-number ((t (:foreground ,*base-highlight-numbers*))))
       `(hl-line ((t (:background ,*base-highlight*))))

       ;; ---------------------------------------------------------------------
       ;; Info and buttons.
       ;; ---------------------------------------------------------------------
       `(button ((t (:foreground ,*base-info* :underline t))))
       `(info-xref ((t (:foreground ,*base-info* :underline t))))
       `(Info-quoted ((t (:foreground ,*base-info*))))

       ;; ---------------------------------------------------------------------
       ;; Fill column line.
       ;; ---------------------------------------------------------------------
       `(fill-column-indicator ((t (:foreground ,*base-fci*))))

       ))

    ;; ------------------------------------------------------------------------
    ;; Mode line and header line.
    ;; ------------------------------------------------------------------------
    (let ((*line-active-bg*   *active-line*)
          (*line-active-fg*   *normal*)
          (*line-perspective* *head-1*)
          (*line-eyebrowse*   *string*)

          (*zoom-window-bg*   (if (eq variant 'dark) "#33455b" "#90AECE"))
          (*line-header-bg*   (if (eq variant 'dark) "#0A1721" "#F2F2F2"))
          (*line-inactive-bg* (if (eq variant 'dark) "#0A1721" "#C9D5E3"))
          (*line-inactive-fg* (if (eq variant 'dark) "#798A9B" "#333333")))

      (custom-theme-set-faces
       theme-name

       ;; ---------------------------------------------------------------------
       ;; Mode line.
       ;; ---------------------------------------------------------------------
       `(mode-line ((t (:background ,*line-active-bg* :foreground ,*line-active-fg*))))
       `(mode-line-inactive ((t (:background ,*line-inactive-bg* :foreground ,*line-inactive-fg* :underline nil))))

       ;; ---------------------------------------------------------------------
       ;; Doom mode line.
       ;; ---------------------------------------------------------------------
       `(doom-modeline-inactive-bar ((t (:inherit mode-line-inactive))))

       ;; ---------------------------------------------------------------------
       ;; Perspective:eyebrowse.
       ;; ---------------------------------------------------------------------
       `(persp-selected-face ((t (:foreground ,*line-perspective* :weight bold))))
       `(eyebrowse-mode-line-active ((t (:foreground ,*line-eyebrowse* :weight bold))))

       ;; ---------------------------------------------------------------------
       ;; Pyvenv virtualenv.
       ;; ---------------------------------------------------------------------
       `(pyvenv-active-face ((t (:foreground ,*line-perspective* :weight bold))))

       ;; ---------------------------------------------------------------------
       ;; Header line.
       ;; ---------------------------------------------------------------------
       `(header-line ((t (:background ,*line-header-bg* :foreground ,*line-active-fg*))))

       )

      (setq zoom-window-mode-line-color *zoom-window-bg*)

      )

    ;; ------------------------------------------------------------------------
    ;; Whitespace.
    ;; ------------------------------------------------------------------------
    (let ((*whitespace-bg* (if (eq variant 'dark) "#2AA1AE" "#A020F0"))
          (*whitespace-fg* (if (eq variant 'dark) "#4E6F91" "#D3D3D3")))

      (custom-theme-set-faces
       theme-name

       `(whitespace-space       ((t (:foreground ,*whitespace-fg*))))
       `(whitespace-indentation ((t (:foreground ,*whitespace-fg*))))
       `(whitespace-trailing    ((t (:background ,*whitespace-bg* :foreground ,*whitespace-fg*))))
       `(trailing-whitespace    ((t (:background ,*whitespace-bg* :foreground ,*whitespace-fg*))))))

    ;; ------------------------------------------------------------------------
    ;; Search highlighting.
    ;; ------------------------------------------------------------------------
    (let ((*search-fail-bg*  *error*)
          (*search-other-bg* *highlight-1*)

          (*search-bg* (if (eq variant 'dark) "#AF81F4" "#AF81F4"))
          (*search-fg* (if (eq variant 'dark) "#E2DAEF" "#333333")))

      (custom-theme-set-faces
       theme-name

       ;; search
       `(isearch ((t (:background ,*search-bg* :foreground ,*search-fg*))))
       `(isearch-fail ((t (:background ,*search-fail-bg*))))
       `(lazy-highlight ((t (:background ,*search-other-bg* :foreground ,*search-fg*))))))

    ;; ------------------------------------------------------------------------
    ;; Parentheses.
    ;; ------------------------------------------------------------------------
    (let ((*paren-match* *success*)
          (*paren-mismatch* *error*)

          (*paren-hlp-1* (if (eq variant 'dark) "#FF6A6A" "#FF6A6A"))
          (*paren-hlp-2* (if (eq variant 'dark) "#EE6363" "#EE6363"))
          (*paren-hlp-3* (if (eq variant 'dark) "#CD5555" "#CD5555"))

          (*paren-rdd-1* (if (eq variant 'dark) "#8AC6F2" "#0000FF"))
          (*paren-rdd-2* (if (eq variant 'dark) "#AF81F4" "#A020F0"))
          (*paren-rdd-3* (if (eq variant 'dark) "#89E14B" "#00BB00"))
          (*paren-rdd-4* (if (eq variant 'dark) "#40E0D0" "#00BBBB"))
          (*paren-rdd-5* (if (eq variant 'dark) "#FFEC99" "#551A8B"))
          (*paren-rdd-6* (if (eq variant 'dark) "#8AC6F2" "#0000FF"))
          (*paren-rdd-7* (if (eq variant 'dark) "#AF81F4" "#A020F0"))
          (*paren-rdd-8* (if (eq variant 'dark) "#89E14B" "#00BB00"))
          (*paren-rdd-9* (if (eq variant 'dark) "#40E0D0" "#00BBBB")))

      (custom-theme-set-faces
       theme-name

       ;; show-paren
       `(show-paren-match ((t (:foreground ,*paren-match* :weight ultra-bold :underline t))))
       `(show-paren-mismatch ((t (:foreground ,*paren-mismatch* :weight ultra-bold :underline t))))

       ;; highlight-parentheses
       `(hl-paren-face ((t (:weight ultra-bold))))

       ;; rainbow delimiters mode
       `(rainbow-delimiters-depth-1-face  ((t (:foreground ,*paren-rdd-1*))))
       `(rainbow-delimiters-depth-2-face  ((t (:foreground ,*paren-rdd-2*))))
       `(rainbow-delimiters-depth-3-face  ((t (:foreground ,*paren-rdd-3*))))
       `(rainbow-delimiters-depth-4-face  ((t (:foreground ,*paren-rdd-4*))))
       `(rainbow-delimiters-depth-5-face  ((t (:foreground ,*paren-rdd-5*))))
       `(rainbow-delimiters-depth-6-face  ((t (:foreground ,*paren-rdd-6*))))
       `(rainbow-delimiters-depth-7-face  ((t (:foreground ,*paren-rdd-7*))))
       `(rainbow-delimiters-depth-8-face  ((t (:foreground ,*paren-rdd-8*))))
       `(rainbow-delimiters-depth-9-face  ((t (:foreground ,*paren-rdd-9*))))

       )

      (setq hl-paren-colors `(,*paren-match*
                              ,*paren-hlp-1*
                              ,*paren-hlp-2*
                              ,*paren-hlp-3*))

      )

    ;; ------------------------------------------------------------------------
    ;; Checker highlights.
    ;; ------------------------------------------------------------------------
    (let ((*check-fic*             *warning*)
          (*flycheck-error-line*   "Red1")
          (*flycheck-warning-line* "DarkOrange")
          (*flycheck-info-line*    "ForestGreen"))

      (custom-theme-set-faces
       theme-name

       ;; fic-mode
       `(fic-face ((t (:foreground ,*check-fic* :weight bold))))

       ;; flycheck
       `(flycheck-error ((t (:underline (:style line :color ,*flycheck-error-line*)))))
       `(flycheck-warning ((t (:underline (:style line :color ,*flycheck-warning-line*)))))
       `(flycheck-info ((t (:underline (:style line :color ,*flycheck-info-line*)))))

       ))

    ;; ------------------------------------------------------------------------
    ;; Font lock.
    ;; ------------------------------------------------------------------------
    (let ((*font-lock-builtin*   *highlight-1*)
          (*font-lock-constant*  *constant*)
          (*font-lock-doc*       *info*)
          (*font-lock-function*  *name*)
          (*font-lock-keyword*   *normal-standout*)
          (*font-lock-negate*    *warning*)
          (*font-lock-preproc*   *normal-standout*)
          (*font-lock-reference* *constant*)
          (*font-lock-string*    *string*)
          (*font-lock-type*      *highlight-2*)
          (*font-lock-variable*  *normal-standout*)
          (*font-lock-warning*   *error*)

          (*font-lock-comment* (if (eq variant 'dark) "#4E6F91" "#B22222"))
          (*font-lock-regexp*  (if (eq variant 'dark) "#EF7760" "#EF7760")))

      (custom-theme-set-faces
       theme-name

       `(font-lock-builtin-face ((t (:foreground ,*font-lock-builtin*))))
       `(font-lock-comment-delimiter-face ((t (:foreground ,*font-lock-comment*))))
       `(font-lock-comment-face ((t (:foreground ,*font-lock-comment*))))
       `(font-lock-constant-face ((t (:foreground ,*font-lock-constant*))))
       `(font-lock-doc-face ((t (:foreground ,*font-lock-doc*))))
       `(font-lock-doc-string-face ((t (:foreground ,*font-lock-doc*))))
       `(font-lock-function-name-face ((t (:foreground ,*font-lock-function* :weight bold))))
       `(font-lock-keyword-face ((t (:foreground ,*font-lock-keyword* :weight bold))))
       `(font-lock-negation-char-face ((t (:foreground ,*font-lock-negate* :weight bold))))
       `(font-lock-preprocessor-face ((t (:foreground ,*font-lock-preproc*))))
       `(font-lock-reference-face ((t (:foreground ,*font-lock-reference*))))
       `(font-lock-regexp-grouping-backslash ((t (:foreground ,*font-lock-regexp*))))
       `(font-lock-regexp-grouping-construct ((t (:foreground ,*font-lock-regexp*))))
       `(font-lock-string-face ((t (:foreground ,*font-lock-string*))))
       `(font-lock-type-face ((t (:foreground ,*font-lock-type* :weight bold))))
       `(font-lock-variable-name-face ((t (:foreground ,*font-lock-variable*))))
       `(font-lock-warning-face ((t (:foreground ,*font-lock-warning* :weight bold))))

       ))

    ;; ------------------------------------------------------------------------
    ;; Company.
    ;; ------------------------------------------------------------------------
    (let ((*company-bg*             *tooltip-bg*)
          (*company-fg*             *tooltip*)

          (*company-annotation*     *normal-standout*)
          (*company-echo-bg*        *normal*)
          (*company-echo-fg*        *bg-1*)
          (*company-scrollbar-bg*   *bg-2*)
          (*company-scrollbar-fg*   *cursor-block*)
          (*company-search*         *active-line*)
          (*company-select-fg*      *normal*)
          (*company-template-field* *region*)

          (*company-select-bg* (if (eq variant 'dark) "#3E71A1" "#AF81F4")))

      (custom-theme-set-faces
       theme-name

       `(company-echo-common ((t (:background ,*company-echo-bg* :foreground ,*company-echo-fg*))))
       `(company-preview ((t (:background ,*company-bg* :foreground ,*company-fg*))))
       `(company-preview-common ((t (:background ,*company-bg* :foreground ,*company-fg*))))
       `(company-preview-search ((t (:background ,*company-search*))))
       `(company-scrollbar-bg ((t (:background ,*company-scrollbar-bg*))))
       `(company-scrollbar-fg ((t (:background ,*company-scrollbar-fg*))))
       `(company-template-field ((t (:background ,*company-template-field*))))
       `(company-tooltip ((t (:background ,*company-bg* :foreground ,*company-fg*))))
       `(company-tooltip-annotation ((t (:foreground ,*company-annotation*))))
       `(company-tooltip-common ((t (:background ,*company-bg* :foreground ,*company-fg*))))
       `(company-tooltip-common-selection ((t (:foreground ,*company-select-fg*))))
       `(company-tooltip-mouse ((t (:background ,*company-select-bg* :foreground ,*company-select-fg*))))
       `(company-tooltip-search ((t (:background ,*company-search*))))
       `(company-tooltip-selection ((t (:background ,*company-select-bg* :foreground ,*company-select-fg*))))

       ))

    ;; ------------------------------------------------------------------------
    ;; Terminal.
    ;; ------------------------------------------------------------------------
    (let ((*term-black*   (if (eq variant 'dark) "#000000" "#000000"))
          (*term-blue*    (if (eq variant 'dark) "#6495ED" "#6495ED"))
          (*term-cyan*    (if (eq variant 'dark) "#00CDCD" "#00CDCD"))
          (*term-green*   (if (eq variant 'dark) "#00CD00" "#00CD00"))
          (*term-magenta* (if (eq variant 'dark) "#CD00CD" "#CD00CD"))
          (*term-red*     (if (eq variant 'dark) "#CD0000" "#CD0000"))
          (*term-white*   (if (eq variant 'dark) "#FFFFFF" "#FFFFFF"))
          (*term-yellow*  (if (eq variant 'dark) "#CDCD00" "#CDCD00")))

      (custom-theme-set-faces
       theme-name

       `(term-color-black   ((t (:background ,*term-black* :foreground ,*term-black*))))
       `(term-color-blue    ((t (:background ,*term-blue* :foreground ,*term-blue*))))
       `(term-color-cyan    ((t (:background ,*term-cyan* :foreground ,*term-cyan*))))
       `(term-color-green   ((t (:background ,*term-green* :foreground ,*term-green*))))
       `(term-color-magenta ((t (:background ,*term-magenta* :foreground ,*term-magenta*))))
       `(term-color-red     ((t (:background ,*term-red* :foreground ,*term-red*))))
       `(term-color-white   ((t (:background ,*term-white* :foreground ,*term-white*))))
       `(term-color-yellow  ((t (:background ,*term-yellow* :foreground ,*term-yellow*))))

       ))

    ;; ------------------------------------------------------------------------
    ;; Org-mode.
    ;; ------------------------------------------------------------------------
    (let ((*org-title*       *name*)
          (*org-1*           *head-1*)
          (*org-2*           *head-2*)
          (*org-3*           *head-3*)
          (*org-4*           *head-4*)
          (*org-checkbox-bg* *bg-1*)
          (*org-checkbox-fg* *normal-standout*)
          (*org-clock-bg*    *info*)
          (*org-clock-fg*    *bg-1*)
          (*org-link*        *normal-standout*)
          (*org-special*     *normal-standout*)

          (*org-date* (if (eq variant 'dark) "#FFCCFF" "#FFCCFF")))

      (custom-theme-set-faces
       theme-name

       `(org-document-title ((t (:foreground ,*org-title* :weight bold :height 1.75 :underline nil))))
       `(org-level-1 ((t (:foreground ,*org-1* :weight bold :height 1.5))))
       `(org-level-2 ((t (:foreground ,*org-2* :weight bold :height 1.25))))
       `(org-level-3 ((t (:foreground ,*org-3* :weight bold))))
       `(org-level-4 ((t (:foreground ,*org-4* :weight bold))))
       `(org-level-5 ((t (:foreground ,*org-1* :weight bold))))
       `(org-level-6 ((t (:foreground ,*org-2* :weight bold))))
       `(org-level-7 ((t (:foreground ,*org-3* :weight bold))))
       `(org-level-8 ((t (:foreground ,*org-4* :weight bold))))

       `(org-checkbox ((t (:background ,*org-checkbox-bg* :foreground ,*org-checkbox-fg* :weight bold))))
       `(org-clock-overlay ((t (:background ,*org-clock-bg* :foreground ,*org-clock-fg*))))
       `(org-date ((t (:foreground ,*org-date* :underline t))))
       `(org-link ((t (:foreground ,*org-link* :underline t))))
       `(org-special-keyword ((t (:foreground ,*org-special*))))

       ))

    ;; ------------------------------------------------------------------------
    ;; Messages.
    ;; ------------------------------------------------------------------------

    (let ((*header-name*    *head-2*)
          (*header-subject* *name*)
          (*header-other*   *head-1*))

      (custom-theme-set-faces
       theme-name

       `(message-header-name ((t (:foreground ,*header-name* :weight bold))))
       `(message-header-subject ((t (:foreground ,*header-subject* :weight bold :height 1.25))))
       `(message-header-other ((t (:foreground ,*header-other* :weight bold))))

       ))

    ;; ------------------------------------------------------------------------
    ;; Elfeed.
    ;; ------------------------------------------------------------------------

    (let ((*search-date* *normal-standout*)
          (*search-feed* *name*)
          (*search-tag*  *normal-standout*))

      (custom-theme-set-faces
       theme-name

       `(elfeed-search-date-face ((t (:foreground ,*search-date*))))
       `(elfeed-search-feed-face ((t (:foreground ,*search-feed*))))
       `(elfeed-search-tag-face ((t (:foreground ,*search-tag* :weight bold))))

       ))

    ;; ------------------------------------------------------------------------
    ;; Diff.
    ;; ------------------------------------------------------------------------
    (let ((*diff-header*        *header-bg*)
          (*diff-refine-fg*     *bg-4*)
          (*diff-added*         *green*)
          (*diff-changed*       *normal-standout*)
          (*diff-removed*       *red*)
          (*diff-hl-added-bg*   *diff-added-bg*)
          (*diff-hl-added-fg*   *green*)
          (*diff-hl-changed-bg* *head-1-bg*)
          (*diff-hl-changed-fg* *head-1*)
          (*diff-hl-removed-bg* *diff-rmvd-bg*)
          (*diff-hl-removed-fg* *red*)

          (*diff-file-header-fg* (if (eq variant 'dark) "#DFEFF6" "#551A8B"))
          (*diff-hunk-bg*        (if (eq variant 'dark) "#364555" "#E2DAEF"))
          (*diff-hunk-fg*        (if (eq variant 'dark) "#E1E1E0" "#000000")))

      (custom-theme-set-faces
       theme-name

       `(diff-header ((t (:background ,*diff-header*))))
       `(diff-file-header ((t (:foreground ,*diff-file-header-fg*))))
       `(diff-hunk-header ((t (:background ,*diff-hunk-bg* :foreground ,*diff-hunk-fg*))))

       `(diff-added ((t (:foreground ,*diff-added*))))
       `(diff-changed ((t (:foreground ,*diff-changed*))))
       `(diff-removed ((t (:foreground ,*diff-removed*))))

       `(diff-indicator-added ((t (:foreground ,*diff-added*))))
       `(diff-indicator-changed ((t (:foreground ,*diff-changed*))))
       `(diff-indicator-removed ((t (:foreground ,*diff-removed*))))

       `(diff-refine-added ((t (:background ,*diff-added* :foreground ,*diff-refine-fg*))))
       `(diff-refine-changed ((t (:background ,*diff-changed* :foreground ,*diff-refine-fg*))))
       `(diff-refine-removed ((t (:background ,*diff-removed* :foreground ,*diff-refine-fg*))))

       `(diff-hl-insert ((t (:background ,*diff-hl-added-bg* :foreground ,*diff-hl-added-fg*))))
       `(diff-hl-change ((t (:background ,*diff-hl-changed-bg* :foreground ,*diff-hl-changed-fg*))))
       `(diff-hl-delete ((t (:background ,*diff-hl-removed-bg* :foreground ,*diff-hl-removed-fg*))))

       ))

    ;; ------------------------------------------------------------------------
    ;; Ediff.
    ;; ------------------------------------------------------------------------
    (let ((*ediff-ancestor-fg*  *head-2*)
          (*ediff-bg*           *bg-2*)
          (*ediff-current-A-bg* *diff-rmvd-bg*)
          (*ediff-current-A-fg* *diff-rmvd-fg*)
          (*ediff-current-B-bg* *diff-added-bg*)
          (*ediff-current-B-fg* *diff-added-fg*)
          (*ediff-current-C-bg* *head-1-bg*)
          (*ediff-current-C-fg* *head-1*)

          (*ediff-ancestor-bg* (if (eq variant 'dark) "#293235" "#293235")))

      (custom-theme-set-faces
       theme-name

       `(ediff-current-diff-Ancestor ((t (:background ,*ediff-ancestor-bg* :foreground ,*ediff-ancestor-fg*))))
       `(ediff-current-diff-A ((t (:background ,*ediff-current-A-bg* :foreground ,*ediff-current-A-fg*))))
       `(ediff-current-diff-B ((t (:background ,*ediff-current-B-bg* :foreground ,*ediff-current-B-fg*))))
       `(ediff-current-diff-C ((t (:background ,*ediff-current-C-bg* :foreground ,*ediff-current-C-fg*))))

       `(ediff-even-diff-Ancestor ((t (:background ,*ediff-bg*))))
       `(ediff-even-diff-A ((t (:background ,*ediff-bg*))))
       `(ediff-even-diff-B ((t (:background ,*ediff-bg*))))
       `(ediff-even-diff-C ((t (:background ,*ediff-bg*))))

       `(ediff-fine-diff-Ancestor ((t (:background nil :weight bold))))
       `(ediff-fine-diff-A ((t (:background nil :weight bold))))
       `(ediff-fine-diff-B ((t (:background nil :weight bold))))
       `(ediff-fine-diff-C ((t (:background nil :weight bold))))

       `(ediff-odd-diff-Ancestor ((t (:background ,*ediff-bg*))))
       `(ediff-odd-diff-A ((t (:background ,*ediff-bg*))))
       `(ediff-odd-diff-B ((t (:background ,*ediff-bg*))))
       `(ediff-odd-diff-C ((t (:background ,*ediff-bg*))))

       ))

    ;; ------------------------------------------------------------------------
    ;; Magit.
    ;; ------------------------------------------------------------------------
    (let ((*magit-blame-bg*        *active-line*)
          (*magit-blame-info*      *string*)
          (*magit-blame-hash*      *name*)
          (*magit-branch-local*    *head-1*)
          (*magit-branch-remote*   *head-2*)
          (*magit-diff-added-bg*   *diff-added-bg*)
          (*magit-diff-added-fg*   *diff-added-fg*)
          (*magit-diff-removed-bg* *diff-rmvd-bg*)
          (*magit-diff-removed-fg* *diff-rmvd-fg*)
          (*magit-heading*         *normal-standout*)
          (*magit-highlight-bg*    *bg-2*)
          (*magit-highlight-fg*    *normal*)
          (*magit-hunk-heading-bg* *tooltip-bg*)
          (*magit-hunk-heading-fg* *tooltip*)
          (*magit-log-author*      *name*)
          (*magit-reflog-add*      *green*)
          (*magit-reflog-change*   *magenta*)
          (*magit-reflog-new*      *head-1*)
          (*magit-reflog-other*    *cyan*)
          (*magit-reflog-remote*   *head-2*)
          (*magit-reflog-reset*    *red*)

          (*magit-err*             *warning*)
          (*magit-ok*              *success*)

          (*magit-blame-fg*           (if (eq variant 'dark) "#FFEC99" "#551A8B"))
          (*magit-diff-added-hl-bg*   (if (eq variant 'dark) "#337733" "#CCEECC"))
          (*magit-diff-added-hl-fg*   (if (eq variant 'dark) "#CCEECC" "#22AA22"))
          (*magit-diff-removed-hl-bg* (if (eq variant 'dark) "#773333" "#EECCCC"))
          (*magit-diff-removed-hl-fg* (if (eq variant 'dark) "#EECCCC" "#AA2222"))
          (*magit-diffstat-added*     (if (eq variant 'dark) "#89E14B" "#22AA22"))
          (*magit-diffstat-removed*   (if (eq variant 'dark) "#C62626" "#AA2222"))
          (*magit-hash*               (if (eq variant 'dark) "#8AC6F2" "#A020F0")))

      (custom-theme-set-faces
       theme-name

       ;; ---------------------------------------------------------------------
       ;; Magit blame.
       ;; ---------------------------------------------------------------------
       `(magit-blame-date ((t :background ,*magit-blame-bg* :foreground ,*magit-blame-info*)))
       `(magit-blame-hash ((t :background ,*magit-blame-bg* :foreground ,*magit-blame-hash*)))
       `(magit-blame-heading ((t :background ,*magit-blame-bg* :foreground ,*magit-blame-info*)))
       `(magit-blame-name ((t :background ,*magit-blame-bg* :foreground ,*magit-blame-fg*)))
       `(magit-blame-summary ((t :background ,*magit-blame-bg* :foreground ,*magit-blame-fg*)))

       ;; ---------------------------------------------------------------------
       ;; Magit branch.
       ;; ---------------------------------------------------------------------
       `(magit-branch-current ((t (:foreground ,*magit-branch-local* :weight bold :box t))))
       `(magit-branch-local ((t (:foreground ,*magit-branch-local* :weight bold))))
       `(magit-branch-remote ((t (:foreground ,*magit-branch-remote* :weight bold))))

       ;; ---------------------------------------------------------------------
       ;; Magit diff.
       ;; ---------------------------------------------------------------------
       `(magit-diff-context-highlight ((t (:background ,*magit-highlight-bg* :foreground ,*magit-highlight-fg*))))
       `(magit-diff-file-heading ((t (:foreground ,*magit-heading*))))
       `(magit-diff-file-heading-highlight ((t (:foreground ,*magit-heading*))))
       `(magit-diff-hunk-heading ((t (:background ,*magit-hunk-heading-bg* :foreground ,*magit-hunk-heading-fg*))))
       `(magit-diff-hunk-heading-highlight ((t (:background ,*magit-hunk-heading-bg* :foreground ,*magit-hunk-heading-fg*))))
       `(magit-diff-added ((t (:background ,*magit-diff-added-bg* :foreground ,*magit-diff-added-fg*))))
       `(magit-diff-added-highlight ((t (:background ,*magit-diff-added-hl-bg* :foreground ,*magit-diff-added-hl-fg*))))
       `(magit-diff-removed ((t (:background ,*magit-diff-removed-bg* :foreground ,*magit-diff-removed-fg*))))
       `(magit-diff-removed-highlight ((t (:background ,*magit-diff-removed-hl-bg* :foreground ,*magit-diff-removed-hl-fg*))))

       ;; ---------------------------------------------------------------------
       ;; Magit diffstat.
       ;; ---------------------------------------------------------------------
       `(magit-diffstat-added ((t (:foreground ,*magit-diffstat-added*))))
       `(magit-diffstat-removed ((t (:foreground ,*magit-diffstat-removed*))))

       ;; ---------------------------------------------------------------------
       ;; Magit log.
       ;; ---------------------------------------------------------------------
       `(magit-log-author ((t (:foreground ,*magit-log-author*))))

       ;; ---------------------------------------------------------------------
       ;; Magit reflog.
       ;; ---------------------------------------------------------------------
       `(magit-reflog-amend ((t (:foreground ,*magit-reflog-change*))))
       `(magit-reflog-checkout ((t (:foreground ,*magit-reflog-new*))))
       `(magit-reflog-cherry-pick ((t (:foreground ,*magit-reflog-add*))))
       `(magit-reflog-commit ((t (:foreground ,*magit-reflog-add*))))
       `(magit-reflog-merge ((t (:foreground ,*magit-reflog-add*))))
       `(magit-reflog-other ((t (:foreground ,*magit-reflog-other*))))
       `(magit-reflog-rebase ((t (:foreground ,*magit-reflog-change*))))
       `(magit-reflog-remote ((t (:foreground ,*magit-reflog-remote*))))
       `(magit-reflog-reset ((t (:foreground ,*magit-reflog-reset*))))

       ;; ---------------------------------------------------------------------
       ;; Magit various.
       ;; ---------------------------------------------------------------------
       `(magit-hash ((t (:foreground ,*magit-hash*))))

       `(magit-process-ng ((t (:foreground ,*magit-err* :weight bold))))
       `(magit-process-ok ((t (:foreground ,*magit-ok* :weight bold))))

       `(magit-section-heading ((t (:foreground ,*magit-heading* :weight bold))))
       `(magit-section-highlight ((t (:background ,*magit-highlight-bg*))))

     ))

    ;; ------------------------------------------------------------------------
    ;; Ivy.
    ;; ------------------------------------------------------------------------
    (let ((*ivy-line*   *active-line*)
          (*ivy-face-2* *head-1*)
          (*ivy-face-3* *head-4*)
          (*ivy-face-4* *head-3*)
          (*ivy-remote* *cyan*))

      (custom-theme-set-faces
       theme-name

       ;; ---------------------------------------------------------------------
       ;; General ivy.
       ;; ---------------------------------------------------------------------
       `(ivy-current-match ((t (:background ,*ivy-line* :weight bold))))
       `(ivy-minibuffer-match-face-1 ((t (:weight bold))))
       `(ivy-minibuffer-match-face-2 ((t (:foreground ,*ivy-face-2* :underline t))))
       `(ivy-minibuffer-match-face-3 ((t (:foreground ,*ivy-face-3* :underline t))))
       `(ivy-minibuffer-match-face-4 ((t (:foreground ,*ivy-face-4* :underline t))))
       `(ivy-remote ((t (:foreground ,*ivy-remote*))))

       ;; ---------------------------------------------------------------------
       ;; Swiper.
       ;; ---------------------------------------------------------------------
       `(swiper-line-face    ((t (:background ,*ivy-line* :weight bold))))
       `(swiper-match-face-1 ((t (:weight bold :underline t))))
       `(swiper-match-face-2 ((t (:foreground ,*ivy-face-2* :underline t))))
       `(swiper-match-face-3 ((t (:foreground ,*ivy-face-3* :underline t))))
       `(swiper-match-face-4 ((t (:foreground ,*ivy-face-4* :underline t))))

     ))

    ;; ------------------------------------------------------------------------
    ;; Helm.
    ;; ------------------------------------------------------------------------
    (let ((*helm-bg*           *bg-1*)
          (*helm-fg-bright*    *normal-standout*)
          (*helm-fg-match*     *head-1*)

          (*helm-header-bg*    *header-bg*)
          (*helm-header-fg*    *normal*)

          (*helm-directory*    *normal-standout*)
          (*helm-exec*         *string*)
          (*helm-line-number*  *highlight-2*)
          (*helm-prefix*       *normal-standout*)
          (*helm-process*      *normal-standout*)
          (*helm-success*      *success*)
          (*helm-symlink*      *cyan*)
          (*helm-warning*      *red*)

          (*helm-select*       *active-line*)
          (*helm-select-line*  *bg-2*)
          (*helm-visible-mark* *bg-3*)

          (*helm-fg*      (if (eq variant 'dark) "#BBD3D5" "#655370"))
          (*helm-fg-dark* (if (eq variant 'dark) "#C56EC3" "#6C4173")))

      (custom-theme-set-faces
       theme-name

       ;; ---------------------------------------------------------------------
       ;; Helm bookmark.
       ;; ---------------------------------------------------------------------
       `(helm-bookmark-directory ((t (:inherit helm-ff-directory))))
       `(helm-bookmark-file ((t (:foreground ,*helm-fg*))))
       `(helm-bookmark-gnus ((t (:foreground ,*helm-fg-dark*))))
       `(helm-bookmark-info ((t (:foreground ,*helm-fg-dark*))))
       `(helm-bookmark-man ((t (:foreground ,*helm-fg-dark*))))
       `(helm-bookmark-w3m ((t (:foreground ,*helm-fg-dark*))))

       ;; ---------------------------------------------------------------------
       ;; Helm buffer.
       ;; ---------------------------------------------------------------------
       `(helm-buffer-directory ((t (:background ,*helm-bg* :foreground ,*helm-fg*))))
       `(helm-buffer-file ((t (:background ,*helm-bg* :foreground ,*helm-fg*))))
       `(helm-buffer-not-saved ((t (:background ,*helm-bg* :foreground ,*helm-fg-dark*))))
       `(helm-buffer-process ((t (:background ,*helm-bg* :foreground ,*helm-process*))))
       `(helm-buffer-saved-out ((t (:background ,*helm-bg* :foreground ,*helm-fg*))))
       `(helm-buffer-size ((t (:background ,*helm-bg* :foreground ,*helm-fg*))))

       ;; ---------------------------------------------------------------------
       ;; Helm ff.
       ;; ---------------------------------------------------------------------
       `(helm-ff-directory ((t (:background ,*helm-bg* :foreground ,*helm-directory* :weight bold))))
       `(helm-ff-dotted-directory ((t (:background ,*helm-bg* :foreground ,*helm-directory* :weight bold))))
       `(helm-ff-dotted-symlink-directory ((t (:background ,*helm-bg* :foreground ,*helm-symlink* :weight bold))))
       `(helm-ff-executable ((t (:background ,*helm-bg* :foreground ,*helm-exec* :weight normal))))
       `(helm-ff-file ((t (:background ,*helm-bg* :foreground ,*helm-fg* :weight normal))))
       `(helm-ff-invalid-symlink ((t (:background ,*helm-bg* :foreground ,*helm-warning* :weight bold))))
       `(helm-ff-prefix ((t (:background ,*helm-prefix* :foreground ,*helm-bg* :weight normal))))
       `(helm-ff-symlink ((t (:background ,*helm-bg* :foreground ,*helm-symlink* :weight bold))))

       ;; ---------------------------------------------------------------------
       ;; Helm grep.
       ;; ---------------------------------------------------------------------
       `(helm-grep-cmd-line ((t (:background ,*helm-bg* :foreground ,*helm-fg*))))
       `(helm-grep-file ((t (:background ,*helm-bg* :foreground ,*helm-fg*))))
       `(helm-grep-finish ((t (:background ,*helm-bg* :foreground ,*helm-fg*))))
       `(helm-grep-lineno ((t (:background ,*helm-bg* :foreground ,*helm-line-number* :weight bold))))
       `(helm-grep-match ((t (:background nil :foreground nil :inherit helm-match))))

       ;; ---------------------------------------------------------------------
       ;; Helm swoop.
       ;; ---------------------------------------------------------------------
       `(helm-swoop-target-line-block-face ((t (:background ,*helm-select* :foreground ,*helm-fg*))))
       `(helm-swoop-target-line-face ((t (:background ,*helm-select*))))
       `(helm-swoop-target-word-face ((t (:background ,*helm-select* :foreground ,*helm-success*))))

       ;; ---------------------------------------------------------------------
       ;; Helm various.
       ;; ---------------------------------------------------------------------
       `(helm-candidate-number ((t (:background ,*helm-select* :foreground ,*helm-fg-bright* :weight bold))))

       `(helm-header ((t (:background ,*helm-bg* :foreground ,*helm-fg* :underline nil :box nil))))
       `(helm-header-line-left-margin ((t (:background ,nil :foreground ,*helm-fg-bright*))))

       `(helm-match ((t (:background ,*helm-bg* :foreground ,*helm-fg-match*))))
       `(helm-match-item ((t (:background ,*helm-bg* :foreground ,*helm-fg-match*))))

       `(helm-moccur-buffer ((t (:background ,*helm-bg* :foreground ,*helm-fg-bright*))))

       `(helm-selection ((t (:background ,*helm-select*))))
       `(helm-selection-line ((t (:background ,*helm-select-line*))))

       `(helm-separator ((t (:background ,*helm-bg* :foreground ,*helm-fg-dark*))))

       `(helm-source-header ((t (:background ,*helm-header-bg* :foreground ,*helm-header-fg* :weight bold))))

       `(helm-time-zone-current ((t (:background ,*helm-bg* :foreground ,*helm-fg-bright*))))
       `(helm-time-zone-home ((t (:background ,*helm-bg* :foreground ,*helm-fg-dark*))))

       `(helm-visible-mark ((t (:background ,*helm-visible-mark* :foreground ,*helm-fg-bright*))))

       ))

    ))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide 'havoc-common)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; havoc-common.el ends here
