;;; havoc-common.el --- definition of `Havoc' themes.

;; Copyright (C) 2017 Wojciech Kozlowski

;; Author: Wojciech Kozlowski <wk@wojciechkozlowski.eu>
;; Keywords: faces
;; Compatibility: 24.1
;; Version: 0.0.1

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

;; havoc-dark - an adaptation of the underwater theme by Jon-Michael Deldin, a
;; gentle deep blue theme.

;; havoc-light - an adaptation of the default Emacs theme.

;;; Code:

(defun create-havoc-theme (variant theme-name) ;;    ** DARK **       ** LIGHT **
  (let ((*active-line*        (if (eq variant 'dark) "#3f444a"        "#90AECE"))
        (*base*               (if (eq variant 'dark) "#BBD3D5"        "#655370"))
        (*bg-1*               (if (eq variant 'dark) "#282c34"        "#FFFFFF"))
        (*bg-2*               (if (eq variant 'dark) "#30343C"        "#ECF6FF"))
        (*bg-3*               (if (eq variant 'dark) "#373B43"        "#D9E2EB"))
        (*bg-4*               (if (eq variant 'dark) "#3F434B"        "#C6CFD6"))
        (*comments*           (if (eq variant 'dark) "#4E6F91"        "#B22222"))
        (*comments-bg*        (if (eq variant 'dark) "#102235"        "#FFFFFF"))
        (*comp*               (if (eq variant 'dark) "#C56EC3"        "#6C4173"))
        (*constant*           (if (eq variant 'dark) "#A45BAD"        "#5F9EA0"))
        (*current-line*       (if (eq variant 'dark) "#3f444a"        "#B4EEB4"))
        (*cursor-block*       (if (eq variant 'dark) "#6785C5"        "#000000"))
        (*cursor-underscore*  (if (eq variant 'dark) "#FFFAAA"        "#B22222"))
        (*fg-standout*        (if (eq variant 'dark) "#FFEC99"        "#551A8B"))
        (*fringe*             (if (eq variant 'dark) "#0A1721"        "#F2F2F2"))
        (*head1*              (if (eq variant 'dark) "#4F97D7"        "#3A81C3"))
        (*head2*              (if (eq variant 'dark) "#2D9574"        "#2D9574"))
        (*head3*              (if (eq variant 'dark) "#67B11D"        "#67B11D"))
        (*head4*              (if (eq variant 'dark) "#AF81F4"        "#AF81F4"))
        (*header-bg*          (if (eq variant 'dark) "#495765"        "#E2DAEF"))
        (*keywords*           (if (eq variant 'dark) "#8AC6F2"        "#A020F0"))
        (*light-purple*       (if (eq variant 'dark) "#FFCCFF"        "#FFCCFF"))
        (*line-number*        (if (eq variant 'dark) "#46577c"        "#B3B3B3"))
        (*line-number-bg*     (if (eq variant 'dark) "#071017"        "#FAFDFF"))
        (*mat*                (if (eq variant 'dark) "#86DC2F"        "#BA2F59"))
        (*mb-prompt*          (if (eq variant 'dark) "#8AC6F2"        "#0000FF"))
        (*method-declaration* (if (eq variant 'dark) "#AF81F4"        "#0000FF"))
        (*mode-line-bg*       (if (eq variant 'dark) "#0A1721"        "#C9D5E3"))
        (*mode-line-fg*       (if (eq variant 'dark) "#798A9B"        "#333333"))
        (*normal*             (if (eq variant 'dark) "#DFEFF6"        "#000000"))
        (*number*             (if (eq variant 'dark) "#96DEFA"        "#AF81F4"))
        (*operators*          (if (eq variant 'dark) "#3E71A1"        "#DA70D6"))
        (*regexp*             (if (eq variant 'dark) "#EF7760"        "#EF7760"))
        (*regexp-alternate*   (if (eq variant 'dark) "#FFFF00"        "#FFFF00"))
        (*regexp-alternate-2* (if (eq variant 'dark) "#B18A3D"        "#B18A3D"))
        (*search-fg*          (if (eq variant 'dark) "#E2DAEF"        "#333333"))
        (*search-bg*          (if (eq variant 'dark) "#AF81F4"        "#AF81F4"))
        (*string*             (if (eq variant 'dark) "#89E14B"        "#BC8F8F"))
        (*success*            (if (eq variant 'dark) "#86DC2F"        "#42AE2C"))
        (*ttip*               (if (eq variant 'dark) "#E1E1E0"        "#000000"))
        (*ttip-sl*            (if (eq variant 'dark) "#3E71A1"        "#AF81F4"))
        (*ttip-bg*            (if (eq variant 'dark) "#495765"        "#E2DAEF"))
        (*type*               (if (eq variant 'dark) "#5BA0EB"        "#228B22"))
        (*variable*           (if (eq variant 'dark) "#8AC6F2"        "#B8860B"))
        (*vertical-border*    (if (eq variant 'dark) "#0A1721"        "#C2C2C2"))
        (*visual-selection*   (if (eq variant 'dark) "#2D3745"        "#8AC6F2"))
        (*warning*            (if (eq variant 'dark) "#C62626"        "#C62626"))
        (*whitespace-bg*      (if (eq variant 'dark) "#2AA1AE"        "#A020F0"))
        (*whitespace-fg*      (if (eq variant 'dark) "#4E6F91"        "#D3D3D3"))

        ;; Rainbow delimiters
        (*rdd-1*              (if (eq variant 'dark) "#8AC6F2"        "#0000FF"))
        (*rdd-2*              (if (eq variant 'dark) "#AF81F4"        "#A020F0"))
        (*rdd-3*              (if (eq variant 'dark) "#89E14B"        "#00BB00"))
        (*rdd-4*              (if (eq variant 'dark) "#40E0D0"        "#00BBBB"))
        (*rdd-5*              (if (eq variant 'dark) "#FFEC99"        "#551A8B"))
        (*rdd-6*              (if (eq variant 'dark) "#8AC6F2"        "#0000FF"))
        (*rdd-7*              (if (eq variant 'dark) "#AF81F4"        "#A020F0"))
        (*rdd-8*              (if (eq variant 'dark) "#89E14B"        "#00BB00"))
        (*rdd-9*              (if (eq variant 'dark) "#40E0D0"        "#00BBBB"))

        ;; Highlight parentheses colours
        (*hlp-1*              (if (eq variant 'dark) "#FF6A6A"        "#FF6A6A"))
        (*hlp-2*              (if (eq variant 'dark) "#EE6363"        "#EE6363"))
        (*hlp-3*              (if (eq variant 'dark) "#CD5555"        "#CD5555"))

        ;; Diff colors
        (*diff-added-bg*      (if (eq variant 'dark) "#336622"        "#DDFFDD"))
        (*diff-added-fg*      (if (eq variant 'dark) "#DDFFDD"        "#22AA22"))
        (*diff-added-hl-bg*   (if (eq variant 'dark) "#337733"        "#CCEECC"))
        (*diff-added-hl-fg*   (if (eq variant 'dark) "#CCEECC"        "#22AA22"))
        (*diff-base-bg*       (if (eq variant 'dark) "#666622"        "#FFFFCC"))
        (*diff-base-fg*       (if (eq variant 'dark) "#FFFFCC"        "#AAAA11"))
        (*diff-base-hl-bg*    (if (eq variant 'dark) "#777722"        "#EEEEBB"))
        (*diff-base-hl-fg*    (if (eq variant 'dark) "#EEEEBB"        "#AAAA11"))
        (*diff-rmvd-bg*       (if (eq variant 'dark) "#663333"        "#FFDDDD"))
        (*diff-rmvd-fg*       (if (eq variant 'dark) "#FFDDDD"        "#AA2222"))
        (*diff-rmvd-hl-bg*    (if (eq variant 'dark) "#773333"        "#EECCCC"))
        (*diff-rmvd-hl-fg*    (if (eq variant 'dark) "#EECCCC"        "#AA2222"))
        (*diffstat-added*     (if (eq variant 'dark) "#89E14B"        "#22AA22"))
        (*diffstat-rmvd*      (if (eq variant 'dark) "#C62626"        "#AA2222"))
        (*hash*               (if (eq variant 'dark) "#8AC6F2"        "#A020F0"))
        (*diff-file-hdr-fg*   (if (eq variant 'dark) "#DFEFF6"        "#551A8B"))
        (*diff-hunk-hdr-bg*   (if (eq variant 'dark) "#364555"        "#E2DAEF"))
        (*diff-hunk-hdr-fg*   (if (eq variant 'dark) "#E1E1E0"        "#000000"))

        ;; Terminal colors
        (*term-black*         (if (eq variant 'dark) "#000000"        "#000000"))
        (*term-blue*          (if (eq variant 'dark) "#6495ED"        "#6495ED"))
        (*term-cyan*          (if (eq variant 'dark) "#00CDCD"        "#00CDCD"))
        (*term-green*         (if (eq variant 'dark) "#00CD00"        "#00CD00"))
        (*term-magenta*       (if (eq variant 'dark) "#CD00CD"        "#CD00CD"))
        (*term-red*           (if (eq variant 'dark) "#CD0000"        "#CD0000"))
        (*term-white*         (if (eq variant 'dark) "#FFFFFF"        "#FFFFFF"))
        (*term-yellow*        (if (eq variant 'dark) "#CDCD00"        "#CDCD00"))

        ;; Base colors
        (*aqua*               (if (eq variant 'dark) "#2D9574"        "#2D9574"))
        (*aqua-bg*            (if (eq variant 'dark) "#293235"        "#293235"))
        (*green*              (if (eq variant 'dark) "#00FF00"        "#00FF00"))
        (*green-bg*           (if (eq variant 'dark) "#293235"        "#293235"))
        (*green-bg-s*         (if (eq variant 'dark) "#29422D"        "#29422D"))
        (*cyan*               (if (eq variant 'dark) "#00FFFF"        "#00FFFF"))
        (*red*                (if (eq variant 'dark) "#FF0000"        "#FF0000"))
        (*red-bg*             (if (eq variant 'dark) "#3C2A2C"        "#3C2A2C"))
        (*red-bg-s*           (if (eq variant 'dark) "#512E31"        "#512E31"))
        (*blue*               (if (eq variant 'dark) "#4F97D7"        "#4F97D7"))
        (*blue-bg*            (if (eq variant 'dark) "#333377"        "#293239"))
        (*magenta*            (if (eq variant 'dark) "#FF00FF"        "#FF00FF"))
        (*yellow*             (if (eq variant 'dark) "#FFFF00"        "#FFFF00"))
        (*yellow-bg*          (if (eq variant 'dark) "#32322C"        "#32322C")))

    (custom-theme-set-faces
     theme-name

     `(bold ((t (:bold t))))
     `(button ((t (:foreground, *keywords* :underline t))))
     `(default ((t (:background, *bg-1* :foreground, *normal*))))
     `(header-line ((t (:background, *mode-line-bg* :foreground, *normal*)))) ;; info header
     `(highlight ((t (:background, *current-line*))))
     `(highlight-face ((t (:background, *current-line*))))
     `(hl-line ((t (:background, *current-line*))))
     `(info-xref ((t (:foreground, *keywords* :underline t))))
     `(Info-quoted ((t (:foreground, *keywords*))))
     `(match ((t (:background, *current-line*))))
     `(region ((t (:background, *visual-selection*))))
     `(underline ((nil (:underline t))))
     `(vhl/default-face ((t (:background, *bg-2*))))

     ;; font-lock
     `(font-lock-builtin-face ((t (:foreground, *operators*))))
     `(font-lock-comment-delimiter-face ((t (:foreground, *comments*))))
     `(font-lock-comment-face ((t (:foreground, *comments*))))
     `(font-lock-constant-face ((t (:foreground, *constant*))))
     `(font-lock-doc-face ((t (:foreground, *string*))))
     `(font-lock-doc-string-face ((t (:foreground, *string*))))
     `(font-lock-function-name-face ((t (:foreground, *method-declaration* :inherit bold))))
     `(font-lock-keyword-face ((t (:inherit bold :foreground, *keywords*))))
     `(font-lock-negation-char-face ((t (:foreground, *warning*))))
     `(font-lock-preprocessor-face ((t (:foreground, *keywords*))))
     `(font-lock-reference-face ((t (:foreground, *constant*))))
     `(font-lock-regexp-grouping-backslash ((t (:foreground, *regexp*))))
     `(font-lock-regexp-grouping-construct ((t (:foreground, *regexp*))))
     `(font-lock-string-face ((t (:foreground, *string*))))
     `(font-lock-type-face ((t (:foreground, *type* :inherit bold))))
     `(font-lock-variable-name-face ((t (:foreground, *variable*))))
     `(font-lock-warning-face ((t (:foreground, *warning*))))

     ;; Highglight
     `(highlight-numbers-number ((t (:foreground, *keywords*))))
     `(highlight-symbol-face ((t (:background, *blue-bg*))))

     ;; whitespace
     `(whitespace-space       ((t (:foreground ,*whitespace-fg*))))
     `(whitespace-indentation ((t (:foreground ,*whitespace-fg*))))
     `(whitespace-trailing    ((t (:background ,*whitespace-bg* :foreground ,*whitespace-fg*))))
     `(trailing-whitespace    ((t (:background ,*whitespace-bg* :foreground ,*whitespace-fg*))))

     ;; GUI
     `(fringe ((t (:foreground, *normal* :background, *bg-1*))))
     `(header-line ((t (:background, *fringe* :foreground, *normal*)))) ;; info header
     `(linum ((t (:foreground, *line-number* :background, *line-number-bg* :underline nil :bold nil :italic nil))))
     `(minibuffer-prompt ((t (:inherit bold :foreground, *mb-prompt*))))
     `(mode-line ((t (:background, *active-line* :foreground, *normal*))))
     `(mode-line-inactive ((t (:background, *mode-line-bg* :foreground, *mode-line-fg* :underline nil))))
     `(cursor ((t (:background, *cursor-block*))))
     `(text-cursor ((t (:background, *cursor-underscore*))))
     `(vertical-border ((t (:foreground, *vertical-border*)))) ;; between splits

     ;; search
     `(isearch ((t (:background, *search-bg* :foreground, *search-fg*))))
     `(isearch-fail ((t (:background, *warning*))))
     `(lazy-highlight ((t (:background, *operators* :foreground, *search-fg*))))

     ;; erb/rhtml-mode
     `(erb-out-delim-face ((t (:foreground, *regexp*))))

     ;; enh-ruby-mode
     `(enh-ruby-op-face ((t (:foreground, *operators*))))
     `(enh-ruby-regexp-delimiter-face ((t (:foreground, *regexp*))))
     `(enh-ruby-string-delimiter-face ((t (:foreground, *normal*))))

     ;; show-paren
     `(show-paren-mismatch ((t (:foreground, *warning* :weight ultra-bold :underline t))))
     `(show-paren-match ((t (:foreground, *success* :weight ultra-bold :underline t))))

     ;; highlight-parentheses
     `(hl-paren-face ((t (:weight ultra-bold))))

     ;; rainbow delimiters mode
     `(rainbow-delimiters-depth-1-face  ((t (:foreground ,*rdd-1*))))
     `(rainbow-delimiters-depth-2-face  ((t (:foreground ,*rdd-2*))))
     `(rainbow-delimiters-depth-3-face  ((t (:foreground ,*rdd-3*))))
     `(rainbow-delimiters-depth-4-face  ((t (:foreground ,*rdd-4*))))
     `(rainbow-delimiters-depth-5-face  ((t (:foreground ,*rdd-5*))))
     `(rainbow-delimiters-depth-6-face  ((t (:foreground ,*rdd-6*))))
     `(rainbow-delimiters-depth-7-face  ((t (:foreground ,*rdd-7*))))
     `(rainbow-delimiters-depth-8-face  ((t (:foreground ,*rdd-8*))))
     `(rainbow-delimiters-depth-9-face  ((t (:foreground ,*rdd-9*))))

     ;; company
     `(company-echo-common ((t (:background ,*normal* :foreground ,*bg-1*))))
     `(company-preview ((t (:background ,*ttip-bg* :foreground ,*ttip*))))
     `(company-preview-common ((t (:background ,*ttip-bg* :foreground ,*normal*))))
     `(company-preview-search ((t (:inherit match))))
     `(company-scrollbar-bg ((t (:background ,*bg-2*))))
     `(company-scrollbar-fg ((t (:background ,*cursor-block*))))
     `(company-template-field ((t (:inherit region))))
     `(company-tooltip ((t (:background ,*ttip-bg* :foreground ,*ttip*))))
     `(company-tooltip-annotation ((t (:foreground ,*keywords*))))
     `(company-tooltip-common ((t (:background ,*ttip-bg* :foreground ,*normal*))))
     `(company-tooltip-common-selection ((t (:foreground ,*normal*))))
     `(company-tooltip-mouse ((t (:inherit *highlight*))))
     `(company-tooltip-search ((t (:inherit match))))
     `(company-tooltip-selection ((t (:background ,*ttip-sl* :foreground ,*normal*))))

     ;; diff
     `(diff-header            ((t (:background ,*header-bg*))))
     `(diff-file-header       ((t (:foreground ,*diff-file-hdr-fg*))))
     `(diff-hunk-header       ((t (:background ,*diff-hunk-hdr-bg* :foreground ,*diff-hunk-hdr-fg*))))
     `(diff-added             ((t (:foreground ,*green*))))
     `(diff-changed           ((t :foreground ,*keywords*)))
     `(diff-removed           ((t (:foreground ,*red*))))
     `(diff-indicator-added   ((t :foreground ,*green*)))
     `(diff-indicator-changed ((t :foreground ,*keywords*)))
     `(diff-indicator-removed ((t :foreground ,*red*)))
     `(diff-refine-added      ((t :background ,*green* :foreground ,*bg-4*)))
     `(diff-refine-changed    ((t :background ,*keywords* :foreground ,*bg-4*)))
     `(diff-refine-removed    ((t :background ,*red* :foreground ,*bg-4*)))

     ;; diff-hl
     `(diff-hl-change ((t :background ,*blue-bg* :foreground ,*blue*)))
     `(diff-hl-delete ((t :background ,*diff-rmvd-bg* :foreground ,*red*)))
     `(diff-hl-insert ((t :background ,*diff-added-bg* :foreground ,*green*)))

     ;; ediff
     `(ediff-current-diff-A        ((t (:background ,*diff-rmvd-bg* :foreground ,*diff-rmvd-fg*))))
     `(ediff-current-diff-Ancestor ((t (:background ,*aqua-bg* :foreground ,*aqua*))))
     `(ediff-current-diff-B        ((t (:background ,*diff-added-bg* :foreground ,*diff-added-fg*))))
     `(ediff-current-diff-C        ((t (:background ,*blue-bg* :foreground ,*blue*))))
     `(ediff-even-diff-A           ((t (:background ,*bg-2*))))
     `(ediff-even-diff-Ancestor    ((t (:background ,*bg-2*))))
     `(ediff-even-diff-B           ((t (:background ,*bg-2*))))
     `(ediff-even-diff-C           ((t (:background ,*bg-2*))))
     `(ediff-fine-diff-A           ((t (:background nil :inherit bold))))
     `(ediff-fine-diff-Ancestor    ((t (:background nil :inherit bold))))
     `(ediff-fine-diff-B           ((t (:background nil :inherit bold))))
     `(ediff-fine-diff-C           ((t (:background nil :inherit bold))))
     `(ediff-odd-diff-A            ((t (:background ,*bg-2*))))
     `(ediff-odd-diff-Ancestor     ((t (:background ,*bg-2*))))
     `(ediff-odd-diff-B            ((t (:background ,*bg-2*))))
     `(ediff-odd-diff-C            ((t (:background ,*bg-2*))))

     ;; magit
     `(magit-blame-culprit ((t :background ,*active-line* :foreground ,*fg-standout*)))
     `(magit-blame-date    ((t :background ,*active-line* :foreground ,*string*)))
     `(magit-blame-hash    ((t :background ,*active-line* :foreground ,*method-declaration*)))
     `(magit-blame-header  ((t :background ,*active-line* :foreground ,*string*)))
     `(magit-blame-heading ((t :background ,*active-line* :foreground ,*string*)))
     `(magit-blame-name    ((t :background ,*active-line* :foreground ,*fg-standout*)))
     `(magit-blame-sha1    ((t :background ,*active-line* :foreground ,*method-declaration*)))
     `(magit-blame-subject ((t :background ,*active-line* :foreground ,*fg-standout*)))
     `(magit-blame-summary ((t :background ,*active-line* :foreground ,*fg-standout*)))
     `(magit-blame-time    ((t :background ,*active-line* :foreground ,*string*)))
     `(magit-branch ((t (:foreground ,*constant* :inherit bold))))
     `(magit-branch-current ((t (:foreground ,*blue* :inherit bold :box t))))
     `(magit-branch-local ((t (:foreground ,*blue* :inherit bold))))
     `(magit-branch-remote ((t (:foreground ,*aqua* :inherit bold))))
     `(magit-diff-context-highlight ((t (:background ,*bg-2* :foreground ,*normal*))))
     `(magit-diff-file-header ((t (:foreground ,*keywords*))))
     `(magit-diff-file-heading ((t (:foreground ,*keywords*))))
     `(magit-diff-file-heading-highlight ((t (:foreground ,*keywords*))))
     `(magit-diff-hunk-header ((t (:background ,*ttip-bg* :foreground ,*ttip*))))
     `(magit-diff-hunk-heading ((t (:background ,*ttip-bg* :foreground ,*ttip*))))
     `(magit-diff-hunk-heading-highlight ((t (:background ,*ttip-bg* :foreground ,*ttip*))))
     `(magit-diff-added ((t (:background ,*diff-added-bg* :foreground ,*diff-added-fg*))))
     `(magit-diff-added-highlight ((t (:background ,*diff-added-hl-bg* :foreground ,*diff-added-hl-fg*))))
     `(magit-diff-base ((t (:background ,*diff-base-bg* :foreground ,*diff-base-fg*))))
     `(magit-diff-base-highlight ((t (:background ,*diff-base-hl-bg* :foreground ,*diff-base-hl-fg*))))
     `(magit-diff-removed ((t (:background ,*diff-rmvd-bg* :foreground ,*diff-rmvd-fg*))))
     `(magit-diff-removed-highlight ((t (:background ,*diff-rmvd-hl-bg* :foreground ,*diff-rmvd-hl-fg*))))
     `(magit-diffstat-added ((t (:foreground ,*diffstat-added*))))
     `(magit-diffstat-removed ((t (:foreground ,*diffstat-rmvd*))))
     `(magit-hash ((t (:foreground ,*hash*))))
     `(magit-hunk-heading ((t (:background ,*bg-3*))))
     `(magit-hunk-heading-highlight ((t (:background ,*bg-3*))))
     `(magit-item-highlight ((t :background ,*bg-2*)))
     `(magit-log-author ((t (:foreground ,*method-declaration*))))
     `(magit-log-head-label-head ((t (:background ,*fg-standout* :foreground ,*bg-1* :inherit bold))))
     `(magit-log-head-label-local ((t (:background ,*keywords* :foreground ,*bg-1* :inherit bold))))
     `(magit-log-head-label-remote ((t (:background ,*success* :foreground ,*bg-1* :inherit bold))))
     `(magit-log-head-label-tags ((t (:background ,*magenta* :foreground ,*bg-1* :inherit bold))))
     `(magit-log-head-label-wip ((t (:background ,*cyan* :foreground ,*bg-1* :inherit bold))))
     `(magit-log-sha1 ((t (:foreground ,*string*))))
     `(magit-process-ng ((t (:foreground ,*warning* :inherit bold))))
     `(magit-process-ok ((t (:foreground ,*method-declaration* :inherit bold))))
     `(magit-reflog-amend ((t (:foreground ,*magenta*))))
     `(magit-reflog-checkout ((t (:foreground ,*blue*))))
     `(magit-reflog-cherry-pick ((t (:foreground ,*string*))))
     `(magit-reflog-commit ((t (:foreground ,*string*))))
     `(magit-reflog-merge ((t (:foreground ,*string*))))
     `(magit-reflog-other ((t (:foreground ,*cyan*))))
     `(magit-reflog-rebase ((t (:foreground ,*magenta*))))
     `(magit-reflog-remote ((t (:foreground ,*cyan*))))
     `(magit-reflog-reset ((t (:foreground ,*red*))))
     `(magit-section-heading ((t (:foreground ,*keywords* :inherit bold))))
     `(magit-section-highlight ((t (:background ,*bg-2*))))
     `(magit-section-title ((t (:background ,*bg-1* :foreground ,*keywords* :inherit bold))))

     ;; helm
     `(helm-bookmark-directory ((t (:inherit helm-ff-directory))))
     `(helm-bookmark-file ((t (:foreground ,*base*))))
     `(helm-bookmark-gnus ((t (:foreground ,*comp*))))
     `(helm-bookmark-info ((t (:foreground ,*comp*))))
     `(helm-bookmark-man ((t (:foreground ,*comp*))))
     `(helm-bookmark-w3m ((t (:foreground ,*comp*))))
     `(helm-buffer-directory ((t (:foreground ,*base* :background ,*bg-1*))))
     `(helm-buffer-file ((t (:foreground ,*base* :background ,*bg-1*))))
     `(helm-buffer-not-saved ((t (:foreground ,*comp* :background ,*bg-1*))))
     `(helm-buffer-process ((t (:foreground ,*keywords* :background ,*bg-1*))))
     `(helm-buffer-saved-out ((t (:foreground ,*base* :background ,*bg-1*))))
     `(helm-buffer-size ((t (:foreground ,*base* :background ,*bg-1*))))
     `(helm-candidate-number ((t (:background ,*active-line* :foreground ,*keywords* :inherit bold))))
     `(helm-ff-directory ((t (:foreground ,*keywords* :background ,*bg-1* :inherit bold))))
     `(helm-ff-dotted-directory ((t (:foreground ,*keywords* :background ,*bg-1* :inherit bold))))
     `(helm-ff-dotted-symlink-directory ((t (:foreground ,*cyan* :background ,*bg-1* :inherit bold))))
     `(helm-ff-executable ((t (:foreground ,*success* :background ,*bg-1* :weight normal))))
     `(helm-ff-file ((t (:foreground ,*base* :background ,*bg-1* :weight normal))))
     `(helm-ff-invalid-symlink ((t (:foreground ,*red* :background ,*bg-1* :inherit bold))))
     `(helm-ff-prefix ((t (:foreground ,*bg-1* :background ,*keywords* :weight normal))))
     `(helm-ff-symlink ((t (:foreground ,*cyan* :background ,*bg-1* :inherit bold))))
     `(helm-grep-cmd-line ((t (:foreground ,*base* :background ,*bg-1*))))
     `(helm-grep-file ((t (:foreground ,*base* :background ,*bg-1*))))
     `(helm-grep-finish ((t (:foreground ,*base* :background ,*bg-1*))))
     `(helm-grep-lineno ((t (:foreground ,*type* :background ,*bg-1* :inherit bold))))
     `(helm-grep-match ((t (:foreground nil :background nil :inherit helm-match))))
     `(helm-header ((t (:foreground ,*base* :background ,*bg-1* :underline nil :box nil))))
     `(helm-header-line-left-margin ((t (:foreground ,*keywords* :background ,nil))))
     `(helm-match ((t (:background ,*bg-1* :foreground ,*head1*))))
     `(helm-match-item ((t (:background ,*bg-1* :foreground ,*head1*))))
     `(helm-moccur-buffer ((t (:foreground ,*variable* :background ,*bg-1*))))
     `(helm-selection ((t (:background ,*current-line*))))
     `(helm-selection-line ((t (:background ,*bg-2*))))
     `(helm-separator ((t (:foreground ,*comp* :background ,*bg-1*))))
     `(helm-source-header ((t (:background ,*header-bg* :foreground ,*normal* :inherit bold))))
     `(helm-time-zone-current ((t (:foreground ,*keywords* :background ,*bg-1*))))
     `(helm-time-zone-home ((t (:foreground ,*comp* :background ,*bg-1*))))
     `(helm-visible-mark ((t (:foreground ,*keywords* :background ,*bg-3*))))

     ;; helm-swoop
     `(helm-swoop-target-line-block-face ((t (:foreground ,*base* :background ,*current-line*))))
     `(helm-swoop-target-line-face ((t (:background ,*current-line*))))
     `(helm-swoop-target-word-face ((t (:background ,*current-line* :foreground ,*mat*))))

     ;; swiper
     `(swiper-line-face    ((t (:background ,*current-line* :inherit bold))))
     `(swiper-match-face-1 ((t (:inherit bold :underline t))))
     `(swiper-match-face-2 ((t (:foreground ,*head1* :underline t))))
     `(swiper-match-face-3 ((t (:foreground ,*head4* :underline t))))
     `(swiper-match-face-4 ((t (:foreground ,*head3* :underline t))))

     ;; ivy
     `(ivy-current-match ((t (:background ,*current-line* :inherit bold))))
     `(ivy-minibuffer-match-face-1 ((t (:inherit bold))))
     `(ivy-minibuffer-match-face-2 ((t (:foreground ,*head1* :underline t))))
     `(ivy-minibuffer-match-face-3 ((t (:foreground ,*head4* :underline t))))
     `(ivy-minibuffer-match-face-4 ((t (:foreground ,*head3* :underline t))))
     `(ivy-remote ((t (:foreground ,*cyan*))))

     ;; terminal colours
     `(term-color-black   ((t (:foreground ,*term-black* :background ,*term-black*))))
     `(term-color-blue    ((t (:foreground ,*term-blue* :background ,*term-blue*))))
     `(term-color-cyan    ((t (:foreground ,*term-cyan* :background ,*term-cyan*))))
     `(term-color-green   ((t (:foreground ,*term-green* :background ,*term-green*))))
     `(term-color-magenta ((t (:foreground ,*term-magenta* :background ,*term-magenta*))))
     `(term-color-red     ((t (:foreground ,*term-red* :background ,*term-red*))))
     `(term-color-white   ((t (:foreground ,*term-white* :background ,*term-white*))))
     `(term-color-yellow  ((t (:foreground ,*term-yellow* :background ,*term-yellow*))))

     ;; fic-mode
     `(fic-face ((t (:foreground ,*warning* :weight bold))))

     ;; org-mode
     `(org-level-8 ((t (:weight bold :foreground ,*head4*))))
     `(org-level-7 ((t (:weight bold :foreground ,*head3*))))
     `(org-level-6 ((t (:weight bold :foreground ,*head2*))))
     `(org-level-5 ((t (:weight bold :foreground ,*head1*))))
     `(org-level-4 ((t (:weight bold :foreground ,*head4*))))
     `(org-level-3 ((t (:weight bold :foreground ,*head3*))))
     `(org-level-2 ((t (:weight bold :height 1.25 :foreground ,*head2*))))
     `(org-level-1 ((t (:weight bold :height 1.5 :foreground ,*head1*))))
     `(org-document-title ((t (:weight bold :height 1.75 :underline nil :foreground ,*method-declaration*))))
     `(org-date ((t (:foreground, *light-purple* :underline t))))
     `(org-special-keyword ((t (:foreground, *variable*))))
     `(org-link ((t (:foreground, *keywords* :underline t))))
     `(org-checkbox ((t (:foreground, *keywords* :background, *bg-1* :bold t))))
     `(org-clock-overlay ((t (:foreground, *mode-line-bg* :background, *string*))))

     ;; flycheck
     `(flycheck-error ((t (:underline (:style line :color "Red1")))))
     `(flycheck-warning ((t (:underline (:style line :color "DarkOrange")))))
     `(flycheck-info ((t (:underline (:style line :color "ForestGreen")))))

     ;; perspective
     `(persp-selected-face ((t (:weight bold :foreground ,*head1*))))

     ;; doom modeline
     `(doom-modeline-eyebrowse ((t (:weight bold :foreground ,*string*))))
     `(doom-modeline-inactive-bar ((t (:inherit mode-line-inactive))))

     )

    ;; Define values for colours that don't use faces.
    (setq fci-rule-color *bg-3*)
    (setq hl-paren-colors `(,*success*
                            ,*hlp-1*
                            ,*hlp-2*
                            ,*hlp-3*))

    ;; Define any faces not already defined in a package.  This is necessary
    ;; for Emacs to recognise these faces if they are being set above.
    (defface doom-modeline-eyebrowse '((t ()))
      "The face used to highlight the current eyebrowse window in the doom modeline.")

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
