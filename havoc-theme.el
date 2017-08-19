;;; havoc-theme.el --- A gentle, deep blue color theme

;; Copyright (C) 2017 Wojciech Kozlowski

;; Author: Wojciech Kozlowski <wojciech.kozlowski@vivaldi.net>
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

;; This an adaptation of the underwater theme by Jon-Michael Deldin

;;; Code:

(deftheme havoc "Adaptation of underwater Emacs theme")

(let ((*bg-1*               "#102235")
      (*bg-2*               "#233445")
      (*bg-3*               "#364555")
      (*bg-4*               "#495765")
      (*brown*              "#E64")
      (*comments*           "#4E6F91")
      (*comments-bg*        "#102235")
      (*constant*           "#A45BAD")
      (*current-line*       "#18374f")
      (*cursor-block*       "#6785c5")
      (*cursor-underscore*  "#FFFAAA")
      (*fringe*             "#0A1721")
      (*keywords*           "#8AC6F2")
      (*light-purple*       "#FFCCFF")
      (*line-number*        "#2F577C")
      (*method-declaration* "#AF81F4")
      (*mode-line-bg*       "#0A1721")
      (*mode-line-fg*       "#8EAFD1")
      (*mode-line-inactive* "#4E6F91")
      (*normal*             "#DFEFF6")
      (*number*             "#96DEFA")
      (*operators*          "#3E71A1")
      (*parens*             "magenta")
      (*red-light*          "#FFB6B0")
      (*regexp*             "#EF7760")
      (*regexp-alternate*   "#FF0")
      (*regexp-alternate-2* "#B18A3D")
      (*search-fg*          "#E2DAEF")
      (*search-bg*          "#AF81F4")
      (*string*             "#89E14B")
      (*success*            "#86dc2f")
      (*ttip*               "#e1e1e0")
      (*ttip-sl*            "#005577")
      (*ttip-bg*            "#495765")
      (*type*               "#5BA0EB")
      (*variable*           "#8AC6F2")
      (*vertical-border*    "#0A1721")
      (*visual-selection*   "#262D51")
      (*warning*            "#C62626")

      ;; Rainbow delimiters
      (*rdd-1*              "#7EB8E3")
      (*rdd-2*              "#73AAD4")
      (*rdd-3*              "#679CC5")
      (*rdd-4*              "#5C8EB7")
      (*rdd-5*              "#5180A8")
      (*rdd-6*              "#457299")
      (*rdd-7*              "#3A648A")
      (*rdd-8*              "#2F577C")
      (*rdd-9*              "#30597E")

      ;; Magit colors
      (*diff-added-bg*      "#336622")
      (*diff-added-fg*      "#ddffdd")
      (*diff-added-hl-bg*   "#337733")
      (*diff-added-hl-fg*   "#cceecc")
      (*diff-base-bg*       "#666622")
      (*diff-base-fg*       "#ffffcc")
      (*diff-base-hl-bg*    "#777722")
      (*diff-base-hl-fg*    "#eeeebb")
      (*diff-rmvd-bg*       "#663333")
      (*diff-rmvd-fg*       "#ffdddd")
      (*diff-rmvd-hl-bg*    "#773333")
      (*diff-rmvd-hl-fg*    "#eecccc")

      ;; Base colors
      (*aqua*                "#2d9574")
      (*aqua-bg*             "#293235")
      (*green*               "green")
      (*green-bg*            "#293235")
      (*green-bg-s*          "#29422d")
      (*cyan*                "cyan")
      (*cyan-bg*             "#003355")
      (*red*                 "red")
      (*red-bg*              "#3c2a2c")
      (*red-bg-s*            "#512e31")
      (*blue*                "#4f97d7")
      (*blue-bg*             "#293239")
      (*magenta*             "magenta")
      (*yellow*              "yellow")
      (*yellow-bg*           "#32322c"))

  (custom-theme-set-faces
   'havoc

   `(bold ((t (:bold t))))
   `(button ((t (:foreground, *keywords* :underline t))))
   `(default ((t (:background, *bg-1* :foreground, *normal*))))
   `(header-line ((t (:background, *mode-line-bg* :foreground, *normal*)))) ;; info header
   `(highlight ((t (:background, *current-line*))))
   `(highlight-face ((t (:background, *current-line*))))
   `(hl-line ((t (:background, *current-line* :underline t))))
   `(info-xref ((t (:foreground, *keywords* :underline t))))
   `(region ((t (:background, *visual-selection*))))
   `(underline ((nil (:underline t))))

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

   ;; GUI
   `(fringe ((t (:foreground, *normal* :background, *fringe*))))
   `(header-line ((t (:background, *fringe* :foreground, *normal*)))) ;; info header
   `(linum ((t (:foreground, *line-number* :background, *vertical-border*))))
   `(minibuffer-prompt ((t (:inherit bold :foreground, *variable*))))
   `(mode-line ((t (:background, *mode-line-bg* :foreground, *mode-line-fg*))))
   `(mode-line-inactive ((t (:background, *mode-line-bg* :foreground, *mode-line-inactive*))))
   `(cursor ((t (:background, *cursor-block*))))
   `(text-cursor ((t (:background, *cursor-underscore*))))
   `(vertical-border ((t (:foreground, *vertical-border*)))) ;; between splits

   ;; show-paren
   `(show-paren-mismatch ((t (:background, *warning* :foreground, *normal* :weight bold))))
   `(show-paren-match ((t (:background, *bg-1* :foreground, *parens* :weight bold))))

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

   ;; org-mode
   `(org-date ((t (:foreground, *light-purple* :underline t))))
   `(org-level-1 ((t (:foreground, *string*))))
   `(org-special-keyword ((t (:foreground, *variable*))))
   `(org-link ((t (:foreground, *keywords* :underline t))))
   `(org-checkbox ((t (:foreground, *keywords* :background, *bg-1* :bold t))))
   `(org-clock-overlay ((t (:foreground, *mode-line-bg* :background, *string*))))

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

   ;;;;; magit
   `(magit-blame-culprit ((t :background ,*cyan-bg* :foreground ,*yellow*)))
   `(magit-blame-date    ((t :background ,*cyan-bg* :foreground ,*green*)))
   `(magit-blame-hash    ((t :background ,*cyan-bg* :foreground ,*method-declaration*)))
   `(magit-blame-header  ((t :background ,*cyan-bg* :foreground ,*green*)))
   `(magit-blame-heading ((t :background ,*cyan-bg* :foreground ,*green*)))
   `(magit-blame-name    ((t :background ,*cyan-bg* :foreground ,*yellow*)))
   `(magit-blame-sha1    ((t :background ,*cyan-bg* :foreground ,*method-declaration*)))
   `(magit-blame-subject ((t :background ,*cyan-bg* :foreground ,*yellow*)))
   `(magit-blame-summary ((t :background ,*cyan-bg* :foreground ,*yellow*)))
   `(magit-blame-time    ((t :background ,*cyan-bg* :foreground ,*green*)))
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
   `(magit-diffstat-added ((t (:foreground ,*green*))))
   `(magit-diffstat-removed ((t (:foreground ,*red*))))
   `(magit-hash ((t (:foreground ,*variable*))))
   `(magit-hunk-heading ((t (:background ,*bg-3*))))
   `(magit-hunk-heading-highlight ((t (:background ,*bg-3*))))
   `(magit-item-highlight ((t :background ,*bg-2*)))
   `(magit-log-author ((t (:foreground ,*method-declaration*))))
   `(magit-log-head-label-head ((t (:background ,*yellow* :foreground ,*bg-1* :inherit bold))))
   `(magit-log-head-label-local ((t (:background ,*keywords* :foreground ,*bg-1* :inherit bold))))
   `(magit-log-head-label-remote ((t (:background ,*success* :foreground ,*bg-1* :inherit bold))))
   `(magit-log-head-label-tags ((t (:background ,*magenta* :foreground ,*bg-1* :inherit bold))))
   `(magit-log-head-label-wip ((t (:background ,*cyan* :foreground ,*bg-1* :inherit bold))))
   `(magit-log-sha1 ((t (:foreground ,*string*))))
   `(magit-process-ng ((t (:foreground ,*warning* :inherit bold))))
   `(magit-process-ok ((t (:foreground ,*method-declaration* :inherit bold))))
   `(magit-reflog-amend ((t (:foreground ,*magenta*))))
   `(magit-reflog-checkout ((t (:foreground ,*blue*))))
   `(magit-reflog-cherry-pick ((t (:foreground ,*green*))))
   `(magit-reflog-commit ((t (:foreground ,*green*))))
   `(magit-reflog-merge ((t (:foreground ,*green*))))
   `(magit-reflog-other ((t (:foreground ,*cyan*))))
   `(magit-reflog-rebase ((t (:foreground ,*magenta*))))
   `(magit-reflog-remote ((t (:foreground ,*cyan*))))
   `(magit-reflog-reset ((t (:foreground ,*red*))))
   `(magit-section-heading ((t (:foreground ,*keywords* :inherit bold))))
   `(magit-section-highlight ((t (:background ,*bg-2*))))
   `(magit-section-title ((t (:background ,*bg-1* :foreground ,*keywords* :inherit bold))))

   ))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

;; Local Variables:
;; no-byte-compile: t
;; End:

(provide-theme 'havoc)
;;; havoc-theme.el ends here
