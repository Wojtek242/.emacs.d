;;; init-buffer.el --- Initial buffer displayed at startup
;;
;; Copyright (C) 2018-2019 Wojciech Kozlowski
;;
;; Author: Wojciech Kozlowski <wk@wojciechkozlowski.eu>
;; Created: 2018-11-05
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;; The code in this file was heavily inspired by Spacemacs.
;;
;;; Code:

(defconst init-buffer/banner-file
  (concat (file-name-as-directory user-emacs-directory)
          "init-buffer/blue-robot.png")
  "Location of the banner image to use.")

(defconst init-buffer/name "*GNU Emacs*"
  "Name of the initial buffer.")

(defconst init-buffer/recentf-list-length 10
  "Length used for recentf list.")

(defconst init-buffer/recentf-length-threshold 75
  "Threshold of filename length to apply different centre rules.
If at least one file in recentf is longer than this, the list
will not be centered, but offset by a constant instead.")

(defconst init-buffer/buttons-recentf-offset 20
  "Relative position between the home buffer buttons and startup lists.")

(defvar init-buffer/buttons-line nil
  "Vertical position of the home buffer buttons.
Internal use, do not set this variable.")

(defvar init-buffer/buttons-position nil
  "Horizontal position of the home buffer buttons.
Internal use, do not set this variable.")

(defvar init-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'widget-button-press)

    (define-key map [tab] 'widget-forward)
    (define-key map (kbd "f") 'widget-forward)
    (define-key map (kbd "n") 'widget-forward)

    (define-key map [backtab] 'widget-backward)
    (define-key map (kbd "b") 'widget-backward)
    (define-key map (kbd "p") 'widget-backward)

    (define-key map (kbd "r") 'init-buffer/refresh)
    (define-key map "q" 'quit-window)
    map)
  "Keymap for initial buffer mode.")

(define-derived-mode init-buffer-mode fundamental-mode "Init"
  "Major mode for startup screen."
  :group 'init-buffer
  :syntax-table nil
  :abbrev-table nil
  (setq buffer-read-only t
        truncate-lines t))

(defun init-buffer/insert-banner ()
  "Display an image banner."
  (let* ((spec (create-image init-buffer/banner-file))
         (size (image-size spec))
         (width (car size))
         (left-margin (max 0 (floor (- (window-width) width) 2))))
    (goto-char (point-min))
    (insert "\n")
    (insert (make-string left-margin ?\s))
    (insert-image spec)
    (insert "\n\n")))

(defun init-buffer/insert-buttons ()
  "Create and insert the interactive buttons under the banner."
  (goto-char (point-max))
  (widget-create 'push-button
                 :help-echo "Upgrade ELPA packages to the latest versions."
                 :action (lambda (&rest ignore) (emodule/upgrade))
                 :mouse-face 'highlight
                 :follow-link "\C-m"
                 (propertize "Upgrade" 'face 'font-lock-keyword-face))
  (insert " ")
  (widget-create 'push-button
                 :help-echo
                 "Restore ELPA directory if something got borked."
                 :action (lambda (&rest ignore) (emodule/restore))
                 :mouse-face 'highlight
                 :follow-link "\C-m"
                 (propertize "Restore"
                             'face 'font-lock-keyword-face))
  (let ((len (- (line-end-position)
                (line-beginning-position))))
    (init-buffer/center-line)
    (setq init-buffer/buttons-line (count-lines 1 (point)))
    (setq init-buffer/buttons-position (- (line-end-position)
                                             (line-beginning-position)
                                             len)))
  (insert "\n"))

(defun init-buffer/center-line (&optional real-width)
  "When point is at the end of a line, center it.
REAL-WIDTH: the real width of the line.  If the line contains an image, the size
            of that image will be considered to be 1 by the calculation method
            used in this function.  As a consequence, the caller must calculate
            himself the correct length of the line taking into account the
            images he inserted in it."
  (let* ((width (or real-width (current-column)))
         (margin (max 0 (floor (/ (- (window-width) width) 2)))))
    (beginning-of-line)
    (insert (make-string margin ?\s))
    (end-of-line)))

(defun init-buffer/insert-file-list (list)
  "Insert an interactive list of files in the home buffer.
LIST-DISPLAY-NAME: the displayed title of the list.
LIST: a list of string pathnames made interactive in this function."
  (when (car list)
    (mapc (lambda (el)
            (insert "\n")
            (widget-create 'push-button
                           :action `(lambda (&rest ignore)
                                      (find-file-existing ,el))
                           :mouse-face 'highlight
                           :follow-link "\C-m"
                           :button-prefix ""
                           :button-suffix ""
                           :format "%[%t%]"
                           (abbreviate-file-name el)))
          list)))

(defun init-buffer/subseq (seq start end)
  "Adapted version of `cl-subseq'.
Use `cl-subseq', but accounting for end points greater than the size of the
list.  Return entire list if end is omitted.
SEQ, START and END are the same arguments as for `cl-subseq'"
  (let ((len (length seq)))
    (cl-subseq seq start (and (number-or-marker-p end)
                              (min len end)))))

(defun init-buffer/get-buffer-width ()
  "Return the length of longest line in the current buffer."
  (save-excursion
    (goto-char 0)
    (let ((current-max 0))
      (while (not (eobp))
        (let ((line-length (- (line-end-position) (line-beginning-position))))
          (if (< current-max line-length)
              (setq current-max line-length)))
        (forward-line 1))
      current-max)))

(defun init-buffer/center-recentf ()
  "Centre recentf list after it was inserted."
  (let* ((lists-width (init-buffer/get-buffer-width))
         (margin (max 0 (- init-buffer/buttons-position
                           init-buffer/buttons-recentf-offset)))
         (final-padding (if (< lists-width init-buffer/recentf-length-threshold)
                            (max 0 (floor (/ (- (window-width) lists-width) 2)))
                          margin)))
    (goto-char (point-min))
    (while (not (eobp))
      (beginning-of-line)
      (insert (make-string final-padding ?\s))
      (forward-line))))

(defun init-buffer/insert-recentf ()
  "Insert startup lists in home buffer."
  (goto-char (point-max))
  (save-restriction
    (narrow-to-region (point) (point))
    (recentf-mode)
    (when (init-buffer/insert-file-list
           (init-buffer/subseq recentf-list 0 init-buffer/recentf-list-length)))
    (init-buffer/center-recentf)))

(defun init-buffer/goto-buffer ()
  "Create the initial buffer and switch to it."
  (with-current-buffer (get-buffer-create init-buffer/name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (init-buffer/insert-banner)
      (init-buffer/insert-buttons)
      (init-buffer/insert-recentf))
    (init-buffer-mode)
    (goto-char (point-min))
    (forward-line (- init-buffer/buttons-line 1))
    (move-to-column (- init-buffer/buttons-position 1))
    (current-buffer)))

(add-hook 'window-setup-hook
          (lambda ()
            (add-hook 'window-configuration-change-hook
                      'init-buffer/resize-on-hook)))

(defun init-buffer/resize-on-hook ()
  "Hook run on window resize events to redisplay the home buffer."
  (let ((home-buffer (get-buffer-window init-buffer/name)))
    (when home-buffer
      (with-selected-window home-buffer
        (init-buffer/goto-buffer)))))

(defun init-buffer/refresh ()
  "Force recreation of the spacemacs buffer."
  (interactive)
  (init-buffer/goto-buffer))

(provide 'init-buffer)

;;; init-buffer.el ends here
