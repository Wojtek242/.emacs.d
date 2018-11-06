;;; initial-buffer.el --- Initial buffer displayed at startup
;;
;; Copyright (C) 2018 Wojciech Kozlowski
;;
;; Author: Wojciech Kozlowski <wk@wojciechkozlowski.eu>
;; Created: 5 Nov 2018
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

(defconst initial-buffer/banner-file "~/.emacs.d/initial-buffer/blue-robot.png"
  "Location of the banner image to use.")

(defconst initial-buffer/name "*GNU Emacs*"
  "Name of the initial buffer.")

(defconst initial-buffer/recentf-list-length 10
  "Length used for recentf list.")

(defconst initial-buffer/recentf-length-threshold 75
  "Threshold of filename length to apply different centre rules.
If at least one file in recentf is longer than this, the list
will not be centered, but offset by a constant instead.")

(defconst initial-buffer/buttons-recentf-offset 20
  "Relative position between the home buffer buttons and startup lists.")

(defvar initial-buffer/buttons-line nil
  "Vertical position of the home buffer buttons.
Internal use, do not set this variable.")

(defvar initial-buffer/buttons-position nil
  "Horizontal position of the home buffer buttons.
Internal use, do not set this variable.")

(defvar initial-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'widget-button-press)

    (define-key map [tab] 'widget-forward)
    (define-key map (kbd "f") 'widget-forward)
    (define-key map (kbd "n") 'widget-forward)

    (define-key map [backtab] 'widget-backward)
    (define-key map (kbd "b") 'widget-backward)
    (define-key map (kbd "p") 'widget-backward)

    (define-key map (kbd "r") 'initial-buffer/refresh)
    (define-key map "q" 'quit-window)
    map)
  "Keymap for initial buffer mode.")

(define-derived-mode initial-buffer-mode fundamental-mode "Initial buffer"
  "Major mode for startup screen."
  :group 'initial-buffer
  :syntax-table nil
  :abbrev-table nil
  (setq buffer-read-only t
        truncate-lines t))

(defun initial-buffer/insert-banner ()
  "Display an image banner."
  (let* ((spec (create-image initial-buffer/banner-file))
         (size (image-size spec))
         (width (car size))
         (left-margin (max 0 (floor (- (window-width) width) 2))))
    (goto-char (point-min))
    (insert "\n")
    (insert (make-string left-margin ?\s))
    (insert-image spec)
    (insert "\n\n")))

(defun initial-buffer/insert-buttons ()
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
    (initial-buffer/center-line)
    (setq initial-buffer/buttons-line (count-lines 1 (point)))
    (setq initial-buffer/buttons-position (- (line-end-position)
                                             (line-beginning-position)
                                             len)))
  (insert "\n"))

(defun initial-buffer/center-line (&optional real-width)
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

(defun initial-buffer/insert-file-list (list)
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

(defun initial-buffer/subseq (seq start end)
  "Adapted version of `cl-subseq'.
Use `cl-subseq', but accounting for end points greater than the size of the
list.  Return entire list if end is omitted.
SEQ, START and END are the same arguments as for `cl-subseq'"
  (let ((len (length seq)))
    (cl-subseq seq start (and (number-or-marker-p end)
                              (min len end)))))

(defun initial-buffer/get-buffer-width ()
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

(defun initial-buffer/center-recentf ()
  "Centre recentf list after it was inserted."
  (let* ((lists-width (initial-buffer/get-buffer-width))
         (margin (max 0 (- initial-buffer/buttons-position
                           initial-buffer/buttons-recentf-offset)))
         (final-padding (if (< lists-width initial-buffer/recentf-length-threshold)
                            (max 0 (floor (/ (- (window-width) lists-width) 2)))
                          margin)))
    (goto-char (point-min))
    (while (not (eobp))
      (beginning-of-line)
      (insert (make-string final-padding ?\s))
      (forward-line))))

(defun initial-buffer/insert-recentf ()
  "Insert startup lists in home buffer."
  (goto-char (point-max))
  (save-restriction
    (narrow-to-region (point) (point))
    (recentf-mode)
    (when (initial-buffer/insert-file-list
           (initial-buffer/subseq recentf-list 0 initial-buffer/recentf-list-length)))
    (initial-buffer/center-recentf)))

(defun initial-buffer/goto-buffer ()
  "Create the initial buffer and switch to it."
  (with-current-buffer (get-buffer-create initial-buffer/name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (initial-buffer/insert-banner)
      (initial-buffer/insert-buttons)
      (initial-buffer/insert-recentf))
    (initial-buffer-mode)
    (goto-char (point-min))
    (forward-line (- initial-buffer/buttons-line 1))
    (move-to-column (- initial-buffer/buttons-position 1))
    (current-buffer)))

(add-hook 'window-setup-hook
          (lambda ()
            (add-hook 'window-configuration-change-hook
                      'initial-buffer/resize-on-hook)))

(defun initial-buffer/resize-on-hook ()
  "Hook run on window resize events to redisplay the home buffer."
  (let ((home-buffer (get-buffer-window initial-buffer/name)))
    (when home-buffer
      (with-selected-window home-buffer
        (initial-buffer/goto-buffer)))))

(defun initial-buffer/refresh ()
  "Force recreation of the spacemacs buffer."
  (interactive)
  (initial-buffer/goto-buffer))

(provide 'initial-buffer)

;;; initial-buffer.el ends here
