;;; zoom-window.el --- Zoom window like tmux -*- lexical-binding: t; -*-

;; Copyright (C) 2019 by Wojciech Kozlowski

;; Author: Wojciech Kozlowski <wk@wojciechkozlowski.eu>
;;         based on `github.com/syohex/emacs-zoom-window'
;;         by Syohei YOSHIDA <syohex@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; zoom-window.el provides functions which zooms specific window in frame and
;; restore original window configuration. This is like tmux's zoom/unzoom
;; features.

;;; Code:

(require 'cl-lib)

(defgroup zoom-window nil
  "Zoom window like tmux"
  :group 'windows)

(defcustom zoom-window-mode-line-color "green"
  "Color of mode-line when zoom-window is enabled."
  :type 'string)

(defvar zoom-window--control-block nil)
(defvar zoom-window--window-configuration (make-hash-table :test #'equal))
(defvar zoom-window--orig-color nil)

(defun zoom-window--save-mode-line-color ()
  "Save the original mode line color."
  (setq zoom-window--orig-color (face-background 'mode-line)))

(defun zoom-window--save-buffers ()
  "Save the current buffer list."
  (let ((buffers (cl-loop for window in (window-list)
                          collect (window-buffer window))))
    (set-frame-parameter (window-frame nil) 'zoom-window-buffers buffers)))

(defun zoom-window--get-buffers ()
  "Get the saved buffer list."
  (frame-parameter (window-frame nil) 'zoom-window-buffers))

(defun zoom-window--restore-mode-line-face ()
  "Restore the original mode line face."
  (let ((color zoom-window--orig-color))
    (set-face-background 'mode-line color (window-frame nil))))

(defun zoom-window--configuration-key ()
  "Get the window configuration key."
  (let ((parent-id (frame-parameter (window-frame nil) 'parent-id)))
    (if (not parent-id)
        :zoom-window ;; not support multiple frame
      (format ":zoom-window-%d" parent-id))))

(defun zoom-window--save-window-configuration ()
  "Save the window configuration."
  (let ((key (zoom-window--configuration-key))
        (window-conf (list (current-window-configuration) (point-marker))))
    (puthash key window-conf zoom-window--window-configuration)))

(defun zoom-window--restore-window-configuration ()
  "Restore the window configuration."
  (let* ((key (zoom-window--configuration-key))
         (window-context (gethash key zoom-window--window-configuration 'not-found)))
    (when (eq window-context 'not-found)
      (error "Window configuration is not found"))
    (let ((window-conf (cl-first window-context))
          (marker (cl-second window-context)))
      (set-window-configuration window-conf)
      (when (marker-buffer marker)
        (goto-char marker))
      (remhash key zoom-window--window-configuration))))

(defun zoom-window--toggle-enabled ()
  "Toggle the enabled flag."
  (let* ((curframe (window-frame nil))
         (status (frame-parameter curframe 'zoom-window-enabled)))
    (set-frame-parameter curframe 'zoom-window-enabled (not status))))

(defun zoom-window--enable-p ()
  "Return t if zoom is enabled."
  (frame-parameter (window-frame nil) 'zoom-window-enabled))

(defsubst zoom-window--goto-line (line)
  "Go to the given LINE in the current buffer."
  (goto-char (point-min))
  (forward-line (1- line)))

(defun zoom-window--do-unzoom ()
  "Unzoom window."
  (let ((current-line (line-number-at-pos))
        (current-column (current-column))
        (current-buf (current-buffer)))
    (zoom-window--restore-mode-line-face)
    (zoom-window--restore-window-configuration)
    (unless (string= (buffer-name current-buf) (buffer-name))
      (switch-to-buffer current-buf))
    (zoom-window--goto-line current-line)
    (move-to-column current-column)))

;;;###autoload
(defun zoom-window-zoom ()
  "Zoom/un-zoom window."
  (interactive)
  (let ((enabled (zoom-window--enable-p))
        (curframe (window-frame nil)))
    (if (and (one-window-p) (not enabled))
        (message "There is only one window!!")
      (if enabled
          (with-demoted-errors "Warning: %S"
            (zoom-window--do-unzoom))
        (zoom-window--save-mode-line-color)
        (zoom-window--save-buffers)
        (zoom-window--save-window-configuration)
        (delete-other-windows)
        (set-face-background 'mode-line zoom-window-mode-line-color curframe))
      (force-mode-line-update)
      (zoom-window--toggle-enabled))))

(defun zoom-window-next ()
  "Switch to next buffer which is in zoomed workspace."
  (interactive)
  (let* ((buffers (zoom-window--get-buffers))
         (targets (member (current-buffer) buffers)))
    (if targets
        (if (cdr targets)
            (switch-to-buffer (cadr targets))
          (switch-to-buffer (car buffers)))
      (switch-to-buffer (car buffers)))))

(provide 'zoom-window)
;;; zoom-window.el ends here
