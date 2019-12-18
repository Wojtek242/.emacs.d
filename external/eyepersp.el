;;; eyepersp.el --- Isolate `eyebrowse' windows in perspectives.
;;
;; Copyright (C) 2019 Wojciech Kozlowski
;;
;; Author: Wojciech Kozlowski <wk@wojciechkozlowski.eu>
;; Created: 2019-12-18
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Be default `perspective' and `eyebrowse' do not know about each other which
;; means that window configurations are saved across perspectives. This is not
;; only undesirable, but also problematic, because perspectives isolate
;; buffers, but window configurations do not. This module resolves this by
;; creating an isolated set of `eyebrowse' configurations for each perspective.
;;
;;; License: GPLv3

;;; Code:

(require 'perspective)
(require 'eyebrowse)

(defvar eyepersp/perspectives nil)

(defun eyepersp/get-persp-parameters (persp)
  "Return alist of parameters for perspective PERSP."
  (assoc persp eyepersp/perspectives))

(defun eyepersp/delete-persp-parameters (persp)
  "Delete PERSP state."
  (assoc-delete-all persp eyepersp/perspectives))

(defun eyepersp/persp-parameter-cons (param-name persp)
  "Return the cons for PARAM-NAME for perspective PERSP.
If none was set, returns nil."
  (assoc param-name (eyepersp/get-persp-parameters persp)))

(defun eyepersp/persp-parameter (param-name persp)
  "Return value of PARAM-NAME for perspective PERSP.
If none was set, returns nil."
  (cdr (eyepersp/persp-parameter-cons param-name persp)))

(defun eyepersp/set-persp-parameter (param-name value persp)
  "Set PARAM-NAME to VALUE for perspective PERSP."
  (let ((param (eyepersp/persp-parameter-cons param-name persp)))
    (when (not param)
      (let ((persp-params (eyepersp/get-persp-parameters persp)))
        (when (not persp-params)
          (setq eyepersp/perspectives
                (cons `(,persp . nil) eyepersp/perspectives))
          (setq persp-params (car eyepersp/perspectives)))
        (setcdr persp-params
                (cons `(,param-name . nil) (cdr persp-params)))
        (setq param (cadr persp-params))))
    (setcdr param value)))

(defun eyepersp/get-persp-workspace (&optional persp)
  "Get the correct workspace parameters for perspective.
PERSP is the perspective, and defaults to the current
perspective."
  (or persp (setq persp (persp-curr)))
  (let ((param-names '(eyepersp-eyebrowse-window-configs
                       eyepersp-eyebrowse-current-slot
                       eyepersp-eyebrowse-last-slot)))
    (--map (eyepersp/persp-parameter it persp) param-names)))

(defun eyepersp/set-persp-workspace (workspace-params &optional persp)
  "Set workspace parameters for perspective.
WORKSPACE-PARAMS should be a list containing 3 elements in
this order:
 - window-configs, as returned by (eyebrowse--get 'window-configs)
 - current-slot, as returned by (eyebrowse--get 'current-slot)
 - last-slot, as returned by (eyebrowse--get 'last-slot)
PERSP is the perspective, and defaults to the current
perspective."
  (or persp (setq persp (persp-curr)))
  (let ((param-names '(eyepersp-eyebrowse-window-configs
                       eyepersp-eyebrowse-current-slot
                       eyepersp-eyebrowse-last-slot)))
    (--zip-with (eyepersp/set-persp-parameter it other persp)
                param-names workspace-params)))

(defun eyepersp/load-eyebrowse-for-perspective ()
  "Load an eyebrowse workspace according to a perspective's parameters.
If the perspective doesn't have a workspace, create one."
  (let* ((workspace-params (eyepersp/get-persp-workspace))
         (window-configs (nth 0 workspace-params))
         (current-slot (nth 1 workspace-params))
         (last-slot (nth 2 workspace-params)))
    (if window-configs
        (progn
          (eyebrowse--set 'window-configs window-configs)
          (eyebrowse--set 'current-slot current-slot)
          (eyebrowse--set 'last-slot last-slot)
          (eyebrowse--load-window-config current-slot))
      (eyebrowse--set 'window-configs nil)
      (eyebrowse-init)
      (eyepersp/save-eyebrowse-for-perspective))))

(defun eyepersp/update-eyebrowse-for-perspective (&rest _args)
  "Update and save current frame's eyebrowse workspace to its perspective."
  (let* ((current-slot (eyebrowse--get 'current-slot))
         (current-tag (nth 2 (assoc current-slot
                                    (eyebrowse--get 'window-configs)))))
    (eyebrowse--update-window-config-element
     (eyebrowse--current-window-config current-slot current-tag)))
  (eyepersp/save-eyebrowse-for-perspective))

(defun eyepersp/save-eyebrowse-for-perspective ()
  "Save FRAME's eyebrowse workspace to FRAME's perspective.
FRAME defaults to the current frame."
  (eyepersp/set-persp-workspace (list (eyebrowse--get 'window-configs)
                                      (eyebrowse--get 'current-slot)
                                      (eyebrowse--get 'last-slot))
                                (persp-curr)))

(add-hook 'persp-before-switch-hook
          #'eyepersp/update-eyebrowse-for-perspective)
(add-hook 'eyebrowse-post-window-switch-hook
          #'eyepersp/save-eyebrowse-for-perspective)
(add-hook 'persp-activated-hook
          #'eyepersp/load-eyebrowse-for-perspective)
(add-hook 'persp-killed-hook
          (lambda () (eyepersp/delete-persp-parameters (persp-curr))))

(provide 'eyepersp)
;;; eyepersp.el ends here
