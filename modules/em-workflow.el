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

  '(perspective
    eyebrowse)

  )

(defvar em-workflow/perspectives nil)

(defun em-workflow/get-persp-parameters (persp)
  "Return alist of parameters for perspective PERSP.
If the alist did not exist, create one and initialise the values
to nil."
  (assoc persp em-workflow/perspectives))

(defun em-workflow/persp-parameter-cons (param-name persp)
  "Return value of PARAM-NAME for perspective PERSP.
If none was set, returns nil."
  (assoc param-name (em-workflow/get-persp-parameters persp)))

(defun em-workflow/persp-parameter (param-name persp)
  "Return value of PARAM-NAME for perspective PERSP.
If none was set, returns nil."
  (cdr (em-workflow/persp-parameter-cons param-name persp)))

(defun em-workflow/set-persp-parameter (param-name value persp)
  "Set PARAM-NAME to VALUE for perspective PERSP."
  (let ((param (em-workflow/persp-parameter-cons param-name persp)))
    (when (not param)
      (let ((persp-params (em-workflow/get-persp-parameters persp)))
        (when (not persp-params)
          (setq em-workflow/perspectives
                (cons `(,persp . nil) em-workflow/perspectives))
          (setq persp-params (car em-workflow/perspectives)))
        (setcdr persp-params
                (cons `(,param-name . nil) (cdr persp-params)))
        (setq param (cadr persp-params))))
    (setcdr param value)))

(defun em-workflow/get-persp-workspace (&optional persp frame)
  "Get the correct workspace parameters for perspective.
PERSP is the perspective, and defaults to the current
perspective.  FRAME is the frame where the parameters are
expected to be used, and defaults to the current frame."
  (or persp (setq persp (persp-curr)))
  (let ((param-names (if (display-graphic-p frame)
                         '(gui-eyebrowse-window-configs
                           gui-eyebrowse-current-slot
                           gui-eyebrowse-last-slot)
                       '(term-eyebrowse-window-configs
                         term-eyebrowse-current-slot
                         term-eyebrowse-last-slot))))
    (--map (em-workflow/persp-parameter it persp) param-names)))

(defun em-workflow/set-persp-workspace (workspace-params &optional persp frame)
  "Set workspace parameters for perspective.
WORKSPACE-PARAMS should be a list containing 3 elements in
this order:
- window-configs, as returned by (eyebrowse--get 'window-configs)
- current-slot, as returned by (eyebrowse--get 'current-slot)
- last-slot, as returned by (eyebrowse--get 'last-slot)
PERSP is the perspective, and defaults to the current
perspective.  FRAME is the frame where the parameters came from,
and defaults to the current frame.  Each perspective has two sets
of workspace parameters: one set for graphical frames, and one
set for terminal frames."
  (or persp (setq persp (persp-curr)))
  (let ((param-names (if (display-graphic-p frame)
                         '(gui-eyebrowse-window-configs
                           gui-eyebrowse-current-slot
                           gui-eyebrowse-last-slot)
                       '(term-eyebrowse-window-configs
                         term-eyebrowse-current-slot
                         term-eyebrowse-last-slot))))
    (--zip-with (em-workflow/set-persp-parameter it other persp)
                param-names workspace-params)))

(defun em-workflow/load-eyebrowse-for-perspective (&optional frame)
  "Load an eyebrowse workspace according to a perspective's parameters.
FRAME's perspective is the perspective that is considered,
defaulting to the current frame's perspective.  If the
perspective doesn't have a workspace, create one."
  (let* ((workspace-params (em-workflow/get-persp-workspace (persp-curr frame) frame))
         (window-configs (nth 0 workspace-params))
         (current-slot (nth 1 workspace-params))
         (last-slot (nth 2 workspace-params)))
    (if window-configs
        (progn
          (eyebrowse--set 'window-configs window-configs frame)
          (eyebrowse--set 'current-slot current-slot frame)
          (eyebrowse--set 'last-slot last-slot frame)
          (eyebrowse--load-window-config current-slot))
      (eyebrowse--set 'window-configs nil frame)
      (eyebrowse-init frame)
      (em-workflow/save-eyebrowse-for-perspective frame))))

(defun em-workflow/update-eyebrowse-for-perspective (&rest _args)
  "Update and save current frame's eyebrowse workspace to its perspective."
  (let* ((current-slot (eyebrowse--get 'current-slot))
         (current-tag (nth 2 (assoc current-slot (eyebrowse--get 'window-configs)))))
    (eyebrowse--update-window-config-element
     (eyebrowse--current-window-config current-slot current-tag)))
  (em-workflow/save-eyebrowse-for-perspective))

(defun em-workflow/save-eyebrowse-for-perspective (&optional frame)
  "Save FRAME's eyebrowse workspace to FRAME's perspective.
FRAME defaults to the current frame."
  (em-workflow/set-persp-workspace (list (eyebrowse--get 'window-configs frame)
                                        (eyebrowse--get 'current-slot frame)
                                        (eyebrowse--get 'last-slot frame))
                                  (persp-curr frame)
                                  frame))

;;; Configuration:

(defun emodule/em-workflow-init ()
  "Initialise the `em-workflow' module."

  ;; --------------------------------------------------------------------------
  ;; Enable `perspective'.
  ;; --------------------------------------------------------------------------

  (use-package perspective
    :config
    (persp-mode))

  (use-package eyebrowse
    :after perspective
    :config
    (eyebrowse-mode)

    (add-hook 'persp-before-switch-hook
              #'em-workflow/update-eyebrowse-for-perspective)
    (add-hook 'eyebrowse-post-window-switch-hook
              #'em-workflow/save-eyebrowse-for-perspective)
    (add-hook 'persp-activated-hook
              #'em-workflow/load-eyebrowse-for-perspective)

    )

  )

(provide 'em-workflow)
;;; em-workflow.el ends here
