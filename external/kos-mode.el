;;; kos-mode.el --- Summary
;; Copyright (C) 2015  Fabian Kantereit
;; Author: Fabian Kantereit
;; Keywords: kOS, KerboScript, KSP, Kerbal SpaceProgram
;;; Commentary:
;;
;;
;;; Code:

;;;;;;;;;;;;;;;
;; Mode hook ;;
;;;;;;;;;;;;;;;
(defvar kos-mode-hook nil)

;;;;;;;;;;;;;;
;; Mode map ;;
;;;;;;;;;;;;;;
(defvar kos-mode-map
  (let ((map (make-keymap)))
    map)
  "Keymap for kOS major mode.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; autoload mode for ks files ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ks\\'" . kos-mode))

;;;;;;;;;;;;;;;;;;;;;
;; Language Syntax ;;
;;;;;;;;;;;;;;;;;;;;;
(defconst kos-font-lock-keywords-1
  (list
   ;; SHIP, STAGE, TIME, ENCOUNTER, CONFIG and TARGET
   '("\\<\\(\\(SHIP\\|STAGE\\|TARGET\\|TIME\\|ENCOUNTER\\|CONFIG\\):\\(\\w*:?\\)*\\)\\>" . font-lock-keyword-face)
   ;; ALT and ETA
   '("\\<\\(ALT:\\(APOAPSIS\\|PERIAPSIS\\|RADAR\\)\\|ETA:\\(APOAPSIS\\|PERIAPSIS\\|TRANSITION\\)\\)\\>" . font-lock-keyword-face)
   ;; GENERAL
   `(,(regexp-opt '("UP" "PROGRAGE" "RETROGRADE" "FACING" "MAXTHRUST" "VELOCITY" "GEOPOSITION" "THROTTLE" "STEERING" "STAGE" "SHIP" "HEADING" "LATITUDE" "LONGITUDE" "NORTH" "BODY" "ANGULARMOMENTUM" "ANGULARVEL" "ANGULARVELOCITY" "COMMRANGE" "MASS" "VERTICALSPEED" "SURFACESPEED" "AIRSPEED" "VESSELNAME" "ALTITUDE" "APOAPSIS" "PERIAPSIS" "SENSORS" "SRFPROGRADE" "SRFRETROGRADE" "OBT" "STATUS" "WHEELTHROTTLE" "WHEELSTEERING" "SAS" "RCS" "GEAR" "LEGS" "CHUTES" "LIGHTS" "PANELS" "BRAKES" "ABORT" "VERSION" "VERSION:MAJOR" "VERSION:MINOR" "SESSIONTIME" "TIME" "MAPVIEW" "WARP" "WARPMODE") 'words) . font-lock-keyword-face)
   ;; Action Groups
   '("\\<AG[0-9]+\\>" . font-lock-keyword-face)
   ;; Builtin Language Features
   `(,(regexp-opt '("SET" "TO" "IF" "ELSE" "UNTIL" "LOCK" "UNLOCK" "PRINT" "AT" "TOGGLE"
                    "WAIT" "WHEN" "THEN"  "OFF"  "CLEARSCREEN"  "ADD" "REMOVE"  "LOG"
                    "BREAK" "PRESERVE" "DECLARE" "PARAMETER" "SWITCH"  "COPY"  "FROM" "RENAME"
                    "VOLUME"  "FILE"  "DELETE"  "EDIT"  "RUN" "COMPILE" "LIST" "REBOOT" "SHUTDOWN"
                    "FOR" "UNSET" "BATCH" "DEPLOY" "IN" "ALL") 'words) . font-lock-builtin-face)
   '("\\('\\w*'\\)" . font-lock-variable-name-face))
  "Primary highlighting expressions for kOS mode.")

(defconst kos-font-lock-keywords-2
  (append kos-font-lock-keywords-1
          (list
           '("\\<\\(ON\\|OFF\\|TRUE\\|FALSE\\)\\>" . font-lock-constant-face) ;; ON / OFF keywords
           '("\"[^\"]+\"" . font-lock-string-face) ;; string literals
           '("\\b[-]?[0-9]+\\b" . font-lock-preprocessor-face) ;; integer literals
           '("\\b[-+]?[0-9]+\.[0-9]*\([eE][-+]?[0-9]+\)?\\b" . font-lock-preprocessor-face) ;; floating point literals
           )
          )
  "Additional Keywords to highlight in kOS (KerboScript) mode.")

(defvar kos-font-lock-keywords kos-font-lock-keywords-2
  "Default highlighting expressions for kOS (KerboScript) mode.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Syntax table for comments ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar kos-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?_ "w" st)
    (modify-syntax-entry ?/ ". 124b" st)
    (modify-syntax-entry ?\n "> b" st)
    st)
  "Syntax table for kos-mode.")

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indentation function ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar kos-indent-offset 4
  "Indentation offset for `kos-mode'.")

(defun kos-indent-line ()
  "Indent current line for any balanced-paren-mode'."
  (interactive)
  (let ((indent-col 0)
        (indentation-increasers "[{(]")
        (indentation-decreasers "[})]"))
    (save-excursion
      (beginning-of-line)
      (condition-case nil
          (while t
            (backward-up-list 1)
            (when (looking-at indentation-increasers)
              (setq indent-col (+ indent-col kos-indent-offset))))
        (error nil)))
    (save-excursion
      (back-to-indentation)
      (when (and (looking-at indentation-decreasers)
                 (>= indent-col kos-indent-offset))
        (setq indent-col (- indent-col kos-indent-offset))))
    (indent-line-to indent-col)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Major mode definition ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-derived-mode kos-mode prog-mode "KerboScript"
  "Major mode for editing KerboScript Files"
  :syntax-table kos-mode-syntax-table
  ;; Keymap
  (use-local-map kos-mode-map)
  ;; set syntax highlighting
  (set (make-local-variable 'font-lock-defaults) '(kos-font-lock-keywords nil t))
  ;; this should be case insensitive
  (set (make-local-variable 'font-lock-keywords-case-fold-search) t)
  ;; indentation
  (set (make-local-variable 'indent-line-function) 'kos-indent-line)

  ;; support for comment shortcuts
  (setq comment-start "// ")
  (setq comment-end "")
  ;; hook for loading additional modules
  (run-hooks 'kos-mode-hook)
  )

(provide 'kos-mode)

;;; kos-mode.el ends here
