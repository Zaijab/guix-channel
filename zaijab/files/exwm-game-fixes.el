;;; exwm-game-fixes.el --- Game window handling for EXWM -*- lexical-binding: t; -*-

;; Debugged live 2026-07-17.  Games have a finite taxonomy of fullscreen
;; behaviors under X; one mechanism per case, nothing per-game:
;;  1. Managed window that sends the EWMH fullscreen request: EXWM handles
;;     natively (exwm.el `exwm--on-net-wm-state'), nothing needed here.
;;  2. Managed window that never asks: `exwm-manage-configurations' below.
;;  3. Window mapped override-redirect, flag cleared later without a remap
;;     (Proton leaving exclusive fullscreen): invisible to EXWM's event
;;     pipeline; s-G adopts it, and adoption applies the configurations.
;;  4. Window still in override-redirect exclusive fullscreen: unmanageable
;;     by X protocol; set the game to borderless once (persisted by the
;;     game), or contain it with gamescope.

(require 'exwm)

;; Case 2: class-generic game policy.  Applied by
;; `exwm-manage--manage-window' both at map time and when s-G adopts.
(setq exwm-manage-configurations
      '(((string-match-p "\\`steam_app_" (or exwm-class-name ""))
         fullscreen t
         char-mode t)
        ((equal exwm-class-name "gamescope")
         fullscreen t
         char-mode t)))

;; Make a newly managed game the selected window, so the focus pipeline
;; (`exwm-input--update-focus' follows Emacs' selected window) targets the
;; game instead of whatever buffer Steam left selected.
(defun my/exwm-select-game ()
  (when (and (stringp exwm-class-name)
             (string-match-p "\\`steam_app_" exwm-class-name))
    (exwm-workspace-switch-to-buffer (current-buffer))))
(add-hook 'exwm-manage-finish-hook #'my/exwm-select-game)

;; Case 3: EXWM reconciles its model with the X server exactly once, at
;; startup (`exwm-manage--scan').  This is the same reconciliation made
;; rerunnable: adopt every viewable, non-override-redirect top-level window
;; EXWM missed.  Idempotent (skips managed ids).  Uses the same internal
;; entry point as the startup scan; revisit if EXWM is upgraded past 0.35.
(defun my/exwm-adopt-orphan-windows ()
  (interactive)
  (let ((tree (xcb:+request-unchecked+reply exwm--connection
                  (make-instance 'xcb:QueryTree :window exwm--root)))
        (n 0))
    (dolist (id (append (slot-value tree 'children) nil))
      (unless (exwm--id->buffer id)
        (let ((attr (xcb:+request-unchecked+reply exwm--connection
                        (make-instance 'xcb:GetWindowAttributes :window id))))
          (when (and attr
                     (= 0 (slot-value attr 'override-redirect))
                     (= xcb:MapState:Viewable (slot-value attr 'map-state)))
            (exwm-manage--manage-window id)
            (setq n (1+ n))))))
    (message "Adopted %d orphaned window(s)" n)))

;; Input tiers: only `exwm-input-global-keys' (root-window grabs) survive
;; char-mode, and fullscreening auto-enters char-mode — so the fullscreen
;; toggle and the rescue command MUST live in this tier or they vanish the
;; moment they are most needed.  `exwm-input-set-key' takes effect
;; immediately in a running session (plain setq would not re-grab).
;; Side effect: these keys are invisible to all applications.
(exwm-input-set-key (kbd "s-F") #'exwm-layout-toggle-fullscreen)
(exwm-input-set-key (kbd "s-G") #'my/exwm-adopt-orphan-windows)

(provide 'exwm-game-fixes)
;;; exwm-game-fixes.el ends here
