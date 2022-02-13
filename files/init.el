;; -*- lexical-binding: t; -*-

;; Needed for Helm Completion
(defvar browse-url-galeon-program 1)
(defvar browse-url-netscape-program 1)

;; Emacs UI
(load-theme 'doom-one t)
(doom-modeline-mode 1)
(setq inhibit-startup-echo-area-message "zjabbar")
(set-face-attribute 'default nil :font "Fira Code-14")
(setq minibuffer-local-must-match-filename-map nil)

;; Tab Bar
(tab-bar-mode)
(display-time-mode)
(customize-set-variable 'tab-bar-format '(tab-bar-format-history tab-bar-format-tabs tab-bar-separator tab-bar-format-add-tab tab-bar-format-align-right tab-bar-format-global))
(customize-set-variable 'mode-line-misc-info '(""))
(customize-set-variable 'display-time-load-average-threshold 100)
(customize-set-variable 'display-time-day-and-date t)
(set-face-attribute 'tab-bar nil :foreground "#FFFFFF")

;; Helm
(helm-mode 1)

;; Hotkeys
(global-set-key (kbd "s-n") #'org-roam-node-find)
(global-set-key (kbd "s-<tab>") #'helm-find-files)
(global-set-key (kbd "s-a") #'(lambda () (interactive) (helm-find-files-1 "~/.config/guix/")))
(global-set-key (kbd "s-x") #'helm-M-x)
(global-set-key (kbd "s-c") #'guix)
(global-set-key (kbd "s-b") #'helm-buffers-list)
(global-set-key (kbd "s-h") #'windmove-left)
(global-set-key (kbd "s-j") #'windmove-down)
(global-set-key (kbd "s-k") #'windmove-up)
(global-set-key (kbd "s-l") #'windmove-right)
(global-set-key (kbd "s-SPC") #'(lambda (command) (interactive (list (read-shell-command "$ "))) (start-process-shell-command command nil command)))
(global-set-key (kbd "s-e") #'(lambda () (interactive) (start-process-shell-command "chromium" nil "chromium")))
(global-set-key (kbd "s-q") #'kill-current-buffer)
(global-set-key (kbd "s-w") #'sly)
(global-set-key (kbd "s-g") #'vterm)
(global-set-key (kbd "s-0") #'delete-window)
(global-set-key (kbd "s-1") #'delete-other-windows)
(global-set-key (kbd "s-2") #'split-window-below)
(global-set-key (kbd "s-3") #'split-window-right)
(global-set-key (kbd "s-4") #'tab-switch)
(global-set-key (kbd "<f7>") #'(lambda () (interactive) (call-process-shell-command "loginctl suspend")))


(which-key-mode)

(exwm-enable)
(defun exwm-rename-buffer-to-title () (exwm-workspace-rename-buffer exwm-title))
(add-hook 'exwm-update-title-hook 'exwm-rename-buffer-to-title)
(defvar super-keys ())
(let ((km (current-global-map)))
  (while km
    (let ((maybe-event (and (listp (car km))
			    (caar km))))
      (if (and (eventp maybe-event)
	       (memq 'super (event-modifiers maybe-event)))
	  (add-to-list 'super-keys maybe-event)))
      (setq km (cdr km))))
(setq exwm-input-prefix-keys (append super-keys '(f7 f8 XF86AudioRaiseVolume XF86AudioLowerVolume XF86AudioNext XF86AudioPlay XF86AudioPrev XF86AudioMute)))
(define-key exwm-mode-map (kbd "C-c") nil)
(server-mode)
(defun fhd/exwm-input-line-mode ()
  "Set exwm window to line-mode and show mode line"
  (call-interactively #'exwm-input-grab-keyboard)
  (exwm-layout-show-mode-line))

(defun fhd/exwm-input-char-mode ()
  "Set exwm window to char-mode and hide mode line"
  (call-interactively #'exwm-input-release-keyboard)
  (exwm-layout-hide-mode-line))

(defun fhd/exwm-input-toggle-mode ()
  "Toggle between line- and char-mode"
  (interactive)
  (with-current-buffer (window-buffer)
    (when (eq major-mode 'exwm-mode)
      (if (equal (nth 1 (nth 1 mode-line-process)) "line")
	  (fhd/exwm-input-char-mode)
	(fhd/exwm-input-line-mode)))))
(defun fhd/toggle-exwm-input-line-mode-passthrough ()
  (interactive)
  (if exwm-input-line-mode-passthrough
      (progn
	(setq exwm-input-line-mode-passthrough nil)
	(message "App receives all the keys now (with some simulation)"))
    (progn
      (setq exwm-input-line-mode-passthrough t)
      (message "emacs receives all the keys now")))
  (force-mode-line-update))
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(projectile-mode +1)
(setq projectile-project-search-path '("~/code/"))

(setq yas-snippet-dirs '("~/.config/emacs/snippets"))
(yas-global-mode)

(undo-tree-mode 1)

(evil-mode 1)
(evil-define-key 'normal org-mode-map (kbd "<tab>") #'org-cycle)

(global-company-mode)

(setq org-babel-python-command "python3")
(setq python-shell-interpreter "python3")

(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode-enable)
(add-hook 'lisp-mode-hook 'rainbow-delimiters-mode-enable)
(add-hook 'scheme-mode-hook 'rainbow-delimiters-mode-enable)

(setq inferior-lisp-program "sbcl")
  (setq sly-lisp-implementations
	'((sbcl ("sbcl" "--userinit" "/home/zjabbar/.config/sbcl/sbclrc"))))

(add-hook 'emacs-lisp-mode-hook 'lispy-mode)
(add-hook 'scheme-mode-hook 'lispy-mode)
(add-hook 'lisp-mode-hook 'lispy-mode)
(add-hook 'common-lisp-mode-hook 'lispy-mode)
(add-hook 'lisp-interaction-mode-hook 'lispy-mode)
(add-hook 'lispy-mode-hook 'lispyville-mode)
(lispyville-set-key-theme '(additional-motions additional additional-insert commentary))

(pdf-tools-install)
(add-hook 'pdf-view-mode-hook #'(lambda () (pdf-view-midnight-minor-mode)))

(setq org-src-fontify-natively nil)

(setq org-agenda-files '("~/notes/20211222094239-workflow.org"))

(setq org-adapt-indentation nil)

(setq org-startup-with-inline-images t)

(add-hook 'org-mode-hook #'valign-mode)

(setq valign-max-table-size 10000)

(require 'google-translate-smooth-ui)
(defun google-translate--search-tkk () "Search TKK." (list 430675 2721866130))
(setq google-translate-backend-method 'curl)
(global-set-key "\C-ct" 'google-translate-smooth-translate)
(setq google-translate-translation-directions-alist
      '(("ja" . "en") ("hi" . "en")))
(setq google-translate-default-target-language "en")
(set-language-environment "English")
(setq org-roam-directory "~/notes")
(setq org-roam-v2-ack t)
