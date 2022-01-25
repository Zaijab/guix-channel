;; -*- lexical-binding: t; -*-

(defvar browse-url-galeon-program 4)
(defvar browse-url-netscape-program 4)

(load-theme 'doom-one t)
(doom-modeline-mode 1)

(setq inhibit-startup-echo-area-message "zjabbar")

(set-face-attribute 'default nil :font "Fira Code-14")

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)
(setq scroll-step 1)
(setq use-dialog-box nil)

(setq minibuffer-local-must-match-filename-map nil)

(helm-mode 1)

(general-define-key
 "s-d" '(lambda () (interactive) (find-file "~/.config/guix/files/init.el"))
 "s-s" '(lambda () (interactive) (find-file "~/.config/guix/channels.scm"))
 "s-a" '(lambda () (interactive) (find-file "~/.config/guix/home-configuration.scm"))
 "s-z" '(lambda () (interactive) (find-file "~/.config/guix/config.scm"))
 "s-n" 'org-roam-node-find
 "s-<tab>" 'helm-find-files
 "s-x" 'helm-M-x
 "s-c" 'guix
 "s-b" 'helm-buffers-list
 "s-h" 'windmove-left
 "s-j" 'windmove-down
 "s-k" 'windmove-up
 "s-l" 'windmove-right
 "s-r" 'persp-next
 "s-f" 'persp-prev
 "s-t" 'google-translate-smooth-translate
 "s-SPC" '(lambda (command) (interactive (list (read-shell-command "$ "))) (start-process-shell-command command nil command))
 "s-q" '(lambda () (interactive) (persp-kill-buffer* (current-buffer)))
 "s-i" 'fhd/exwm-input-toggle-mode
 "s-o" 'fhd/toggle-exwm-input-line-mode-passthrough
 "s-w" 'sly
 "s-g" 'vterm
 "s-p" 'projectile-command-map
 "s-e" '(lambda () (interactive) (start-process-shell-command "nyxt" nil "nyxt"))
 "s-0" 'delete-window
 "s-1" 'delete-other-windows
 "s-2" 'split-window-below
 "s-3" 'split-window-right
 "s-4" 'exwm-workspace-switch
 "<f7>" '(lambda () (interactive) (call-process-shell-command "loginctl suspend"))
 "<f8>" 'fhd/toggle-exwm-input-line-mode-passthrough)
(defun zain-guix-home ()
  (interactive)
  (call-process-shell-command "guix home reconfigure /home/zjabbar/.config/guix/home-configuration.scm"))

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

(setq display-buffer-alist
      '((".*"
         (display-buffer-reuse-window display-buffer-same-window)
         (reusable-frames . t))))

(setq even-window-sizes nil)

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
(add-hook 'pdf-view-mode-hook '(lambda () (pdf-view-midnight-minor-mode)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)))

(setq org-src-fontify-natively nil)

(setq org-agenda-files '("~/notes/20211222094239-workflow.org"))

(setq org-adapt-indentation nil)

(setq org-startup-with-inline-images t)

(defun zain-tangler ()
  (interactive)
  (call-process-shell-command
   (concat "emacs -Q --batch --eval \"(require \'org)\" --eval \'(org-babel-tangle-file \""
	   (buffer-file-name (current-buffer))
	   "\")'") nil 0))
(add-hook 'org-mode-hook #'valign-mode)
(add-hook 'org-mode-hook
          (lambda () (add-hook 'after-save-hook #'zain-tangler nil 'local)))
(setq valign-max-table-size 10000)

(setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))

(require 'google-translate-smooth-ui)
(defun google-translate--search-tkk () "Search TKK." (list 430675 2721866130))
(setq google-translate-backend-method 'curl)
(global-set-key "\C-ct" 'google-translate-smooth-translate)
(setq google-translate-translation-directions-alist
      '(("ja" . "en") ("hi" . "en")))
(setq google-translate-default-target-language "en")
(set-language-environment "Japanese")


(setq org-roam-directory "~/notes")
(setq org-roam-v2-ack t)
