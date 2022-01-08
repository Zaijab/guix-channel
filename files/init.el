;; -*- lexical-binding: t; -*-

(leaf guix
  :require t)

(leaf leaf)

(leaf leaf-keywords
  :config (leaf-keywords-init))

(leaf doom-themes
  :config
  (load-theme 'doom-one t))

(setq inhibit-startup-echo-area-message "zjabbar")

(set-face-attribute 'default nil :font "Fira Code-14")

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)
(setq scroll-step 1)
(setq use-dialog-box nil)

(setq minibuffer-local-must-match-filename-map nil)
(leaf helm
  :config
  (leaf helm-config)
  (helm-mode 1))

(leaf general
  :config
  
  (general-define-key
   :prefix "SPC"
   :states '(normal)
   "p" 'projectile-command-map
   "n" 'org-roam-node-find)

  (general-define-key
   :prefix ","
   :states '(normal visual)
   :keymaps 'polymode-mode-map
   "j" 'polymode-next-chunk
   "k" 'polymode-previous-chunk
   "i" 'polymode-insert-new-chunk
   "u" 'polymode-insert-new-chunk-code-only
   "U" 'polymode-insert-new-chunk-output-only
   "p" 'polymode-insert-new-plot
   "o" 'polymode-insert-yaml
   "d" 'polymode-kill-chunk
   "e" 'polymode-export
   "E" 'polymode-set-exporter
   "w" 'polymode-weave
   "W" 'polymode-set-weaver
   "$" 'polymode-show-process-buffer
   "n" 'polymode-eval-region-or-chunk
   "," 'polymode-eval-region-or-chunk
   "." '(lambda () (interactive) (polymode-eval-region-or-chunk) (org-display-inline-images))
   "N" 'polymode-eval-buffer
   "1" 'polymode-eval-buffer-from-beg-to-point
   "0" 'polymode-eval-buffer-from-point-to-end)

  (general-define-key
   :prefix "SPC"
   :states '(normal)
   "0" 'delete-window
   "1" 'delete-other-windows
   "2" 'split-window-below
   "3" 'split-window-right
   "4" 'exwm-workspace-switch
   "b" 'helm-buffers-list
   "q" '(lambda () (interactive) (persp-kill-buffer* (current-buffer)))) 

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
  (general-define-key
   "s-M-h" '(lambda () (interactive) (exwm-layout-enlarge-window 10 t))
   "s-M-j" '(lambda () (interactive) (exwm-layout-enlarge-window 10))
   "s-M-k" '(lambda () (interactive) (exwm-layout-enlarge-window -10))
   "s-M-l" '(lambda () (interactive) (exwm-layout-enlarge-window -10 t))))

(defun zain-guix-home ()
  (interactive)
  (call-process-shell-command "guix home reconfigure /home/zjabbar/.config/guix/home-configuration.scm"))

(leaf which-key
  :config
  (which-key-mode))

(leaf origami
  :config
  (global-origami-mode))

(leaf exwm
  :config
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
  )

(defun dw/send-polybar-hook (name number)
  (start-process-shell-command "polybar-msg" nil (format "polybar-msg hook %s %s" name number)))

(defun dw/update-polybar-exwm (&optional path)
  (dw/send-polybar-hook "exwm" 1)
  (dw/send-polybar-hook "exwm-path" 1))

(defun dw/update-polybar-telegram ()
  (dw/send-polybar-hook "telegram" 1))

(defun dw/polybar-exwm-workspace ()
  (pcase exwm-workspace-current-index
    (0 "0")
    (1 "1")
    (2 "2")
    (3 "3")
    (4 "4")))

  (setq exwm-workspace-number 4)

(defun dw/polybar-exwm-workspace-path ()
  (let ((workspace-path (frame-parameter nil 'bufler-workspace-path-formatted)))
    (if workspace-path
        (substring-no-properties workspace-path)
      "")))

(defun dw/polybar-mail-count (max-count)
  (if (and t t)
    (let* ((mail-count (shell-command-to-string
                         (format "mu find --nocolor -n %s \"%s\" | wc -l" max-count dw/mu4e-inbox-query))))
      (format "Mail %s" (string-trim mail-count)))
    ""))

(defun dw/telega-normalize-name (chat-name)
  (let* ((trimmed-name (string-trim-left (string-trim-right chat-name "}") "◀{"))
         (first-name (nth 0 (split-string trimmed-name " "))))
    first-name))

(defun dw/propertized-to-polybar (buffer-name)
  (if-let* ((text (substring-no-properties buffer-name))
            (fg-face (get-text-property 0 'face buffer-name))
            (fg-color (face-attribute fg-face :foreground)))
    (format "%%{F%s}%s%%{F-}" fg-color (dw/telega-normalize-name text))
    text))

(defun dw/polybar-telegram-chats ()
  (if (> (length tracking-buffers) 0)
    (format "Chat %s" (string-join (mapcar 'dw/propertized-to-polybar tracking-buffers) ", "))
    ""))

(add-hook 'exwm-workspace-switch-hook #'dw/update-polybar-exwm)
(add-hook 'bufler-workspace-set-hook #'dw/update-polybar-exwm)

(setq display-buffer-alist
      '((".*"
         (display-buffer-reuse-window display-buffer-same-window)
         (reusable-frames . t))))

(setq even-window-sizes nil)

(leaf perspective
  :init (persp-mode))

(leaf no-littering
  :config
  (setq auto-save-file-name-transforms
	`((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(leaf projectile
  :config
  (projectile-mode +1)
  (setq projectile-project-search-path '("~/code/")))

(leaf yasnippet
  :hook ((post-command-hook . my-yas-try-expanding-auto-snippets))
  :defun my-yas-try-expanding-auto-snippets
  :config
  (setq yas-snippet-dirs '("~/.config/emacs/snippets"))
  (yas-global-mode)
  (defun my-yas-try-expanding-auto-snippets ()
    (when yas-minor-mode
      (let ((yas-buffer-local-condition ''(require-snippet-condition . auto)))
	(yas-expand))))
  (defun type-declare-variables (yas-input-string)
    (let* ((value "")
	   (typed-lambda-list (split-string yas-input-string "\n\t+")))
      (dolist (element (butlast typed-lambda-list))
	(setq value (concat value (cadr (split-string element " " t "[()]")) " ")))
      (setq value (concat value (cadr (split-string (car (last typed-lambda-list)) " " t "[()]")))))))

(leaf undo-tree
  :require t
  :config (undo-tree-mode 1))

(leaf evil
  :require t
  :config
  (evil-mode 1)
  (evil-define-key 'normal org-mode-map (kbd "<tab>") #'org-cycle))

(leaf company
  :config
  (global-company-mode))

(leaf lsp-mode
  :hook ((python-mode-hook . lsp)
	 (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(setq org-babel-python-command "python3")
(setq python-shell-interpreter "python3")
(add-hook 'python-mode-hook '(lambda () (setq polymode-eval-region-function #'python-shell-send-region)))

(leaf rainbow-delimiters
  :hook ((emacs-lisp-mode-hook . (rainbow-delimiters-mode-enable))
	 (lisp-mode-hook . (rainbow-delimiters-mode-disable))
	 (scheme-mode-hook . (rainbow-delimiters-mode-disable))))

(leaf sly
  :config
  (setq inferior-lisp-program "sbcl")
  (setq sly-lisp-implementations
	'((sbcl ("sbcl" "--userinit" "/home/zjabbar/.config/sbcl/sbclrc")))))

(leaf lispy
  :hook ((emacs-lisp-mode-hook . lispy-mode)
	 (scheme-mode-hook . lispy-mode)
	 (lisp-mode-hook . lispy-mode) 
	 (common-lisp-mode-hook . lispy-mode)
	 (lisp-interaction-mode . lispy-mode)))

(leaf lispyville
  :hook ((lispy-mode-hook . lispyville-mode))
  :config (lispyville-set-key-theme '(additional-motions additional additional-insert commentary)))

(leaf csv-mode
  :hook (csv-mode-hook . (csv-mode)))

(leaf pdf-tools
  :init
  (pdf-tools-install)
  :hook (pdf-view-mode-hook . (lambda () (pdf-view-midnight-minor-mode))))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)))

(setq org-src-fontify-natively nil)

(setq org-agenda-files '("~/notes/20211222094239-workflow.org"))

(setq org-adapt-indentation nil)

(setq org-startup-with-inline-images t)

(leaf polymode)
(leaf poly-org
  :config
  (poly-org-mode 1))

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

(leaf google-translate
  :require google-translate-smooth-ui
  :config
  (defun google-translate--search-tkk () "Search TKK." (list 430675 2721866130))
  (setq google-translate-backend-method 'curl)
  (global-set-key "\C-ct" 'google-translate-smooth-translate)
  (setq google-translate-translation-directions-alist
    '(("ja" . "en") ("hi" . "en")))
  (setq google-translate-default-target-language "en"))
(set-language-environment "Japanese")

(leaf org-roam
  :init
  (setq org-roam-directory "~/notes")
  (setq org-roam-v2-ack t))

(leaf org-drill
  :require t
  :config (setq org-drill-learn-fraction 0.75))
