;; -*- lexical-binding: t; -*-

;; Needed for Helm Completion
(defvar browse-url-galeon-program 1)
(defvar browse-url-netscape-program 1)

;; Emacs UI
(load-theme 'doom-one t)
(doom-modeline-mode 1)
(blink-cursor-mode 0)
(set-face-attribute 'default nil :font "Fira Code-14")
(which-key-mode)

;; Helm
(helm-mode 1)

;; Hotkeys
(evil-mode 1)
(evil-define-state windows
  "Change Window States. Edit files / buffers to show and orientation."
  :tag " <W> "
  :message "-- WINDOW MODE --"
  :input-method t
  :intercept-esc nil)

;; File Hotkeys
(evil-define-key 'normal 'global (kbd "<SPC>ff") #'helm-find-files)
(evil-define-key 'normal 'global (kbd "<SPC>fc") #'(lambda () (interactive) (helm-find-files-1 "~/.config/guix/")))
(evil-define-key 'normal 'global (kbd "<SPC>x") #'helm-M-x)
(evil-define-key 'normal 'global (kbd "<SPC>t") #'vterm)
(evil-define-key 'normal 'global (kbd "<SPC>b") #'helm-buffers-list)
(evil-define-key 'normal org-mode-map (kbd "<tab>") #'org-cycle)


(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(projectile-mode +1)
(setq projectile-project-search-path '("~/code/"))

(setq yas-snippet-dirs '("~/.config/emacs/snippets"))
(yas-global-mode)

(undo-tree-mode 1)


(global-company-mode)


(setq org-babel-python-command "python3")
(setq python-shell-interpreter "python3")

(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode-enable)
(add-hook 'lisp-mode-hook 'rainbow-delimiters-mode-enable)
(add-hook 'scheme-mode-hook 'rainbow-delimiters-mode-enable)
(add-hook 'scheme-mode-hook 'guix-devel-mode)

(setq inferior-lisp-program "sbcl")
  (setq sly-lisp-implementations
	'((sbcl ("sbcl" "--userinit" "/home/zjabbar/.config/sbcl/sbclrc"))))

(add-hook 'emacs-lisp-mode-hook 'lispy-mode)
(add-hook 'scheme-mode-hook 'lispy-mode)
(add-hook 'lisp-mode-hook 'lispy-mode)
(add-hook 'common-lisp-mode-hook 'lispy-mode)
(add-hook 'lisp-interaction-mode-hook 'lispy-mode)
(add-hook 'sly-mrepl-mode-hook 'lispy-mode)
(add-hook 'lispy-mode-hook 'lispyville-mode)
(lispyville-set-key-theme '(additional-motions additional additional-insert commentary slurp/barf-cp wrap))

(pdf-tools-install)
(add-hook 'pdf-view-mode-hook #'(lambda () (pdf-view-midnight-minor-mode)))

(setq org-src-fontify-natively nil)

(setq org-agenda-files '("~/notes/20211224040925-todo.org"))

(setq org-adapt-indentation nil)

(setq org-startup-with-inline-images t)

(setq org-roam-directory "~/notes")
(setq org-roam-v2-ack t)
