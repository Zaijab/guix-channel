;; The module zaijab / services / emacs
;; Creates the module "Emacs" in the "Services" for the "zaijab" Channel

(define-module (zaijab services emacs)
  #:use-module (gnu home)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages education)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages bittorrent)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages video)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages java)
  #:use-module (gnu packages inkscape)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages shellutils)
  #:use-module (gnu packages browser-extensions)
  #:use-module (gnu packages gdb)
  #:use-module (gnu packages gnuzilla)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages jami)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages scheme)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages hunspell)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages texlive)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages librewolf)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages tree-sitter)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages base)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 popen)
  #:use-module (guix transformations)
  #:use-module (guix gexp)
  #:use-module (zaijab packages emacs-xyz)
  #:use-module (zaijab packages python-xyz)
  #:use-module (nongnu packages emacs)
  #:use-module (ruther packages ripgrep-all)
  #:use-module (nongnu packages fonts)
  #:use-module (nongnu packages nvidia)
  #:use-module (guix-science packages python)
  #:use-module (guix-science packages machine-learning)
  #:export (home-emacs-service-type
	    home-emacs-configuration
	    home-emacs-total-configuration))

(define file-likes? (list-of file-like?))

(define-configuration/no-serialization home-emacs-configuration
  (emacs
   (file-like emacs)
   "The Emacs package to use.")
  (packages
   (file-likes '())
   "The packages this configuration will add to home-profile.
    Usually these will be emacs-* packages.")
  (early-init
   (list '())
   "A list whose contents will inserted into @file{$XDG_CONFIG_HOME/emacs/early-init.el}")
  (init
   (list '())
   "A list whose contents will inserted into @file{$XDG_CONFIG_HOME/emacs/init.el}")
  (extra-files
   (file-likes '())
   "A list of files to be placed in @file{$XDG_CONFIG_HOME/emacs/}."))

(define home-emacs-service-type
  (service-type (name 'emacs-configuration)
		(extensions
		 (list (service-extension
			home-profile-service-type
			(lambda (config) `(,(home-emacs-configuration-emacs config)
                                           ,@(home-emacs-configuration-packages config))))
		       (service-extension
			home-xdg-configuration-files-service-type
			(lambda (config)
                          `(("emacs/early-init.el"
			     ,(scheme-file "early-init.el"
					   (home-emacs-configuration-early-init config)
					   #:splice? #:t))
			    ("emacs/init.el"
			     ,(scheme-file "init.el"
					   (home-emacs-configuration-init config)
					   #:splice? #:t))
                            ,@(map (lambda (file) (list (string-append "emacs/" (scheme-file-name file))
                                                        file))
                                   (home-emacs-configuration-extra-files config)))))))
		(default-value (home-emacs-configuration))
		(description "Configures Emacs and installs packages to home-profile.")))


;;; EMACS CONFIG

;; Completion Style - DONE
(define orderless-configuration
  (home-emacs-configuration
   (packages (list emacs-orderless))
   (init '((use-package orderless
			:custom
			(completion-styles '(orderless basic))
			(completion-category-overrides '((file (styles basic partial-completion))))
			(orderless-smart-case nil)
			(completion-ignore-case t)
			(read-file-name-completion-ignore-case t)
			(read-buffer-completion-ignore-case t))))))

;; Completion UI - DONE
(define vertico-configuration
  (home-emacs-configuration
   (packages (list emacs-vertico))
   (init '((use-package vertico
			:custom
			(vertico-cycle t)
			:init
			(vertico-mode))))))

;; Annotations - DONE
(define marginalia-configuration
  (home-emacs-configuration
   (packages (list emacs-marginalia))
   (init '((use-package marginalia
			:bind
			(:map minibuffer-local-map
			 ("M-A" . marginalia-cycle))
			:init
			(marginalia-mode))))))

;; Interactive Completing Read
(define consult-configuration
  (home-emacs-configuration
   (packages (list emacs-consult
		   grep
		   ripgrep
		   ripgrep-all
		   poppler
		   poppler-data))
   (init '(;; Example configuration for Consult
	   (use-package consult
			:demand t
			;; Replace bindings. Lazily loaded by `use-package'.
			:bind (;; C-c bindings in `mode-specific-map'
			       ("C-c M-x" . consult-mode-command)
			       ("C-c h" . consult-history)
			       ("C-c k" . consult-kmacro)
			       ("C-c m" . consult-man)
			       ("C-c i" . consult-info)
			       ("<remap> <Info-search>" . consult-info)
			       ;; C-x bindings in `ctl-x-map'
			       ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
			       ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
			       ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
			       ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
			       ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
			       ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
			       ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
			       ;; Custom M-# bindings for fast register access
			       ("M-#" . consult-register-load)
			       ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
			       ("C-M-#" . consult-register)
			       ;; Other custom bindings
			       ("M-y" . consult-yank-pop)                ;; orig. yank-pop
			       ;; M-g bindings in `goto-map'
			       ("M-g e" . consult-compile-error)
			       ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
			       ("M-g g" . consult-goto-line)             ;; orig. goto-line
			       ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
			       ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
			       ("M-g m" . consult-mark)
			       ("M-g k" . consult-global-mark)
			       ("M-g i" . consult-imenu)
			       ("M-g I" . consult-imenu-multi)
			       ;; M-s bindings in `search-map'
			       ("M-s d" . consult-find)                  ;; Alternative: consult-fd
			       ("M-s c" . consult-locate)
			       ("M-s g" . consult-grep)
			       ("M-s G" . consult-git-grep)
			       ("M-s r" . consult-ripgrep)
			       ("M-s l" . consult-line)
			       ("M-s L" . consult-line-multi)
			       ("M-s k" . consult-keep-lines)
			       ("M-s u" . consult-focus-lines)
			       ;; Isearch integration
			       ("M-s e" . consult-isearch-history)
			       :map isearch-mode-map
			       ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
			       ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
			       ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
			       ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
			       ;; Minibuffer history
			       :map minibuffer-local-map
			       ("M-s" . consult-history)                 ;; orig. next-matching-history-element
			       ("M-r" . consult-history))                ;; orig. previous-matching-history-element

			;; Enable automatic preview at point in the *Completions* buffer. This is
			;; relevant when you use the default completion UI.
			:hook (completion-list-mode . consult-preview-at-point-mode)

			;; The :init configuration is always executed (Not lazy)
			:init

			;; Optionally configure the register formatting. This improves the register
			;; preview for `consult-register', `consult-register-load',
			;; `consult-register-store' and the Emacs built-ins.
			(setq register-preview-delay 0.5
			      register-preview-function (function consult-register-format))

			(setq consult-preview-partial-size 104857600)

			;; Optionally tweak the register preview window.
			;; This adds thin lines, sorting and hides the mode line of the window.
			(advice-add (function register-preview) :override (function consult-register-window))

			;; Use Consult to select xref locations with preview
			(setq xref-show-xrefs-function (function consult-xref)
			      xref-show-definitions-function (function consult-xref))

			;; Configure other variables and modes in the :config section,
			;; after lazily loading the package.
			:commands (consult-ripgrep-all consult-search-library)
			:config

			;; Optionally configure preview. The default value
			;; is 'any, such that any key triggers the preview.
			;; (setq consult-preview-key 'any)
			;; (setq consult-preview-key "M-.")
			;; (setq consult-preview-key '("S-<down>" "S-<up>"))
			;; For some commands and buffer sources it is useful to configure the
			;; :preview-key on a per-command basis using the `consult-customize' macro.
			(consult-customize
			 consult-theme :preview-key '(:debounce 0.2 any)
			 consult-ripgrep consult-git-grep consult-grep
			 consult-bookmark consult-recent-file consult-xref
			 consult--source-bookmark consult--source-file-register
			 consult--source-recent-file consult--source-project-recent-file
			 ;; :preview-key "M-."
			 :preview-key '(:debounce 0.4 any))

			;; Optionally configure the narrowing key.
			;; Both < and C-+ work reasonably well.
			(setq consult-narrow-key "<") ;; "C-+"

			;; Optionally make narrowing help available in the minibuffer.
			;; You may want to use `embark-prefix-help-command' or which-key instead.
			(keymap-set consult-narrow-map (concat consult-narrow-key " ?") (function consult-narrow-help))

			(defcustom consult-ripgrep-all-args
			  "rga --null --line-buffered --color=never --max-columns=1000 --path-separator /\
   --smart-case --no-heading --with-filename --line-number"
			  "Command line arguments for ripgrep, see `consult-ripgrep'.
The dynamically computed arguments are appended.
Can be either a string, or a list of strings or expressions."
			  :type '(choice string (repeat (choice string sexp))))

			(eval-expression (quote
					  (defun consult--ripgrep-all-make-builder (paths)
					    "Create ripgrep command line builder given PATHS."
					    (let* ((cmd (consult--build-args consult-ripgrep-all-args))
						   (type (if (consult--grep-lookahead-p (car cmd) "-P") 'pcre 'extended)))
					      (lambda (input)
						(let* ((arg  (car (consult--command-split input)))
						       (opts (cdr (consult--command-split input)))
						       (flags (append cmd opts))
						       (ignore-case
							(and (not (or (member "-s" flags) (member "--case-sensitive" flags)))
							     (or (member "-i" flags) (member "--ignore-case" flags)
								 (and (or (member "-S" flags) (member "--smart-case" flags))
								      (let (case-fold-search)
									;; Case insensitive if there are no uppercase letters
									(not (string-match-p "[[:upper:]]" arg))))))))
						  (if (or (member "-F" flags) (member "--fixed-strings" flags))
						      (cons (append cmd (list "-e" arg) opts paths)
							    (apply-partially (function consult--highlight-regexps)
									     (list (regexp-quote arg)) ignore-case))
						      (let ((re (car (funcall consult--regexp-compiler arg type ignore-case)))
							    (hl (cdr (funcall consult--regexp-compiler arg type ignore-case))))
							(when re
							  (cons (append cmd (and (eq type 'pcre) '("-P"))
									(list "-e" (consult--join-regexps re type))
									opts paths)
								hl))))))))))

			(defun consult-ripgrep-all (&optional dir initial)
			  "Search with `rg' for files in DIR with INITIAL input.
See `consult-grep' for details."
			  (interactive "P")
			  (consult--grep "Ripgrep All" (function consult--ripgrep-all-make-builder) dir initial))

			(defun consult-search-library () (interactive)
			  (consult-ripgrep-all "~/library/"))


			)





	   ))))

;;
(define embark-configuration
  (home-emacs-configuration
   (packages (list emacs-embark
		   emacs-embark-consult))
   (init '((use-package embark
			:ensure t

			:bind
			(("C-." . embark-act)         ;; pick some comfortable binding
			 ("C-;" . embark-dwim)        ;; good alternative: M-.
			 ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

			:init

			;; Optionally replace the key help with a completing-read interface
			(setq prefix-help-command (function embark-prefix-help-command))

			;; Show the Embark target at point via Eldoc. You may adjust the
			;; Eldoc strategy, if you want to see the documentation from
			;; multiple providers. Beware that using this can be a little
			;; jarring since the message shown in the minibuffer can be more
			;; than one line, causing the modeline to move up and down:

			;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
			;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

			:config

			;; Hide the mode line of the Embark live/completions buffers
			(add-to-list 'display-buffer-alist
				     '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
				       nil
				       (window-parameters (mode-line-format . none)))))

	   ;; Consult users will also want the embark-consult package.
	   (use-package embark-consult
			:hook
			(embark-collect-mode . consult-preview-at-point-mode))))))


;; Spell checking - DONE
(define spellcheck-configuration
  (home-emacs-configuration
   (packages (list emacs-jinx
		   hunspell-dict-en-us))
   (init '((use-package jinx
			:hook (emacs-startup . global-jinx-mode)
			:bind (("M-$" . jinx-correct)
			       ("C-M-$" . jinx-languages)))))))

(define openwith-configuration
  (home-emacs-configuration
   (packages (list emacs-openwith))
   (init '((require 'openwith)
	   (openwith-mode t)
	   (setq openwith-associations
		 (list
		  (list (openwith-make-extension-regexp
			 '("mpg" "mpeg" "mp3" "mp4"
			   "avi" "wmv" "wav" "mov" "flv"
			   "ogm" "ogg" "mkv" "webm"))
			"mpv"
			'(file))
		  ))))))


(define llm-configuration
  (home-emacs-configuration
   (packages (list emacs-gptel))
   (init '())))

(define lsp-configuration
  (home-emacs-configuration
   (packages (list emacs-treemacs))
   (init '((setq eldoc-echo-area-use-multiline-p nil)))))

(define dape-configuration
  (home-emacs-configuration
   (packages (list emacs-dape
		   gdb
		   python-debugpy))
   (init '((use-package dape
			:preface
			;; By default dape shares the same keybinding prefix as `gud'
			;; If you do not want to use any prefix, set it to nil.
			;; (setq dape-key-prefix "\C-x\C-a")

			:hook
			;; Save breakpoints on quit
			(kill-emacs . dape-breakpoint-save)
			;; Load breakpoints on startup
			(after-init . dape-breakpoint-load)

			:config
			;; Turn on global bindings for setting breakpoints with mouse
			(dape-breakpoint-global-mode)

			;; Info buffers to the right
			(setq dape-buffer-window-arrangement 'right)

			;; Info buffers like gud (gdb-mi)
			(setq dape-buffer-window-arrangement 'gud)
			(setq dape-info-hide-mode-line nil)

			;; Pulse source line (performance hit)
			(add-hook 'dape-display-source-hook 'pulse-momentary-highlight-one-line)

			;; Showing inlay hints
			(setq dape-inlay-hints t)

			;; Save buffers on startup, useful for interpreted languages
			(add-hook 'dape-start-hook (lambda () (save-some-buffers t t)))
			
			(add-to-list 'dape-configs
				     (list 'debugpy-attach-port-zain
					   'modes '(python-mode python-ts-mode)
					   'port '(lambda () (read-number "Port: " 5678))
					   ':request "attach"
					   ':type "python"
					   ':pathMappings (vector '(:localRoot "/home/zjabbar/code/" :remoteRoot "/home/zjabbar/code/"))
					   ':justMyCode 'nil
					   ':showReturnValue 't))


			;; Kill compile buffer on build success
			(add-hook 'dape-compile-hook 'kill-buffer))
	   ))))

;; In Buffer Completion
(define corfu-configuration
  (home-emacs-configuration
   (packages (list emacs-corfu
		   #;((options->transformation '((with-branch . "emacs-compat=main")))
		   emacs-compat)))
   (init '((use-package corfu
			;; Optional customizations
			:custom
			(corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
			(corfu-auto t)                 ;; Enable auto completion
			(corfu-separator ?\s)          ;; Orderless field separator
			(corfu-quit-at-boundary 'separator)   ;; Never quit at completion boundary
			(corfu-quit-no-match nil)      ;; Never quit, even if there is no match
			(corfu-preview-current nil)    ;; Disable current candidate preview
			(corfu-preselect 'prompt)      ;; Preselect the prompt
			(corfu-on-exact-match nil)     ;; Configure handling of exact matches
			(corfu-scroll-margin 5)        ;; Use scroll margin

			;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
			;; :hook ((prog-mode . corfu-mode)
			;;        (shell-mode . corfu-mode)
			;;        (eshell-mode . corfu-mode))

			;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
			;; be used globally (M-/).  See also the customization variable
			;; `global-corfu-modes' to exclude certain modes.
			:init
			(global-corfu-mode))
	   ;; (global-corfu-mode)
	   ;; (corfu-history-mode)
	   ;; (setq corfu-cycle t
	   ;; 	 corfu-auto t
	   ;; 	 corfu-auto-prefix 2
	   ;; 	 corfu-auto-delay 0.0
	   ;; 	 corfu-quit-at-boundary 'separator
	   ;; 	 corfu-echo-documentation 0.25
	   ;; 	 corfu-preview-current 'insert
	   ;; 	 corfu-preselect-first nil)
	   ;; (define-key corfu-map (kbd "M-<SPC>") (function corfu-insert-separator))
	   ;; (define-key corfu-map (kbd "M-}") (function corfu-next))
	   ;; (define-key corfu-map (kbd "M-{") (function corfu-previous))
	   ))))

;; Templates
(define tempel-configuration
  (home-emacs-configuration
   (packages (list emacs-tempel))
   (init '((require 'tempel)
	   (defun tempel-setup-capf ()
	     (setq-local completion-at-point-functions
			 (cons (function tempel-expand)
			       completion-at-point-functions)))
	   (defun tempel-reload ()
	     (interactive)
	     (setq tempel--path-templates nil))
	   (add-hook 'prog-mode-hook 'tempel-setup-capf)
	   (add-hook 'text-mode-hook 'tempel-setup-capf)
	   (add-hook 'text-mode-hook
		     (lambda ()
		       (remove-hook 'completion-at-point-functions
				    'ispell-completion-at-point t)))
	   (define-key tempel-map (kbd "C-a") (function tempel-previous))
	   (define-key tempel-map (kbd "C-d") (function tempel-next))
	   (global-set-key (kbd "M-+") (function tempel-complete))))))

;; Completion at Point Functions
(define cape-configuration
  (home-emacs-configuration
   (packages (list emacs-cape))
   (init '((use-package cape
			:bind ("C-c p" . cape-prefix-map)
			:init
			(setq tab-always-indent 'complete)
			(add-to-list 'completion-at-point-functions (function cape-file)))
	   ))))

(define file-configuration
  (home-emacs-configuration
   (init '((setq dired-dwim-target t)))))


(define web-configuration
  (home-emacs-configuration
   (init '((setq eww-search-prefix "http://localhost:8080/search?q=")))))


(define citation-configuration
  (home-emacs-configuration
   (packages (list emacs-citar-org-roam))
   (init '((use-package citar
			:bind (:map citar-map
			       ("a" . citar-add-file-to-library))
			:bind-keymap ("C-c c" . citar-map)
			:custom
			(citar-bibliography '("/home/zjabbar/notes/bibtex/general_bibliography.bib"))
			(citar-notes-paths (list "/home/zjabbar/notes/")) ; List of directories for reference nodes
			(citar-library-paths (list "/home/zjabbar/library/")) ;
			(citar-open-note-function 'orb-citar-edit-note) ; Open notes in `org-roam'
			(citar-at-point-function 'embark-act)           ; Use `embark'
			:hook
			(LaTeX-mode . citar-capf-setup)
			(org-mode . citar-capf-setup))

	   (use-package citar-org
			:after (citar oc)
			:custom
			(org-cite-insert-processor 'citar)
			(org-cite-follow-processor 'citar)
			(org-cite-activate-processor 'citar))

	   (use-package citar-embark
			:after citar embark
			:no-require
			:config (citar-embark-mode))

	   (use-package citar-org-roam
			:after (citar citar-org org-roam org-roam-bibtex)
			:custom (citar-org-roam-capture-template-key "r")
			:config (citar-org-roam-mode))))))


(define buffer-configuration
  (home-emacs-configuration
   (packages (list emacs-tabspaces emacs-ace-window))
   (init '((use-package tabspaces
			:demand t
			:hook (after-init . tabspaces-mode)
			:commands (tabspaces-switch-or-create-workspace
				   tabspaces-open-or-create-project-and-workspace)
			:custom
			(tabspaces-use-filtered-buffers-as-default t)
			(tabspaces-default-tab "Default")
			(tabspaces-remove-to-default t)
			(tabspaces-include-buffers '("*scratch*"))
			(tab-bar-new-tab-choice "*scratch*")
			:config
			(dolist (name '(
					"State Estimation" ; State Estimation Learning
					"Foundations" ; Pure Math / Flashcards
					"Fair Active Learning" ; Finalizing Peter's Work
					"Development" ; Actual Work
					"Puzzles" ; LeetCode
					"Japanese" ; Japanese Learning / Coursework
					"Communications" ; Google Voice / Email / Discord
					"System" ; Emacs / Guix Sysadmin
					) ())
				(sleep-for 0.01)
				(tab-switch name))
			(tab-bar-close-tab-by-name "*scratch*")

			(with-eval-after-load 'consult
					      (consult-customize consult--source-buffer :hidden t :default nil)
					      ;; set consult-workspace buffer list
					      (defvar consult--source-workspace
						(list :name     "Workspace Buffers"
						      :narrow   ?w
						      :history  'buffer-name-history
						      :category 'buffer
						      :state    (function consult--buffer-state)
						      :default  t
						      :items    (lambda () (consult--buffer-query
									    :predicate (function tabspaces--local-buffer-p)
									    :sort 'visibility
									    :as (function buffer-name))))

						"Set workspace buffer list for consult-buffer.")
					      (add-to-list 'consult-buffer-sources 'consult--source-workspace)))))
   (early-init '((setq desktop-restore-frames nil
		       desktop-restore-in-current-display nil)
		 (setq switch-to-buffer-obey-display-actions t)
		 (defun mp-toggle-window-dedication ()
		   "Toggles window dedication in the selected window."
		   (interactive)
		   (set-window-dedicated-p (selected-window)
					   (not (window-dedicated-p (selected-window)))))))))

(define undo-configuration
  (home-emacs-configuration
   (packages (list emacs-vundo))
   (init '((global-set-key (kbd "C-x u") (function vundo))))))

(define project-configuration
  (home-emacs-configuration
   (packages (list git
		   direnv
		   emacs-envrc))
   (init '((require 'ansi-color)
	   (setq project-vc-ignores '(".direnv/*" "*.pyc" ".ob-jupyter/*"))
	   (defun colorize-compilation-buffer ()
	     (interactive)
	     (let ((inhibit-read-only t))
	       (ansi-color-apply-on-region (point-min) (point-max))))
	   (add-hook 'compilation-filter-hook (function colorize-compilation-buffer))
	   (add-hook 'org-mode-hook (function colorize-compilation-buffer))))))


(define hindi-configuration
  (home-emacs-configuration
   (packages (list font-lohit))))

(define urdu-configuration
  (home-emacs-configuration
   (packages (list font-vazir
		   font-amiri))))

(define japanese-configuration
  (home-emacs-configuration
   (packages (list emacs-ddskk
		   inkscape
		   jbr21
		   font-ipa-mj-mincho))
   (early-init '((global-unset-key (kbd "C-x t"))))
   (init '((use-package skk
			:after (consult)
			:if (display-graphic-p))
	   
	   (use-package facemenu
			:after skk)
	   
	   (defun jisho-word->japanese-part (jisho-word)
	     (list (gethash "word" (elt (gethash "japanese" jisho-word) 0))
		   (gethash "reading" (elt (gethash "japanese" jisho-word) 0))))

	   (defun jisho-word->english-part (jisho-word)
	     (gethash "english_definitions" (elt (gethash "senses" jisho-word) 0)))

	   (defun word->drill (word)
	     (if (car word)
		 (kanji-word->drill word)
		 (kana-word->drill (cdr word))))
	   (defun simple-word->drill (word)
	     (if (car word)
		 (simple-kanji-word->drill word)
		 (kana-word->drill (cdr word))))

	   (defvar *jisho-results* ())

	   (defun jisho-search->completing-read ()
	     (interactive)
	     (let* ((search-term (read-string "Search JISHO: "))
		    (url (concat "https://www.jisho.org/api/v1/search/words?keyword=" search-term)) ; Get correct URL to access Jisho API ; ; ;
		    ;; Get JSON File from URL
		    (contents (with-temp-buffer
			       (url-insert-file-contents url)
			       (json-parse-buffer :array-type 'list)))
		    ;; JSON File contains metadata on status, then a list of words
		    (status (gethash "meta" contents))
		    (words (gethash "data" contents))
		    ;; We will iterate over all words via word, and return a certain list in results
		    (word)
		    (vertico-sort-override-function (function identity)))
	       ;; Build Results
	       (setq *jisho-results* ())
	       (while words
		 (setq word (car words))
		 (setq total-word (append (jisho-word->japanese-part word) (jisho-word->english-part word)))
		 (setq *jisho-results* (append *jisho-results* (list (cons (string-join total-word " ") total-word))))
		 (setq words (cdr words)))
	       (alist-get
		(completing-read "Results: " *jisho-results*)
		*jisho-results* nil nil 'equal)))

	   (defun kanji-word->drill (word)
	     (apply 'format "{{%s}}\n{{%s}} {{%s}}\n" word))

	   (defun kana-word->drill (word)
	     (apply 'format "{{%s}}\n{{%s}}\n" word))

	   (defun simple-kanji-word->drill (word)
	     (apply 'format "%s\n{{%s}} {{%s}}\n" word))

	   (defun jisho->fc ()
	     (interactive)
	     (org-roam-with-file "/home/zjabbar/notes/20240702021713-japanese_vocabulary.org" t
				 (end-of-buffer)
				 (insert (concat "* " (word->drill (jisho-search->completing-read)) "\n"))
				 (org-fc-type-cloze-init 'deletion)))

	   (defun simple-jisho->fc ()
	     (interactive)
	     (org-roam-with-file "/home/zjabbar/notes/20240702021713-japanese_vocabulary.org" t
				 (end-of-buffer)
				 (insert (concat "* " (simple-word->drill (jisho-search->completing-read)) "\n"))
				 (org-fc-type-cloze-init 'deletion)))))))

(define graphical-browser-configuration
  (home-emacs-configuration
   (packages (list ;icecat
					;ublock-origin/icecat
	      librewolf
	      emacs-exwm-firefox
	      jami
	      jami-docs
	      hicolor-icon-theme
	      passff-host
	      
	      passff/icecat))
   (init '((setq browse-url-new-window-flag t)))))

(define pdf-tools-configuration
  (home-emacs-configuration
   (packages (list emacs-pdf-tools
		   emacs-nov-el))
   (init '((add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
	   (pdf-tools-install)
	   (defvar *current-mode* 'light)
	   #;(defun pdf-view-redisplay (&optional window)
	   "Redisplay page in WINDOW.

	   If WINDOW is t, redisplay pages in all windows."
	   (setq window nil)
	   (unless pdf-view-inhibit-redisplay
	   (if (not (eq t window))
	   (pdf-view-display-page
	   (pdf-view-current-page window)
	   window)
	   (dolist (win (get-buffer-window-list nil nil t))
	   (pdf-view-display-page
	   (pdf-view-current-page win)
	   win))
	   (when (consp image-mode-winprops-alist)
	   (dolist (window (mapcar (function car image-mode-winprops-alist)))
	   (unless (or (not (window-live-p window))
	   (eq (current-buffer)
	   (window-buffer window)))
	   (setf (pdf-view-window-needs-redisplay window) t)))))
	   (force-mode-line-update)))

	   (defun my/dark-mode ()
	     (interactive)
	     (cond ((eq *current-mode* 'light)
		    (modus-themes-toggle)
		    (add-hook 'pdf-view-mode-hook (function pdf-view-midnight-minor-mode)))
		   (t
		    (modus-themes-toggle)
		    (remove-hook 'pdf-view-mode-hook (function pdf-view-midnight-minor-mode)))
		   ))

	   ))))

(define cryptography-configuration
  (home-emacs-configuration
   (packages (list pinentry
		   emacs-pinentry
		   pinentry-emacs
		   password-store
					;pass-import
		   pass-otp
		   emacs-pass
		   emacs-password-store
		   emacs-password-store-otp
		   gnupg
		   openssh))
   (init '((defun pinentry-reload () (interactive)
	     (shell-command "gpg-connect-agent reloadagent /bye"))
	   (pinentry-start)
	   (require 'password-store)
	   (require 'password-store-otp)

	   (defun password-store-otp-token (entry)
	     "Return an OTP token from ENTRY."
	     (password-store-otp--related-error
	      (caddr (s-split "\n" (password-store--run "otp" entry)))))
	   (defun copy-zjabbar-hawaii-otp ()
	     (interactive)
	     (password-store-otp-token-copy "hawaii_edu_otp"))


	   ))))

(define elfeed-configuration
  (home-emacs-configuration
   (packages (list
	      mpv
	      emacs-empv
	      yt-dlp
	      emacs-elfeed
	      emacs-elfeed-tube
	      curl))
   (init '((setq elfeed-feeds '(("https://almostsuremath.com/feed/" math almost-sure reading)
				("https://karthinks.com/index.xml" crafter karthinks reading)
				("https://jamesg.blog/hf-papers.xml" machine-learning reading)
				("https://www.youtube.com/feeds/videos.xml?channel_id=UC2D2CMWXMOVWx7giW1n3LIg" health huberman)
				("https://www.youtube.com/feeds/videos.xml?channel_id=UCe0TLA0EsQbE-MjuHXevj2A" health jeff)

				;; ("https://www.youtube.com/feeds/videos.xml?channel_id=UCkFJBuwX2iPKCgCITXt2Bnw" fun fatguy)
				;; ("https://www.youtube.com/feeds/videos.xml?channel_id=UCrTW8WZTlOZMvvn_pl1Lpsg" fun nicob)
				;; ("https://twitchrss.appspot.com/vod/nicob" fun nicob twitch)
				;; ("https://www.youtube.com/feeds/videos.xml?channel_id=UCP9q8DRbsTDPhU4E0R3-1rA" fun league pekin)
				;; ("https://www.youtube.com/feeds/videos.xml?channel_id=UCT0fBcIYwMsp6IRCm5E3eTA" fun league pekin)
				;; ("https://twitchrss.appspot.com/vodonly/pekinwoof" fun league pekin twitch)
				;; ("https://www.youtube.com/feeds/videos.xml?channel_id=UCIkcvRgwGlzEtfGf7k2oL3g" fun league virkayu)
				;; ("https://www.youtube.com/feeds/videos.xml?channel_id=UCc3cbGWviHbC1OLJKFDfogA" fun league virkayu)
				;; ("https://www.youtube.com/feeds/videos.xml?channel_id=UCwE00vEJFzpO6j1rDJMLDfg" fun league virkayu)
				;; ("https://www.youtube.com/feeds/videos.xml?channel_id=UCkaw-9Mo41X_N8sT15EyRzA" fun league eagz)
				;; ("https://www.youtube.com/feeds/videos.xml?channel_id=UCu-3KO4dBHSuz-57j4RHTKw" fun league hidon)
				;; ("https://www.youtube.com/feeds/videos.xml?channel_id=UCcWrPkUDJRSPqt4kAF9DVsA" fun league leo)
				;; ("https://www.youtube.com/feeds/videos.xml?channel_id=UCOQe4ma4be9SZ1n8B2ijihQ" fun league rogue)
				;; ("https://www.youtube.com/feeds/videos.xml?channel_id=UCF_b_1kpeajcy03cb36zCkQ" fun ow awkward)
				;; ("https://www.youtube.com/feeds/videos.xml?channel_id=UCzxgSHk0g-D-eErxMiDy9UA" fun ow awkward)
				;; ("https://twitchrss.appspot.com/vod/awkward" fun ow awkward twitch)

				("https://www.youtube.com/feeds/videos.xml?channel_id=UCYO_jab_esuFRV4b17AJtAw" math grant)
				("https://www.youtube.com/feeds/videos.xml?channel_id=UCgBRykS2v-WV2YYUpR2V9jw" math allangles)
				("https://www.youtube.com/feeds/videos.xml?channel_id=UCPb7xe-MQ0KiJpaKBWFZtTA" math normal)
				("https://www.youtube.com/feeds/videos.xml?channel_id=UCr22xikWUK2yUW4YxOKXclQ" math beautiful)
				("https://www.youtube.com/feeds/videos.xml?channel_id=UCm5mt-A4w61lknZ9lCsZtBw" math brunton)
				("https://www.youtube.com/feeds/videos.xml?channel_id=UCoxcjq-8xIDTYp3uz647V5A" math numberphile)
				("https://www.youtube.com/feeds/videos.xml?channel_id=UCU8Nm_HV-GouPa-WmujPltQ" math barker)
				("https://www.youtube.com/feeds/videos.xml?channel_id=UC6jM0RFkr4eSkzT5Gx0HOAw" math penn)
				("https://www.youtube.com/feeds/videos.xml?channel_id=UCSju5G2aFaWMqn-_0YBtq5A" math parker)
				("https://www.youtube.com/feeds/videos.xml?channel_id=UCIu2_2RVc6_F6_7wv5TDHoA" math jsb)
				("https://www.youtube.com/feeds/videos.xml?playlist_id=PLTGkWQjAP0wovgVKVz1tUCYKZXCSmqsLm" math geometric-algebra)

				("https://www.youtube.com/feeds/videos.xml?channel_id=UCAiiOTio8Yu69c3XnR7nQBQ" crafter david)
				("https://www.youtube.com/feeds/videos.xml?channel_id=UC0uTPqBCFIpZxlz_Lv1tk_g" crafter prot)
				("https://protesilaos.com/codelog.xml" crafter prot)
				("https://www.youtube.com/feeds/videos.xml?channel_id=UCuj_loxODrOPxSsXDfJmpng" crafter andrew)
				("https://www.youtube.com/feeds/videos.xml?channel_id=UChW6oX-CYk5jWYZKZaMpVKg" crafter guix_social)

				("https://www.youtube.com/feeds/videos.xml?channel_id=UCkdmU8hGK4Fg3LghTVtKltQ" japanese cure-dolly)
				("https://www.youtube.com/feeds/videos.xml?channel_id=UC2_krAagEXVPftDXZCDiVZA" japanese kanamenaito)
				("https://www.youtube.com/feeds/videos.xml?channel_id=UCiX01KrL5XyKsxhjRhCC7oA" japanese takashi)
				("https://www.youtube.com/feeds/videos.xml?channel_id=UCpGJxlhKXfdOKkBhuDH6ujA" japanese ikechan)
				("https://www.youtube.com/feeds/videos.xml?channel_id=UCkex1wLTvYFnF0hfLXGE1kQ" japanese yuka)

				("https://www.youtube.com/feeds/videos.xml?channel_id=UC2Zs9v2hL2qZZ7vsAENsg4w" learning sung)
				("https://www.youtube.com/feeds/videos.xml?channel_id=UCjmynbA3C3Tm0koVy_8pfLw" learning sung)

				("https://www.youtube.com/feeds/videos.xml?channel_id=UCm0MFprGs8VWcfsq743FJ7A" lecture machine-learning washington intro)
				("https://www.youtube.com/feeds/videos.xml?playlist_id=PL5B3KLQNAC5j46Ro64xF7hLV6Uf-gHUHL" lecture continuum-mechanics)
				("https://www.youtube.com/feeds/videos.xml?playlist_id=PLoROMvodv4rMiGQp3WXShtMGgzqpfVfbU" lecture machine-learning statistics ng)
				("https://www.youtube.com/feeds/videos.xml?playlist_id=PLAYxx7zX5F1P5GG-9U8eJPL_MIsl1_8Zh" lecture statistics jsb)
				("https://www.youtube.com/feeds/videos.xml?playlist_id=PLAYxx7zX5F1OVOpgHZ42Swv-D1Fvo9cFh" lecture statistics jsb)
				("https://www.youtube.com/feeds/videos.xml?playlist_id=PLAYxx7zX5F1MUAvvhZQyA4ISyKYuoKdH6" lecture statistics jsb)
				("https://www.youtube.com/feeds/videos.xml?playlist_id=PLAYxx7zX5F1N2MCd_qzd_MLCyODNSUEVT" lecture statistics jsb)
				("https://www.youtube.com/feeds/videos.xml?playlist_id=PLAYxx7zX5F1PIvRju27s4CRAwhSGoxhVg" lecture statistics jsb)
				("https://www.youtube.com/feeds/videos.xml?playlist_id=PLPW2keNyw-usgvmR7FTQ3ZRjfLs5jT4BO" lecture machine-learning statistics theory)
				("https://www.youtube.com/feeds/videos.xml?playlist_id=PLoROMvodv4rP8nAmISxFINlGKSK4rbLKh" lecture machine-learning statistics theory)
				("https://www.youtube.com/feeds/videos.xml?playlist_id=PLhhyoLH6IjfxVOdVC1P1L5z5azs0XjMsb" lecture machine-learning tensorflow intro)
				("https://www.youtube.com/feeds/videos.xml?playlist_id=PLUl4u3cNGP62EaLLH92E_VCN4izBKK6OE" lecture machine-learning matrix-calculus)
				("https://www.youtube.com/feeds/videos.xml?playlist_id=PLUl4u3cNGP63oMNUHXqIUcrkS2PivhN3k" lecture machine-learning matrix-methods)
				("https://www.youtube.com/feeds/videos.xml?playlist_id=PLArBKNfJxuukeHyrA_bqc1g52Acr_YQkz" lecture math optimal-transport)
				("https://www.youtube.com/feeds/videos.xml?playlist_id=PLJ6garKOlK2qKVhRm6UwvcQ46wK-ciHbl" lecture math optimal-transport)
				("https://www.youtube.com/feeds/videos.xml?playlist_id=PLhhyoLH6IjfxeoooqP9rhU3HJIAVAJ3Vz" lecture machine-learning pytorch intro)
				("https://www.youtube.com/feeds/videos.xml?playlist_id=PLE18841CABEA24090" lecture mit sicp)
				("https://www.youtube.com/feeds/videos.xml?playlist_id=PLUl4u3cNGP60B0PQXVQyGNdCyCTDU1Q5j" lecture mit ml-health)
				("https://www.youtube.com/feeds/videos.xml?playlist_id=PLUl4u3cNGP63gFHB6xb-kVBiQHYe_4hSi" lecture mit ai)
				("https://www.youtube.com/feeds/videos.xml?playlist_id=PLUl4u3cNGP63pfpS1gV5P9tDxxL_e4W8O" lecture mit cv)
				("https://www.youtube.com/feeds/videos.xml?playlist_id=PLUl4u3cNGP60uVBMaoNERc6knT_MgPKS0" lecture mit stats)
				("https://www.youtube.com/feeds/videos.xml?playlist_id=PLUl4u3cNGP61MdtwGTqZA0MreSaDybji8" lecture mit prob)
				("https://www.youtube.com/feeds/videos.xml?playlist_id=PLEC88901EBADDD980" lecture mit odes)
				("https://www.youtube.com/feeds/videos.xml?playlist_id=PL221E2BBF13BECF6C" lecture mit linear)
				("https://www.youtube.com/feeds/videos.xml?playlist_id=PLNfNKbfMcu1GohKHv_u7Kf0AQwD2uyFew" lecture arnold feec)
				("https://www.youtube.com/feeds/videos.xml?playlist_id=PL1ysOEBe5977vlocXuRt6KBCYu_sdu1Ru" lecture cuda)
				("https://www.youtube.com/feeds/videos.xml?playlist_id=PLUl4u3cNGP63micsJp_--fRAjZXPrQzW_" lecture mit functional)
				("https://www.youtube.com/feeds/videos.xml?playlist_id=PLo4jXE-LdDTTIIIRwqK35CbFJieSJEcVR" lecture functional)
				("https://www.youtube.com/feeds/videos.xml?playlist_id=PLLq_gUfXAnkkvL_UoCGivS0wOYhwCtczI" lecture pde ictp-2016)
				("https://www.youtube.com/feeds/videos.xml?playlist_id=PLp0hSY2uBeP_mPvDhVS-MwLy2xYrINRrU" lecture pde ictp-2020)
				("https://www.youtube.com/feeds/videos.xml?playlist_id=PLGCj8f6sgswntUil8yzohR_qazOfYZCg_" lecture pde tisdell)
				("https://www.youtube.com/feeds/videos.xml?playlist_id=PLF6061160B55B0203" lecture pde commutant)
				("https://www.youtube.com/feeds/videos.xml?playlist_id=PLdgVBOaXkb9Ab7UM8sCfQWgdbzxkXTNVD" lecture pde khan)
				("https://www.youtube.com/feeds/videos.xml?playlist_id=PLMrJAkhIeNNQromC4WswpU1krLOq5Ro6S" lecture pde brunton)
				("https://www.youtube.com/feeds/videos.xml?playlist_id=PLLq_gUfXAnkmC-VWIJ_HW8cdOZLEtHfXJ" lecture ictp dynamical-systems)
				("https://www.youtube.com/feeds/videos.xml?playlist_id=PLbN57C5Zdl6j_qJA-pARJnKsmROzPnO9V" lecture strogatz dynamical-systems)
				("https://www.youtube.com/feeds/videos.xml?playlist_id=PL4C4C8A7D06566F38" lecture mit calc multivariable applied)
				("https://www.youtube.com/feeds/videos.xml?playlist_id=PLzUeAPxtWcqzr80lS25FrzMn7a36BuXhj" lecture algebra gross)
				("https://www.youtube.com/feeds/videos.xml?playlist_id=PLpRLWqLFLVTCL15U6N3o35g4uhMSBVA2b" lecture topology pierre-albin)
				("https://www.youtube.com/feeds/videos.xml?playlist_id=PLTBqohhFNBE_09L0i-lf3fYXF5woAbrzJ" lecture topology tokieda)
				("https://www.youtube.com/feeds/videos.xml?playlist_id=PLLq_gUfXAnkl8bjQh-hGQ9u24xZP9x0dx" lecture topology bruno)
				("https://www.youtube.com/feeds/videos.xml?playlist_id=PLp0hSY2uBeP_HDgkCSrG5pccHYfudTJYI" lecture topology pavel)
				("https://www.youtube.com/feeds/videos.xml?playlist_id=PLBEeOnR8lrBHNZWwk8-pHOQLQnP3u8bO8" lecture topology clark)
				("https://www.youtube.com/feeds/videos.xml?playlist_id=PL6763F57A61FE6FE8" lecture topology wildberger)))
	   (define-advice elfeed-search--header (:around (oldfun &rest args))
	     (if elfeed-db
		 (apply oldfun args)
		 "No database loaded yet"))
	   (require 'elfeed-tube)
	   (require 'elfeed-tube-fill)
	   (elfeed-tube-setup)
	   (setq-default elfeed-search-filter "")
	   (setq-default elfeed-search-title-max-width 100)
	   (setq-default elfeed-search-title-min-width 100)

	   ;; (defun browse-url-mpv (url &optional new-window)
	   ;;   (start-process "mpv" "*mpv*" "mpv" "--ytdl-format=\"bestvideo[height<?720]\"" url))

	   ;; (defun browse-url-mpv (url &optional new-window)
	   ;;   (start-process "mpv" "*mpv*" "mpv" "--ytdl-format=mp4" url))

	   (defun browse-url-mpv (url &optional new-window)
	     (start-process "mpv" "*mpv*" "mpv" "--ytdl-format=bestvideo[height<=?480]+bestaudio/best" url))

	   ;; (defun browse-url-mpv (url &optional new-window)
	   ;;   (start-process "mpv" "*mpv*" "mpv" url))

	   (add-to-list 'browse-url-handlers (cons "https:\\/\\/www\\.youtube." 'browse-url-mpv))
	   (add-to-list 'browse-url-handlers (cons "https:\\/\\/www\\.twitch." 'browse-url-mpv))

	   (add-hook 'elfeed-new-entry-hook
		     (elfeed-make-tagger :feed-url "youtube\\.com"
					 :add '(video youtube)))


	   (setq youtube-dl-path "yt-dlp --sponsorblock-remove all")
	   (setq youtube-dl-output-dir "~/lectures/")

	   (defun elfeed-download-video-best-quality ()
	     "Download a video using youtube-dl."
	     (interactive)
	     (async-shell-command (format "%s -o \"%s%s\" -f bestvideo+bestaudio %s"
					  youtube-dl-path
					  youtube-dl-output-dir
					  "%(title)s.%(ext)s"
					  (elfeed-entry-link elfeed-show-entry))))

	   (defun elfeed-download-video-audio-only ()
	     "Download a video using youtube-dl."
	     (interactive)
	     (async-shell-command (format "%s -o \"%s%s\" -x %s"
					  youtube-dl-path
					  youtube-dl-output-dir
					  "%(title)s.%(ext)s"
					  (elfeed-entry-link elfeed-show-entry))))

	   ))))

(define music-configuration
  (home-emacs-configuration
   (packages (list
	      ffmpeg
	      alsa-utils
	      emacs-alsamixer-el
	      emacs-bluetooth
	      emacs-emms
					;python-tinytag
	      ))
   (init '((use-package emms
			:config
			(require 'emms-setup)
			(require 'emms-info-tinytag)
			(setq emms-info-tinytag-python-name "python3")

			(emms-all)
			(setq emms-player-list '(emms-player-mpv)
			      emms-info-functions '(emms-info-tinytag))

			(defvar emms-player-mpv-volume 100)
			(defun emms-player-mpv-get-volume ()
			  (emms-player-mpv-cmd '(get_property volume)
					       (function (lambda (vol err)
							   (unless err
							     (let ((vol (truncate vol)))
							       (setq emms-player-mpv-volume vol)
							       (message "Music volume: %s%%"
									vol)))))))

			(defun emms-player-mpv-raise-volume (&optional amount)
			  (interactive)
			  (let* ((amount (or amount 10))
				 (new-volume (+ emms-player-mpv-volume amount)))
			    (if (> new-volume 100)
				(emms-player-mpv-cmd '(set_property volume 100))
				(emms-player-mpv-cmd (list 'add 'volume amount))))
			  (emms-player-mpv-get-volume))
			(defun emms-player-mpv-lower-volume (&optional amount)
			  (interactive)
			  (emms-player-mpv-cmd (list 'add 'volume (- (or amount '10))))
			  (emms-player-mpv-get-volume))
					;(emms-add-directory-tree "~/music/random/")
					;(emms-shuffle)
			(emms-player-mpv-lower-volume 30)

			(global-set-key (kbd "<XF86AudioPrev>") 'emms-previous)
			(global-set-key (kbd "<XF86AudioNext>") 'emms-next)
			(global-set-key (kbd "<XF86AudioPlay>") 'emms-pause)
			(global-set-key (kbd "<XF86AudioRaiseVolume>") 'alsamixer-up-volume)
			(global-set-key (kbd "<XF86AudioLowerVolume>") 'alsamixer-down-volume)
			(global-set-key (kbd "<XF86AudioMute>") 'alsamixer-toggle-mute)
			(global-set-key (kbd "<f9>") 'emms-previous)
			(global-set-key (kbd "<f10>") 'emms-next)
			(global-set-key (kbd "<f11>") 'emms-pause)
			(global-set-key (kbd "<f3>") 'alsamixer-up-volume)
			(global-set-key (kbd "<f2>") 'alsamixer-down-volume)
			(global-set-key (kbd "<f1>") 'alsamixer-toggle-mute))
	   ))))

(define email-configuration
  (home-emacs-configuration
   (packages (list
	      #;((options->transformation
	      '((with-branch . "emacs-org-msg=master")
	      #;(with-git-url . "emacs-org-msg=https://github.com/danielfleischer/org-msg.git")))
	      emacs-org-msg)
	      emacs-org-msg-master
	      isync
	      mu
	      emacs-mu4e-alert
	      msmtp))
   (init '((setq movemail-program-name "movemail")
	   (require 'mu4e)
	   (require 'mu4e-alert)
	   (defun mu4e--modeline-string () "")
	   (add-to-list 'display-buffer-alist
			(list (regexp-quote mu4e-main-buffer-name)
			      'display-buffer-same-window))
	   (setq mu4e-get-mail-command (format "INSIDE_EMACS=%s mbsync -c ~/.config/mbsyncrc -a" emacs-version)
		 epa-pinentry-mode 'ask
		 mu4e-sent-messages-behavior 'delete)
	   (setq org-msg-enforce-css "~/notes/static/css/site.css")
	   (setq mu4e-hide-index-messages t)
	   (setq mu4e-mu-home "/home/zjabbar/.cache/mu")
	   (setq mail-user-agent 'mu4e-user-agent)
	   (add-hook 'after-init-hook (function mu4e-alert-enable-mode-line-display))
	   (require 'org-msg)
	   (setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil title:nil email:nil tex:mathjax"
		 org-msg-startup "hidestars indent inlineimages"
		 org-msg-greeting-fmt "\nAloha%s,\n\n"
		 org-msg-recipient-names '(("zaijab2000@gmail.com" . "Zain")
					   ("pyw@hawaii.edu" . "Dr. Washington")
					   ("apopov@hawaii.edu" . "Dr. Popov")
					   ("sue@math.hawaii.edu" . "Sue"))
		 org-msg-greeting-name-limit 3
		 org-msg-default-alternatives '((new		. (text html))
						(reply-to-html	. (text html))
						(reply-to-text	. (text)))
		 org-msg-convert-citation t)
	   (org-msg-mode)
	   (add-hook 'org-msg-edit-mode-hook 'mml-secure-message-sign)
	   (setq mml-secure-openpgp-sign-with-sender t)
	   (setq mu4e-change-filenames-when-moving t)
	   (setq mu4e-update-interval 300)
	   (setq mu4e-get-mail-command  "mbsync -a -c ~/.config/mbsyncrc")

	   (setq org-export-with-toc nil)
	   (setq org-export-with-tile t)
	   (setq org-mu4e-convert-to-html t)
	   (setq message-send-mail-function 'message-send-mail-with-sendmail)
	   (setq sendmail-program "msmtp")
	   (setq message-sendmail-extra-arguments '("--read-envelope-from"))
	   (setq message-sendmail-f-is-evil t)
	   (setq mu4e-contexts
		 (list (make-mu4e-context
			:name "Personal"
			:enter-func (lambda ()
				      (mu4e-message "Entering zaijab2000_gmail context")
				      (when (string-match-p (buffer-name (current-buffer)) "mu4e-main")
					(revert-buffer)))
			:leave-func (lambda ()
				      (mu4e-message "Leaving gmail context")
				      (when (string-match-p (buffer-name (current-buffer)) "mu4e-main")
					(revert-buffer)))
			:match-func (lambda (msg)
				      (when msg
					(or (mu4e-message-contact-field-matches msg :to "zaijab2000@gmail.com")
					    (mu4e-message-contact-field-matches msg :from "zaijab2000@gmail.com")
					    (mu4e-message-contact-field-matches msg :cc "zaijab2000@gmail.com")
					    (mu4e-message-contact-field-matches msg :bcc "zaijab2000@gmail.com")
					    (string-match-p "^/zaijab2000/Inbox" (mu4e-message-field msg :maildir)))))
			:vars '((user-mail-address            . "zaijab2000@gmail.com")
				(smtpmail-smtp-user           . "zaijab2000@gmail.com")
				(mu4e-compose-signature       . "Mahalo")
				(smtpmail-smtp-server         . "smtp.gmail.com")
				(smtpmail-smtp-service        . 587 )
				(mu4e-maildir-shortcuts       . ((:maildir "/zaijab2000/Inbox" :key ?i)))
				(mu4e-bookmarks
				 .
				 ((:name  "Unread messages"
				   :query "maildir:/gmail/Inbox AND flag:unread AND NOT flag:trashed AND NOT outdoorexperten"
				   :key ?u)
				  (:name "Today's messages"
				   :query "maildir:/gmail/Inbox AND date:today..now"
				   :key ?t)
				  (:name "Last 7 days"
				   :query "maildir:/gmail/Inbox AND date:7d..now"
				   :hide-unread t
				   :key ?w)
				  (:name "Deleted"
				   :query "flag:trashed"
				   :key ?d)
				  (:name "Possibly garbage"
				   :query "bokio OR outdoorexperten"
				   :key ?g)))))

		       (make-mu4e-context
			:name "School"
			:enter-func (lambda ()
				      (mu4e-message "Entering school context")
				      (when (string-match-p (buffer-name (current-buffer)) "mu4e-main")
					(revert-buffer)))
			:leave-func (lambda ()
				      (mu4e-message "Leaving school context")
				      (when (string-match-p (buffer-name (current-buffer)) "mu4e-main")
					(revert-buffer)))
			:match-func (lambda (msg)
				      (when msg
					(or (mu4e-message-contact-field-matches msg :to "zjabbar@hawaii.edu")
					    (mu4e-message-contact-field-matches msg :from "zjabbar@hawaii.edu")
					    (mu4e-message-contact-field-matches msg :cc "zjabbar@hawaii.edu")
					    (mu4e-message-contact-field-matches msg :bcc "zjabbar@hawaii.edu")


					    )))

			:vars '((user-mail-address       . "zjabbar@hawaii.edu")
				(smtpmail-smtp-user      . "zjabbar@hawaii.edu")
				(smtpmail-smtp-server    . "smtp.gmail.com")
				(smtpmail-smtp-service   . 587)
				(mu4e-compose-signature  . "Mahalo")
				(mu4e-maildir-shortcuts  . ((:maildir "/zjabbar/Inbox" :key ?i)))
				(mu4e-bookmarks .
						((:name  "All school mails"
						  :query "maildir:/zjabbar/Inbox"
						  :key ?a)
						 (:name  "Unread school messages"
						  :query "maildir:/zjabbar/Inbox AND flag:unread AND NOT flag:trashed"
						  :key ?u)))))
		       
		       (make-mu4e-context
			:name "Math"
			:enter-func (lambda ()
				      (mu4e-message "Entering Math context")
				      (when (string-match-p (buffer-name (current-buffer)) "mu4e-main")
					(revert-buffer)))
			:leave-func (lambda ()
				      (mu4e-message "Leaving Math context")
				      (when (string-match-p (buffer-name (current-buffer)) "mu4e-main")
					(revert-buffer)))
			:match-func (lambda (msg)
				      (when msg
					(or (mu4e-message-contact-field-matches msg :to "zjabbar@math.hawaii.edu")
					    (mu4e-message-contact-field-matches msg :from "zjabbar@math.hawaii.edu")
					    (mu4e-message-contact-field-matches msg :cc "zjabbar@math.hawaii.edu")
					    (mu4e-message-contact-field-matches msg :bcc "zjabbar@math.hawaii.edu")


					    )))

			:vars '((user-mail-address       . "zjabbar@math.hawaii.edu")
				(smtpmail-smtp-user      . "zjabbar@math.hawaii.edu")
				(smtpmail-smtp-server    . "smtp.gmail.com")
				(smtpmail-smtp-service   . 587)
				(mu4e-compose-signature  . "Mahalo")
				(mu4e-maildir-shortcuts  . ((:maildir "/zjabbar_hawaii_math/Inbox" :key ?i)))
				(mu4e-bookmarks .
						((:name  "All school mails"
						  :query "maildir:/zjabbar_hawaii_math/Inbox"
						  :key ?a)
						 (:name  "Unread school messages"
						  :query "maildir:/zjabbar_hawaii_math/Inbox AND flag:unread AND NOT flag:trashed"
						  :key ?u)))))
		       ))

	   ))))

(define notes-configuration
  (home-emacs-configuration
   (packages (list
	      emacs-org-roam
	      emacs-org-roam-ui
	      emacs-org-roam-bibtex
	      emacs-org-fc
	      anki
	      emacs-org-drill
	      emacs-kanji))
   (init '((use-package sqlite)

	   (use-package org-roam
			:demand t
			:after (org sqlite)
			:custom
			(org-roam-directory "~/notes")
			(org-roam-v2-ack t)
			(org-roam-capture-templates
			 '(("i" "Default" plain "%?"
			    :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE: ${title}\n#+SETUPFILE: latex_header.org\n#+FILETAGS:")
			    :unnarrowed t)
			   ("p" "Python" plain "%?"
			    :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE: ${title}\n#+SETUPFILE: latex_header.org\n#+FILETAGS: :Programming:Python:\n#+PROPERTY: header-args:jupyter-python :session ${slug}")
			    :unnarrowed t)
			   ("r" "reference" plain "%?" :if-new
			    (file+head
			     "%(concat (when citar-org-roam-subdir (concat citar-org-roam-subdir \"/\")) \"${citar-citekey}.org\")"
			     "#+TITLE: ${note-title}\n#+SETUPFILE: latex_header.org\n#+FILETAGS: :Reference:\n")
			    :unnarrowed t :immediate-finish t)))
			(org-roam-db-node-include-function (lambda () (not (member "FC" (org-get-tags)))))
			(org-roam-node-display-template (concat "${title:*} " (propertize "${tags}" 'face 'org-tag)))
			:config
			(org-roam-db-autosync-mode)
			(define-key org-mode-map (kbd "C-c C-t") (function org-roam-tag-add)))

	   (use-package org-roam-node
			:after org-roam)

	   (use-package org-roam-bibtex
			:after org-roam
			:config
			(org-roam-bibtex-mode)
			(setq bibtex-completion-bibliography '("/home/zjabbar/notes/bibtex/general_bibliography.bib")))


	   (use-package websocket
			:after org-roam)

	   (use-package org-roam-ui
			:after org-roam
			:hook (after-init . org-roam-ui-mode)
			:custom
			(org-roam-ui-sync-theme t)
			(org-roam-ui-follow t)
			(org-roam-ui-update-on-save t)
			(org-roam-ui-open-on-start nil))

	   (use-package org-fc
			:custom
			(org-fc-directories '("~/notes/"))
			(org-fc-flashcard-tag "FC")
			(org-fc-suspended-tag "Suspended")
			:config
			;; (org-fc-cache-mode)
			;; Based on `org-log-beginning'
			(defun org-fc-review-data-position (&optional create)
			  "Return (BEGINNING . END) points of the review data drawer.
When optional argument CREATE is non-nil, the function creates a
drawer, if necessary.  Returned position ignores narrowing.

BEGINNING is the start of the first line inside the drawer,
END is the start of the line with :END: on it."
			  (org-with-wide-buffer
			   (org-end-of-meta-data)
			   (let ((regexp (concat "^[ \t]*:" (regexp-quote org-fc-review-data-drawer) ":[ \t]*$"))
				 (end (if (org-at-heading-p) (point)
					  (save-excursion (outline-next-heading) (point))))
				 (case-fold-search t))
			     (catch 'exit
			       ;; Try to find existing drawer.
			       (while (re-search-forward regexp end t)
				 (let ((element (org-element-at-point)))
				   (when (eq (org-element-type element) 'drawer)
				     (throw 'exit
					    (cons (org-element-property :contents-begin element)
						  (org-element-property :contents-end element))))))
			       ;; No drawer found.  Create one, if permitted.
			       (when create
				 (unless (bolp) (insert "\n"))
				 (let ((beg (point)))
				   (insert ":" org-fc-review-data-drawer ":\n:END:\n")
				   (org-indent-region beg (point)))
				 (cons
				  (line-beginning-position 0)
				  (line-beginning-position 0)))))))

			(define-key org-fc-review-rate-mode-map (kbd "n") (function org-fc-review-skip-card)))

	   ))))

(define website-configuration
  (home-emacs-configuration
   (packages (list python-pygments emacs-engrave-faces))
   (init '((require 'ucs-normalize)
	   (defun commonplace/get-title (file)
	     "For a given file, get its TITLE keyword."
	     (with-current-buffer
	      (get-file-buffer file)
	      (cadar (org-collect-keywords '("TITLE")))))
	   (require 'cl)
	   (require 'cl-lib)

	   (defun commonplace/slugify-title (title)
	     "Convert TITLE to a filename-suitable slug.  Use hyphens rather than underscores."
	     (cl-flet* ((nonspacing-mark-p (char)
					   (eq 'Mn (get-char-code-property char 'general-category)))
			(strip-nonspacing-marks (s)
						(apply (function string) (seq-remove (function nonspacing-mark-p)
										     (ucs-normalize-NFD-string s))))
			(cl-replace (title pair)
				    (replace-regexp-in-string (car pair) (cdr pair) title)))
		       (let* ((pairs
			       (quote (("['\\?,%]" . "")
				       ("[^[:alnum:][:digit:]]" . "-") ;; convert anything not alphanumeric
				       ("--*" . "-") ;; remove sequential underscores
				       ("^-" . "") ;; remove starting underscore
				       ("-$" . "")))) ;; remove ending underscore
			      (slug (-reduce-from (function cl-replace) (strip-nonspacing-marks title) pairs)))
			 (downcase slug))))


	   (defun get-title (file)
	     "For a given file, get its TITLE keyword."
	     (with-current-buffer
	      (get-file-buffer file)
	      (cadar (org-collect-keywords '("TITLE")))))

	   (defun commonplace/slugify-export-output-file-name (output-file)
	     (message output-file)
	     (let* ((title (get-title (buffer-file-name (buffer-base-buffer))))
		    (directory (file-name-directory output-file))
		    (slug (s-truncate 80 (commonplace/slugify-title title) "")))
	       (concat directory slug ".html")))


	   (defun my-trim-left (s)
	     "Remove whitespace at the beginning of S."
	     (declare (pure t) (side-effect-free t))
	     (save-match-data
	      (if (string-match "[0-9]+-" s)
		  (replace-match "" t t s)
		  s)))

	   (advice-add 'org-export-resolve-id-link :filter-return (function my-trim-left))
	   (defun my/org-id-path-fix (strlist)
	     (file-name-nondirectory strlist))

	   (advice-add 'org-export-resolve-id-link :filter-return (function my/org-id-path-fix))
	   (defun my/org-id-underscore-fix (strlist)
	     (s-replace-regexp "_" "-" strlist))

	   (advice-add 'org-export-resolve-id-link :filter-return (function my/org-id-underscore-fix))
	   (setq org-export-with-broken-links 'mark
		 org-export-with-properties nil
		 org-export-with-tags nil
		 org-export-babel-evaluate nil
		 org-export-with-drawers '(not "LOGBOOK" "REVIEW_DATA"))
	   (defun zain-publish ()
	     (interactive)
	     (let ((current-prefix-arg (list 4))
		   (default-directory "~/code/zaijab.github.io"))
	       (advice-add 'org-export-output-file-name :filter-return (function commonplace/slugify-export-output-file-name))
	       (call-interactively 'org-publish-all)
	       (advice-remove 'org-export-output-file-name (function commonplace/slugify-export-output-file-name))
	       (call-process-shell-command "git add -A;git commit -am \"Updating Website\";git push -fu origin roam" nil 0)))
	   (global-set-key (kbd "s-p") 'zain-publish)

	   (defun org-export-output-file-name-modified (orig-fun extension &optional subtreep pub-dir)
	     (unless pub-dir
	       (setq pub-dir "/home/zjabbar/.cache/note-export/")
	       (unless (file-directory-p pub-dir)
		 (make-directory pub-dir)))
	     (apply orig-fun extension subtreep pub-dir nil))
	   (advice-add 'org-export-output-file-name :around (function org-export-output-file-name-modified))
	   (setq org-latex-listings 'engraved)

	   (use-package org-latex-preview
			:config
			;; Increase preview width
			(plist-put org-latex-preview-appearance-options
				   :page-width 0.8)

			(plist-put org-latex-preview-appearance-options
				   :zoom 1.5)
			;; Use dvisvgm to generate previews
			;; You don't need this, it's the default:
			(setq org-latex-preview-process-default 'dvisvgm)
			
			;; Turn on auto-mode, it's built into Org and much faster/more featured than
			;; org-fragtog. (Remember to turn off/uninstall org-fragtog.)
			(add-hook 'org-mode-hook 'org-latex-preview-auto-mode)

			;; Block C-n, C-p etc from opening up previews when using auto-mode
			(setq org-latex-preview-auto-ignored-commands
			      '(next-line previous-line mwheel-scroll
					  scroll-up-command scroll-down-command))

			;; Enable consistent equation numbering
			(setq org-latex-preview-numbered t)

			;; Bonus: Turn on live previews.  This shows you a live preview of a LaTeX
			;; fragment and updates the preview in real-time as you edit it.
			;; To preview only environments, set it to '(block edit-special) instead
			(setq org-latex-preview-live t)

			;; More immediate live-previews -- the default delay is 1 second
			(setq org-latex-preview-live-debounce 0.25))

	   
	   (setq texmathp-tex-commands '(("lflalign" env-on)))
	   (setq org-latex-compiler "pdflatex")
	   (setq org-latex-title-command (concat
					  "\\pagestyle{fancy}"
					  "\\begin{titlepage}\n"
					  "\\vspace*{\\fill}\n"
					  "\\centering\n"
					  "{\\huge \\textmd{\\textbf{%t}} \\par }\n"
					  "\\vspace{0.1in}\n"
					  "{\\normalsize %a \\par}\n"
					  "\\vspace{0.1in}\n"
					  "{\\large For \\professor \\ On \\duedate \\ \\par}\n"
					  "\\vspace*{\\fill}\n"
					  "\\end{titlepage}\n"))


	   (setq org-publish-project-alist
		 '(("orgfiles"
		    :base-directory "~/notes/"
		    :publishing-directory "~/code/zaijab.github.io/"
		    :publishing-function org-html-publish-to-html
		    :exclude "latex_header.org"
		    :include ("20220925152629-index.org"
			      "20220925155207-about.org"
			      "20230225143306-posts.org"
			      "20230225142818-notation.org"
			      "20230225142533-category_theory.org")
		    :with-toc nil
		    :exclude-tags ("draft" "private" "noexport")
		    :html-head
		    "
          <link rel=\"stylesheet\" href=\"static/css/site.css\" type=\"text/css\"/>
          <header><div class=\"menu\"><ul>
          <li><a href=\"/\">/</a></li>
          <li><a href=\"/about\">/about</a></li>
          <li><a href=\"/posts\">/posts</a></li></ul></div></header>
          <script src=\"static/js/nastaliq.js\"></script>
          <script src=\"static/js/stacking.js\"></script>
          <link href='https://unpkg.com/tippy.js@6.2.3/themes/light.css' rel='stylesheet'>
          <script src=\"https://unpkg.com/@popperjs/core@2\"></script>
          <script src=\"https://unpkg.com/tippy.js@6\"></script>
          <script>
          document.addEventListener('DOMContentLoaded', function() {
            let page = document.querySelector('.page');
            if (page) {
              initializePreviews(page);
            }
          });
          </script>
<script>MathJax = { loader: { load: ['[custom]/xypic.js'], paths: {custom: 'https://cdn.jsdelivr.net/gh/sonoisa/XyJax-v3@3.0.1/build/'} }, tex: { packages: {'[+]': ['xypic']}, macros: { R: \"{\\\\bf R}\" } } };</script><script id=\"MathJax-script\" async src=\"https://cdn.jsdelivr.net/npm/mathjax@3.2.2/es5/tex-chtml-full.js\"></script>
<div class=\"grid-container\"><div class=\"ds-grid\"><div class=\"page\">"
		    :html-preamble nil
		    :html-postamble nil
		    :recursive nil
		    :html-mathjax-template "<script>MathJax = { loader: { load: ['[custom]/xypic.js'], paths: {custom: 'https://cdn.jsdelivr.net/gh/sonoisa/XyJax-v3@3.0.1/build/'} }, tex: { packages: {'[+]': ['xypic']}, macros: { R: \"{\\\\bf R}\" } } };</script><script id=\"MathJax-script\" async src=\"https://cdn.jsdelivr.net/npm/mathjax@3.2.2/es5/tex-chtml-full.js\"></script>")
		   ("static"
		    :base-directory "~/notes/static"
		    :base-extension any
		    :recursive t
		    :publishing-directory "~/code/zaijab.github.io/static"
		    :publishing-function org-publish-attachment)
		   ("CNAME"
		    :base-directory "~/notes/CNAME/"
		    :base-extension any
		    :publishing-directory "~/code/zaijab.github.io/"
		    :publishing-function org-publish-attachment)
		   ("zaindaman" :components ("orgfiles" "static" "CNAME"))))



	   ))))


(define org-mode-configuration
  (home-emacs-configuration
   (packages (list 
					;		   emacs-org-modern
	      emacs-cdlatex
	      font-latin-modern
	      emacs-tempel
	      emacs-valign
	      emacs-org-present
	      emacs-org-tree-slide
	      emacs-consult-org-roam
	      emacs-calfw
	      emacs-calfw-blocks
	      texlive
	      texlive-xypic
	      texlive-bin
	      imagemagick))
   (init '(
	   (use-package consult-org-roam
			:after (org-roam org consult)
			:config (consult-org-roam-mode))
	   
	   (use-package org
			:config

			(require 'org-tree-slide)
			(setq org-tree-slide-activate-message ""
			      org-tree-slide-deactivate-message "")
			
			(setq org-tree-slide-cursor-init nil)
			(require 'ox)
			(require 'calfw)
			(require 'calfw-org)
			(require 'calfw-blocks)
			(setq org-startup-folded t)

			(setq org-list-allow-alphabetical t)
			(setq org-structure-template-alist
			      '(("a" . "export ascii") ("c" . "center") ("C" . "comment")
				("e" . "example") ("E" . "export") ("h" . "export html")
				("l" . "export latex") ("q" . "quote") ("s" . "src") ("v" . "verse")
				("p" . "PROOF") ("t" . "THEOREM")))

			(defun cfw:date-before (date num)
			  "Return the date before NUM days from DATE."
			  (calendar-gregorian-from-absolute
			   (- (calendar-absolute-from-gregorian date) num)))

			(setq calfw-blocks-lines-per-hour 3
			      calfw-blocks-min-block-width 1
			      calfw-blocks-earliest-visible-time '(6 0))
			(define-key cfw:calendar-mode-map (kbd "W") (function calfw-blocks-change-view-block-week))
			(define-key cfw:calendar-mode-map (kbd "D") (lambda () (interactive) (calfw-blocks-change-view-block-nday 3)))
			(setq org-cycle-separator-lines 1)
			(setq org-agenda-show-log-scoped t)
			(setq org-agenda-prefix-format '((agenda  . "  • %?-12t% s")
							 (timeline  . "  % s")
							 (todo  . " %i %-12:c")
							 (tags  . " %i %-12:c")
							 (search . " %i %-12:c")))

			(defun cfw:render-truncate (org limit-width &optional ellipsis)
			  "[internal] Truncate a string ORG with LIMIT-WIDTH, like `truncate-string-to-width'."
			  (setq org (replace-regexp-in-string "\n" " " org))
			  (if (< limit-width (string-width org))
			      (let ((str (truncate-string-to-width
					  (substring org 0) limit-width 0 nil nil)))
				(cfw:tp str 'mouse-face 'highlight)
				(unless (get-text-property 0 'help-echo str)
				  (cfw:tp str 'help-echo org))
				str)
			      org))

			(defun cfw:org-get-timerange (text)
			  "Return a range object (begin end text). If TEXT does not have a range, return nil."
			  (let* ((dotime (cfw:org-tp text 'dotime)))
			    (and (stringp dotime) (string-match org-ts-regexp dotime)
				 (let* ((matches  (s-match-strings-all org-ts-regexp dotime))
					(start-date (nth 1 (car matches)))
					(end-date (nth 1 (nth 1 matches)))
					(extra (cfw:org-tp text 'extra)))
				   (if (string-match "(\\([0-9]+\\)/\\([0-9]+\\)): " extra)
				       (list( calendar-gregorian-from-absolute
					      (time-to-days
					       (org-read-date nil t start-date)))
					    (calendar-gregorian-from-absolute
					     (time-to-days
					      (org-read-date nil t end-date))) text))))))

			(setq org-tags-column 0
			      org-image-actual-width nil)
					;			(global-org-modern-mode)
			(add-to-list 'org-babel-after-execute-hook (function org-latex-preview))


			(setq org-todo-keywords
			      '((sequence "TODO(t)" "|" "DONE(d)" "WAITING(w)" "CANCELED(c)")))
			(defconst org-latex-math-environments-re
			  (format
			   "\\`[ \t]*\\\\begin{%s\\*?}"
			   (regexp-opt
			    '("equation" "eqnarray" "math" "displaymath"
			      "align"  "gather" "multline" "flalign"  "alignat"
			      "xalignat" "xxalignat"
			      "subequations" "lflalign"
			      ;; breqn
			      "dmath" "dseries" "dgroup" "darray"
			      ;; empheq
			      "empheq")))
			  "Regexp of LaTeX math environments.")


					;			(custom-set-variables '(org-modern-table nil))
					;(add-hook 'org-mode-hook (function valign-mode))
			(add-hook 'org-mode-hook (function visual-line-mode))
			(add-hook 'org-mode-hook (function org-toggle-pretty-entities))
			(add-hook 'org-mode-hook (function org-cdlatex-mode))
			(setq cfw:org-agenda-schedule-args '(:scheduled
							     :sexp
							     :closed
							     :deadline
							     :todo
							     :timestamp))

			(setq org-agenda-files '("/home/zjabbar/notes/20211224040925-todo.org"
						 "/home/zjabbar/notes/20240815234918-calendar.org"
						 "/home/zjabbar/notes/20240731154916-uh_jpn_102.org"))
			(setq cdlatex-math-modify-alist
			      '((?a "\\mathbf" nil t nil nil)
				(?b "\\mathbb" nil t nil nil)
				(?f "\\mathfrak" nil t nil nil)
				))

			(setq org-startup-with-inline-images t
			      cdlatex-simplify-sub-super-scripts nil
			      org-pretty-entities-include-sub-superscripts nil)

			(setq org-agenda-time-grid
			      (list '(daily weekly remove-match)
				    (mapcar (lambda (x) (* 100 x)) (number-sequence 6 21))
				    " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄"))

			(defun org-time-to-minutes (time)
			  "Convert an HHMM time to minutes"
			  (+ (* (/ time 100) 60) (% time 100)))

			(defun org-time-from-minutes (minutes)
			  "Convert a number of minutes to an HHMM time"
			  (+ (* (/ minutes 60) 100) (% minutes 60)))

			;; (defadvice org-agenda-add-time-grid-maybe (around mde-org-agenda-grid-tweakify
			;; 						  (list ndays todayp))
			;;   (if (member 'remove-match (car org-agenda-time-grid))
			;;       (flet ((extract-window
			;; 	      (line)
			;; 	      (let ((start (get-text-property 1 'time-of-day line))
			;; 		    (dur (get-text-property 1 'duration line)))
			;; 		(cond
			;; 		 ((and start dur)
			;; 		  (cons start
			;; 			(org-time-from-minutes
			;; 			 (truncate
			;; 			  (+ dur (org-time-to-minutes start))))))
			;; 		 (start start)
			;; 		 (t nil)))))
			;; 	    (let* ((windows (delq nil (mapcar 'extract-window list)))
			;; 		   (org-agenda-time-grid
			;; 		    (list
			;; 		     (car org-agenda-time-grid)
			;; 		     (remove-if
			;; 		      (lambda (time)
			;; 			(find-if (lambda (w)
			;; 				   (if (numberp w)
			;; 				       (equal w time)
			;; 				       (and (>= time (car w))
			;; 					    (< time (cdr w)))))
			;; 				 windows))
			;; 		      (cadr org-agenda-time-grid) )
			;; 		     (caddr org-agenda-time-grid)
			;; 		     (cadddr org-agenda-time-grid)
			;; 		     )))
			;; 	      ad-do-it))
			;;       ad-do-it))
			;; (ad-activate 'org-agenda-add-time-grid-maybe)
			;; (setq org-confirm-babel-evaluate nil)
			(setq org-startup-with-latex-preview t)
			;; (setq org-preview-latex-default-process 'dvisvgm)
					;(add-hook 'org-mode-hook 'org-fragtog-mode)
			(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
			(add-hook 'org-babel-after-execute-hook 'colorize-compilation-buffer)
			(setq python-indent-guess-indent-offset-verbose nil)
			(setq org-preview-latex-image-directory "/home/zjabbar/.cache/dvisvgm/")

			(defun clear-latex-cache () (interactive)
			  (mapc (function delete-file) (file-expand-wildcards (s-concat org-preview-latex-image-directory "*"))))

			(add-hook 'org-mode-hook (function (lambda () (set-syntax-table
								       (let ((table (make-syntax-table)))
									 (modify-syntax-entry ?< "w" table)
									 (modify-syntax-entry ?> "w" table)
									 table)))))
			(setq org-startup-with-inline-images t)
			(setf (alist-get :title org-export-options-alist) '("TITLE" nil "Maybe, में भि میں بھی, 明媚." t))
			(setf (alist-get :with-latex org-export-options-alist) '("t" "tex" (function org-export-with-latex))))
	   ))))

(define python-configuration
  (home-emacs-configuration
   (packages (list
	      python
	      jupyter
	      guix-jupyter
	      python-virtualenv
	      
	      expect
	      sshpass

	      emacs-ob-async
	      emacs-jupyter
	      emacs-pydoc
	      
	      python-lsp-server
	      
	      tree-sitter
	      tree-sitter-python

	      emacs-csv-mode
	      emacs-py-isort
	      emacs-python-black
	      python-pip
	      pandoc))
   (init '((use-package jupyter
			:config
			(defun gm/jupyter-api-request-xsrf-cookie-error-advice (func &rest args)
			  (condition-case nil
					  (apply func args)
					  (jupyter-api-http-error nil)))
			(advice-add 'jupyter-api-request-xsrf-cookie :around (function gm/jupyter-api-request-xsrf-cookie-error-advice))
			(setq jupyter-org-resource-directory "/home/zjabbar/notes/static/jupyter/")
			(setq org-babel-python-command "python3"
			      org-confirm-babel-evaluate nil
			      python-interpreter "python3"
			      python-shell-interpreter "python3"
			      treesit-extra-load-path '("/home/zjabbar/.guix-home/profile/lib/tree-sitter"))
			
			(org-babel-do-load-languages 'org-babel-load-languages '((scheme .t)
										 (python . t)
										 (sql . t)
										 (eshell . t)
										 (shell . t)
										 (jupyter . t)))
			
			(add-to-list 'org-src-lang-modes (cons "python3" 'python)))

	   (use-package envrc
			:demand t
       			:bind-keymap ("C-c e" . envrc-command-map)
			:hook (after-init . envrc-global-mode))
	   
	   (use-package eglot
			:config
			
			(add-to-list 'eglot-server-programs '(python-mode . ("pylsp")))
			(setq-default eglot-workspace-configuration
				      (list (cons :pylsp
						  (list :configurationSources (vector "flake8")
							:plugins
							'(:pycodestyle (:enabled :json-false)
							  :pyflakes (:enabled :json-false)
							  :flake8 (:enabled t))))))
			(setq eglot-send-changes-idle-time 0.1)
			(defun sloth/org-babel-edit-prep (info)
			  (setq buffer-file-name (or (alist-get :file (caddr info))
						     "org-src-babel-tmp"))
			  (eglot-ensure))
			
			(advice-add 'org-edit-src-code
				    :before (defun sloth/org-edit-src-code/before (&rest args)
					      (when-let* ((element (org-element-at-point))
							  (type (org-element-type element))
							  (lang (org-element-property :language element))
							  (mode (org-src-get-lang-mode lang))
							  ((eglot--lookup-mode mode))
							  (edit-pre (intern
								     (format "org-babel-edit-prep:%s" lang))))
							 (if (fboundp edit-pre)
							     (advice-add edit-pre :after (function sloth/org-babel-edit-prep))
							     (fset edit-pre (function sloth/org-babel-edit-prep)))))))


	   (use-package python
			:config
			(add-hook 'python-mode-hook
				  (lambda ()
				    (add-hook 'eglot-managed-mode-hook
					      (lambda () (setq-local completion-at-point-functions (list (cape-capf-super (function jupyter-completion-at-point) (function python-completion-at-point) (function eglot-completion-at-point)))))
					      nil t)))
			
			(add-hook 'python-mode-hook (function python-black-on-save-mode))
			(add-hook 'python-mode-hook (function eglot-ensure)))))))

(define lisp-configuration
  (home-emacs-configuration
   (packages (list
	      sicp
	      xdot
	      emacs-guix
	      emacs-daemons
	      tree-sitter-scheme
	      emacs-srfi
	      emacs-arei
	      guile-ares-rs
	      guile-next))
   (init '(#;(with-eval-after-load 'guix-repl
	   (setq guix-guile-program  '("guix" "repl")
	   guix-config-scheme-compiled-directory  nil
	   guix-repl-use-latest  nil
	   guix-repl-use-server  nil))
	   (setq geiser-guile-binary '("guix" "repl"))
	   (require 'guix)
	   (global-guix-prettify-mode)

	   (setq geiser-mode-auto-p nil)

	   (defun arei-server-start () "Start Arei with Default Port" (interactive)
	     (async-shell-command "guix shell --pure --preserve='^GUILE_LOAD_PATH$' guix guile-next guile-ares-rs -- guile -c '(begin (use-modules (guix gexp)) ((@ (ares server) run-nrepl-server) #:port 7888))'"))

	   (defun arei-server-start-guix-repl () "Start Arei with Default Port" (interactive)
	     (async-shell-command "guix shell guile-next guile-ares-rs -- echo '(begin (use-modules (guix gexp)) ((@ (ares server) run-nrepl-server) #:port 7888))' | guix repl"))

	   (defun auto-start-arei ()
	     (if (string= "" (shell-command-to-string "sudo ss -tulpn | grep LISTEN.*7888"))
		 (progn (arei-server-start))))

	   ;; (add-hook 'scheme-mode-hook (function auto-start-arei))
	   ;; (add-hook 'scheme-mode-hook (function arei-mode))
	   (remove-hook 'scheme-mode-hook (function geiser-mode--maybe-activate))



	   (setq user-full-name "Zain Jabbar")
	   (setq user-mail-address "zaijab2000@gmail.com")
	   (add-hook 'scheme-mode-hook 'guix-devel-mode)
	   (setq safe-local-variable-values '((eval modify-syntax-entry 43 "'")
					      (eval modify-syntax-entry 36 "'")
					      (eval modify-syntax-entry 126 "'")))
	   
	   ))))


(define blight-configuration '())
(if (string= (read-delimited "\n" (open-input-pipe "echo $HOSTNAME")) "euler")
    (set! blight-configuration
	  (home-emacs-configuration
	   (packages (list emacs-blight))
	   (init '((shell-command "sudo chmod 777 /sys/class/backlight/amdgpu_bl0/brightness")
		   (require 'blight)
		   (setq my/blight (blight-sysfs))
		   (blight-sysfs :min 0)
		   (global-set-key (kbd "<XF86MonBrightnessDown>") (blight-step my/blight -10))
		   (global-set-key (kbd "<XF86MonBrightnessUp>") (blight-step my/blight 10))
		   (global-set-key (kbd "<f5>") (blight-step my/blight -10))
		   (global-set-key (kbd "<f6>") (blight-step my/blight 10)))))))

(define shell-configuration
  (home-emacs-configuration
   (packages (list emacs-eat))
   (init '(#;(use-package eat
	   :demand t
	   :hook
	   (eshell-load . (function eat-eshell-mode))
	   (eshell-load . (function eat-eshell-visual-command-mode)))))))

(define exwm-configuration
  (home-emacs-configuration
   (packages (list
	      emacs-exwm
	      emacs-windsize
	      emacs-eat
	      xrandr
	      arandr))
   (init '((use-package exwm
			:if (display-graphic-p)
			:init
			(setq exwm-randr-workspace-monitor-plist '(0 "eDP-1" 1 "HDMI-A-0"))
			(exwm-randr-mode)
			(require 'xelb)
			(require 'windsize)
			(advice-add (function exwm-layout--hide)
				    :after (lambda (id)
					     (with-current-buffer (exwm--id->buffer id)
								  (setq exwm--ewmh-state
									(delq xcb:Atom:_NET_WM_STATE_HIDDEN exwm--ewmh-state))
								  (exwm-layout--set-ewmh-state id)
								  (xcb:flush exwm--connection))))
			(unbind-key (kbd "C-x C-z") 'global-map)
			(global-set-key (kbd "<f7>") (function
						      (lambda () (interactive)
							      (call-process-shell-command "loginctl suspend"))))
			(global-set-key (kbd "<f4>") (function
						      (lambda () (interactive)
							      (call-process-shell-command "xset dpms force off"))))

			(defun my/tabspace-kill-current-buffer () (interactive)
			  (let ((buffer-list (cl-remove-if (lambda (buf) (string-match-p (regexp-quote "*Minibuf-") (buffer-name buf))) (tabspaces--buffer-list))))
			    (cond
			     ((and (string-match-p (regexp-quote "*scratch*") (buffer-name)) (< 1 (length buffer-list))) (tabspaces-switch-to-buffer (cadr buffer-list)))
			     ((and (not (string-match-p (regexp-quote "*scratch*") (buffer-name))) (< 1 (length buffer-list))) (kill-current-buffer))
			     ((and (string-match-p (regexp-quote "*scratch*") (buffer-name)) (= 1 (length buffer-list))) (set-buffer-modified-p nil) (erase-buffer))
			     ((and (not (string-match-p (regexp-quote "*scratch*") (buffer-name))) (= 1 (length buffer-list))) (let ((buf (current-buffer))) (scratch-buffer) (kill-buffer buf))))))

			(global-set-key (kbd "s-q") (function my/tabspace-kill-current-buffer))


			(global-set-key (kbd "<f8>") 'toggle-exwm-input-line-mode-passthrough)
			(define-key exwm-mode-map (kbd "M-<escape>") (function toggle-exwm-input-line-mode-passthrough))
			(define-key exwm-mode-map (kbd "C-c C-c") (function exwm-input-send-next-key))

			(global-set-key (kbd "s-0") 'delete-window)
			(global-set-key (kbd "s-1") 'delete-other-windows)
			(global-set-key (kbd "s-2") 'split-window-below)
			(global-set-key (kbd "s-3") 'split-window-right)
			(global-set-key (kbd "s-5") 'exwm-workspace-switch)
			(global-set-key (kbd "s-w") 'tab-bar-switch-to-tab)

			(setq browse-url-firefox-program "librewolf")
			(global-set-key (kbd "s-e") (function
						     (lambda () (interactive)
							     (start-process-shell-command
							      "librewolf"
							      nil
							      "librewolf"))))
			(global-set-key (kbd "s-E") (function
						     (lambda () (interactive)
							     (start-process-shell-command
							      "librewolf --private-window http://localhost:8080"
							      nil
							      "librewolf --private-window http://localhost:8080"))))
			(global-set-key (kbd "s-v") (function
						     (lambda () (interactive)
							     (start-process-shell-command "Kanji Dojo" nil "guix shell jbr coreutils --preserve='^LD_LIBRARY_PATH$' --preserve='^DISPLAY$' -- java -jar /home/zjabbar/notes/data/kanji.jar"))))
			(global-set-key (kbd "s-r") (function eat))
			(global-set-key (kbd "s-t") (function eval-region))
			(global-set-key (kbd "s-K") 'windsize-up)
			(global-set-key (kbd "s-J") 'windsize-down)
			(global-set-key (kbd "s-f") 'exwm-floating-toggle-floating)
			(global-set-key (kbd "s-<tab>") 'consult-buffer)
			(global-set-key (kbd "s-<escape>") 'execute-extended-command)
			(global-set-key (kbd "s-`") 'eshell-command)
			(global-set-key (kbd "s-c") (function
						     (lambda () (interactive)
							     (find-file "~/code/guix-channel/zaijab/services/emacs.scm"))))
			(global-set-key (kbd "s-b") (function consult-search-library))
			(global-set-key (kbd "s-n") 'org-roam-node-find)
			(global-set-key (kbd "s-i") 'org-roam-node-insert)
			(global-set-key (kbd "s-N") 'org-roam-dailies-capture-today)
			(global-set-key (kbd "C-x C-n") 'org-roam-node-find)
			(global-set-key (kbd "s-a") 'cfw:open-org-calendar)
			(global-set-key (kbd "s-s") (function jisho->fc))
			(global-set-key (kbd "s-m") 'mu4e)
			(global-set-key (kbd "s-z") (function elfeed))
			(global-set-key (kbd "s-g") (function guix))
			(global-set-key (kbd "s-x") (function eww))
			(global-set-key (kbd "s-H") 'windsize-left)
			(global-set-key (kbd "s-L") 'windsize-right)
			(global-set-key (kbd "s-h") 'windmove-left)
			(global-set-key (kbd "s-j") 'windmove-down)
			(global-set-key (kbd "s-k") 'windmove-up)
			(global-set-key (kbd "s-l") 'windmove-right)
			(global-set-key (kbd "s-<SPC>") (function
							 (lambda (command)
							   (interactive (list (read-shell-command "$ ")))
							   (start-process-shell-command command nil command))))
			
					;(global-set-key (kbd "s-a") 'toggle-exwm-input-line-mode-passthrough)

			(setq exwm-input-simulation-keys
			      (list (cons (kbd "C-s") (kbd "C-f"))
				    (cons (kbd "M-w") (kbd "C-c"))))
			(defun exwm-rename-buffer-to-title () (exwm-workspace-rename-buffer exwm-class-name))
			(defun exwm-rename-buffer ()
			  (interactive)
			  (exwm-workspace-rename-buffer
			   (concat exwm-class-name ":"
				   (if (<= (length exwm-title) 25) exwm-title
				       (concat (substring exwm-title 0 24) "...")))))

			;; Add these hooks in a suitable place (e.g., as done in exwm-config-default)
			(add-hook 'exwm-update-class-hook 'exwm-rename-buffer)
			(add-hook 'exwm-update-title-hook 'exwm-rename-buffer)
			(defvar super-keys ())
			(let ((km (current-global-map)))
			  (while km
			    (let ((maybe-event (and (listp (car km))
						    (caar km))))
			      (if (and (eventp maybe-event)
				       (memq 'super (event-modifiers maybe-event)))
				  (add-to-list 'super-keys maybe-event)))
			    (setq km (cdr km))))

			(setq exwm-input-prefix-keys (append super-keys '(f7
									  XF86AudioRaiseVolume
									  XF86AudioLowerVolume
									  XF86AudioNext
									  XF86AudioPlay
									  XF86AudioPrev
									  XF86AudioMute
									  XF86MonBrightnessDown
									  XF86MonBrightnessUp)))

			(setq exwm-input-global-keys (list (cons (kbd "M-x") 'execute-extended-command)))
			(defun exwm-input-line-mode ()
			  "Set exwm window to line-mode and show mode line"
			  (call-interactively 'exwm-input-grab-keyboard)
			  (exwm-layout-show-mode-line))

			(defun exwm-input-char-mode ()
			  "Set exwm window to char-mode and hide mode line"
			  (call-interactively 'exwm-input-release-keyboard)
			  (exwm-layout-hide-mode-line))

			(defun exwm-input-toggle-mode ()
			  "Toggle between line- and char-mode"
			  (interactive)
			  (with-current-buffer (window-buffer)
					       (when (eq major-mode 'exwm-mode)
						 (if (equal (nth 1 (nth 1 mode-line-process)) "line")
						     (exwm-input-char-mode)
						     (exwm-input-line-mode)))))
			(defun toggle-exwm-input-line-mode-passthrough ()
			  (interactive)
			  (if exwm-input-line-mode-passthrough
			      (progn
			       (setq exwm-input-line-mode-passthrough nil)
			       (message "App receives all the keys now (with some simulation)"))
			      (progn
			       (setq exwm-input-line-mode-passthrough t)
			       (message "emacs receives all the keys now")))
			  (force-mode-line-update)))))))

(define theme-configuration
  (home-emacs-configuration
   (init '((load-theme 'modus-operandi t)))))

(define font-configuration
  (home-emacs-configuration
   (init '((if (display-graphic-p)
	       (progn
		(set-face-attribute 'default nil :font "Iosevka-14")
		(set-fontset-font nil 'tibetan "Iosevka")
		(set-fontset-font nil 'symbol "Iosevka")
		(set-fontset-font nil 'han "IPAmjMincho")
		(set-fontset-font nil 'kana "IPAmjMincho")
		(set-fontset-font nil 'cjk-misc "IPAmjMincho")))))))

(define ui-configuration
  (home-emacs-configuration
   (packages (list emacs-rainbow-delimiters
		   emacs-speed-type
		   font-iosevka))
   (early-init '((setq package-enable-at-startup nil
		       indicate-empty-lines nil
		       menu-bar-mode nil
		       tool-bar-mode nil
		       scroll-bar-mode nil
		       window-divider-default-places t
		       window-divider-default-bottom-width 1
		       window-divider-default-right-width 1
		       use-dialog-box nil
		       x-gtk-use-system-tooltips nil
		       ring-bell-function 'ignore
		       inhibit-splash-screen t
		       inhibit-startup-message t
		       initial-scratch-message nil
		       byte-compile-root-dir nil
		       frame-inhibit-implied-resize t
		       redisplay-dont-pause t
		       max-mini-window-height 10
	    	       initial-scratch-message nil
	    	       large-file-warning-threshold 100000000)

		 (set-face-attribute 'mode-line nil :box nil)
		 (set-face-attribute 'mode-line-inactive nil :box nil)

		 (defcustom password-colon-equivalents
		   '(?\u003a ; ?\N{COLON}
		     ?\uff1a ; ?\N{FULLWIDTH COLON}
		     ?\ufe55 ; ?\N{SMALL COLON}
		     ?\ufe13 ; ?\N{PRESENTATION FORM FOR VERTICAL COLON}
		     ?\u17d6 ; ?\N{KHMER SIGN CAMNUC PII KUUH}
		     )
		   "List of characters equivalent to trailing colon in \"password\" prompts."
		   :type '(repeat character)
		   :version "30.1"
		   :group 'processes)
		 (setq-default mode-line-format (remove 'mode-line-modes mode-line-format))
		 (setq org-src-fontify-natively t)
		 (setq org-src-tab-acts-natively t)
		 (setq org-src-preserve-indentation nil
		       org-edit-src-content-indentation 0)
		 (setq auto-window-vscroll nil)
		 (tooltip-mode -1)
		 (tool-bar-mode -1)
		 (scroll-bar-mode -1)
		 (menu-bar-mode -1)
		 (global-auto-revert-mode)
		 (fset (function yes-or-no-p) (function y-or-n-p))
		 (push '(menu-bar-lines . 0)   default-frame-alist)
		 (push '(tool-bar-lines . 0)   default-frame-alist)
		 (push '(vertical-scroll-bars) default-frame-alist)
		 (blink-cursor-mode 0)
		 (setq comp-async-report-warnings-errors nil)
		 (setq native-comp-async-report-warnings-errors nil)
		 (setq warning-suppress-log-types '((comp) (comp)))
		 (setq warning-suppress-types '((comp) (comp)))
		 (setq user-emacs-directory "~/.config/emacs")
		 (setq byte-compile-warnings '(cl-functions))
		 (setq make-backup-files nil)
		 (setq auto-save-default nil)
		 (setq create-lockfiles nil)
		 (pixel-scroll-precision-mode)))
   (init '((setq org-startup-truncated nil)
	   (tab-bar-mode)
	   (setq custom-file (locate-user-emacs-file "custom.el"))
	   (load custom-file :no-error-if-file-is-missing)
	   ;; (setq load-suffixes '(".el"))
	   (set-face-attribute 'tab-bar nil :height 140)
	   (display-time-mode)
	   (setq battery-mode-line-limit 97)
	   (customize-set-variable 'tab-bar-format
				   '(tab-bar-format-history
				     tab-bar-format-tabs
				     tab-bar-separator
				     tab-bar-format-add-tab
				     tab-bar-format-align-right
				     tab-bar-format-global))

	   (defun move-to-second (word list)
	     (cons "" (cons word (remove word (cdr list)))))
	   (setq global-mode-string (move-to-second 'emms-mode-line-string global-mode-string))
	   (setq global-mode-string (move-to-second 'emms-playing-time-string global-mode-string))
	   (customize-set-variable 'mode-line-misc-info '(""))
	   (customize-set-variable 'display-time-load-average-threshold 100)
	   (customize-set-variable 'display-time-day-and-date t)
	   (set-default 'truncate-lines t)
	   (require 'rainbow-delimiters)
	   (define-globalized-minor-mode global-rainbow-delimiters-mode rainbow-delimiters-mode rainbow-delimiters-mode-enable)
	   (global-rainbow-delimiters-mode)
	   (setq minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
	   (add-hook 'minibuffer-setup-hook (function cursor-intangible-mode))
	   (defun crm-indicator (args)
	     (cons (format "[CRM%s] %s"
			   (replace-regexp-in-string
			    "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
			    crm-separator)
			   (car args))
		   (cdr args)))
	   (advice-add (function completing-read-multiple) :filter-args (function crm-indicator))
	   (setq enable-recursive-minibuffers t)
	   (add-to-list 'save-some-buffers-action-alist
			(list "d"
			      (lambda (buffer) (diff-buffer-with-file (buffer-file-name buffer)))
			      "show diff between the buffer and its file"))
	   (setq compile-command "make")


	   
	   (defun quick-restart ()
	     (interactive)
	     (shell-command "sudo reboot --kexec"))
	   (setq read-extended-command-predicate (function command-completion-default-include-p))))))

;;; Combine all Emacs-Configurations within module

(define home-emacs-total-configuration
  (fold (lambda (config-1 config-2) (home-emacs-configuration
				     (init (append (home-emacs-configuration-init config-1)
						   (home-emacs-configuration-init config-2)))
				     (early-init (append (home-emacs-configuration-early-init config-1)
							 (home-emacs-configuration-early-init config-2)))
				     (packages (append (home-emacs-configuration-packages config-1)
						       (home-emacs-configuration-packages config-2)))))
	(home-emacs-configuration)

	(filter home-emacs-configuration?
		(map variable-ref
		     (filter variable-bound?
			     (hash-map->list (lambda (x y) y) (struct-ref (current-module) 0)))))))


(define (use-emacs-next package)
  (if #t
      ((options->transformation '((with-git-url . "emacs-org=https://code.tecosaur.net/tec/org-mode.git")
				  (with-commit . "emacs-org=ce4a745b0aa746686376c5927b3165fe4cb4b4d7")
				  (without-tests . "emacs-org")))
       package)
      package))

(define home-emacs-total-configuration
  (home-emacs-configuration
   (inherit home-emacs-total-configuration)
   (packages (map use-emacs-next
		  (home-emacs-configuration-packages home-emacs-total-configuration)))))
