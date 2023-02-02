(define-module (zaijab services emacs)
  #:use-module (gnu home)
  #:use-module (gnu packages)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 pretty-print)
  #:use-module (guix transformations)
  #:use-module (guix gexp)
  #:use-module (zaijab packages emacs-xyz)
  #:use-module (zaijab packages python-xyz)
  #:use-module (nongnu packages chrome)
  #:use-module (nongnu packages fonts)
  #:export (home-emacs-service-type
	    home-emacs-configuration
	    home-emacs-total-configuration))

(define file-likes? (list-of file-like?))

(define-configuration/no-serialization home-emacs-configuration
  (emacs
   (file-like (specification->package "emacs-next"))
   "The Emacs package to use.")
  (packages
   (file-likes '())
   "The packages this configuration will add to home-profile. Usually these will be emacs-* packages.")
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

;; Completions

(define orderless-configuration
  (home-emacs-configuration
   (packages (list (specification->package "emacs-orderless")))
   (init '((setq completion-styles '(orderless basic)
		 completion-category-overrides '((file (styles basic partial-completion))))
	   (setq orderless-smart-case nil
		 completion-ignore-case t
		 read-file-name-completion-ignore-case t
		 read-buffer-completion-ignore-case t)))))

(define vertico-configuration
  (home-emacs-configuration
   (packages (list (specification->package "emacs-vertico")))
   (init '((vertico-mode 1)))))

(define corfu-configuration
  (home-emacs-configuration
   (packages (list (specification->package "emacs-corfu")))
   (init '((global-corfu-mode)
	   (corfu-history-mode)
	   (setq corfu-cycle t
		 corfu-auto t
		 corfu-auto-prefix 2
		 corfu-auto-delay 0.0
		 corfu-quit-at-boundary 'separator
		 corfu-echo-documentation 0.25
		 corfu-preview-current 'insert
		 corfu-preselect-first nil)
	   (define-key corfu-map (kbd "M-<SPC>") (function corfu-insert-separator))
	   (define-key corfu-map (kbd "<tab>") (function corfu-next))
	   (define-key corfu-map (kbd "<backtab>") (function corfu-previous))))))

(define tempel-configuration
  (home-emacs-configuration
   (packages (list (specification->package "emacs-tempel")))
   (init '((require 'tempel)
	   (defun tempel-setup-capf ()
	     (setq-local completion-at-point-functions
			 (cons (function tempel-expand)
			       completion-at-point-functions)))
	   (add-hook 'prog-mode-hook 'tempel-setup-capf)
	   (add-hook 'text-mode-hook 'tempel-setup-capf)
	   (define-key tempel-map (kbd "C-a") (function tempel-prev))
	   (define-key tempel-map (kbd "C-d") (function tempel-next))))))

(define cape-configuration
  (home-emacs-configuration
   (packages (list (specification->package "emacs-cape")))
   (init '((setq tab-always-indent 'complete)
					;(add-to-list 'completion-at-point-functions (function cape-file))
	   ))))

(define marginalia-configuration
  (home-emacs-configuration
   (packages (list (specification->package "emacs-marginalia")))
   (init '((marginalia-mode)))))

(define embark-configuration
  (home-emacs-configuration
   (packages (list (specification->package "emacs-embark")))
   (init '((require 'embark)
	   (add-to-list 'display-buffer-alist
			'("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
			  nil
			  (window-parameters (mode-line-format . none))))))))

;; (define consult-configuration
;;   (home-emacs-configuration
;;    (packages (list (specification->package "emacs-consult")))
;;    (init '((require 'consult)
;; 	   (add-hook 'completion-list-mode-hook consult-preview-at-point-mode)
;; 	   (setq register-preview-delay 0.5
;; 		 register-preview-function (function consult-register-format))
;; 	   (advice-add (function register-preview) :override (function consult-register-window))
;; 	   (setq xref-show-xrefs-function (function consult-xref)
;; 		 xref-show-definitions-function (function consult-xref))
;; 	   (consult-customize
;; 	    consult-theme :preview-key '(:debounce 0.2 any)
;; 	    consult-ripgrep consult-git-grep consult-grep
;; 	    consult-bookmark consult-recent-file consult-xref
;; 	    consult--source-bookmark consult--source-file-register
;; 	    consult--source-recent-file consult--source-project-recent-file
;; 	    :preview-key '(:debounce 0.4 any))

;; 	   (setq consult-narrow-key "<")))))

(define evil-configuration
  (home-emacs-configuration
   (packages (list (specification->package "emacs-evil")
		   (specification->package "emacs-xah-fly-keys") 
		   (specification->package "emacs-evil-collection")))
   (init '((evil-collection-init)
	   (evil-mode 1)))
   (early-init '((setq evil-want-keybinding nil)))))

;; (define polymode-configuration
;;   (home-emacs-configuration
;;    (packages (list (specification->package "emacs-polymode")
;; 		   (specification->package "emacs-polymode-org")))
;;    (init '(()))))

(define undo-configuration
  (home-emacs-configuration
   (packages (list (specification->package "emacs-undo-tree")))
   (init '((require 'undo-tree)
	   (setq undo-tree-history-directory-alist  '(("." . "~/.config/emacs/undo-tree/")))
	   (add-to-list 'undo-tree-incompatible-major-modes 'elfeed-search-mode)

	   
	   (global-undo-tree-mode)))))

(define indentation-configuration
  (home-emacs-configuration
   (packages (list (specification->package "emacs-aggressive-indent")
		   (specification->package "emacs-smart-hungry-delete")))
   (init '((require 'aggressive-indent)
	   (require 'smart-hungry-delete)
	   (smart-hungry-delete-add-default-hooks)
	   (global-set-key (kbd "<backspace>") 'smart-hungry-delete-backward-char)
	   (global-set-key (kbd "<delete>") 'smart-hungry-delete-backward-char)
	   (global-set-key (kbd "C-d") 'smart-hungry-delete-forward-char)           
	   (global-aggressive-indent-mode 1)))))

(define project-configuration
  (home-emacs-configuration
   (packages (list (specification->package "git")
		   (specification->package "direnv")
		   (specification->package "emacs-envrc")))
   (init '((require 'ansi-color)
	   (defun colorize-compilation-buffer ()
	     (interactive)
	     (let ((inhibit-read-only t))
	       (ansi-color-apply-on-region (point-min) (point-max))))
	   (add-hook 'compilation-filter-hook (function colorize-compilation-buffer))
	   (add-hook 'org-mode-hook (function colorize-compilation-buffer))
	   ))))

(define language-configuration
  (home-emacs-configuration
   (packages (list (specification->package "emacs-ddskk")
		   (specification->package "font-fira-code")
		   (specification->package "font-google-noto")
		   (specification->package "font-lohit")
		   (specification->package "font-vazir")
		   (specification->package "font-ipa-mj-mincho")
		   (specification->package "font-iosevka")
		   font-microsoft-couirer-new))
   (init '((require 'facemenu)
	   (defun jisho-word->japanese-part (jisho-word)
	     (list (gethash "word" (elt (gethash "japanese" jisho-word) 0))
		   (gethash "reading" (elt (gethash "japanese" jisho-word) 0))))
	   (defun jisho-word->english-part (jisho-word)
	     (gethash "english_definitions" (elt (gethash "senses" jisho-word) 0)))
	   (defun kanji-word->drill (word)
	     (apply 'format "%s :drill:Japanese:\n:PROPERTIES:\n:DRILL_CARD_TYPE: twosided\n:END:\n** Kanji\n%s [%s]\n** English\n%s\n" (cons (car word) word)))
	   (defun kana-word->drill (word)
	     (apply 'format "%s :drill:Japanese:\n:PROPERTIES:\n:DRILL_CARD_TYPE: twosided\n:END:\n** Kana\n%s\n** English\n%s\n" (cons (car word) word)))
	   (defun word->drill (word)
	     (if (car word)
		 (kanji-word->drill word)
		 (kana-word->drill (cdr word))))
	   (defun my-presorted-completion-table (completions)
	     (lambda (string pred action)
	       (if (eq action 'metadata)
		   `(metadata (display-sort-function . ,(function identity)))
		   (complete-with-action action completions string pred))))
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
	   ))))

(define garbage-configuration
  (home-emacs-configuration
   (packages (list (specification->package "emacs-gcmh")
		   (specification->package "emacs-explain-pause-mode")))
   (init '((gcmh-mode 1)))))

(define eww-configuration
  (home-emacs-configuration
   (init '((setq browse-url-browser-function 'eww-browse-url)
	   (setq eww-search-prefix "http://127.0.0.1:8888/search?q=")))))


(define pdf-tools-configuration
  (home-emacs-configuration
   (packages (list (specification->package "emacs-pdf-tools")))
   (init '((pdf-tools-install)))))

(define mmm-configuration
  (home-emacs-configuration
   (packages (list (specification->package "emacs-mmm-mode")))
   (init '())))

(define cryptography-configuration
  (home-emacs-configuration
   (packages (list (specification->package "pinentry")
		   (specification->package "emacs-pinentry")
		   (specification->package "pinentry-emacs")
		   (specification->package "password-store")
		   (specification->package "gnupg")
		   (specification->package "openssh")
		   (specification->package "openconnect")))
   (init '((pinentry-start)))))

(define elfeed-configuration
  (home-emacs-configuration
   (packages (list (specification->package "mpv")
		   (specification->package "yt-dlp")
		   (specification->package "emacs-elfeed")
		   (specification->package "ktorrent")
		   emacs-elfeed-tube))
   (init '(
	   (setq elfeed-feeds '(("https://www.youtube.com/feeds/videos.xml?channel_id=UC2D2CMWXMOVWx7giW1n3LIg" health huberman)
					;("https://www.youtube.com/feeds/videos.xml?channel_id=UCe0TLA0EsQbE-MjuHXevj2A" health jeff)
				("https://www.youtube.com/feeds/videos.xml?channel_id=UCkFJBuwX2iPKCgCITXt2Bnw" fun fatguy)
				("https://www.youtube.com/feeds/videos.xml?channel_id=UCrTW8WZTlOZMvvn_pl1Lpsg" fun nicob)
				("https://www.youtube.com/feeds/videos.xml?channel_id=UCP9q8DRbsTDPhU4E0R3-1rA" fun pekin)
				("https://www.youtube.com/feeds/videos.xml?channel_id=UCT0fBcIYwMsp6IRCm5E3eTA" fun pekin)
				("https://www.youtube.com/feeds/videos.xml?channel_id=UCYO_jab_esuFRV4b17AJtAw" math grant)
				("https://www.youtube.com/feeds/videos.xml?channel_id=UCm5mt-A4w61lknZ9lCsZtBw" math brunton)
				("https://www.youtube.com/feeds/videos.xml?channel_id=UCAiiOTio8Yu69c3XnR7nQBQ" crafter david)
				("https://www.youtube.com/feeds/videos.xml?channel_id=UCuj_loxODrOPxSsXDfJmpng" crafter andrew)
				("https://www.youtube.com/feeds/videos.xml?channel_id=UCkdmU8hGK4Fg3LghTVtKltQ" japanese cure-dolly)
				("https://www.youtube.com/feeds/videos.xml?playlist_id=PL5B3KLQNAC5j46Ro64xF7hLV6Uf-gHUHL" lecture continuum-mechanics)
				("https://www.youtube.com/feeds/videos.xml?playlist_id=PLoROMvodv4rMiGQp3WXShtMGgzqpfVfbU" lecture machine-learning statistics ng)
				("https://www.youtube.com/feeds/videos.xml?playlist_id=PLE18841CABEA24090" lecture mit sicp)
				("https://www.youtube.com/feeds/videos.xml?playlist_id=PLEC88901EBADDD980" lecture mit odes)
				("https://www.youtube.com/feeds/videos.xml?playlist_id=PL221E2BBF13BECF6C" lecture mit linear)
				("https://www.youtube.com/feeds/videos.xml?playlist_id=PLUl4u3cNGP60B0PQXVQyGNdCyCTDU1Q5j" lecture mit ml-health)
				("https://www.youtube.com/feeds/videos.xml?playlist_id=PLUl4u3cNGP63gFHB6xb-kVBiQHYe_4hSi" lecture mit ai)
				("https://www.youtube.com/feeds/videos.xml?playlist_id=PLUl4u3cNGP63pfpS1gV5P9tDxxL_e4W8O" lecture mit cv)
				("https://www.youtube.com/feeds/videos.xml?playlist_id=PLUl4u3cNGP60uVBMaoNERc6knT_MgPKS0" lecture mit stats)
				("https://www.youtube.com/feeds/videos.xml?playlist_id=PLUl4u3cNGP61MdtwGTqZA0MreSaDybji8" lecture mit prob)
				("https://www.youtube.com/feeds/videos.xml?playlist_id=PLo4jXE-LdDTTIIIRwqK35CbFJieSJEcVR" lecture functional)
				("https://www.youtube.com/feeds/videos.xml?playlist_id=PLLq_gUfXAnkkvL_UoCGivS0wOYhwCtczI" lecture pde)
				("https://www.youtube.com/feeds/videos.xml?playlist_id=PLLq_gUfXAnkmC-VWIJ_HW8cdOZLEtHfXJ" lecture dynamical-systems)
				("https://www.youtube.com/feeds/videos.xml?playlist_id=PL4C4C8A7D06566F38" lecture mit multivariable-calc)
				("https://www.youtube.com/feeds/videos.xml?playlist_id=PLzUeAPxtWcqzr80lS25FrzMn7a36BuXhj" lecture algebra gross)
				("https://www.youtube.com/feeds/videos.xml?playlist_id=PLpRLWqLFLVTCL15U6N3o35g4uhMSBVA2b" lecture topology pierre-albin)
				("https://www.youtube.com/feeds/videos.xml?playlist_id=PLTBqohhFNBE_09L0i-lf3fYXF5woAbrzJ" lecture topology tokieda)
				("https://www.youtube.com/feeds/videos.xml?playlist_id=PLLq_gUfXAnkl8bjQh-hGQ9u24xZP9x0dx" lecture topology bruno)
				("https://www.youtube.com/feeds/videos.xml?playlist_id=PLp0hSY2uBeP_HDgkCSrG5pccHYfudTJYI" lecture topology pavel)
				("https://www.youtube.com/feeds/videos.xml?playlist_id=PLBEeOnR8lrBHNZWwk8-pHOQLQnP3u8bO8" lecture topology clark)
				("https://www.youtube.com/feeds/videos.xml?playlist_id=PL6763F57A61FE6FE8" lecture topology wildberger)))
	   (require 'elfeed-tube)
	   (elfeed-tube-setup)
	   (setq-default elfeed-search-filter "-fun -crafter")
	   (setq-default elfeed-search-title-max-width 100)
	   (setq-default elfeed-search-title-min-width 100)
	   (defun browse-url-mpv (url &optional new-window)
	     (start-process "mpv" "*mpv*" "mpv" url))

	   (add-to-list 'browse-url-handlers (cons "https:\\/\\/www\\.youtube." 'browse-url-mpv))
	   (add-hook 'elfeed-new-entry-hook
		     (elfeed-make-tagger :feed-url "youtube\\.com"
					 :add '(video youtube)))


	   (setq youtube-dl-path "yt-dlp --sponsorblock-remove all")
	   (setq youtube-dl-output-dir "~/lectures/")

	   (defun elfeed-download-video ()
	     "Download a video using youtube-dl."
	     (interactive)
	     (async-shell-command (format "%s -o \"%s%s\" -f bestvideo+bestaudio %s"
					  youtube-dl-path
					  youtube-dl-output-dir
					  "%(title)s.%(ext)s"
					  (elfeed-entry-link elfeed-show-entry))))))))

(define music-configuration
  (home-emacs-configuration
   (packages (list (specification->package "emacs-libmpdel")
		   (specification->package "alsa-utils")
		   (specification->package "pavucontrol")
		   (specification->package "emacs-alsamixer-el")
		   (specification->package "emacs-bluetooth")
		   (specification->package "mpd-mpc")
		   (specification->package "emacs-emms")))
   (init '((global-set-key (kbd "<XF86AudioPrev>") 'libmpdel-playback-previous)
	   (global-set-key (kbd "<XF86AudioNext>") 'libmpdel-playback-next)
	   (global-set-key (kbd "<XF86AudioPlay>") 'libmpdel-playback-play-pause)
	   (global-set-key (kbd "<XF86AudioRaiseVolume>") 'alsamixer-up-volume)
	   (global-set-key (kbd "<XF86AudioLowerVolume>") 'alsamixer-down-volume)
	   (global-set-key (kbd "<XF86AudioMute>") 'alsamixer-toggle-mute)))))

(define email-configuration
  (home-emacs-configuration
   (packages (list
	      ((options->transformation '((with-git-url . "emacs-org-msg=https://github.com/jeremy-compostella/org-msg.git")
					  (with-branch . "emacs-org-msg=master")))
	       (specification->package "emacs-org-msg"))
	      (specification->package "isync")
	      (specification->package "mu")
	      ((options->transformation
		'((with-git-url . "emacs-mu4e-alert=https://github.com/progfolio/mu4e-alert.git")
		  (with-branch . "emacs-mu4e-alert=fix/mu4e--switch-context-advice"))) (specification->package "emacs-mu4e-alert"))
	      (specification->package "msmtp")))
   (init '((require 'mu4e)
	   (require 'mu4e-alert)
	   
	   (setq mu4e-get-mail-command (format "INSIDE_EMACS=%s mbsync -c ~/.config/mbsyncrc -a" emacs-version)
		 epa-pinentry-mode 'ask)
	   (setq mu4e-hide-index-messages t)
	   (setq mail-user-agent 'mu4e-user-agent)
	   (add-hook 'after-init-hook (function mu4e-alert-enable-mode-line-display))
	   (require 'org-msg)
	   (setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil tex:imagemagick \\n:t"
		 org-msg-startup "hidestars indent inlineimages"
		 org-msg-greeting-fmt "\nAloha%s,\n\n"
		 org-msg-recipient-names '(("zaijab2000@gmail.com" . "Zain"))
		 org-msg-greeting-name-limit 3
		 ;; org-msg-default-alternatives '((new		. (text html))
		 ;; 				(reply-to-html	. (text html))
		 ;; 				(reply-to-text	. (text)))
		 org-msg-convert-citation t)
	   (org-msg-mode)
	   ;; (add-hook 'mu4e-compose-mode-hook 'mml-secure-message-sign)
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
			:name "zaijab2000_gmail"
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
			:name "school"
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
					    (mu4e-message-contact-field-matches msg :bcc "zjabbar@hawaii.edu"))))

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
						  :key ?u)))))))))))

(define notes-configuration
  (home-emacs-configuration
   (packages (list
	      ((options->transformation
		'((without-tests . "emacs-magit")))
	       (specification->package "emacs-org-roam"))
	      (specification->package "emacs-org-drill")))
   (init '((require 'org-roam-node)
	   (require 'org-drill) 
	   (setq org-drill-learn-fraction 0.4)
	   (setq org-drill-hide-item-headings-p t)
	   (setq org-roam-directory "~/notes")
	   (setq org-roam-v2-ack t)
	   (org-roam-db-autosync-mode)
	   (setq org-roam-capture-templates
		 '(("i" "Default" plain "%?"
		    :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE: ${title}\n")
		    :unnarrowed t)
		   ("j" "Japanese" entry "* %(word->drill (jisho-search->completing-read))"
		    :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "\n")
		    :unnarrowed t)
		   ("d" "Drill" entry "* TITLE\n:PROPERTIES:\n:DRILL_CARD_TYPE: twosided\n:END:\n\n** \n\n** \n\n"
		    :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "\n")
		    :unnarrowed t)))
	   (global-set-key (kbd "s-i") (function org-roam-capture))))))

(define website-configuration
  (home-emacs-configuration
   (packages (list google-chrome-unstable
		   (specification->package "python-pygments")))
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
		    (slug (commonplace/slugify-title title)))
	       (concat directory slug ".html")))

	   (defun zain-publish ()
	     (interactive)
	     (let ((current-prefix-arg (list 4))
		   (default-directory "~/code/zaijab.github.io"))
	       (advice-add 'org-export-output-file-name :filter-return (function commonplace/slugify-export-output-file-name))
	       (call-interactively 'org-publish-all)
	       (advice-remove 'org-export-output-file-name (function commonplace/slugify-export-output-file-name))
	       (shell-command "git add -A;git commit -am \"Updating Website\";git push -fu origin roam" "*Messages*")))
	   (global-set-key (kbd "s-p") 'zain-publish)

	   (defun org-export-output-file-name-modified (orig-fun extension &optional subtreep pub-dir)
	     (unless pub-dir
	       (setq pub-dir "/home/zjabbar/.cache/note-export/")
	       (unless (file-directory-p pub-dir)
		 (make-directory pub-dir)))
	     (apply orig-fun extension subtreep pub-dir nil))
	   (advice-add 'org-export-output-file-name :around (function org-export-output-file-name-modified))
	   (setq org-latex-listings 'minted)
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
		    :exclude ".*org"
		    :include ("20220925152629-index.org" "20220925155207-about.org") 
		    :with-toc nil
		    :html-head
		    "<title></title><link rel=\"stylesheet\" href=\"/css/main-dark.css\" type=\"text/css\"/>\n<header><div class=\"menu\"><ul>\n<li><a href=\"/\">/</a></li>\n<li><a href=\"/about\">/about</a></li>\n</ul></div></header>"
		    :recursive t
		    :html-postamble nil)
		   ("css"
		    :base-directory "~/notes/css"
		    :base-extension any
		    :publishing-directory "~/code/zaijab.github.io/css"
		    :publishing-function org-publish-attachment)
		   ("images"
		    :base-directory "~/notes/attachments/"
		    :base-extension any
		    :publishing-directory "~/code/zaijab.github.io/attachments"
		    :publishing-function org-publish-attachment)
		   ("CNAME"
		    :base-directory "~/notes/CNAME/"
		    :base-extension any
		    :publishing-directory "~/code/zaijab.github.io/"
		    :publishing-function org-publish-attachment)
		   ("zaindaman" :components ("orgfiles" "images" "css" "CNAME"))))))))


(define org-mode-configuration
  (home-emacs-configuration
   (packages (list (specification->package "emacs-org-fragtog")
		   (specification->package "emacs-org-modern")
		   (specification->package "emacs-tempel")
		   (specification->package "emacs-valign")
		   (specification->package "emacs-org-present")
		   (specification->package "emacs-calfw") 
		   (specification->package "texlive")
		   (specification->package "texlive-bin")
		   (specification->package "ispell")))
   (init '((require 'org)
	   (require 'ox)
	   (require 'calfw)
	   (require 'calfw-org)
	   (setq org-agenda-show-log-scoped t)
	   (setq org-agenda-prefix-format '((agenda  . "  • %?-12t% s")
					    (timeline  . "  % s")
					    (todo  . " %i %-12:c")
					    (tags  . " %i %-12:c")
					    (search . " %i %-12:c")))

	   (defun cfw:org-get-timerange (text)
	     "Return a range object (begin end text). If TEXT does not have a range, return nil."
	     (let* ((dotime (cfw:org-tp text 'dotime)))
	       (and (stringp dotime) (string-match org-ts-regexp dotime)
		    (let* ((matches  (s-match-strings-all org-ts-regexp dotime))
			   (start-date (nth 1 (car matches)))
			   (end-date (nth 1 (nth 1 matches)))
			   (extra (cfw:org-tp text 'extra)))
		      (if (string-match "(\\([0-9]+\\)/\\([0-9]+\\)): " extra)
			  ( list( calendar-gregorian-from-absolute
				  (time-to-days
				   (org-read-date nil t start-date))
				  )
			    (calendar-gregorian-from-absolute
			     (time-to-days
			      (org-read-date nil t end-date))) text))))))
	   
	   (setq org-tags-column 0)
	   (global-org-modern-mode)
	   (custom-set-variables '(org-modern-table nil))
	   (add-hook 'org-mode-hook (function valign-mode))
	   (setq cfw:org-agenda-schedule-args '(:scheduled :sexp :closed :deadline :todo :timestamp))
	   
	   (setq org-agenda-files '("~/notes/"))
	   
	   (setq org-startup-with-inline-images t)

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

	   (defadvice org-agenda-add-time-grid-maybe (around mde-org-agenda-grid-tweakify
							     (list ndays todayp))
	     (if (member 'remove-match (car org-agenda-time-grid))
		 (flet ((extract-window
			 (line)
			 (let ((start (get-text-property 1 'time-of-day line))
			       (dur (get-text-property 1 'duration line)))
			   (cond
			    ((and start dur)
			     (cons start
				   (org-time-from-minutes
				    (truncate
				     (+ dur (org-time-to-minutes start))))))
			    (start start)
			    (t nil)))))
		       (let* ((windows (delq nil (mapcar 'extract-window list)))
			      (org-agenda-time-grid
			       (list
				(car org-agenda-time-grid)
				(remove-if
				 (lambda (time)
				   (find-if (lambda (w)
					      (if (numberp w)
						  (equal w time)
						  (and (>= time (car w))
						       (< time (cdr w)))))
					    windows))
				 (cadr org-agenda-time-grid) )
				(caddr org-agenda-time-grid)
				(cadddr org-agenda-time-grid)
				)))
			 ad-do-it))
		 ad-do-it))
	   (ad-activate 'org-agenda-add-time-grid-maybe)
	   (setq org-startup-with-latex-preview t)
	   (setq org-preview-latex-default-process 'dvisvgm)
	   (add-hook 'org-mode-hook 'org-fragtog-mode)
	   (add-hook 'org-mode-hook 'flyspell-mode)
	   (setq org-confirm-babel-evaluate nil)
	   (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
	   (add-hook 'org-babel-after-execute-hook 'colorize-compilation-buffer)
	   (setq python-indent-guess-indent-offset-verbose nil)
	   (setq org-preview-latex-image-directory "/home/zjabbar/.cache/dvisvgm/")
	   (with-eval-after-load 'evil-maps (define-key evil-motion-state-map (kbd "TAB") nil))
	   (add-hook 'org-mode-hook (function (lambda () (set-syntax-table
							  (let ((table (make-syntax-table)))
							    (modify-syntax-entry ?< "w" table)
							    (modify-syntax-entry ?> "w" table)
							    table)))))
	   (setq org-startup-with-inline-images t)
	   (defun my/text-scale-adjust-latex-previews ()
	     "Adjust the size of latex preview fragments when changing the buffer's text scale."
	     (pcase major-mode
		    ('latex-mode
		     (dolist (ov (overlays-in (point-min) (point-max)))
			     (if (eq (overlay-get ov 'category)
				     'preview-overlay)
				 (my/text-scale--resize-fragment ov))))
		    ('org-mode
		     (dolist (ov (overlays-in (point-min) (point-max)))
			     (if (eq (overlay-get ov 'org-overlay-type)
				     'org-latex-overlay)
				 (my/text-scale--resize-fragment ov))))))

	   (defun my/text-scale--resize-fragment (ov)
	     (overlay-put
	      ov 'display
	      (cons 'image
		    (plist-put
		     (cdr (overlay-get ov 'display))
		     :scale (+ 1.0 (* 0.25 text-scale-mode-amount))))))

	   (add-hook 'text-scale-mode-hook (function my/text-scale-adjust-latex-previews))
	   
	   (setq org-format-latex-options '(:foreground default :background default :scale 2 :html-foreground "Black" :html-background "Transparent" :html-scale 1.0 :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))
	   (setf (alist-get :title org-export-options-alist) '("TITLE" nil "Maybe, में भि میں بھی, 明媚." t))
	   (setf (alist-get :with-latex org-export-options-alist) '("t" "tex" (function org-export-with-latex)))
	   ))))


(define python-configuration
  (home-emacs-configuration
   (packages (list
	      ((options->transformation		
		'((with-branch . "emacs-jupyter=master"))) (specification->package "emacs-jupyter"))
	      (specification->package "python")
	      (specification->package "python-lsp-server")
	      python-tree-sitter
	      (specification->package "jupyter")
	      (specification->package "pandoc")
	      (specification->package "remmina")
	      (specification->package "ungoogled-chromium")
	      (specification->package "gtk+")
	      (specification->package "python-pygobject")
	      (specification->package "python-srt")
	      (specification->package "python-pyqt")
	      (specification->package "glib-networking")
	      (specification->package "qtwebkit")
	      (specification->package "gsettings-desktop-schemas")
	      (specification->package "webkitgtk-with-libsoup2")
	      (specification->package "network-manager-applet")
	      (specification->package "hicolor-icon-theme")
	      (specification->package "python-sqlalchemy")
	      (specification->package "python-sqlalchemy-utils")
	      (specification->package "python-sshtunnel")
	      (specification->package "python-psycopg2")
	      (specification->package "python-pandas")
	      (specification->package "python-matplotlib")
	      (specification->package "python-scipy")
	      (specification->package "python-sympy")
	      (specification->package "python-scikit-learn")
	      (specification->package "python-seaborn")
	      (specification->package "python-xgboost")
	      (specification->package "python-numexpr")
	      (specification->package "python-patsy")
	      (specification->package "python-statsmodels")
	      (specification->package "emacs-csv-mode")))
   (init '((setq org-babel-python-command "python3")
	   (setq python-shell-interpreter "python3")
	   (org-babel-do-load-languages 'org-babel-load-languages '((scheme .t)
								    (python . t)
								    (sql . t)
								    (eshell . t)
								    (R . t)
								    (shell . t)
								    (jupyter . t)))
	   (add-to-list 'org-src-lang-modes (cons "python3" 'python))
	   (org-babel-jupyter-override-src-block "python3")
	   (defun jupyter-ansi-color-apply-on-region (begin end)
	     (ansi-color-apply-on-region begin end t))))))

(define lisp-configuration
  (home-emacs-configuration
   (packages (list 
	      emacs-symex
					;emacs-rigpa
	      (specification->package "sicp")
	      ((options->transformation '((with-branch . "emacs-guix=master")))
	       (specification->package "emacs-guix"))
	      (specification->package "emacs-debbugs")
	      (specification->package "emacs-srfi")
	      (specification->package "emacs-geiser")
	      (specification->package "emacs-geiser-guile")
	      (specification->package "guile-chickadee")))
   (init '((require 'geiser-guile)
	   (setq geiser-default-implementation 'guile)
	   (require 'guix)
	   (load "/home/zjabbar/.guix-profile/share/emacs/site-lisp/subdirs.el")
	   (global-guix-prettify-mode)
	   (setq user-full-name "Zain Jabbar")
	   (setq user-mail-address "zaijab2000@gmail.com")
	   (symex-initialize)
	   (global-set-key (kbd "s-y") 'symex-mode-interface)
					;(require 'rigpa)
					;(setq rigpa-mode t)

					;(remove-hook 'evil-symex-state-exit-hook (function symex-disable-editing-minor-mode))
	   ;; custom config
					;(setq rigpa-show-menus nil)

	   ;; navigating meta modes
	   ;; (global-unset-key (kbd "s-m"))
	   ;; (global-set-key (kbd "s-m s-m") 'rigpa-flashback-to-last-tower)
	   ;; (global-set-key (kbd "C-<escape>")
	   ;; 		   (lambda ()
	   ;; 		     (interactive)
	   ;; 		     (when (eq rigpa--complex rigpa-meta-complex)
	   ;; 		       (rigpa-exit-mode-mode))
	   ;; 		     (rigpa-enter-tower-mode)))
	   ;; (global-set-key (kbd "M-<escape>") 'rigpa-enter-mode-mode)
	   ;; (global-set-key (kbd "s-<escape>") 'rigpa-enter-mode-mode)
	   ;; (global-set-key (kbd "M-<return>")
	   ;; 		   (lambda ()
	   ;; 		     (interactive)
	   ;; 		     (when (eq rigpa--complex rigpa-meta-complex)
	   ;; 		       (rigpa-enter-selected-level)
	   ;; 		       (let ((ground (rigpa--get-ground-buffer)))
	   ;; 			 (rigpa-exit-mode-mode)
	   ;; 			 (switch-to-buffer ground)))))
	   ;; (global-set-key (kbd "s-<return>")
	   ;; 		   (lambda ()
	   ;; 		     (interactive)
	   ;; 		     (when (eq rigpa--complex rigpa-meta-complex)
	   ;; 		       (rigpa-enter-selected-level)
	   ;; 		       (let ((ground (rigpa--get-ground-buffer)))
	   ;; 			 (rigpa-exit-mode-mode)
	   ;; 			 (switch-to-buffer ground)))))
	   ;; (global-set-key (kbd "C-<return>")
	   ;; 		   (lambda ()
	   ;; 		     (interactive)
	   ;; 		     (when (eq rigpa--complex rigpa-meta-tower-complex)
	   ;; 		       (rigpa-exit-tower-mode)
	   ;; 		       (rigpa-enter-mode-mode))))

	   ;; indexed entry to various modes
	   (global-set-key (kbd "s-n") 'evil-normal-state)
					;(global-set-key (kbd "s-y") ; symex mode
					;		   (lambda ()
					;		     (interactive)
					;		     (rigpa-enter-mode "symex")))
					;  (global-set-key (kbd "s-;") (kbd "s-y"))
	   ;; (global-set-key (kbd "s-W") ; window mode
	   ;; 		   (lambda ()
	   ;; 		     (interactive)
	   ;; 		     (rigpa-enter-mode "window")))
	   ;; (global-set-key (kbd "s-v") ; view mode
	   ;; 		   (lambda ()
	   ;; 		     (interactive)
	   ;; 		     (rigpa-enter-mode "view")))
	   ;; (global-set-key (kbd "s-x") ; char mode
	   ;; 		   (lambda ()
	   ;; 		     (interactive)
	   ;; 		     (rigpa-enter-mode "char")))
	   ;; (global-set-key (kbd "s-a") ; activity mode
	   ;; 		   (lambda ()
	   ;; 		     (interactive)
	   ;; 		     (rigpa-enter-mode "activity")))
	   ;; (global-set-key (kbd "s-z") ; text mode
	   ;; 		   (lambda ()
	   ;; 		     (interactive)
	   ;; 		     (rigpa-enter-mode "text")))
	   ;; (global-set-key (kbd "s-g") ; history mode
	   ;; 		   (lambda ()
	   ;; 		     (interactive)
	   ;; 		     (rigpa-enter-mode "history")))
	   ;; (global-set-key (kbd "s-i") ; system mode
	   ;; 		   (lambda ()
	   ;; 		     (interactive)
	   ;; 		     (rigpa-enter-mode "system")))
	   ;; (global-set-key (kbd "s-b") ; buffer mode
	   ;; 		   (lambda ()
	   ;; 		     (interactive)
	   ;; 		     (rigpa-enter-mode "buffer")))
	   ;; (global-set-key (kbd "s-f") ; file mode
	   ;; 		   (lambda ()
	   ;; 		     (interactive)
	   ;; 		     (rigpa-enter-mode "file")))
	   ;; (global-set-key (kbd "s-t") ; tab mode
	   ;; 		   (lambda ()
	   ;; 		     (interactive)
	   ;; 		     (rigpa-enter-mode "tab")))
	   ;; (global-set-key (kbd "s-b") ; line mode
	   ;; 		   (lambda ()
	   ;; 		     (interactive)
	   ;; 		     (rigpa-enter-mode "line")))
	   ;; (global-set-key (kbd "s-E") ; application mode
	   ;; 		   (lambda ()
	   ;; 		     (interactive)
	   ;; 		     (rigpa-enter-mode "application")))
	   ;; (global-set-key (kbd "s-R") ; word mode
	   ;; 		   (lambda ()
	   ;; 		     (interactive)
	   ;; 		     (rigpa-enter-mode "word")))

					;(define-key symex-mode-map (kbd ""))
	   (add-hook 'scheme-mode-hook 'guix-devel-mode)
	   (add-hook 'after-init-hook 'envrc-global-mode)
	   (with-eval-after-load 'envrc
				 (define-key envrc-mode-map (kbd "C-c e") 'envrc-command-map))))

   (early-init '((setq symex-modal-backend 'evil)))))

(define sql-configuration
  (home-emacs-configuration
   (packages (list (specification->package "emacs-vterm")))
   (init '((setq sql-connection-alist
		 '((uhm-campus-energy
		    (sql-product 'postgres)
		    (sql-default-directory "/ssh:zain@128.171.46.101:")
		    (sql-server "localhost")
		    (sql-user "zain")
		    (sql-database "uhm2023")
		    (sql-port 5432))
		   (zain-campus-energy
		    (sql-product 'postgres)
		    (sql-default-directory "/ssh:zain@128.171.46.101:")
		    (sql-server "localhost")
		    (sql-user "zain")
		    (sql-database "zain")
		    (sql-port 5432))))))))



(define exwm-configuration
  (home-emacs-configuration
   (packages (list
	      ((options->transformation
		'((with-git-url . "emacs-exwm=https://github.com/ch11ng/exwm.git")))
	       (specification->package "emacs-exwm"))
	      (specification->package "emacs-xelb")
	      (specification->package "picom")
	      (specification->package "emacs-windsize")
	      (specification->package "unclutter") 
	      (specification->package "xrandr")
	      (specification->package "arandr")))
   (init '((require 'exwm)
	   (require 'xelb)
	   (require 'windsize)
	   (global-set-key (kbd "<f7>") (function
					 (lambda () (interactive)
						 (call-process-shell-command "loginctl suspend"))))
	   (global-set-key (kbd "<f8>") 'toggle-exwm-input-line-mode-passthrough)
	   (global-set-key (kbd "s-0") 'delete-window)
	   (global-set-key (kbd "s-1") 'delete-other-windows)
	   (global-set-key (kbd "s-2") 'split-window-below)
	   (global-set-key (kbd "s-3") 'split-window-right)
	   (global-set-key (kbd "s-5") 'exwm-workspace-switch)
	   (global-set-key (kbd "s-q") (function (lambda () (interactive) (kill-buffer (current-buffer)))))
	   (global-set-key (kbd "s-w") 'tab-bar-switch-to-tab)
	   (global-set-key (kbd "s-e") (function
					(lambda () (interactive)
						(start-process-shell-command "google-chrome-unstable" nil "google-chrome-unstable"))))
	   (global-set-key (kbd "s-r") (function eshell))
	   (global-set-key (kbd "s-t") (function eval-region))
	   (global-set-key (kbd "s-K") 'windsize-up)
	   (global-set-key (kbd "s-J") 'windsize-down)
	   (global-set-key (kbd "s-f") 'exwm-floating-toggle-floating)
	   (global-set-key (kbd "s-<tab>") 'switch-to-buffer)
	   (global-set-key (kbd "s-<escape>") 'execute-extended-command)
	   (global-set-key (kbd "s-`") 'eshell-command)
	   (global-set-key (kbd "s-;") 'shell-command)
	   (global-set-key (kbd "s-:") 'eval-expression)
	   (global-set-key (kbd "s-c") (function
					(lambda () (interactive)
						(find-file "~/code/guix-channel/zaijab/services/emacs.scm"))))
	   (global-set-key (kbd "s-b") (function
					(lambda () (interactive)
						(find-file (read-file-name "" "~/books/")))))
	   (global-set-key (kbd "C-x C-t") 'vterm)
	   (global-set-key (kbd "C-x C-n") 'org-roam-node-find)           
	   (global-set-key (kbd "s-a") 'cfw:open-org-calendar)
	   (global-set-key (kbd "s-m") 'mu4e)
	   (global-set-key (kbd "s-d") (function geiser-guile))
	   (global-set-key (kbd "s-z") (function elfeed))
	   (global-set-key (kbd "s-g") (function guix))
	   (global-set-key (kbd "s-x") (function eww))
	   (global-set-key (kbd "s-s") (function org-roam-capture))
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
	   (defun exwm-rename-buffer-to-title () (exwm-workspace-rename-buffer exwm-class-name))
	   (defun exwm-rename-buffer ()
	     (interactive)
	     (exwm-workspace-rename-buffer
	      (concat exwm-class-name ":"
		      (if (<= (length exwm-title) 25) exwm-title
			  (concat (substring exwm-title 0 24) "...")))))
	   
	   ;; Add these hooks in a suitable place (e.g., as done in exwm-config-default)
	   (add-hook 'exwm-update-class-hook 'exwm-rename-buffer-to-title)
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
	   (setq exwm-input-prefix-keys (append super-keys '(f7
							     f8
							     XF86AudioRaiseVolume
							     XF86AudioLowerVolume
							     XF86AudioNext
							     XF86AudioPlay
							     XF86AudioPrev
							     XF86AudioMute)))
	   (define-key exwm-mode-map (kbd "C-c") nil)
	   
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
	     (force-mode-line-update))))))

(define ui-configuration
  (home-emacs-configuration
   (packages (list (specification->package "emacs-modus-themes")
		   (specification->package "emacs-nano-modeline")
                   (specification->package "emacs-rainbow-delimiters")
                   (specification->package "emacs-which-key")))
   (early-init '((setq gc-cons-threshold most-positive-fixnum
		       package-enable-at-startup nil
		       indicate-buffer-boundaries nil
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
		       nitial-scratch-message nil
		       byte-compile-root-dir nil
		       frame-inhibit-implied-resize t
		       redisplay-dont-pause t
		       max-mini-window-height 1
		       initial-scratch-message nil)
		 (setq org-src-fontify-natively t)
		 (setq org-src-tab-acts-natively t)
		 (setq org-src-preserve-indentation nil
		       org-edit-src-content-indentation 0)
		 (tooltip-mode -1)
		 (scroll-bar-mode -1)
		 (menu-bar-mode -1)
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
		 (pixel-scroll-precision-mode)
		 ))
   (init '((set-face-attribute 'default nil :font "Iosevka-14")
	   (setq nano-modeline-position 'bottom)
	   (nano-modeline-mode)
	   (tab-bar-mode)
	   (set-face-attribute 'tab-bar nil :height 140)
	   (display-time-mode)
	   (customize-set-variable 'tab-bar-format
				   '(tab-bar-format-history
				     tab-bar-format-tabs
				     tab-bar-separator
				     tab-bar-format-add-tab
				     tab-bar-format-align-right
				     tab-bar-format-global))

	   (customize-set-variable 'mode-line-misc-info '(""))
	   (customize-set-variable 'display-time-load-average-threshold 100)
	   (customize-set-variable 'display-time-day-and-date t)
	   (set-default 'truncate-lines t)
	   (load-theme 'modus-operandi t)
	   
	   (define-globalized-minor-mode global-rainbow-delimiters-mode rainbow-delimiters-mode rainbow-delimiters-mode-enable)
	   (global-rainbow-delimiters-mode)
	   (which-key-mode)

	   ))))

;;; Combine all Emacs-Configurations within module

(define home-emacs-total-configuration
  (fold (lambda (config-1 config-2) (home-emacs-configuration
				     (emacs emacs-next-tree-sitter) 
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
