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
  #:use-module (nongnu packages chrome)

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

(define project-configuration
  (home-emacs-configuration
   (packages (list (specification->package "git")))
   (init '((require 'ansi-color)
	   (defun colorize-compilation-buffer ()
	     (let ((inhibit-read-only t))
	       (ansi-color-apply-on-region (point-min) (point-max))))
	   (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)))))

(define japanese-configuration
  (home-emacs-configuration
   (packages (list (specification->package "emacs-ddskk")
		   (specification->package "font-fira-code")
		   (specification->package "font-google-noto")
		   (specification->package "font-lohit")
		   (specification->package "font-vazir")
		   (specification->package "font-ipa-mj-mincho")
		   (specification->package "font-iosevka")))
   (init '((require 'facemenu)

	   (defun jisho-word->japanese-part (jisho-word)
	     (list (gethash "word" (elt (gethash "japanese" jisho-word) 0))
		   (gethash "reading" (elt (gethash "japanese" jisho-word) 0))))
	   (defun jisho-word->english-part (jisho-word)
	     (gethash "english_definitions" (elt (gethash "senses" jisho-word) 0)))

	   (defun kanji-word->drill (word)
	     ;; (setq word (cons (car word) word))
	     (apply 'format "%s :drill:Japanese:\n:PROPERTIES:\n:DRILL_CARD_TYPE: twosided\n:END:\n** Kanji\n%s\n** Picture\n\n** Kana\n%s\n** English\n%s\n" (cons (car word) word)))

	   (defun kana-word->drill (word)
	     ;; (setq word (cons (car word) word))
	     (apply 'format "%s :drill:Japanese:\n:PROPERTIES:\n:DRILL_CARD_TYPE: twosided\n:END:\n** Kana\n%s\n** Picture\n\n** English\n%s\n" (cons (car word) word)))

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
		*jisho-results* nil nil 'equal)))))))

(define garbage-configuration
  (home-emacs-configuration
   (packages (list (specification->package "emacs-gcmh")
		   (specification->package "emacs-explain-pause-mode")))
   (init '((gcmh-mode 1)))))

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
	   (define-key corfu-map (kbd "TAB") (function corfu-next))
	   (define-key corfu-map (kbd "S-TAB") (function corfu-previous))))))

(define cape-configuration
  (home-emacs-configuration
   (packages (list
	      (specification->package "emacs-cape")
	      (specification->package "emacs-tempel")
	      ))
   (init '((require 'tempel)
	   (defun tempel-setup-capf ()
	     (setq-local completion-at-point-functions
			 (cons (function tempel-expand)
			       completion-at-point-functions)))

	   (add-hook 'prog-mode-hook 'tempel-setup-capf)
	   (add-hook 'text-mode-hook 'tempel-setup-capf)
	   
	   (setq tab-always-indent 'complete)
	   (add-to-list 'completion-at-point-functions (function cape-symbol))
	   (add-to-list 'completion-at-point-functions (function cape-line))
	   ))))

(define pdf-tools-configuration
  (home-emacs-configuration
   (packages (list (specification->package "emacs-pdf-tools")))
   (init '((pdf-tools-install)))))

(define cryptography-configuration
  (home-emacs-configuration
   (packages (list (specification->package "emacs-pdf-tools")
		   (specification->package "emacs-pinentry")
		   (specification->package "pinentry-emacs")
		   (specification->package "password-store")
		   (specification->package "browserpass-native")
		   (specification->package "pinentry")
		   (specification->package "gnupg")
		   (specification->package "openssh")
		   ))
   (init '(
	   (defun chomp (str)
	     "Chomp tailing whitespace from STR."
	     (replace-regexp-in-string (rx (* (any " \t\n")) eos)
				       ""
				       str))
	   
	   (let ((ssh_auth_sock (chomp (shell-command-to-string "gpgconf --list-dirs agent-ssh-socket"))))
	     (setenv "SSH_AUTH_SOCK" ssh_auth_sock))
	   (setq epa-pinentry-mode 'loopback)
	   (setq epg-pinentry-mode 'loopback)
	   (pinentry-start)
	   (shell-command "gpg-connect-agent /bye")
	   (setq auth-sources '(password-store))
	   (auth-source-pass-enable)
	   ;; (setq mml-secure-openpgp-signers '("F2E03744BDA622D8"))
	   ))))

(define elfeed-configuration
  (home-emacs-configuration
   (packages (list (specification->package "emacs-elfeed")
		   emacs-elfeed-tube
		   (specification->package "mpv")
		   (specification->package "yt-dlp")
		   (specification->package "ffmpeg")))
   (init '(
	   (setq elfeed-feeds '(("https://www.youtube.com/feeds/videos.xml?channel_id=UC2D2CMWXMOVWx7giW1n3LIg" health huberman)
				("https://www.youtube.com/feeds/videos.xml?channel_id=UCe0TLA0EsQbE-MjuHXevj2A" health jeff)
				("https://www.youtube.com/feeds/videos.xml?channel_id=UCkFJBuwX2iPKCgCITXt2Bnw" fun fatguy)
				("https://www.youtube.com/feeds/videos.xml?channel_id=UCrTW8WZTlOZMvvn_pl1Lpsg" fun nicob)
				("https://www.youtube.com/feeds/videos.xml?channel_id=UCP9q8DRbsTDPhU4E0R3-1rA" fun pekin)
				("https://www.youtube.com/feeds/videos.xml?channel_id=UCT0fBcIYwMsp6IRCm5E3eTA" fun pekin)
				("https://www.youtube.com/feeds/videos.xml?channel_id=UCYO_jab_esuFRV4b17AJtAw" fun grant)
				("https://www.youtube.com/feeds/videos.xml?channel_id=UCAiiOTio8Yu69c3XnR7nQBQ" crafter david)
				("https://www.youtube.com/feeds/videos.xml?channel_id=UCuj_loxODrOPxSsXDfJmpng" crafter andrew)
				("https://www.youtube.com/feeds/videos.xml?playlist_id=PL5B3KLQNAC5j46Ro64xF7hLV6Uf-gHUHL" lecture continuum-mechanics)
				("https://www.youtube.com/feeds/videos.xml?playlist_id=PLEC88901EBADDD980" lecture mit odes)
				("https://www.youtube.com/feeds/videos.xml?playlist_id=PL221E2BBF13BECF6C" lecture mit linear-algebra)
				("https://www.youtube.com/feeds/videos.xml?playlist_id=PLLq_gUfXAnkkvL_UoCGivS0wOYhwCtczI" lecture mit pde)
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
	   (setq-default elfeed-search-filter "")
	   (setq-default elfeed-search-title-max-width 100)
	   (setq-default elfeed-search-title-min-width 100)
	   (defun browse-url-mpv (url &optional new-window)
	     (start-process "mpv" "*mpv*" "mpv" url))

	   (add-to-list 'browse-url-handlers (cons "https:\\/\\/www\\.youtube." 'browse-url-mpv))
	   (add-hook 'elfeed-new-entry-hook
		     (elfeed-make-tagger :feed-url "youtube\\.com"
					 :add '(video youtube)))


	   (setq youtube-dl-path "yt-dlp --sponsorblock-mark all")
	   (setq youtube-dl-output-dir "~/media/")

	   (defun elfeed-download-video ()
	     "Download a video using youtube-dl."
	     (interactive)
	     (async-shell-command (format "%s -o \"%s%s\" -f bestvideo+bestaudio %s"
					  youtube-dl-path
					  youtube-dl-output-dir
					  "%(title)s.%(ext)s"
					  (elfeed-entry-link elfeed-show-entry))))
	   ))))

(define music-configuration
  (home-emacs-configuration
   (packages (list (specification->package "emacs-libmpdel")
		   (specification->package "alsa-utils")
		   (specification->package "emacs-alsamixer-el")
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
	       (specification->package "emacs-org-roam"))))
   (init '((require 'org-roam-node)
	   (setq org-roam-directory "~/notes")
	   (setq org-roam-v2-ack t)
	   (org-roam-db-autosync-mode)
	   (setq org-roam-capture-templates
		 '(("n" "default" plain "%?"
		    :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
		    :unnarrowed t)
		   ("d" "drill" entry "* %(word->drill (jisho-search->completing-read))"
		    :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "\n")
		    :unnarrowed t)))

	   ))))

(define website-configuration
  (home-emacs-configuration
   (packages (list google-chrome-unstable
		   (specification->package "ungoogled-chromium")))
   (init '((defun zain-publish ()
	     (interactive)
	     (let ((current-prefix-arg (list 4))
		   (default-directory "~/code/zaijab.github.io"))
	       (call-interactively 'org-publish-all)
	       (shell-command "git add -A;git commit -am \"Updating Website\";git push -fu origin roam")))

	   (global-set-key (kbd "C-c C-p C-w") 'zain-publish)
	   
	   (require 'ucs-normalize)
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
	     "Gets the title of the org file and uses this (slugified) for the output filename.
This is mainly to override org-roam's default filename convention of `timestamp-title_of_your_note` which doesn't work well with Agora."
	     (let* ((title (get-title (buffer-file-name (buffer-base-buffer))))
		    (directory (file-name-directory output-file))
		    (slug (commonplace/slugify-title title)))
	       (concat directory slug ".html")))
      
	   (advice-add (function org-export-output-file-name) :filter-return (function commonplace/slugify-export-output-file-name))
      
	   (setq org-publish-project-alist
		 '(("orgfiles"
		    :base-directory "~/notes/"
		    :publishing-directory "~/code/zaijab.github.io/"
		    :publishing-function org-html-publish-to-html
		    :base-extension "org"
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
		   ("zaindaman" :components ("orgfiles" "images" "css" "CNAME"))))
	   ))))

(define hotkey-configuration
  (home-emacs-configuration
   (packages (list (specification->package "emacs-evil")))
   (init '((evil-mode 1)))))

(define org-mode-configuration
  (home-emacs-configuration
   (packages (list (specification->package "emacs-org-fragtog")
		   (specification->package "emacs-org-modern")
		   (specification->package "emacs-tempel")
		   (specification->package "emacs-valign")
		   (specification->package "texlive")
		   (specification->package "ispell")
		   ))
   (init '((require 'org)
	   (require 'ox)
	   (global-org-modern-mode)
	   (global-unset-key (kbd "C-x C-r"))
	   (global-unset-key (kbd "C-x C-z"))
	   (global-unset-key (kbd "C-x C-n"))
	   (global-set-key (kbd "C-x C-n") 'org-roam-node-find)
	   (global-set-key (kbd "C-x C-r C-n") 'org-roam-capture)
	   (setq org-agenda-files '("~/notes/20211222094239-workflow.org"))
	   (setq org-startup-with-inline-images t)
	   (setq org-startup-with-latex-preview t)
	   (setq org-preview-latex-default-process 'dvisvgm)
	   (add-hook 'org-mode-hook 'org-fragtog-mode)
	   ;; (add-hook 'org-mode-hook 'flyspell-mode)
	   (add-hook 'org-mode-hook 'visual-line-mode)
	   (setq org-confirm-babel-evaluate nil)
	   (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
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
	      (specification->package "jupyter")
	      (specification->package "remmina")
	      (specification->package "gtk+")
	      (specification->package "webkitgtk-with-libsoup2")
	      (specification->package "glib-networking")
	      (specification->package "gsettings-desktop-schemas")
	      (specification->package "python-pygobject")
	      (specification->package "qtwebkit")
	      (specification->package "network-manager-applet")
	      (specification->package "hicolor-icon-theme")
	      (specification->package "python-sqlalchemy")
	      (specification->package "python-sqlalchemy-utils")
	      ;; (specification->package "tensorflow")
	      ;; ((options->transformation
	      ;;   '((with-input . "python-pyparsing=python-pyparsing@3.0.6")
	      ;;     (without-tests . "python-pydot")
	      ;;     (without-tests . "python-keras")))
	      ;;  (specification->package "python-keras"))
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
   (packages (list (specification->package "emacs-lispy")
		   (specification->package "emacs-lispyville")
		   (specification->package "direnv")
		   ;; (specification->package "emacs-symex")
		   (specification->package "emacs-guix")
		   (specification->package "emacs-envrc")
		   (specification->package "emacs-geiser")
		   (specification->package "emacs-geiser-guile")))
   (init '((require 'geiser-guile)
	   (setq geiser-default-implementation 'guile)
	   (require 'guix)
	   (load "/home/zjabbar/.guix-profile/share/emacs/site-lisp/subdirs.el")
	   (global-guix-prettify-mode)
	   (setq user-full-name "Zain Jabbar")
	   (setq user-mail-address "zaijab2000@gmail.com")
	   ;; (setq symex-modal-backend 'evil)
	   (add-hook 'scheme-mode-hook 'guix-devel-mode)
	   (add-hook 'emacs-lisp-mode-hook 'lispy-mode)
	   (add-hook 'scheme-mode-hook 'lispy-mode)
	   (add-hook 'after-init-hook 'envrc-global-mode)
	   (with-eval-after-load 'envrc
				 (define-key envrc-mode-map (kbd "C-c e") 'envrc-command-map))))))

(define sql-configuration
  (home-emacs-configuration
   (packages (list (specification->package "emacs-vterm")))
   (init '((setq sql-connection-alist
		 '((uhm-campus-energy
		    (sql-product 'postgres)
		    (sql-default-directory "/ssh:zain@128.171.46.101:")
		    (sql-server "localhost")
		    (sql-user "zain")
		    (sql-database "uhm2022")
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
	      (specification->package "xrandr")))
   (init '((require 'exwm)
	   (require 'xelb)
	   (require 'windsize)
	   (global-set-key (kbd "s-H") 'windsize-left)
	   (global-set-key (kbd "s-L") 'windsize-right)
	   (global-set-key (kbd "s-J") 'windsize-down)
	   (global-set-key (kbd "s-K") 'windsize-up)
	   (global-set-key (kbd "s-1") 'delete-other-windows)
	   (global-set-key (kbd "s-2") 'split-window-below)
	   (global-set-key (kbd "s-3") 'split-window-right)
	   (global-set-key (kbd "s-w") 'tab-bar-switch-to-tab)
	   (global-set-key (kbd "s-5") 'exwm-workspace-switch)
	   (global-set-key (kbd "s-<tab>") 'switch-to-buffer)
	   (global-set-key (kbd "s-c") (function
					(lambda () (interactive)
						(find-file "~/code/guix-channel/zaijab/services/emacs.scm"))))
	   (global-set-key (kbd "C-x C-t") 'vterm)
	   (global-set-key (kbd "s-s") (function org-roam-capture))
	   (global-set-key (kbd "s-r") (function eval-region))
	   (global-set-key (kbd "s-0") 'delete-window)
	   (global-set-key (kbd "s-h") 'windmove-left)
	   (global-set-key (kbd "s-j") 'windmove-down)
	   (global-set-key (kbd "s-k") 'windmove-up)
	   (global-set-key (kbd "s-l") 'windmove-right)
	   (global-set-key (kbd "s-q") (function (lambda () (interactive) (kill-buffer (current-buffer)))))
	   
	   (global-set-key (kbd "s-<SPC>") (function
					    (lambda (command)
					      (interactive (list (read-shell-command "$ ")))
					      (start-process-shell-command command nil command))))
	   (global-set-key (kbd "s-e") (function
					(lambda () (interactive)
						(start-process-shell-command "google-chrome-unstable" nil "google-chrome-unstable"))))
	   (global-set-key (kbd "<f7>") (function
					 (lambda () (interactive)
						 (call-process-shell-command "loginctl suspend"))))
	   (global-set-key (kbd "<f8>") 'toggle-exwm-input-line-mode-passthrough)
	   (global-set-key (kbd "s-a") 'toggle-exwm-input-line-mode-passthrough)
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
		 (setq warning-suppress-log-types '((comp) (comp)))
		 (setq warning-suppress-types '((comp) (comp)))
		 (setq user-emacs-directory "~/.config/emacs")
		 (setq byte-compile-warnings '(cl-functions))
		 (setq make-backup-files nil)
		 (setq auto-save-default nil)
		 (setq create-lockfiles nil)
		 (pixel-scroll-precision-mode)))
   (init '((set-face-attribute 'default nil :font "Iosevka-14")
	   (load-theme 'modus-operandi t)
	   (setq nano-modeline-position 'bottom)
	   (nano-modeline-mode)
	   (tab-bar-mode)
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
	   
	   (define-globalized-minor-mode global-rainbow-delimiters-mode rainbow-delimiters-mode rainbow-delimiters-mode-enable)
	   (global-rainbow-delimiters-mode)
	   (which-key-mode)))))

;;; Combine all Emacs-Configurations within module

(define home-emacs-total-configuration
  (fold (lambda (config-1 config-2) (home-emacs-configuration
				     (emacs ((options->transformation
					      '((with-branch . "emacs-next=master"))) (specification->package "emacs-next")))
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
 
