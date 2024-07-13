;;; The module zaijab / services / emacs
;; Creates the module "Emacs" in the "Services" for the "zaijab" Channel

(define-module (zaijab services emacs)
  #:use-module (gnu home)
  #:use-module (gnu packages)
  #:use-module (gnu packages emacs)
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
  #:use-module (nongnu packages fonts)
  #:use-module (guix-science packages python)
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

;; Completion Style
(define orderless-configuration
  (home-emacs-configuration
   (packages (list (specification->package "emacs-orderless")))
   (init '((setq completion-styles '(orderless basic)
		 completion-category-overrides '((file (styles basic partial-completion)))
		 orderless-smart-case nil
		 completion-ignore-case t
		 read-file-name-completion-ignore-case t
		 read-buffer-completion-ignore-case t)))))

;; Completion UI
(define vertico-configuration
  (home-emacs-configuration
   (packages (list (specification->package "emacs-vertico")))
   (init '((setq minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
	   (add-hook 'minibuffer-setup-hook (function cursor-intangible-mode))
	   (defun crm-indicator (args)
	     (cons (format "[CRM%s] %s"
			   (replace-regexp-in-string
			    "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
			    crm-separator)
			   (car args))
		   (cdr args)))
	   (advice-add (function completing-read-multiple) :filter-args (function crm-indicator))
	   (vertico-mode 1)))))

;; In Buffer Completion
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
	   (define-key corfu-map (kbd "M-}") (function corfu-next))
	   (define-key corfu-map (kbd "M-{") (function corfu-previous))))))

;; Templates
(define tempel-configuration
  (home-emacs-configuration
   (packages (list (specification->package "emacs-tempel")))
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
	   (define-key tempel-map (kbd "C-a") (function tempel-prev))
	   (define-key tempel-map (kbd "C-d") (function tempel-next))
	   (global-set-key (kbd "M-+") (function tempel-complete))))))

;; Completion at Point Functions
(define cape-configuration
  (home-emacs-configuration
   (packages (list (specification->package "emacs-cape")))
   (init '((setq tab-always-indent 'complete)
	   (add-to-list 'completion-at-point-functions (function cape-file))))))

;; Annotations
(define marginalia-configuration
  (home-emacs-configuration
   (packages (list (specification->package "emacs-marginalia")))
   (init '((marginalia-mode)))))

(define web-configuration
  (home-emacs-configuration
   (init '((setq eww-search-prefix "http://localhost:8080/search?q=")))))

;; Live Preview Selection
(define embark-configuration
  (home-emacs-configuration
   (packages (list (specification->package "emacs-embark")
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

;; "Right Click"
(define consult-configuration
  (home-emacs-configuration
   (packages (list (specification->package "emacs-consult")))
   (init '((require 'consult)
	   (add-hook 'completion-list-mode-hook consult-preview-at-point-mode)
	   (setq register-preview-delay 0.5
		 register-preview-function (function consult-register-format))
	   (advice-add (function register-preview) :override (function consult-register-window))
	   (setq xref-show-xrefs-function (function consult-xref)
		 xref-show-definitions-function (function consult-xref))
	   (consult-customize
	    consult-theme :preview-key '(:debounce 0.2 any)
	    consult-ripgrep consult-git-grep consult-grep
	    consult-bookmark consult-recent-file consult-xref
	    consult--source-bookmark consult--source-file-register
	    consult--source-recent-file consult--source-project-recent-file
	    :preview-key '(:debounce 0.4 any))

	   (setq consult-narrow-key "<")
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
	   (add-to-list 'consult-buffer-sources 'consult--source-workspace)

	   ))))

(define citation-configuration
  (home-emacs-configuration
   (packages (list (specification->package "emacs-citar")
		   (specification->package "emacs-citar-org-roam")))
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
   (packages (list emacs-tabspaces))
   (init '((use-package tabspaces
			:hook (after-init . tabspaces-mode) 
			:commands (tabspaces-switch-or-create-workspace
				   tabspaces-open-or-create-project-and-workspace)
			:custom
			(tabspaces-use-filtered-buffers-as-default t)
			(tabspaces-default-tab "Default")
			(tabspaces-remove-to-default t)
			(tabspaces-include-buffers '("*scratch*"))
			(tab-bar-new-tab-choice "*scratch*")
			;; sessions
			(tabspaces-session t)
			(tabspaces-session-auto-restore t))))
   (early-init '((setq desktop-restore-frames nil
		       desktop-restore-in-current-display nil)
		 (setq switch-to-buffer-obey-display-actions t)
		 (defun mp-toggle-window-dedication ()
		   "Toggles window dedication in the selected window."
		   (interactive)
		   (set-window-dedicated-p (selected-window)
					   (not (window-dedicated-p (selected-window)))))))))

(define meow-configuration
  (home-emacs-configuration
   (packages (list (specification->package "emacs-meow")))
   (init '((setq meow-use-clipboard t)
	   (defun meow-setup ()
	     (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
	     (meow-motion-overwrite-define-key
	      '("j" . meow-next)
	      '("k" . meow-prev)
	      '("<escape>" . ignore))
	     (meow-leader-define-key
	      ;; SPC j/k will run the original command in MOTION state.
	      '("j" . "H-j")
	      '("k" . "H-k")
	      ;; Use SPC (0-9) for digit arguments.
	      '("1" . meow-digit-argument)
	      '("2" . meow-digit-argument)
	      '("3" . meow-digit-argument)
	      '("4" . meow-digit-argument)
	      '("5" . meow-digit-argument)
	      '("6" . meow-digit-argument)
	      '("7" . meow-digit-argument)
	      '("8" . meow-digit-argument)
	      '("9" . meow-digit-argument)
	      '("0" . meow-digit-argument)
	      '("/" . meow-keypad-describe-key)
	      '("?" . meow-cheatsheet))
	     (meow-normal-define-key
	      '("0" . meow-expand-0)
	      '("9" . meow-expand-9)
	      '("8" . meow-expand-8)
	      '("7" . meow-expand-7)
	      '("6" . meow-expand-6)
	      '("5" . meow-expand-5)
	      '("4" . meow-expand-4)
	      '("3" . meow-expand-3)
	      '("2" . meow-expand-2)
	      '("1" . meow-expand-1)
	      '("-" . negative-argument)
	      '(";" . meow-reverse)
	      '("," . meow-inner-of-thing)
	      '("." . meow-bounds-of-thing)
	      '("[" . meow-beginning-of-thing)
	      '("]" . meow-end-of-thing)
	      '("a" . meow-append)
	      '("A" . meow-open-below)
	      '("b" . meow-back-word)
	      '("B" . meow-back-symbol)
	      '("c" . meow-change)
	      '("d" . meow-delete)
	      '("D" . meow-backward-delete)
	      '("e" . meow-next-word)
	      '("E" . meow-next-symbol)
	      '("f" . meow-find)
	      '("g" . meow-cancel-selection)
	      '("G" . meow-grab)
	      '("h" . meow-left)
	      '("H" . meow-left-expand)
	      '("i" . meow-insert)
	      '("I" . meow-open-above)
	      '("j" . meow-next)
	      '("J" . meow-next-expand)
	      '("k" . meow-prev)
	      '("K" . meow-prev-expand)
	      '("l" . meow-right)
	      '("L" . meow-right-expand)
	      '("m" . meow-join)
	      '("n" . meow-search)
	      '("o" . meow-block)
	      '("O" . meow-to-block)
	      '("p" . meow-yank)
	      '("q" . meow-quit)
	      '("Q" . meow-goto-line)
	      '("r" . meow-replace)
	      '("R" . meow-swap-grab)
	      '("s" . meow-kill)
	      '("t" . meow-till)
	      '("u" . meow-undo)
	      '("U" . meow-undo-in-selection)
	      '("v" . meow-visit)
	      '("w" . meow-mark-word)
	      '("W" . meow-mark-symbol)
	      '("x" . meow-line)
	      '("X" . meow-goto-line)
	      '("y" . meow-save)
	      '("Y" . meow-sync-grab)
	      '("z" . meow-pop-selection)
	      '("'" . repeat)
	      '("<escape>" . ignore)))
	   (require 'meow)
	   (meow-setup)
	   (meow-global-mode 1)
	   #;(add-to-list 'meow-mode-state-list '(pdf-view-mode . normal))
	   #;(add-to-list 'meow-mode-state-list '(compilation-mode . normal))))))

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
   (init '((require 'smart-hungry-delete)
	   (smart-hungry-delete-add-default-hooks)
	   (global-set-key (kbd "<backspace>") 'smart-hungry-delete-backward-char)
	   (global-set-key (kbd "<delete>") 'smart-hungry-delete-backward-char)
	   (global-set-key (kbd "C-d") 'smart-hungry-delete-forward-char)))))

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
		   (specification->package "font-lohit")
		   (specification->package "font-vazir")
		   (specification->package "font-ipa-mj-mincho")
		   (specification->package "font-iosevka")
		   (specification->package "jbr")
		   #;font-microsoft-couirer-new))
   (init '((require 'facemenu)
	   (advice-add
	    'skk-previous-candidate :around
	    (lambda (func &optional arg)
	      (interactive "p")
	      (if (and (not (eq skk-henkan-mode 'active))
		       (not (eq last-command 'skk-kakutei-henkan))
		       last-command-event
		       (eq last-command-event
			   (seq-first (car (where-is-internal
					    'meow-prev
					    meow-normal-state-keymap)))))
		  (previous-line)
		  (funcall func arg))))))))

(define svg-configuration
  (home-emacs-configuration
   (init '(;;; svg.el --- SVG image creation functions -*- lexical-binding: t -*-

;; Copyright (C) 2014-2020 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;;         Felix E. Klee <felix.klee@inka.de>
;;         Anand Tamariya <atamariya@gmail.com> 2021
;; Keywords: image
;; Version: 1.0
;; Package-Requires: ((emacs "25"))

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package allows creating SVG images in Emacs.  SVG images are
;; vector-based XML files, really, so you could create them directly
;; as XML.  However, that's really tedious, as there are some fiddly
;; bits.

;; In addition, the `svg-insert-image' function allows inserting an
;; SVG image into a buffer that's updated "on the fly" as you
;; add/alter elements to the image, which is useful when composing the
;; images.

;; Here are some usage examples:

;; Create the base image structure, add a gradient spec, and insert it
;; into the buffer:
;;
;;     (setq svg (svg-create 800 800 :stroke "orange" :stroke-width 5))
;;     (svg-gradient svg "gradient" 'linear '(0 . "red") '(100 . "blue"))
;;     (save-excursion (goto-char (point-max)) (svg-insert-image svg))

;; Then add various elements to the structure:
;;
;;     (svg-rectangle svg 100 100 500 500 :gradient "gradient" :id "rec1")
;;     (svg-circle svg 500 500 100 :id "circle1")
;;     (svg-ellipse svg 100 100 50 90 :stroke "red" :id "ellipse1")
;;     (svg-line svg 100 190 50 100 :id "line1" :stroke "yellow")
;;     (svg-polyline svg '((200 . 100) (500 . 450) (80 . 100))
;;                   :stroke "green" :id "poly1")
;;     (svg-polygon svg '((100 . 100) (200 . 150) (150 . 90))
;;                  :stroke "blue" :fill "red" :id "gon1")

;;; Code:

(require 'cl)
(require 'xml)
(require 'dom)
(require 'subr-x)

(defun svg-create (width height &rest args)
  "Create a new, empty SVG image with dimensions WIDTH x HEIGHT.
ARGS can be used to provide `stroke' and `stroke-width' parameters to
any further elements added."
  (dom-node 'svg
	    `((width . ,width)
	      (height . ,height)
	      (version . "1.1")
	      (xmlns . "http://www.w3.org/2000/svg")
              ,@(unless (plist-get args :xmlns:xlink)
                  '((xmlns:xlink . "http://www.w3.org/1999/xlink")))
              ,@(svg--arguments nil args))))

(defun svg-gradient (svg id type stops)
  "Add a gradient with ID to SVG.
TYPE is `linear' or `radial'.
STOPS is a list of percentage/color pairs."
  (svg--def
   svg
   (apply
    'dom-node
    (if (eq type 'linear)
	'linearGradient
      'radialGradient)
    `((id . ,id)
      (x1 . 0)
      (x2 . 0)
      (y1 . 0)
      (y2 . 1))
    (mapcar
     (lambda (stop)
       (dom-node 'stop `((offset . ,(format "%s%%" (car stop)))
			 (stop-color . ,(cdr stop)))))
     stops))))

(defun svg-rectangle (svg x y width height &rest args)
  "Create a rectangle on SVG, starting at position X/Y, of WIDTH/HEIGHT.
ARGS is a plist of modifiers.  Possible values are

:stroke-width PIXELS   The line width.
:stroke-color COLOR    The line color.
:gradient ID           The gradient ID to use."
  (svg--append
   svg
   (dom-node 'rect
	     `((width . ,width)
	       (height . ,height)
	       (x . ,x)
	       (y . ,y)
	       ,@(svg--arguments svg args)))))

(defun svg-circle (svg x y radius &rest args)
  "Create a circle of RADIUS on SVG.
X/Y denote the center of the circle."
  (svg--append
   svg
   (dom-node 'circle
	     `((cx . ,x)
	       (cy . ,y)
	       (r . ,radius)
	       ,@(svg--arguments svg args)))))

(defun svg-ellipse (svg x y x-radius y-radius &rest args)
  "Create an ellipse of X-RADIUS/Y-RADIUS on SVG.
X/Y denote the center of the ellipse."
  (svg--append
   svg
   (dom-node 'ellipse
	     `((cx . ,x)
	       (cy . ,y)
	       (rx . ,x-radius)
	       (ry . ,y-radius)
	       ,@(svg--arguments svg args)))))

(defun svg-line (svg x1 y1 x2 y2 &rest args)
  "Create a line starting in X1/Y1, ending at X2/Y2 on SVG."
  (svg--append
   svg
   (dom-node 'line
	     `((x1 . ,x1)
	       (x2 . ,x2)
	       (y1 . ,y1)
	       (y2 . ,y2)
	       ,@(svg--arguments svg args)))))

(defun svg-polyline (svg points &rest args)
  "Create a polyline going through POINTS on SVG.
POINTS is a list of x/y pairs."
  (svg--append
   svg
   (dom-node
    'polyline
    `((points . ,(mapconcat (lambda (pair)
			      (format "%s,%s" (car pair) (cdr pair)))
			    points
			    " "))
      ,@(svg--arguments svg args)))))

(defun svg-polygon (svg points &rest args)
  "Create a polygon going through POINTS on SVG.
POINTS is a list of x/y pairs."
  (svg--append
   svg
   (dom-node
    'polygon
    `((points . ,(mapconcat (lambda (pair)
			      (format "%s,%s" (car pair) (cdr pair)))
			    points
			    " "))
      ,@(svg--arguments svg args)))))

(defun svg-use (svg id &rest args)
  (svg--append
   svg
   (dom-node
    'use
    `((href . ,(format "#%s" id))
      ,@(svg--arguments svg args)))))

(defun svg-group (svg children &rest args)
  "Create a group element with list of CHILDREN."
  (svg--append
   svg
   (apply 'dom-node
    'g
    `(,@(svg--arguments svg args))
    children)))

(defun svg-embed (svg image image-type datap &rest args)
  "Insert IMAGE into the SVG structure.
IMAGE should be a file name if DATAP is nil, and a binary string
otherwise.  IMAGE-TYPE should be a MIME image type, like
\"image/jpeg\" or the like."
  (svg--append
   svg
   (dom-node
    'image
    `((xlink:href . ,(svg--image-data image image-type datap))
      ,@(svg--arguments svg args)))))

(defun svg-embed-href (svg image &rest args)
  "Insert IMAGE into the SVG structure.
IMAGE should be a file name."
  (svg--append
   svg
   (dom-node
    'image
    `((xlink:href . ,image)
      ,@(svg--arguments svg args)))))

(defun svg-text (svg text &rest args)
  "Add TEXT to SVG."
  (svg--append
   svg
   (dom-node
    'text
    `(,@(svg--arguments svg args))
    (svg--encode-text text))))

(defun svg-tspan (svg text &rest args)
  "Add TEXT to SVG."
  (svg--append
   svg
   (dom-node
    'tspan
    `(,@(svg--arguments svg args))
    (svg--encode-text text))))

(defun svg-animate (svg attr dur &rest args)
  "Add ANIMATE to SVG."
  (svg--append
   svg
   (dom-node
    'animate
    `((attributeName . ,attr)
      (dur . ,dur)
      ,@(svg--arguments svg args))
    )))

(defun svg--encode-text (text)
  ;; Apparently the SVG renderer needs to have all non-ASCII
  ;; characters encoded, and only certain special characters.
  (if text
  (with-temp-buffer
    (insert text)
    (dolist (substitution '(("&" . "&amp;")
			    ("<" . "&lt;")
			    (">" . "&gt;")))
      (goto-char (point-min))
      (while (search-forward (car substitution) nil t)
	(replace-match (cdr substitution) t t nil)))
    (goto-char (point-min))
    (while (not (eobp))
      (let ((char (following-char)))
        (if (< char 128)
            (forward-char 1)
          (delete-char 1)
          (insert "&#" (format "%d" char) ";"))))
    (buffer-string))))

(defun svg--decode-text (text)
  ;; Apparently the SVG renderer needs to have all non-ASCII
  ;; characters encoded, and only certain special characters.
  (with-temp-buffer
    (insert text)
    (dolist (substitution '(("&amp;" . "&")
			    ("&lt;" . "<")
			    ("&gt;" . ">")))
      (goto-char (point-min))
      (while (search-forward (car substitution) nil t)
	(replace-match (cdr substitution) t t nil)))
    (replace-regexp-in-string "&#\\([0-9]+\\);"
                              (lambda (a)
                                (string (string-to-number (match-string 1 a))))
                              (buffer-string))
    ))

(defun svg--append (svg node)
  ;; id is expected to be unique.
  ;; (let ((old (and (dom-attr node 'id)
  ;;       	  (dom-by-id svg
  ;;                            (concat "\\`" (regexp-quote (dom-attr node 'id))
  ;;                                    "\\'")))))
  ;;   (if old
  ;;       ;; FIXME: This was (dom-set-attributes old (dom-attributes node))
  ;;       ;; and got changed by commit f7ea7aa11f6211b5142bbcfc41c580d75485ca56
  ;;       ;; without any explanation.
  ;;       ;; (setcdr (car old) (cdr node))
  ;;       ;; Remove old node. New node might be a different type.
  ;;       (mapc (lambda (a)
  ;;                 (dom-remove-node svg a))
  ;;               old))
  ;;   (dom-append-child svg node))
  (let ((children (dom-children svg))
        (added nil))
    (if children
        (while (and children
                    (null added))
          (when (equal (dom-attr (car children) 'id)
                       (dom-attr node 'id))
            (setcar children node)
            (setq added t))
          (unless (or added (cdr children))
            (setcdr children (list node)))
          (setq children (cdr children)))
      (dom-append-child svg node)))
  ;; (svg-possibly-update-image svg)
  node)

(defun svg--image-data (image image-type datap)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (if datap
        (insert image)
      (insert-file-contents image))
    (base64-encode-region (point-min) (point-max) t)
    (goto-char (point-min))
    (insert "data:" image-type ";base64,")
    (buffer-string)))

(defun svg--arguments (svg args)
  (let ((stroke-width (or (plist-get args :stroke-width)
			  (dom-attr svg 'stroke-width)))
	(stroke-color (or (plist-get args :stroke-color)
                          (dom-attr svg 'stroke-color)))
        (fill-color (plist-get args :fill-color))
	attr)
    (when stroke-width
      (push (cons 'stroke-width stroke-width) attr))
    (when stroke-color
      (push (cons 'stroke stroke-color) attr))
    (when fill-color
      (push (cons 'fill fill-color) attr))
    (when (plist-get args :gradient)
      (setq attr
	    (append
	     ;; We need a way to specify the gradient direction here...
	     `((x1 . 0)
	       (x2 . 0)
	       (y1 . 0)
	       (y2 . 1)
	       (fill . ,(format "url(#%s)"
				(plist-get args :gradient))))
	     attr)))
    (cl-loop for (key value) on args by #'cddr
	     unless (memq key '(:stroke-color :stroke-width :gradient
                                              :fill-color))
	     ;; Drop the leading colon.
	     do (push (cons (intern (substring (symbol-name key) 1) obarray)
			    value)
		      attr))
    attr))

(defun svg--def (svg def)
  (dom-append-child
   (or (dom-by-tag svg 'defs)
       (let ((node (dom-node 'defs)))
	 (dom-add-child-before svg node)
	 node))
   def)
  svg)

(defun svg-string (svg)
  "Return a string representation of SVG object."
  (with-temp-buffer
    (svg-print svg)
    (buffer-string)))

(defun svg-image (svg &rest props)
  "Return an image object from SVG object.
PROPS is passed on to `create-image' as its PROPS list."
  (apply
   #'create-image
   (svg-string svg)
   'svg t props))

(defun svg-image-from-xml (xml-file-or-string &rest props)
  "Return an image object from SVG XML.
PROPS is passed on to `create-image' as its PROPS list."
  (apply
   #'create-image
   xml-file-or-string
   'svg (not (file-exists-p xml-file-or-string)) props))

(defun svg-insert-image (svg)
  "Insert SVG as an image at point.
If the SVG is later changed, the image will also be updated."
  (let ((image (svg-image svg))
	(marker (point-marker)))
    (insert-image image)
    (dom-set-attribute svg :image marker)))

(defun svg-possibly-update-image (svg)
  (let ((marker (dom-attr svg :image))
        (inhibit-read-only t)
        (inhibit-modification-hooks t)
        image keymap map end)
    (when (and marker
	       (buffer-live-p (marker-buffer marker)))
      (with-current-buffer (marker-buffer marker)
        (setq image (svg-image svg)
              keymap (let ((map (make-sparse-keymap)))
                       (define-key map [nil mouse-1] #'svg-on-click)
                       map)
              end  (1+ marker)
              map (svg-image-map svg))
        (if (image-get-display-property)
            (setq marker (point-min-marker)
                  end (point-max-marker)))
        (image--set-property image :map map)
        (image--set-property image :pointer 'arrow)
	(put-text-property marker end 'keymap keymap)
        (put-text-property marker end 'display image)))))

(defun svg-load (file)
  "Load SVG from file."
  (let (image)
    (with-temp-buffer
      (insert-file-contents file)
      (setq image (libxml-parse-xml-region (point-min) (point-max))))
    (setq image (svg--scrub-image image))
    image))

(defun svg-load-from-xml (xml)
  "Load SVG from XML."
  (let (image)
    (with-temp-buffer
      (insert xml)
      (setq image (libxml-parse-xml-region (point-min) (point-max))))
    (setq image (svg--scrub-image image))
    image))

(defun svg-print (dom)
  "Convert DOM into a string containing the xml representation."
  (when dom
  (if (stringp dom)
      (insert dom)
    (insert (format "<%s" (car dom)))
    (dolist (attr (nth 1 dom))
      ;; Ignore attributes that start with a colon.
      (unless (or (= (aref (format "%s" (car attr)) 0) ?:)
                  (null (cdr attr)))
        (insert (format " %s=\"%s\"" (car attr) (cdr attr)))))
    (insert ">")
    (dolist (elem (nthcdr 2 dom))
      ;; Avoid adding extra space to text node
      ;; (insert " ")
      (svg-print elem))
    (insert (format "</%s>" (car dom))))))

(defun svg-remove (svg id)
  "Remove the element identified by ID from SVG."
  (let* ((node (car (dom-by-id
                     svg
                     (concat "\\`" (regexp-quote id)
                             "\\'")))))
    (when node (dom-remove-node svg node))))

;; Function body copied from `org-plist-delete' in Emacs 26.1.
(defun svg--plist-delete (plist property)
  "Delete PROPERTY from PLIST.
This is in contrast to merely setting it to 0."
  (let (p)
    (while plist
      (if (not (eq property (car plist)))
          (setq p (plist-put p (car plist) (nth 1 plist))))
      (setq plist (cddr plist)))
    p))

(defun svg--path-command-symbol (command-symbol command-args)
  (let ((char (symbol-name command-symbol))
        (relative (if (plist-member command-args :relative)
                      (plist-get command-args :relative)
                    (plist-get command-args :default-relative))))
    (if relative (downcase char) (upcase char))))

(defun svg--elliptical-arc-coordinates
    (rx ry x y &rest args)
  (list
   rx ry
   (or (plist-get args :x-axis-rotation) 0)
   (if (plist-get args :large-arc) 1 0)
   (if (plist-get args :sweep) 1 0)
   x y))

(defun svg--elliptical-arc-command (coordinates-list &rest args)
  (cons
   (svg--path-command-symbol 'a args)
   (svg--list-to-str coordinates-list)))
   ;; (apply 'append
   ;;        (mapcar
   ;;         (lambda (coordinates)
   ;;           (apply 'svg--elliptical-arc-coordinates
   ;;                  coordinates))
   ;;         coordinates-list))))

(defun svg--moveto-command (coordinate-list &rest args)
  (cons
   (svg--path-command-symbol 'm args)
   (svg--list-to-str (list coordinate-list))))
   ;; (apply 'append
   ;;        (mapcar
   ;;         (lambda (coordinates)
   ;;           (list (car coordinates) (cdr coordinates)))
   ;;         coordinates-list))))

(defun svg--closepath-command (&rest args)
  (list (svg--path-command-symbol 'z args)))

(defun svg--lineto-command (coordinates-list &rest args)
  (cons
   (svg--path-command-symbol 'l args)
   (svg--list-to-str coordinates-list)))
   ;; (apply 'append
   ;;        (mapcar
   ;;         (lambda (coordinates)
   ;;           (list (car coordinates) (cdr coordinates)))
   ;;         coordinates-list))))

(defun svg--horizontal-lineto-command (length &rest args)
  (cons
   (svg--path-command-symbol 'h args)
   (list (format "%s" length))))

(defun svg--vertical-lineto-command (length &rest args)
  (cons
   (svg--path-command-symbol 'v args)
   (list (format "%s" length))))

(defun svg--curveto-command (coordinates-list &rest args)
  (cons
   (svg--path-command-symbol 'c args)
   (svg--list-to-str coordinates-list)))
   ;; (apply 'append coordinates-list)))

(defun svg--smooth-curveto-command (coordinates-list &rest args)
  (cons
   (svg--path-command-symbol 's args)
   (svg--list-to-str coordinates-list)))
   ;; (apply 'append coordinates-list)))

(defun svg--quadratic-bezier-curveto-command (coordinates-list
                                              &rest args)
  (cons
   (svg--path-command-symbol 'q args)
   (svg--list-to-str coordinates-list)))
   ;; (apply 'append coordinates-list)))

(defun svg--smooth-quadratic-bezier-curveto-command (coordinates-list
                                                     &rest args)
  (cons
   (svg--path-command-symbol 't args)
   (svg--list-to-str coordinates-list)))

(defun svg--list-to-str (coordinates)
  (cl-loop for a in coordinates
           collect (format "%s,%s" (car a) (cdr a))))

(defun svg--eval-path-command (command default-relative)
  (cl-letf
      (((symbol-function 'moveto) #'svg--moveto-command)
       ((symbol-function 'closepath) #'svg--closepath-command)
       ((symbol-function 'lineto) #'svg--lineto-command)
       ((symbol-function 'horizontal-lineto)
        #'svg--horizontal-lineto-command)
       ((symbol-function 'vertical-lineto)
        #'svg--vertical-lineto-command)
       ((symbol-function 'curveto) #'svg--curveto-command)
       ((symbol-function 'smooth-curveto)
        #'svg--smooth-curveto-command)
       ((symbol-function 'quadratic-bezier-curveto)
        #'svg--quadratic-bezier-curveto-command)
       ((symbol-function 'smooth-quadratic-bezier-curveto)
        #'svg--smooth-quadratic-bezier-curveto-command)
       ((symbol-function 'elliptical-arc)
        #'svg--elliptical-arc-command)
       (extended-command (append command (list :default-relative
                                               default-relative))))
    ;; (mapconcat 'prin1-to-string (apply extended-command) " ")
    (apply extended-command)
    ))

(defun svg-path (svg commands &rest args)
  "Add the outline of a shape to SVG according to COMMANDS.
Coordinates by default are absolute.  ARGS is a plist of
modifiers.  If :relative is t, then coordinates are relative to
the last position, or -- initially -- to the origin."
  (let* ((default-relative (plist-get args :relative))
         (stripped-args (svg--plist-delete args :relative))
         (d (mapconcat 'identity
                       (apply 'append
                       (mapcar
                        (lambda (command)
                          (svg--eval-path-command command
                                                  default-relative))
                        commands)) " ")))
    (svg--append
     svg
     (dom-node 'path
               `((d . ,d)
                 ,@(svg--arguments svg stripped-args))))))

(defun svg-clip-path (svg &rest args)
  "Add a clipping path to SVG, where ARGS is a plist of modifiers.
If applied to a shape via the :clip-path property, parts of that
shape which lie outside of the clipping path are not drawn."
  (let ((new-dom-node (dom-node 'clipPath
                                `(,@(svg--arguments svg args)))))
    (svg--append svg new-dom-node)
    new-dom-node))

(defun svg-node (svg tag &rest args)
  "Add the custom node TAG to SVG."
  (let ((new-dom-node (dom-node tag
                                `(,@(svg--arguments svg args)))))
    (svg--append svg new-dom-node)
    new-dom-node))

(defun svg-bbox (node &optional ref)
  "Return bounding box of node.
Return value is (TOP-LEFT-X TOP-LEFT-Y BOTTOM-RIGHT-X BOTTOM-RIGHT-Y)"
  (if (stringp node)
      nil
    (let* ((tag (dom-tag node))
           (transform (dom-attr node 'transform))
           (point nil)
           (res nil)
           (ox 0)
           (oy 0)
           x1 y1 x2 y2)
      ;; bbox without transformation
      (setq res
            (pcase tag
              ('line (setq x1 (dom-attr node 'x1)
                           y1 (dom-attr node 'y1)
                           x2 (dom-attr node 'x2)
                           y2 (dom-attr node 'y2))
                     (if (or (stringp x1) (stringp y1)
                             (stringp x2) (stringp y2))
                         ;; Ignoring percentange calculations for now
                         nil
                       (setq point (svg--apply-transform 0 0 transform)
                           ox (car point)
                           oy (cdr point))
                     (list (+ ox x1)
                           (+ oy y1)
                           (+ ox x2)
                           (+ oy y2))))
              ('circle (setq x1 (dom-attr node 'cx)
                             y1 (dom-attr node 'cy)
                             point (svg--apply-transform x1 y1 transform)
                             ox (car point)
                             oy (cdr point))
                       (list (- ox (dom-attr node 'r))
                             (- oy (dom-attr node 'r))
                             (+ ox (dom-attr node 'r))
                             (+ oy (dom-attr node 'r))))
              ('text (let* ((size (or (dom-attr node 'font-size) 15))
                            (font-size (if (stringp size)
                                           (string-to-number size)
                                         size))
                            (w 0)
                            (font (car (internal-char-font nil ?m)))
                            (ref-size (aref (font-info font) 2))
                            (glyph nil)
                            (asc 0)
                            (des 0)
                            (text (car (dom-children node)))
                            )
                       ;; x,y denotes bottom left corner on baseline
                       ;; size is 1em
                       (setq x1 (or (dom-attr node 'x) 0)
                             y1 (or (dom-attr node 'y) 0)
                             text (if text (svg--decode-text text))
                             point (svg--apply-transform x1 y1 transform)
                             ox (car point)
                             oy (cdr point))
                       (mapc (lambda (a)
                               (setq glyph (lgstring-glyph
                                            (composition-get-gstring
                                             0 1 font (string a))
                                            0)
                                     w (+ w (lglyph-width glyph))
                                     asc (max asc (lglyph-ascent glyph))
                                     des (max des (lglyph-descent glyph))
                                     )
                               ;; (message "%s" glyph)
                               )
                             text)
                       (setq w (* (/ font-size 1.0 ref-size) w)
                             asc (* (/ font-size 1.0 ref-size) asc)
                             des (* (/ font-size 1.0 ref-size) des))
                       ;; (message "%s %s" (dom-children node)
                       (list ox (- oy asc) (+ ox w) (+ oy des))))
              ((or 'rect 'image) (when (and (numberp (dom-attr node 'x))
                                (numberp (dom-attr node 'y))
                                (numberp (dom-attr node 'width))
                                (numberp (dom-attr node 'height)))
                       ;; Handle grid rectangle which doesn't have x,y
                       ;; (let (x1 y1 cx cy)
                       ;;   ;; If rotation is about (0,0), this will work.
                       ;;   (setq x1 (dom-attr node 'x)
                       ;;         y1 (dom-attr node 'y)
                       ;;         cx (/ (dom-attr node 'width) 2.0)
                       ;;         cy (/ (dom-attr node 'height) 2.0)
                       ;;         point (svg--apply-transform (+ x1 cx)
                       ;;                                     (+ y1 cy)
                       ;;                                     transform)
                       ;;         ox (- (car point) cx)
                       ;;         oy (- (cdr point) cy))
                       ;;   (list ox oy
                       ;;         (+ ox (dom-attr node 'width))
                       ;;         (+ oy (dom-attr node 'height))))
                       (setq ox (dom-attr node 'x)
                             oy (dom-attr node 'y))
                       (list ox oy
                             (+ ox (dom-attr node 'width))
                             (+ oy (dom-attr node 'height)))
                       ))
              ('ellipse (setq x1 (dom-attr node 'cx)
                              y1 (dom-attr node 'cy)
                              point (svg--apply-transform x1 y1 transform)
                              ox (car point)
                              oy (cdr point))
                        (list (- ox (dom-attr node 'rx))
                              (- oy (dom-attr node 'ry))
                              (+ ox (dom-attr node 'rx))
                              (+ oy (dom-attr node 'ry))))
              ((or 'polyline 'polygon)
               (let ((points (dom-attr node 'points))
                               point x y)
                           (mapc (lambda (a)
                                   (setq point (split-string a ",")
                                         x (string-to-number (car point))
                                         y (string-to-number (cadr point))
                                         x1 (if x1 (min x1 x) x)
                                         y1 (if y1 (min y1 y) y)
                                         x2 (if x2 (max x2 x) x)
                                         y2 (if y2 (max y2 y) y)))
                                 (split-string points))
                           (when x1
                             (list x1 y1 x2 y2))
                           ))
              ('path
               (let ((points (split-string (dom-attr node 'd)))
                     point x y pos cmd rx ry dx dy sweep mult skip)
                 (while points
                   (setq skip nil)
                   (when (and (string-match "[[:alpha:]]" (car points))
                              (> (length (car points)) 1))
                     (setq cmd (match-string 0 (car points))
                           points (cons (substring (car points) 1) (cdr points))))
                   (if (member (upcase (car points))
                               '("M" "L" "C" "S" "Q" "T" "A" "H" "V" "Z"))
                       (setq cmd (car points)
                             points (cdr points)))

                   (when (string= (upcase cmd) "C")
                     ;; Skip control points
                     (setq points (cddr points))
                     (setq point (split-string (car points) ",")
                           point (cons (string-to-number (car point))
                                       (string-to-number (cadr point)))))

                   (when (string= (upcase cmd) "A")
                     ;; Skip control points
                     (setq rx (string-to-number (nth 0 points))
                           ry (string-to-number (nth 1 points))
                           ;; large-arc (string-to-number (nth 3 points))
                           sweep (string-to-number (nth 4 points))
                           points (nthcdr 5 points)
                           point (split-string (car points) ",")
                           x (string-to-number (car point))
                           y (string-to-number (cadr point)))
                     (if (string= cmd "A")
                         (setq dx (- x (car pos))
                               dy (- y (cdr pos)))
                       (setq dx x dy y
                             x (+ (car pos) dx)
                             y (+ (cdr pos) dy)))
                     (setq mult (if (= sweep 1) (signum dy) 1)
                           pos (cons x y)
                           skip t
                           x (+ (* mult rx) x)
                           y (- (* mult ry) y)))

                   (when (member (upcase cmd)
                                 '("M" "L" "C" "S" "Q" "T"))
                     (setq point (split-string (car points) ","))
                     (if (= (length point) 2)
                         (setq point (cons (string-to-number (car point))
                                           (string-to-number (cadr point))))
                       (setq point (cons (string-to-number (car point))
                                         (string-to-number (car points)))
                             points (cdr points)))
                     (if (and pos
                              (member cmd '("m" "l" "c" "s" "q" "t")))
                         (progn
                           (setcar pos (+ (car pos) (car point)))
                           (setcdr pos (+ (cdr pos) (cdr point)))
                           )
                       (setq pos point)))

                   (when (member (upcase cmd) '("H" "V"))
                     (cond ((string= cmd "h")
                            (setcar pos (+ (car pos)
                                           (string-to-number (car points)))))
                           ((string= cmd "v")
                            (setcdr pos (+ (cdr pos)
                                           (string-to-number (car points)))))
                           ((string= cmd "H")
                            (setcar pos (string-to-number (car points))))
                           ((string= cmd "V")
                            (setcdr pos (string-to-number (car points))))))

                   ;; (message "%s" pos)
                   (unless skip
                   (setq x (car pos)
                         y (cdr pos)))
                   (setq
                         x1 (if x1 (min x1 x) x)
                         y1 (if y1 (min y1 y) y)
                         x2 (if x2 (max x2 x) x)
                         y2 (if y2 (max y2 y) y))
                   (setq points (cdr points)))
                 (when x1
                   (list x1 y1 x2 y2))
                 ))
              ('use (setq ref (or ref
                                  (if canvas--svg
                                      (car (dom-by-id
                                            (car canvas--svg)
                                            (concat
                                             (substring (dom-attr node 'href) 1)
                                             "$")))))
                          res (or (svg-bbox ref)
                                  '(0 0 0 0))
                          ox (or (dom-attr node 'x) 0)
                          oy (or (dom-attr node 'y) 0)
                          x1 (+ ox (nth 0 res))
                          y1 (+ oy (nth 1 res))
                          x2 (+ ox (nth 2 res))
                          y2 (+ oy (nth 3 res)))
                    (list x1 y1 x2 y2))
              ((or 'g 'svg 'symbol)
               (let (x y xx yy bbox flag)
                 (when (eq tag 'svg)
                   (setq ox (or (dom-attr node 'x) 0)
                         oy (or (dom-attr node 'y) 0)
                         xx (or (dom-attr node 'width) 0)
                         yy (or (dom-attr node 'height) 0)
                         x1 ox y1 oy
                         x2 (+ ox xx)
                         y2 (+ oy yy)
                         ))
                    (mapc (lambda (a)
                            (setq bbox (svg-bbox a))
                            ;; (message "%s" bbox)
                            (when bbox
                              (when (and (eq tag 'g) (null flag))
                                (setq x1 (nth 0 bbox)
                                      flag t
                                      y1 (nth 1 bbox)))
                                (setq x  (+ ox (nth 0 bbox))
                                      y  (+ oy (nth 1 bbox))
                                      xx (+ ox (nth 2 bbox))
                                      yy (+ oy (nth 3 bbox))
                                      x1 (min (or x1 ox) x xx)
                                      y1 (min (or y1 oy) y yy)
                                      x2 (max (or x2 ox) x xx)
                                      y2 (max (or y2 oy) y yy))))
                          (dom-children node))
                    (when x1
                      (list x1 y1 x2 y2))
                    ))
              ))

      ;; Apply transformation
      (unless (or (null res)
                  (null transform)
                  (memq tag '(circle line text)))
        (let (point1 point2 point3 point4)
          (setq x1 (nth 0 res)
                y1 (nth 1 res)
                x2 (nth 2 res)
                y2 (nth 3 res))

          (setq point1 (svg--apply-transform x1 y1 transform)
                point2 (svg--apply-transform x2 y2 transform)
                point3 (svg--apply-transform x1 y2 transform)
                point4 (svg--apply-transform x2 y1 transform))

          ;; (message "transform %s %s %s %s" point1 point2 point3 point4)
          (setq x1 (min (car point1) (car point2)
                        (car point3) (car point4))
                y1 (min (cdr point1) (cdr point2)
                        (cdr point3) (cdr point4))
                x2 (max (car point1) (car point2)
                        (car point3) (car point4))
                y2 (max (cdr point1) (cdr point2)
                        (cdr point3) (cdr point4)))
          (setq res (list x1 y1 x2 y2))
          ))
      res)))

(defun svg--apply-transform (x y transform)
  ;; Calculate offset due to transform
  (let* ((ox x)
         (oy y)
         (num "\\([+-]?[0-9]*[.]?[0-9]+\\(e[+-]?[0-9]*\\)*\\)")
         (sep ",? *")
         (mat (concat "matrix(" num sep num sep num
                      sep num sep num sep num ")"))
         (transl (concat "translate(" num "\\(" sep num "\\)*)"))
         (scale (concat "scale(" num "\\(" sep num "\\)?)"))
         (rot (concat "rotate(" num "\\(" sep num sep num "\\)?)"))
         (start 0)
         (tlist (and transform
                     (nreverse
                      (split-string (replace-regexp-in-string ")" ")|" transform)
                                    "|"))))
         deg cx cy ca sa)
    ;; Transformation matrices are applied in order. However, matrix
    ;; multiplication ends up "working" from right to left.
    (while tlist
      (setq transform (string-trim-left (pop tlist)))
      (cond
       ((string-match transl transform start)
        (setq ox (+ x (string-to-number (match-string 1 transform))))
        (if (match-string 3 transform)
          (setq oy (+ y (string-to-number (match-string 4 transform)))))
        )
       ((string-match scale transform start)
        (setq ox (* x (string-to-number (match-string 1 transform))))
        (if (match-string 4 transform)
            (setq oy (* y (string-to-number (match-string 4 transform)))))
        )
       ((string-match rot transform start)
        (setq deg (* (/ pi 180) (string-to-number (match-string 1 transform))))
        (if (match-string 3 transform)
          (setq cx (string-to-number (match-string 4 transform))
                cy (string-to-number (match-string 6 transform)))
          (setq cx 0 cy 0))
        (setq ca (cos deg) sa (sin deg))
        ;; rotate(45, 250, 250) =
        ;; matrix(0.707, 0.707, -0.707, 0.707, 250.000, -103.553)
        ;; (message "matrix(%.3f, %.3f, %.3f, %.3f, %.3f, %.3f)"
        ;;          (* ca ) (* sa) (* -1 sa) (* ca)
        ;;          (* (+ (* -1 ca cx) (* sa cy) cx))
        ;;          (* (+ (* -1 sa cx) (* -1 ca cy) cy)))
        (setq ox (+ (* ca x) (* -1 sa y)
                    (* (+ (* -1 ca cx) (* sa cy) cx))
                    ))
        (setq oy (+ (* sa x) (* ca y)
                    (* (+ (* -1 sa cx) (* -1 ca cy) cy))
                    ))
        )
       ((string-match mat transform start):
        ;; matrix(a,b,c,d,e,f)
        ;; x = a x (prevCoordSys) + c y (prevCoordSys) + e
        ;; y = b x (prevCoordSys) + d y (prevCoordSys) + f
        (let* ((a (string-to-number (match-string 1 transform)))
               (b (string-to-number (match-string 3 transform)))
               (c (string-to-number (match-string 5 transform)))
               (d (string-to-number (match-string 7 transform)))
               (e (string-to-number (match-string 9 transform)))
               (f (string-to-number (match-string 11 transform))))
          (setq ox (+ (* a x) (* c y) e))
          (setq oy (+ (* b x) (* d y) f))
          ))
       )
      (setq x ox y oy))
    (cons ox oy)))

(defun svg-init-pos (node)
  "Return initial position of node."
  (let ((tag (dom-tag node)))
    (pcase tag
      ('line (list (dom-attr node 'x1)
                   (dom-attr node 'y1)))
      ((or 'circle 'ellipse)
       (list (dom-attr node 'cx)
             (dom-attr node 'cy)))
      ('rect (list (dom-attr node 'x)
                   (dom-attr node 'y)))
      ('polyline
       (let* ((points (split-string (dom-attr node 'points)))
              point x y)
         (setq point (split-string (car points) ",")
               x (string-to-number (car point))
               y (string-to-number (cadr point)))
         (list x y)))
      )))

(defun svg--shapes-in-region (svg area)
  "Return a list of SVG nodes inside the AREA."
  (let (res bbox)
    (mapc (lambda (a)
            (setq bbox (svg-bbox a))
            ;; (message "%s %s %s" bbox area a)
            (if (and bbox
                     (>= (nth 0 bbox) (nth 0 area))
                     (>= (nth 1 bbox) (nth 1 area))
                     (<= (nth 2 bbox) (nth 2 area))
                     (<= (nth 3 bbox) (nth 3 area)))
                (push a res )))
          (dom-children svg))
    res))

(defun svg--point-in-region (x y area)
  "Return non NIL if (x . y) is inside the AREA."
  (and (>= x (nth 0 area))
       (>= y (nth 1 area))
       (<= x (nth 2 area))
       (<= y (nth 3 area))))

(defun svg-widget (svg frag &optional width height &rest args)
  "Create widget from SVG FRAG.
If FRAG is nil, use SVG."
  (let ((e (svg--extract-fragment svg frag width height nil args)))
    (if e
        (apply 'widget-create 'push-button :display (svg-image e) args)
    )))

(defun svg--extract-fragment (svg frag &optional width height explode &rest args)
  "Return an SVG image containing FRAG with view adjusted to
fragment's bounding box."
  ;; Skip comment tags
  (if (and frag (memq (dom-tag frag)
                      '(comment defs title metadata script style)))
      nil
      ;; svg is required for namespaces
      (let* ((width (or width 32))
             (height (or height 32))
             (stroke-width .1)
             (padding 10)
             bbox attr e ref defs
             x y bwidth bheight
             )
        (if frag
            (progn
              (when (eq (dom-tag frag) 'use)
                ;; Handle <use/> tag
                (setq defs (car (dom-by-tag svg 'defs))
                      ref (car (dom-by-id
                                svg (concat (substring (dom-attr frag 'href) 1)
                                            "$")))))
              (setq bbox (svg-bbox frag ref)
                    attr (copy-tree (dom-attributes svg))
                    e (if (and explode (eq (dom-tag frag) 'g))
                          (append `(svg ,attr) (append defs (dom-children frag)))
                        `(svg ,attr ,(append defs frag))
                        )))
          (setq e svg))

        (dom-set-attribute e 'width width)
        (dom-set-attribute e 'height height)
        (when (and frag bbox)
          (setq stroke-width (/ (- (nth 2 bbox) (nth 0 bbox)) 1.0 width))
          (setq stroke-width (if (> stroke-width 0) stroke-width 0.1))
          (setq bwidth (abs (- (nth 2 bbox) (nth 0 bbox)))
                padding (or (plist-get args :padding) padding)
                bheight (abs (- (nth 3 bbox) (nth 1 bbox))))
          (if (> bwidth bheight)
              (setq bheight bwidth)
            (setq bwidth bheight))
          (setq x (- (/ (+ (nth 0 bbox) (nth 2 bbox)) 2)
                     (/ bwidth 2) padding)
                y (- (/ (+ (nth 1 bbox) (nth 3 bbox)) 2)
                     (/ bheight 2) padding)
                bwidth (+ bwidth (* 2 padding))
                bheight (+ bheight (* 2 padding)))

          ;; Border
          (when (plist-get args :border)
            (svg-rectangle e x y bwidth bheight
                           :stroke (or (plist-get args :border) "red")
                           :stroke-width stroke-width
                           :fill (or (plist-get args :fill) "none")
                           :rx (* (or (plist-get args :rx) 0) stroke-width)
                           ))
          (dom-set-attribute e 'viewBox (format "%0.3f %0.3f %0.3f %0.3f"
                                                x y bwidth bheight
                                                )))
        ;; (message "%s\n%s %s" e bbox (dom-attr e 'viewBox))
        e)))

(defvar canvas-plugin-map nil
  "Plugin keymap.")
(defvar canvas-plugin-fn 'canvas--select-mode
  "Plugin function to substitute for canvas key handling.")
(defvar canvas-act-in-region-fn 'canvas--act-in-region
  "Plugin function to substitute for canvas action in a region handling.")
(defvar canvas--move-to-fn 'canvas--move-to
  "Plugin function to substitute for move animation.")
(defvar canvas-exit-fn nil
  "Plugin function to substitute for exit handling.")

(make-variable-buffer-local 'canvas-plugin-map)
(make-variable-buffer-local 'canvas-plugin-fn)
(make-variable-buffer-local 'canvas-act-in-region-fn)
(make-variable-buffer-local 'canvas--move-to-fn)
(make-variable-buffer-local 'canvas-exit-fn)

(defvar canvas--svg nil)
;; (make-variable-buffer-local 'canvas--svg)
(defvar canvas--mode nil
  "Object to draw.")
(defvar canvas--id nil
  "Identifier of the object being drawn.")
(defvar canvas--stroke-width nil
  "Stroke width.")
(defvar canvas--stroke-color nil
  "Stroke color.")
(defvar canvas--fill-color nil
  "Fill color.")
(defvar canvas--font-family nil
  "Font family.")
(defvar canvas--rotation nil
  "Rotation angle.")
(defvar canvas--text nil
  "List of params for drawing text.")
(defvar canvas--nearest-objects nil
  "Identifier of the object being selected.")
(defvar canvas--undo-marker nil
  "Identifier of the object being selected for undo.")
(defvar canvas--temp-points nil
  "Points while drawing connections.")
(defvar canvas--dialog-object nil
  "Current object in dialog.")
(defvar canvas--port-id nil
  "Current port id.")
(defvar canvas--port-name nil
  "Current port name.")
(defvar canvas--routing nil
  "Use 45 degree bends for routing e.g. in a PCB.")
(defvar canvas--ns-prefix nil
  "Current namespace prefix used for generating names.")
(defvar canvas--annotation nil
  "Status of annotation display.")
(defvar canvas--layers nil
  "ID of layers in the diagram.")
(defvar canvas--widget-size 100
  "Widget size.")
(defvar canvas--grid-size 10
  "Grid size in pixels for snapping.
Set to NIL to turn it off.")
(defvar canvas--defs nil
  "Canvas default definitions.")
(defvar canvas--defs-xml
  "
<g id=\"_defs\">
  <defs>
    <!-- arrowhead marker definition -->
    <marker id=\"arrow\" viewBox=\"0 0 10 10\" refX=\"5\" refY=\"5\"
        markerWidth=\"6\" markerHeight=\"6\"
        orient=\"auto-start-reverse\">
      <path d=\"M 0 0 L 10 5 L 0 10 z\" />
    </marker>

    <!-- simple dot marker definition -->
    <marker id=\"dot\" viewBox=\"0 0 10 10\" refX=\"5\" refY=\"5\"
        markerWidth=\"10\" markerHeight=\"10\" markerUnits=\"userSpaceOnUse\">
      <circle cx=\"5\" cy=\"5\" r=\"5\" fill=\"red\" />
    </marker>
    <marker id=\"dot01\" viewBox=\"0 0 10 10\" refX=\"5\" refY=\"5\"
        markerWidth=\"5\" markerHeight=\"5\" markerUnits=\"userSpaceOnUse\">
      <circle cx=\"5\" cy=\"5\" r=\"5\" fill=\"red\" />
    </marker>
    <marker id=\"dot001\" viewBox=\"0 0 10 10\" refX=\"5\" refY=\"5\"
        markerWidth=\"2\" markerHeight=\"2\" markerUnits=\"userSpaceOnUse\">
      <circle cx=\"5\" cy=\"5\" r=\"5\" fill=\"red\" />
    </marker>

    <!-- grid pattern -->
    <pattern id=\"x5Grid\" viewBox=\"0 0 10 10\" width=\"5\" height=\"5\"
        patternUnits=\"userSpaceOnUse\">
      <path d=\"M 10 0 L 0 0 0 10\" fill=\"none\" stroke=\"blue\" stroke-width=\"0.1\"/>
    </pattern>
    <pattern id=\"x10Grid\" viewBox=\"0 0 10 10\" width=\"10\" height=\"10\"
        patternUnits=\"userSpaceOnUse\">
      <path d=\"M 10 0 L 0 0 0 10\" fill=\"none\" stroke=\"red\" stroke-width=\"0.1\"/>
    </pattern>
    <pattern id=\"x20Grid\" viewBox=\"0 0 10 10\" width=\"20\" height=\"20\"
        patternUnits=\"userSpaceOnUse\">
      <path d=\"M 10 0 L 0 0 0 10\" fill=\"none\" stroke=\"gray\" stroke-width=\"0.1\"/>
    </pattern>
    <pattern id=\"grid\" width=\"100\" height=\"100\" patternUnits=\"userSpaceOnUse\">
      <rect width=\"100\" height=\"100\" fill=\"url(#x20Grid)\"/>
      <path d=\"M 100 0 L 0 0 0 100\" fill=\"none\" stroke=\"gray\" stroke-width=\"1\"/>
    </pattern>
  </defs>

  <rect id=\"_grid\" width=\"%d\" height=\"%d\" fill=\"url(#grid)\" />
</g>
"
  "XML string for definitions.")
(defvar canvas--outline-style 'default
  "Use solid or default (dashes) style.")
(make-variable-buffer-local 'canvas--outline-style)

(defvar canvas--outline-marker "#dot"
  "Use a different outline marker for different zoom levels.")
(defvar canvas--init-bbox nil
  "Restore zoom to initial content bbox.")
(defvar canvas--sym-lib nil
  "Currently loaded symbol library.")
(defvar canvas--color-picker nil
  "Color picker.")
(defvar canvas--color-picker-xml
  "
<g id=\"color-picker\" width=\"256\" height=\"300\" >
  <defs>
    <linearGradient id=\"saturation\" gradientTransform=\"rotate(0)\" >
      <stop offset=\"0\"  stop-color=\"#000000\" />
      <stop offset=\"50%\" stop-color=\"#00ff00\" />
      <stop offset=\"100%\" stop-color=\"#ffffff\" />
    </linearGradient>
    <linearGradient id=\"hue\" gradientTransform=\"rotate(0)\" >
      <stop offset=\"0\"  stop-color=\"#f00\" />
      <stop offset=\"16.66%\"  stop-color=\"#ff0\" />
      <stop offset=\"33.33%\" stop-color=\"#0f0\" />
      <stop offset=\"50%\"  stop-color=\"#0ff\" />
      <stop offset=\"66.66%\" stop-color=\"#00f\" />
      <stop offset=\"83.33%\"  stop-color=\"#f0f\" />
      <stop offset=\"100%\" stop-color=\"#f00\" />
    </linearGradient>
  </defs>
  <rect x=\"0\" y=\"0\" width=\"256\" height=\"256\" fill=\"url('#saturation')\" />
  <rect x=\"0\" y=\"270\" width=\"256\" height=\"25\" fill=\"url('#hue')\" />
</g>
"
  "XML String for color picker.")
(defvar canvas--hide-prompt nil
  "If non-nil, don't display a prompt")

(defun canvas--pick-color ()
  "Draw color picker widget."
  (if canvas--color-picker
      canvas--color-picker
    (with-temp-buffer
      (insert canvas--color-picker-xml)
      (setq canvas--color-picker-xml
            (libxml-parse-xml-region (point-min) (point-max))))
    ))

(defun svg--scrub-image (svg)
  "Convert some important attributes to number.
For bbox calculations, these must be numbers."
  ;; Handle XML PI
  (when (and svg (eq (dom-tag svg) 'top))
    (setq svg (car (dom-by-tag svg 'svg))))
  (dolist (a (dom-attributes svg))
    (if (and (memq (car a) '(x y width height x1 y1 x2 y2 cx cy r rx ry))
             (stringp (cdr a))
             (not (string-match-p "%" (cdr a))))
        (setcdr a (string-to-number (cdr a))))
    )
  (dolist (node (dom-children svg))
    (unless (stringp node)
      (svg--scrub-image node)))
  svg)

(defun canvas-mode-on-click (event)
  (interactive "e")
  (let* ((e (event-start event))
         (pos (posn-point e))
         (win (posn-window e))
         (image (posn-image e)))
    ;; (message "%s" e)
    (select-window win)
    (when image
      (save-excursion
        (goto-char pos)
        (setq pos (point-marker)))
      (canvas-mode nil image pos))
    ))

;;;###autoload
(defun canvas-mode (&optional prefix svg point)
  "Canvas mode.
Use PREFIX to override width and height values."
  (interactive "P")
  ;; Allow multiple images in the document.
  (let* ((image (or svg (get-text-property (point) 'display)))
         (marker (or point (point-marker)))
         (buf (or (if point (marker-buffer point)) (current-buffer)))
        (file (plist-get (cdr image) :file))
        (width 400)
        (height 400))
    (local-set-key [down-mouse-1] 'canvas-mode-on-click)
    (if prefix
        (setq file (car (find-file-read-args "Find file: "
                                             (confirm-nonexistent-file-or-buffer)))
              width (read-number "Width: " (window-pixel-width))
              height (read-number "Height: " (window-pixel-height))))

    (if (and file (not (file-directory-p file)))
        (progn
          (with-current-buffer (find-file-noselect file)
            (setq image (libxml-parse-xml-region (point-min) (point-max))))
          ;; Handle XML PI
          (when (and image (eq (dom-tag image) 'top))
            (setq image (car (dom-by-tag image 'svg))))
          (when image
            (svg--scrub-image image)
            (dom-set-attribute image 'width width)
            (dom-set-attribute image 'height height)
            (dom-set-attribute image :image marker)
            (with-current-buffer buf (svg-insert-image image))))

    (if (eq 'svg (plist-get (cdr image) :type))
        (with-temp-buffer
          (insert (plist-get (cdr image) :data))
          (setq image (libxml-parse-xml-region (point-min) (point-max)))
          (svg--scrub-image image)
          (dom-set-attribute image :image marker))
      (setq image (svg-create width height :stroke-width 1))
      (with-current-buffer buf (svg-insert-image image))))

    (setq canvas--svg nil)
    (canvas--set image marker)
    (ewp-crop-image-1 image)
    ))

(defun canvas--set (svg marker)
  "Set SVG into canvas for current display.
MARKER identifies the image uniquely."
  (let ((width (dom-attr svg 'width))
        (height (dom-attr svg 'height))
        (image svg)
        )
    ;; (svg--scrub-image image)
    (dom-set-attribute image 'width width)
    (dom-set-attribute image 'height height)
    (dom-set-attribute image :image marker)

    (unless canvas--defs
      (setq canvas--defs
            (with-temp-buffer
              (insert (format canvas--defs-xml width height))
              (libxml-parse-xml-region (point-min) (point-max)))))
    (unless (dom-by-id image "_defs")
      ;; Keep it at the top so that undo works fine for deleting
      ;; visible elements
      (dom-add-child-before image canvas--defs)
      (svg-possibly-update-image image))
    ;; Add canvas namespace
    (unless (dom-attr image 'xmlns:canvas)
      (dom-set-attribute image 'xmlns:canvas
                         "http://www.gnu.org/canvas"))

    (setq canvas--mode 'polyline
          canvas--undo-marker nil
          canvas--stroke-color (or canvas--stroke-color
                                   (foreground-color-at-point))
          canvas--fill-color (or canvas--fill-color "none")
          canvas--outline-marker "#dot"
          canvas--rotation 90
          canvas--init-bbox (or (svg-bbox image)
                                (list 0 0 width height)))

    (mapc (lambda (a)
            (setq canvas--id (max (string-to-number (or (dom-attr a 'id) ""))
                                  (or canvas--id 0))))
          (dom-children image))
    (setq canvas--id (1+ canvas--id))

    (push image canvas--svg)
    ))

(defun canvas--generate-names (xml attr &optional prefix)
  "Generate names by adding numbered suffix to type."
  (let* ((attr (if prefix (concat prefix ":" attr) attr))
         (attr-type (if attr (intern attr)))
         (attr-name (intern (if prefix (concat prefix ":name") "name")))
         name num i e)
    (dolist (node (dom-children xml))
      (setq name (dom-attr node attr-type))
      (when name
        (setq e (assoc name num)
              i (or (cdr e) 0)
              i (1+ i))
        (if e
            (setcdr e i)
          (push (cons name i) num))
        (dom-set-attribute node attr-name (format "%s%d" (upcase name) i))
        ))
    ;; (svg-print xml)
    ))

;;;###autoload
(defun canvas-symbol-lib (&optional file group)
  "Load symbol library."
  (interactive "fFile: \nP")
  (let ((i canvas--widget-size)
        (b (get-buffer-create "*Symbols*"))
        image groups children e)
    (with-current-buffer (find-file-noselect file)
      (setq image (libxml-parse-xml-region (point-min) (point-max))))
    ;; Handle XML PI
    (when (and image (eq (dom-tag image) 'top))
      (setq image (car (dom-by-tag image 'svg))))
    (svg--scrub-image image)
    (setq canvas--sym-lib file)

    ;; Collect groups and allow selection via minibuffer
    (setq children (dom-children image))
    (when group ;(> (length children) 50)
      (dolist (el children)
        (setq group (substring (dom-attr el 'id) 0 3)
              e (assoc group groups)
              children (cdr e))
        (push el children)
        (if e
            (setcdr e children)
          (push (cons group children) groups)))
      (setq group (completing-read "Select group: " (mapcar #'car groups))
            children (cdr (assoc group groups))))

    (with-current-buffer b
      ;; (setq inhibit-read-only nil)
      (erase-buffer)
      (dolist (el children)
        (svg-widget image el i i
                    :help-echo (dom-attr el 'id)
                    :value (dom-attr el 'id)
                    :action (lambda (widget &optional _s)
                              (setq canvas--undo-marker
                                    (svg-load-from-xml
                                     (image-property
                                      (widget-get widget :display)
                                      :data)))
                              ;; (message "%s" (widget-value widget))
                              (if (> (recursion-depth) 0)
                                  (exit-recursive-edit))
                              )
                    :tag (dom-attr el 'id)))
      ;; (widget-setup)
      (goto-char (point-min))
      )
    (pop-to-buffer b t t)
    image))

(defun canvas--annotate (svg node)
  "Add annotation for name, type and value using namespace.
Name is usually optional since it can be generted by numbering the type."
  (let* ((attrs (dom-attributes svg))
         ns prefix name value attr)
    (mapc (lambda (a)
            (if (string-match-p "xmlns" (symbol-name (car a)))
                (push (cdr a) ns)))
          attrs)

    (setq ns (completing-read "Namespace: " ns))
    (if (setq attr (rassoc ns attrs))
        (setq prefix (car (last (split-string (symbol-name (car attr)) ":"))))
      (setq prefix (read-string "Prefix: "))
      (if (string-empty-p prefix)
          (setq prefix nil)
        (setq canvas--ns-prefix prefix)
        (dom-set-attribute svg (intern (concat "xmlns:" prefix)) ns)))

    (setq name (read-string "Attribute Name: ")
          value (read-string "Attribute Value: "))
    ;; (message "%s" (rassoc ns attrs))
    (unless (string-empty-p
             (setq name (concat (if prefix (concat prefix ":")) name)))
        (dom-set-attribute node (intern name) value))
    ))

(defun canvas--show-annotation (svg &optional node)
  "Show annotations for name and value."
  (let* ((all-attrs (dom-attributes node))
         x1 y1 x2 y2 w h bbox name value attrs len1 len2 font-size id)
    (when node
      (mapc (lambda (a)
              (setq name (symbol-name (car a)))
              (if (string-match "\\(name\\|value\\)" name)
                  (push (cons (match-string 1 name) (cdr a)) attrs)))
            all-attrs)
      ;; (message "%s" attrs)
      (when attrs
        (setq bbox (svg-bbox node)
              font-size 8
              x1 (nth 0 bbox)
              y1 (nth 1 bbox)
              x2 (nth 2 bbox)
              y2 (nth 3 bbox)
              id (or (dom-attr node 'id) 0)
              name (cdr (assoc "name" attrs))
              value (cdr (assoc "value" attrs))
              len1 (* font-size (length name) 0.5)
              len2 (* font-size (length value) 0.5)
              w (/ (- x2 x1) 2)
              h (/ (- y2 y1) 2))
        (if (> w h)
            ;; Place on top and bottom
            (progn
              (if name
                  (svg-text svg name :id (format "_anno_a_%s_%d" id 0)
                            :x (+ x1 w (- len1)) :y (- y1 font-size)))
              (if value
                  (svg-text svg value :id (format "_anno_a_%s_%d" id 1)
                            :x (+ x1 w (- len2)) :y (+ y2 (* 2 font-size)))))
          ;; Place on right
          (if name
              (svg-text svg name :id (format "_anno_a_%s_%d" id 0)
                        :x (+ x2 font-size) :y (+ y1 font-size)))
          (if value
              (svg-text svg value :id (format "_anno_a_%s_%d" id 1)
                        :x (+ x2 font-size) :y y2))
          )
        ))

    (unless node
      (mapc (lambda (a)
              (canvas--show-annotation svg a))
            (dom-children svg))
      (svg-possibly-update-image svg))
    ))

(defun canvas--hide-annotation (svg)
  "Set display:none to all annotation nodes."
  (mapc (lambda (a)
          (dom-set-attribute a 'display "none"))
        (dom-by-id svg "_anno"))
  (svg-possibly-update-image svg))

(defun canvas--select-mode (key svg)
  (let ()
    (unless (or (memq key '(?u ?m ?g ?G ?R ?\M-w ?> escape ?\C-x ?A))
                ;; Ignore mouse movement
                (listp key))
      ;; All operations which don't use canvas--undo-marker should
      ;; deselect objects
      (canvas--deselect))

    (setq canvas--mode
          (pcase key
            ;; Objects
            (?l 'line)
            (?c 'circle)
            (?r 'rect)
            (?e 'ellipse)
            (?W (setq canvas--temp-points nil)
                'conn)
            (?t (let ((str (read-string "Text: " nil))
                      node size font)
                  (when str
                    (setq size (read-number "Size: " 15)
                          ;; font (completing-read "Font family: "
                                                ;; (font-family-list))
                          node (svg-text svg str
                                         :font-size size
                                         :font-family
                                         (unless (string-empty-p font) font)
                                         :id (number-to-string canvas--id)
                                         :stroke canvas--stroke-color)
                          canvas--id (1+ canvas--id))
                    (canvas--select (list node))
                    (canvas--move-init node)
                    'move)))
            (?p 'polyline)
            (?P (setq canvas--temp-points nil)
                'path)

            ;; Object properties
            (?C (setq canvas--stroke-color
                      (read-string "Stroke color: " (foreground-color-at-point)))
                canvas--mode)
            (?w (setq canvas--stroke-width (read-number "Stroke width: " 1))
                canvas--mode)
            (?f (setq canvas--fill-color
                      (read-string "Fill color: " (foreground-color-at-point)))
                canvas--mode)
            (?F (setq canvas--font-family
                      (completing-read "Font family: " (font-family-list)))
                canvas--mode)
            (?A (when canvas--undo-marker
                  ;; (message "%s" canvas--undo-marker)
                  (canvas--annotate svg (car canvas--undo-marker))
                  (setq canvas--annotation t)
                  (canvas--show-annotation svg (car canvas--undo-marker))
                  (canvas--deselect)
                  (svg-possibly-update-image svg))
                canvas--mode)
            (?n (setq canvas--ns-prefix
                      (read-string "Prefix: " canvas--ns-prefix))
                canvas--mode)

            ;; Object operations
            (?u (canvas--undo svg))
            ('escape (when (eq canvas--mode 'move)
                       (canvas--move-cancel))
                     (when (eq canvas--mode 'place)
                       (canvas--undo svg))
                     (when (eq canvas--mode 'path)
                       (canvas--path svg canvas--temp-points
                                     (number-to-string canvas--id))
                       (setq canvas--temp-points nil
                             canvas--id (1+ canvas--id)))
                     (when (and (eq canvas--mode 'conn)
                                canvas--temp-points)
                       (svg-polyline svg canvas--temp-points
                                     :stroke-width canvas--stroke-width
                                     :id (number-to-string canvas--id)
                                     :stroke-color (foreground-color-at-point)
                                     :fill "none")
                       (setq canvas--temp-points nil
                             canvas--id (1+ canvas--id)))
                     (canvas--deselect)
                     (svg-possibly-update-image svg)
                     nil)
            (?m (when canvas--undo-marker
                  (mapc #'canvas--move-init canvas--undo-marker)
                  'move))
            (?\M-g (let* ((node (car (dom-by-id svg "_grid")))
                          (attr (dom-attr node 'display)))
                     ;; Toggle grid
                     (dom-set-attribute node 'display
                                        (if attr nil "none"))
                     (svg-possibly-update-image svg))
                   canvas--mode)
            (?\M-w (let ((objs (copy-tree canvas--undo-marker)))
                     ;; Change ids and append to svg
                     (mapc (lambda (a)
                             (dom-set-attribute a 'id
                                                (number-to-string canvas--id))
                             (setq canvas--id (1+ canvas--id)))
                           objs)
                     (nconc svg objs)
                     (canvas--deselect)
                     (svg-possibly-update-image svg)
                     (setq canvas--undo-marker objs))
                   'move)
            (?d (setq canvas--rotation
                      (read-number "Degrees: " 90))
                nil)
            (?> ;; Enter define-port. If currently on a group, start a
             ;; dialog with the children of the group.
             (let* ((width (dom-attr svg 'width))
                    (height (dom-attr svg 'height))
                    (marker (dom-attr svg :image))
                    (frag (car canvas--undo-marker))
                    (image nil))
               (when (and frag (eq (dom-tag frag) 'g))
                 (dom-set-attribute frag 'stroke-dasharray nil)
                 (setq image (svg--extract-fragment svg frag width height)
                       canvas--dialog-object frag)
                 (canvas--set image marker)
                 (setq canvas--port-id (or (length (dom-by-id svg "_port")) 0))
                 'define-port)))
            (?D (when (eq canvas--mode 'define-port)
                  (setq canvas--port-id (1+ canvas--port-id)
                        canvas--port-id (read-number "Number: "
                                                     canvas--port-id)
                        canvas--port-name (read-string "Name: "))
                  )
                canvas--mode)
            (?< ;; Exit define-port
             (when (eq canvas--mode 'define-port)
               (let ((image (pop canvas--svg))
                     str x y nodes)
                 (setq str (mapconcat (lambda (a)
                                        (setq x (dom-attr a 'cx)
                                              ;; name (dom-attr a 'name)
                                              y (dom-attr a 'cy))
                                        (format "%s,%s" x y))
                                      (dom-by-id image "_port")
                                      " ")
                       nodes (append (dom-by-id image "_port")
                                     (dom-by-id image "_name")))

                 ;; Add canvas namespace and canvas:ports attribute
                 (unless (dom-attr (car canvas--svg) 'xmlns:canvas)
                   (dom-set-attribute (car canvas--svg) 'xmlns:canvas
                                      "http://www.gnu.org/canvas"))
                 (dom-set-attribute canvas--dialog-object 'canvas:ports str)
                 (nconc canvas--dialog-object nodes)
                 (svg-possibly-update-image (car canvas--svg))
                 ;; (message "define-port1: %s %s" nodes canvas--dialog-object)
                 (setq canvas--dialog-object nil)
                 ))
             nil)
            (?\M-r (canvas--generate-names svg "type" canvas--ns-prefix))
            (?R (when (and canvas--undo-marker
                           (= (length canvas--undo-marker) 1))
                  ;; In-place rotation of multiple objects is not desirable.
                  ;; For rotating multiple objects, use a group.
                  (mapc (lambda (a)
                          (let* ((bbox (svg-bbox a))
                                 (angle (% (+ (or (dom-attr a :angle) 0)
                                              canvas--rotation)
                                           360))
                                 (x (nth 0 bbox))
                                 (y (nth 1 bbox)))
                            (dom-set-attribute a :angle
                                               (if (/= angle 0) angle))
                            (canvas--move-init a)
                            (canvas--move-to a x y)
                            ))
                        canvas--undo-marker)
                  (if canvas--annotation
                      (canvas--show-annotation svg (car canvas--undo-marker)))

                  (canvas--highlight-obj svg canvas--undo-marker)
                  (svg-possibly-update-image svg))
                  canvas--mode)
            (?g (canvas--group svg canvas--undo-marker)
                nil)
            (?G (canvas--ungroup svg canvas--undo-marker)
                nil)

            ;; File operations
            (?D (let (width height)
                  (setq width (read-number "Width: " width)
                        height (read-number "Height: " height))
                  (dom-set-attribute svg 'width width)
                  (dom-set-attribute svg 'height height)
                  (svg-possibly-update-image svg))
                canvas--mode)
            (?O (let* ((file (car (find-file-read-args
                                   "Find file: "
                                   (confirm-nonexistent-file-or-buffer))))
                       (image (create-image file))
                       (size (image-size image t)))
                  (svg-embed svg file
                             (format "image/%s" (image-type file)) nil
                             :id (number-to-string canvas--id)
                             :x 0 :y 0
                             :width (car size)
                             :height (cdr size)
                             )
                  (setq canvas--id (1+ canvas--id))
                  canvas--mode))
            (?s (with-temp-buffer
                  (canvas--remove-marker svg)
                  (svg-print svg)
                  (write-file nil))
                ;; Save file
                canvas--mode)

            ;; Tools
            (?x (setq canvas--stroke-color "white"
                      canvas--stroke-width 5)
                'erase)
            (?X 'crop)
            ((or ?z ?0 ?+ ?-)
             (let* ((viewBox nil)
                    factor width height x y bbox w)
               (setq x 0 y 0
                     width (dom-attr svg 'width)
                     height (dom-attr svg 'height))
               (when (eq key ?z)
                 (setq canvas--mode 'zoom))
               (when (eq key ?0)
                 ;; Use the larger bbox
                 (setq bbox (svg-bbox svg)
                       width (nth 2 bbox)
                       w (nth 2 canvas--init-bbox))
                 (canvas--zoom svg (if (or (null bbox) (> w width))
                                    canvas--init-bbox
                                  bbox)))

               (when (memq key '(?+ ?-))
                 (setq factor (pcase key
                                (?+ 0.1)
                                (?- -0.1)))
                 (setq viewBox (svg-parse-viewBox svg)
                       x       (nth 0 viewBox)
                       y       (nth 1 viewBox)
                       width   (nth 2 viewBox)
                       height  (nth 3 viewBox))

                 ;; Reduce the area by factor
                 (canvas--zoom svg (list (+ x (* 0.5 factor width))
                                      (+ y (* 0.5 factor height))
                                      (+ x (* (- 1 (* 0.5 factor)) width))
                                      (+ y (* (- 1 (* 0.5 factor)) height)))
                 ))
               canvas--mode))
            (?o ;; Layers
             ;; (message "%s" canvas--layers)
             (when canvas--layers
               (let* ((layers canvas--layers)
                      node)
                 ;; Use comma to separate inputs
                 (setq layers (completing-read-multiple "Select Layers to show:"
                                                        layers))
                 (mapc (lambda (a)
                         (setq node (car (dom-by-id svg a)))
                         (if (member a layers)
                             (dom-set-attribute node 'display nil)
                           (dom-set-attribute node 'display "none"))
                         ;; (message "%s" (dom-attr node 'display))
                         )
                       canvas--layers)
                 (svg-possibly-update-image svg)
               ))
             nil)
            ((or 'up 'down 'right 'left)
             ;; Pan view
             (let* ((viewBox nil)
                    width height x y)
               (setq viewBox (svg-parse-viewBox svg)
                     x       (nth 0 viewBox)
                     y       (nth 1 viewBox)
                     width   (nth 2 viewBox)
                     height  (nth 3 viewBox))

               (pcase key
                 ('left (setq x (- x 10)))
                 ('right (setq x (+ x 10)))
                 ('up (setq y (- y 10)))
                 ('down (setq y (+ y 10))))
               (canvas--zoom svg (list x y (+ x width) (+ y height)))
               ;; (message "%s %s %s" key x y)
               canvas--mode))
            (?L (if canvas--sym-lib
                    (canvas-symbol-lib canvas--sym-lib)
                  (call-interactively 'canvas-symbol-lib))
                (recursive-edit)
                (switch-to-buffer (current-buffer))

                (let* ((node canvas--undo-marker)
                       (viewBox (svg-bbox node))
                       (x (nth 0 viewBox))
                       (y (nth 1 viewBox))
                       (attrs (dom-attributes svg)))
                  ;; Merge namespaces in main SVG
                  (dolist (attr (dom-attributes node))
                    ;; (message "%s %s" attr (eq (car attr) 'viewBox))
                    ;; (if (eq (car attr) 'viewBox)
                    ;;     (setq viewBox (cdr attr)))
                    (if (and (string-match-p "xmlns:" (symbol-name (car attr)))
                             (not (assoc (car attr) attrs)))
                      (dom-set-attribute svg (car attr) (cdr attr))))

                  ;; Set transform, width and viewbox
                  ;; (when (string-match "\\([0-9.-]*\\) \\([0-9.-]*\\)" viewBox)
                  ;;   (setq x (- (string-to-number (match-string 1 viewBox)))
                  ;;         y (- (string-to-number (match-string 2 viewBox)))))
                  (setq viewBox (format "%s %s %s %s" x y
                                        (- (nth 2 viewBox) (nth 0 viewBox))
                                        (- (nth 3 viewBox) (nth 1 viewBox))))
                  ;; (message "%s " viewBox)
                  (setq attrs `((width . ,canvas--widget-size)
                                (viewBox . ,viewBox)
                                ;; (:transformed . t)
                                ;; (transform . ,(format "translate(%s, %s)"
                                ;;                       (- x) (- y)))
                                ))
                  (setq node (list (dom-node 'g attrs
                                             (car (dom-children node)))))

                  ;; Append to current image
                  (nconc svg node)
                  (setq canvas--undo-marker nil)
                  (canvas--select node)
                  (canvas--move-init node))
                'place)
            (?\M-a (if canvas--annotation
                       (progn
                         (setq canvas--annotation nil)
                         (canvas--hide-annotation svg))
                     (setq canvas--annotation t)
                     (canvas--show-annotation svg))
                   canvas--mode)
            (?\C-x (unless (or (mouse-movement-p key)
                               (null canvas-plugin-map))
                     (setq key (read-key "C-x"))
                     (let ((mode (lookup-key canvas-plugin-map (vector key))))
                       (if mode
                           (funcall (nth 1 mode) svg)
                         (setq canvas--mode nil))
                       (message "%s %s %s" key mode canvas--mode)
                       ))
                   canvas--mode)
            ;; Retain mode even if mouse moves outside the image
            (_ canvas--mode)
            ))
    key))

(defun canvas--remove-marker (svg)
  ;; Remove grid and markers
  (mapc (lambda (a)
          (dom-remove-node svg a))
        (dom-by-id svg "_marker"))
  )

(defun canvas--move-init (node)
  "Rotation changes the left corner of bounding box. Call this
function after every rotation or before move to update the information in
`canvas--temp-points'."
  (let* ((bbox (svg-bbox node))
         (str nil)
         (angle (dom-attr node :angle))
         (x (/ (+ (nth 0 bbox) (nth 2 bbox)) 2))
         (y (/ (+ (nth 1 bbox) (nth 3 bbox)) 2)))
    ;; Save original position
    (canvas--apply-transform (list node) t)
    ;; (message "%s" angle)

    ;; First rotate and find bbox.
    (when angle
      (setq str (format "rotate(%s, %s, %s)"
                        angle x y
                        )))
    (dom-set-attribute node 'transform str)
    ;; Use new bbox corner for translation.
    (setq bbox (svg-bbox node))

    ;; Use this offset to move the object to cursor position.
    ;; Left top corner for translation and center for rotation
    (setq canvas--temp-points (cons (cons (nth 0 bbox) (nth 1 bbox))
                                    (cons x y)))
    ))

(defun canvas--move-to (node x y)
  (let* ((angle (dom-attr node :angle))
         (off (car canvas--temp-points))
         (center (cdr canvas--temp-points))
         (str (format "translate(%s, %s)"
                      (- x (car off)) (- y (cdr off))
                      )))
    (if angle
        (setq str (concat str (format " rotate(%s, %s, %s)"
                                      angle (car center) (cdr center)
                                      ))))
    (dom-set-attribute node 'transform str)
    ))

(defun canvas--move-cancel ()
  "Restore status before move."
  (mapc (lambda (a)
          (dom-set-attribute a 'transform
                             (dom-attr a :init)))
        canvas--undo-marker))

(defun canvas--select (nodes)
  (mapc (lambda (a)
          (unless (and (dom-attr a 'id)
                       (string-match-p "^_[ms]" (dom-attr a 'id)))
            (push a canvas--undo-marker)
            (dom-set-attribute a 'stroke-dasharray "10,10")))
        nodes))

(defun canvas--deselect ()
  (mapc (lambda (a)
          (dom-set-attribute a 'stroke-dasharray nil))
        canvas--undo-marker)
  ;; (message "%s" canvas--undo-marker)
  (setq canvas--undo-marker nil)
  )

(defun canvas--group (svg objects)
  (when (> (length objects) 1)
    (canvas--deselect)
    (mapc (lambda (node)
            (dom-remove-node svg node))
          objects)
    (dom-append-child svg
                      (nconc (dom-node
                              'g
                              `((id . ,(number-to-string canvas--id))))
                             objects))
    (setq canvas--id (1+ canvas--id))
    (svg-possibly-update-image svg)
    ))

(defun canvas--ungroup (svg objects)
  (mapc (lambda (node)
          (when (eq (dom-tag node) 'g)
            (dom-remove-node svg node)
            (let ((attr (dom-attr node 'transform)))
              (if attr
                  (dolist (a (dom-children node))
                    (dom-set-attribute a 'transform attr))))
            (nconc svg (dom-children node))
            ))
        objects)
  (svg-possibly-update-image svg))

(defun canvas--undo (svg)
  (unless canvas--undo-marker
    (setq canvas--undo-marker (last (dom-children svg))))
  (nconc canvas--undo-marker (dom-by-id svg "_marker"))
  ;; (message "%s" canvas--undo-marker)
  (mapc (lambda (a)
          (unless (member (dom-attr a 'id)
                          '("_defs"))
            (dom-remove-node svg a)))
        canvas--undo-marker)
  (setq canvas--undo-marker nil)
  ;; Undo: Delete node using current canvas--undo-marker.
  ;; Unset mode
  (svg-possibly-update-image svg))

(defun canvas--transform (svg area)
  "Transform co-ordinates according to view box.
Returns (LIST X Y W H SCALE-X SCALE-Y)."
  (let* ((viewBox nil)
        ;; x, y, w and h represents the drawing box
        (w (- (getf area :right) (getf area :left)))
        (h (- (getf area :bottom) (getf area :top)))
        (x (getf area :left))
        (y (getf area :top))
        x1 y1 ;; ViewBox co-ordinates
        ;; Variables for scaling
        width1 height1
        width height scale-x scale-y
        )

    (setq x1 0 y1 0
          width (dom-attr svg 'width)
          height (dom-attr svg 'height))
    (setq viewBox (svg-parse-viewBox svg)
          x1       (nth 0 viewBox)
          y1       (nth 1 viewBox)
          width1   (nth 2 viewBox)
          height1  (nth 3 viewBox))

    (assert (and (> height 0) (> width 0)))
    ;; Scale dimensions
    (setq scale-x (/ width1 1.0 width)
          scale-y (/ height1 1.0 height)
          x (+ x1 (* x scale-x))
          y (+ y1 (* y scale-y))
          w (* w scale-x)
          h (* h scale-y))
    (list x y w h scale-x scale-y)))

(defun canvas--apply-transform (nodes &optional force)
  "Apply linear transformation to NODES."
  ;; While moving objects, we want to start at current position of the
  ;; object AFTER applying existing transformation. This function
  ;; helps in that.
  (mapc (lambda (a)
          (let ((transformed (dom-attr a :transformed)))
            (when (or transformed force)
              ;; Transformation is not necessarily linear only.
              ;; Hence, save transform values.
              (dom-set-attribute a :init (dom-attr a 'transform))
              (dom-set-attribute a :transformed nil)
              ))
          ;; (if (dom-attr a 'transform)
          ;;     (let ((bbox (svg-bbox a))
          ;;           x y)
          ;;       (pcase (dom-tag a)
          ;;         ('line (dom-set-attribute a 'x1 (nth 0 bbox))
          ;;                (dom-set-attribute a 'y1 (nth 1 bbox))
          ;;                (dom-set-attribute a 'x2 (nth 2 bbox))
          ;;                (dom-set-attribute a 'y2 (nth 3 bbox)))
          ;;         ('rect (dom-set-attribute a 'x (nth 0 bbox))
          ;;                (dom-set-attribute a 'y (nth 1 bbox)))
          ;;         ((or 'circle ellipse)
          ;;          (setq x (/ (+ (nth 0 bbox) (nth 2 bbox)) 2)
          ;;                y (/ (+ (nth 1 bbox) (nth 3 bbox)) 2))
          ;;          (dom-set-attribute a 'cx x)
          ;;          (dom-set-attribute a 'cy y))
          ;;         )
          ;;       (dom-set-attribute a 'transform nil)
          ;;       ))
          )
        nodes))

(defun canvas--adjacent-obj (svg x y)
  "Highlight the object closest to point (X, Y).
Returns a point on the object closer than `limit' if it exists;
else NIL."
  (let ((tags (dom-children svg))
        (flag t)
        (limit 10)
        (closest 10)
        bbox x1 y1 x2 y2 id tag dist points
        closest-x closest-y
        )
    ;; (setq x (getf area :right)
    ;;       y (getf area :bottom))

    ;; Reset markers
    ;; (svg-polygon svg nil :id "_marker_o" :display "none")
    ;; (svg-line svg 0 0 0 0 :id "_marker_r" :display "none")
    (mapc (lambda (a)
            (dom-set-attribute a 'display "none"))
          (dom-by-id svg "_marker"))

    (setq canvas--nearest-objects nil)
    (while (and tags flag)
      (setq tag (car tags)
            bbox (svg-bbox tag)
            id (dom-attr tag 'id))
      ;; (when (string-match-p "_marker" id)
      ;;   ;; Reset markers
      ;;   (dom-set-attribute tag 'r 0)
      ;;   (svg-circle svg 0 0 1 :id id :visibility "none")
      ;;   )

      (when (and bbox id
                 (/= canvas--id (string-to-number id))
                 (not (string-match-p "^_" id)))
        (setq x1 (nth 0 bbox)
              y1 (nth 1 bbox)
              x2 (nth 2 bbox)
              y2 (nth 3 bbox))
        (when (and (> x (- (min x1 x2) limit))
                   (> y (- (min y1 y2) limit))
                   (< x (+ (max x1 x2) limit))
                   (< y (+ (max y1 y2) limit)))
          (setq flag nil)
          ;; Using approximation for speed
          (setq dist (min (- x (min x1 x2))
                          (abs (- x (/ (+ x1 x2) 2)))
                          (- (max x1 x2) x)
                          (- y (min y1 y2))
                          (abs (- y (/ (+ y1 y2) 2)))
                          (- (max y1 y2) y)))
          (when (or (<= dist closest)
                    (eq canvas--outline-style 'solid))
            ;; (setq closest dist)
            (setq points (canvas--obj-outline tag
                                                x1 y1 x2 y2 limit)
                  closest (seq-reduce (lambda (r a)
                                        (let* ((x1 (- (car a) x))
                                               (y1 (- (cdr a) y))
                                               (d (+ (* x1 x1)
                                                     (* y1 y1))))
                                          (if (> d r)
                                              r
                                            (setq closest-x (car a)
                                                  closest-y (cdr a))
                                            d)))
                                      points (* limit limit))
                  closest (sqrt closest))
            (push tag canvas--nearest-objects)
            )))
      (setq tags (cdr tags)))
    (if closest-x (cons closest-x closest-y))))

(defun canvas--highlight-obj (svg objects)
  "Highlight nearest object."
  (mapc (lambda (a)
          (unless (and (dom-attr a 'id)
                       ;; Don't hightlight currently drawn object
                       (or (= canvas--id (string-to-number (dom-attr a 'id)))
                           (string-match-p "^_[ms]" (dom-attr a 'id))))
          (let* ((bbox (svg-bbox a))
                 (x1 (nth 0 bbox))
                 (y1 (nth 1 bbox))
                 (x2 (nth 2 bbox))
                 (y2 (nth 3 bbox))
                 ;; (id "_marker_o")
                 (id (format "_marker_o_%s" (dom-attr a 'id)))
                 (marker (when canvas--outline-style
                           (format "url(%s)" canvas--outline-marker)))
                 (points (canvas--obj-outline a
                                              x1 y1 x2 y2 10)))
            (cond ((eq (dom-tag a) 'polyline)
                   (setq points (dom-attr a 'points))
                   (svg-node svg 'polyline :id id :fill "none"
                             :points points
                             :stroke canvas--stroke-color
                             :stroke-width 2
                             :stroke-dasharray "5,10"
                             :marker-end marker
                             :marker-start marker :marker-mid marker))
                  ((eq (dom-tag a) 'g)
                   (canvas--highlight-obj svg (dom-children a)))
                  ((eq canvas--outline-style 'solid)
                   (svg-polygon svg points :id id :fill "none"
                                :stroke-width 8
                                :stroke-linejoin "round"
                                :opacity 0.6
                                :stroke "red" :rx 0 :ry 0))
                  (t
                   (svg-polygon svg points :id id :fill "none"
                                :stroke (if (memq (dom-tag a)
                                                  '(circle ellipse))
                                            0 canvas--stroke-color)
                                :stroke-width 2
                                :stroke-dasharray "5,10"
                                :marker-start marker :marker-mid marker)))
            )))
        objects))

(defun canvas--obj-outline (tag x1 y1 x2 y2 limit)
  "Return points on the outline."
  (let (points p)
    (if (and (eq canvas--mode 'conn)
             (dom-attr tag 'canvas:ports))
        (dolist (a (split-string (dom-attr tag 'canvas:ports) "[ ,]"))
          (if (= (length p) 1)
              (push (cons (string-to-number (pop p))
                          (string-to-number a))
                    points)
            (push a p)))
    (pcase (dom-tag tag)
      ('line (if (< (abs (- x2 x1)) (/ limit 2))
                 (setq x2 x1))
             (if (< (abs (- y2 y1)) (/ limit 2))
                 (setq y2 y1))
             (push (cons x1 y1) points)
             (push (cons (/ (+ x1 x2) 2) (/ (+ y1 y2) 2)) points)
             (push (cons x2 y2) points))
      ((or 'circle 'ellipse)
               (push (cons x1 (/ (+ y1 y2) 2)) points)
               (push (cons (/ (+ x1 x2) 2) y2) points)
               (push (cons x2 (/ (+ y1 y2) 2)) points)
               (push (cons (/ (+ x1 x2) 2) y1) points))
      ;; ('polyline nil)
      (_
              (push (cons x1 y1) points)
              (push (cons x1 (/ (+ y1 y2) 2)) points)
              (push (cons x1 y2) points)
              (push (cons (/ (+ x1 x2) 2) y2) points)
              (push (cons x2 y2) points)
              (push (cons x2 (/ (+ y1 y2) 2)) points)
              (push (cons x2 y1) points)
              (push (cons (/ (+ x1 x2) 2) y1) points))
      ))
    points))

(defun canvas--draw-init (svg area)
  (let (res x y w h closest)
    (canvas--apply-transform canvas--undo-marker)
    (setq res (canvas--transform svg area)
          x (nth 0 res)
          y (nth 1 res)
          w (nth 2 res)
          h (nth 3 res)
          canvas--nearest-objects nil)
    ;; Use the end point of area
    (setq closest (canvas--adjacent-obj svg (+ x w) (+ y h)))
    (canvas--highlight-obj svg canvas--nearest-objects)
    (canvas--highlight-obj svg canvas--undo-marker)

    (when (and (eq canvas--mode 'conn)
               canvas--temp-points)
      ;; Animate drawing of connection points
      (let* ((x (or (car closest) x))
             (y (or (cdr closest) y))
             (point (car canvas--temp-points))
             (x1 (car point))
             (y1 (cdr point))
             (id (number-to-string canvas--id))
             (fg-color canvas--stroke-color)
             (fill canvas--fill-color)
             (points nil)
             dx dy
             )
        ;; (message "%s %s %s %s" canvas--nearest-objects x y canvas--temp-points)
        ;; (message "%s" canvas--temp-points)
        ;; points must be a list
        (setq points (append (list (cons x y)
                                   (unless (or (null point) (= x1 x) (= y1 y))
                                     (setq dx (- x1 x)
                                           dy (- y1 y))

                                     (if canvas--routing
                                         (if (> (abs dx) (abs dy))
                                             (cons (- x1 (* (signum dx) (abs dy))) y)
                                           (cons x (- y1 (* (signum dy) (abs dx)))))
                                       (if (> (abs dx) (abs dy))
                                           (cons x y1)
                                         (cons x1 y)))))
                             canvas--temp-points))
          (svg-polyline svg points
                        :stroke-width canvas--stroke-width
                        :id id :stroke-color fg-color :fill fill)
          ))

    (when (memq canvas--mode '(place move))
      ;; Animate movement of selected object
      (mapc (lambda (a)
              (funcall canvas--move-to-fn a x y))
            canvas--undo-marker))

    (svg-possibly-update-image svg)
  ))

(defun canvas--draw (svg area)
  (let ((fg-color canvas--stroke-color)
        (points nil)
        (fill canvas--fill-color)
        (res nil)
        x1 y1 closest x y w h node
        (id (number-to-string canvas--id)))

    (setq res (canvas--transform svg area)
          x (nth 0 res)
          y (nth 1 res)
          w (nth 2 res)
          h (nth 3 res))

    ;; Snap to nearest object
    (when (memq canvas--mode
                '(line circle rect ellipse text conn move define-port path))
      (setq closest (canvas--adjacent-obj svg (+ x w) (+ y h)))
      (if closest
          (if (= w h 0)
              ;; Start point
              (progn
                (setq x (car closest)
                      y (cdr closest))
                (setf (getf area :left) x
                      (getf area :top) y))
            (setq x1 (car closest)
                  y1 (cdr closest)
                  w (- x1 x)
                  h (- y1 y)))

        ;; Snap to grid
        (when canvas--grid-size
          (setq x (* (round (/ x canvas--grid-size)) canvas--grid-size)
                y (* (round (/ y canvas--grid-size)) canvas--grid-size))
          (setq w (* (round (/ w canvas--grid-size)) canvas--grid-size)
                h (* (round (/ h canvas--grid-size)) canvas--grid-size)))
        ))
    ;; (message "%s %s %s %s %s %s" res area x y w h)


    ;; Horizontal or vertical ruler
    (when (memq canvas--mode
                '(line move))
      (cond ((= h 0)
             (svg-line svg "0%" y "100%" y
                       :id "_marker_r" :fill "none" :stroke fg-color
                       :stroke-dasharray "5,5"))
            ((= w 0)
             (svg-line svg x "0%" x "100%"
                       :id "_marker_r" :fill "none" :stroke fg-color
                       :stroke-dasharray "5,5"))))

    (pcase canvas--mode
      ('line (svg-line svg x y (+ x w) (+ y h)
                       :stroke-width canvas--stroke-width
                       :id id :stroke-color fg-color))
      ('circle (svg-circle svg x y w
                           :stroke-width canvas--stroke-width
                           :id id :stroke-color fg-color :fill fill))
      ('rect (svg-rectangle svg x y w h
                            :stroke-width canvas--stroke-width
                            :id id :stroke-color fg-color :fill fill))
      ('ellipse (svg-ellipse svg x y (abs w) (abs h)
                             :stroke-width canvas--stroke-width
                             :id id :stroke-color fg-color :fill fill))
      ('text (svg-text svg (nth 0 canvas--text)
                       :font-size (number-to-string (nth 1 canvas--text))
                       :font-family (nth 2 canvas--text)
                       :x x :y y
                       :stroke-width canvas--stroke-width
                       :id id :stroke-color fg-color :fill fill))
      ((or 'polyline 'erase)
       (setq node (car (dom-by-id svg (format "^%s$" id)))
             points (dom-attr node 'points))
       ;; (message "%s %s %s" id points node)
       (if points
           (progn
             ;; points is a string
             (setq points (format "%s %s,%s" points (+ x w) (+ y h)))
             (dom-set-attribute node 'points points)
             (svg-possibly-update-image svg))

         ;; points must be a list
         (setq points (list (cons x y)))
         ;; (message "%s" area)
         (svg-polyline svg points
                       :stroke-width canvas--stroke-width
                       :id id :stroke-color fg-color :fill fill)))
      ('path
       (setq node (car (dom-by-id svg (format "^%s$" id)))
             canvas--temp-points (append canvas--temp-points `((,x . ,y)))
             points canvas--temp-points)
       ;; (message "%s %s %s" id points node)
       (canvas--path svg canvas--temp-points id))
      ('define-port
        (when canvas--port-name
        ;; (message "%s %s %s" x y canvas--nearest-objects)
        ;; Place text outside bbox
        (let* ((bbox (svg-bbox (car canvas--nearest-objects)))
               (x1 (nth 0 bbox))
               (y1 (nth 1 bbox))
               (x2 (nth 2 bbox))
               (y2 (nth 3 bbox))
               (tw (length canvas--port-name))
               (tx x)
               (ty y))
          (if (<= x x1) (setq tx (- x canvas--grid-size tw)))
          (if (>= x x2) (setq tx (+ x canvas--grid-size)))
          (if (<= y y1) (setq ty (- y canvas--grid-size)))
          (if (>= y y2) (setq ty (+ y canvas--grid-size)))
        (svg-text svg canvas--port-name
                  :x tx
                  :y ty
                  :stroke-width canvas--stroke-width
                  :id (concat "_name" (number-to-string canvas--port-id))
                  :stroke-color fg-color :fill fill)
        (svg-circle svg x y 5
                           :stroke-width canvas--stroke-width
                           :id (concat "_port" (number-to-string canvas--port-id))
                           :name canvas--port-name
                           :stroke-color fg-color :fill fill))))
      ('conn (let* ((point (car canvas--temp-points))
                    (x1 (car point))
                    (y1 (cdr point))
                    dx dy
                    )
               ;; (message "%s" canvas--temp-points)
               (if (null point)
                   (push (cons x y) canvas--temp-points)
               (unless (or (= x1 x) (= y1 y))
                 (setq dx (- x1 x)
                       dy (- y1 y))

                 (setq canvas--temp-points
                       (append (list
                                (cons x y)
                                (if canvas--routing
                                    (if (> (abs dx) (abs dy))
                                        (cons (- x1 (* (signum dx) (abs dy))) y)
                                      (cons x (- y1 (* (signum dy) (abs dx)))))))
                               canvas--temp-points))
                 ))))
      ((or 'move 'place)
             (when (eq canvas--mode 'place)
               ;; Translate to x,y. Set id.
               (setq w x h y canvas--id (1+ canvas--id))
               (dom-set-attribute (car canvas--undo-marker) 'id id))

             ;; (cond ((null canvas--undo-marker)
             ;;        (setq canvas--mode nil))
             ;;       (t
             ;;        (mapc (lambda (a)
             ;;                (let ((str (dom-attr a :init))
             ;;                      (off canvas--temp-points))
             ;;                  (dom-set-attribute a :transformed t)
             ;;                  (dom-set-attribute a 'transform
             ;;                                     (format "%s translate(%s, %s)"
             ;;                                             (or str "")
             ;;                                             (- x (car off))
             ;;                                             (- y (cdr off))
             ;;                                             ))))
             ;;              canvas--undo-marker)))

             (svg-possibly-update-image svg))
      (_ (svg-rectangle svg x y w h
                        :stroke-dasharray (format "%0.2f,%0.2f"
                                                  (* 10 (nth 4 res))
                                                  (* 10 (nth 5 res)))
                        :id "_selection"
                        :stroke-color (foreground-color-at-point) :fill "none"))
      )
    (canvas--highlight-obj svg canvas--nearest-objects)
    ))

(defun canvas--path (svg points id)
  "Convert points to SVG path."
  (let* ((fg-color canvas--stroke-color)
         (fill canvas--fill-color)
         cmds p)
    (setq p (pop points)
          cmds (append `((moveto ,p))
                       (cl-loop for (a b) on points by #'cddr while b
                                collect `(smooth-curveto (,a ,b)))))
    ;; (message "%s" cmds)
    (svg-path svg cmds
              :stroke-width canvas--stroke-width
              :id id :stroke-color fg-color :fill fill)
    ))

(defun canvas--zoom (svg area)
    (let* ((x (nth 0 area))
           (y (nth 1 area))
           (width (- (nth 2 area) (nth 0 area)))
           (height (- (nth 3 area) (nth 1 area)))
           (aspect 1)
           grid node
           )
      (setq aspect (/ (dom-attr svg 'width) (dom-attr svg 'height) 1.0))
      (if (> width height)
          (setq height (/ width aspect))
        (setq width (* height aspect)))
      ;; (message "%s %s %s" width height (= height width 0))
      (assert (and (> height 0) (> width 0)))

      (setq canvas--outline-marker
            (cond ((< width 20) "#dot001")
                  ((< width 50) "#dot01")
                  (t "#dot")))
      (dom-set-attribute svg 'viewBox
                         (format "%s %s %s %s" x y width height))
      (setq grid
            (cond ((< width 40) "x5Grid")
                  ((< width 100) "x10Grid")
                  (t "x20Grid"))
            node (car (dom-children (car (dom-by-id svg "^grid$")))))
      (when node
      (dom-set-attribute node 'fill (format "url(#%s)" grid)))

      (svg-possibly-update-image svg)))

(defun canvas--select-in-group (node x y)
  "Select the object closest to x, y in a group."
  (let (res prev transform)
    (when (eq (dom-tag node) 'g)
      (setq transform (dom-attr node 'transform))
      (mapc (lambda (a)
              (when (eq (dom-tag a) 'g)
                (setq prev (dom-attr a 'transform))
                (dom-set-attribute a 'transform transform)
                (if (canvas--select-in-group a x y)
                    (push a res))
                (dom-set-attribute a 'transform prev)
                ))
            (dom-children node))
      (unless res
        (let* ((bbox (svg-bbox node))
               (x1 (nth 0 bbox))
               (y1 (nth 1 bbox))
               (x2 (nth 2 bbox))
               (y2 (nth 3 bbox)))
          ;; (message "%s %s" bbox node)
          (if (and (>= x x1) (<= x x2)
                   (>= y y1) (<= y y2))
              (setq res node)
            ))
        ))
    res))

(defun canvas--act-in-region (svg area)
  "Zoom or highlight shapes inside area via dashes."
  (let* ((res (canvas--transform svg area))
         (x (nth 0 res))
         (y (nth 1 res))
         (w (nth 2 res))
         (h (nth 3 res)))
  (pcase canvas--mode
    ('zoom ;; Remove selection box
           ;; (message "%s\n%s" area (last (dom-children svg)))
           (dom-remove-node svg (car (dom-by-id svg "_selection")))
           (canvas--zoom svg (list x y (+ x w) (+ y h)))
           ;; (message "%s %s" (dom-attributes svg) area)
           (svg-possibly-update-image svg))
    ('crop ;; Remove selection box
     (when (y-or-n-p "Crop region? ")
       ;; (setq x 0 y 0 w 400 h 400)
       ;; (message "%s" (list x y w h))
       (let* ((tmp (copy-tree svg))
              (node (car (dom-by-id tmp "_grid")))
              (attr (dom-attr node 'display))
              file)
         ;; Co-ordinates are absolute; remove viewbox
         ;; Disable grid
         (dom-set-attribute tmp 'viewBox nil)
         (dom-set-attribute node 'display "none")
         (dom-remove-node tmp (car (dom-by-id svg "_selection")))
         (setq file (canvas-crop-image tmp (list x y (+ x w) (+ y h))))
         (dom-set-attribute node 'display attr)

         (setq canvas--undo-marker (dom-children svg))
         (canvas--undo svg)
         (svg-embed svg file
                    (format "image/%s" (image-type file nil t)) t
                    :id (number-to-string canvas--id)
                    :x x :y y
                    :width w
                    :height h
                    )
         (setq canvas--id (1+ canvas--id)
               canvas--mode nil)
         (svg-possibly-update-image svg)))
     (dom-remove-node svg (car (dom-by-id svg "_selection")))
     )
    ((guard (eq canvas--mode nil))
     ;; Remove selection box
     (dom-remove-node svg (car (dom-by-id svg "_selection")))
     (canvas--select (or (svg--shapes-in-region svg (list x y (+ x w) (+ y h)))
                         canvas--nearest-objects))
     ;; (message "%s" canvas--undo-marker)
     (svg-possibly-update-image svg))
    ((or 'conn 'path) nil)
    ('place (let* ((node (copy-tree canvas--undo-marker)))
              (canvas--deselect)
              ;; Append to current image
              (nconc svg node)
              (setq canvas--undo-marker nil)
              (canvas--select node)
              (canvas--move-init node)))
    ('move
     (setq canvas--mode nil)
     (canvas--deselect))
    (_ (setq canvas--id (1+ canvas--id))))))

(defun canvas--split-window (file &optional svg name)
  "Show SVG FILE in a new NAME window.
Resizes the image to fit the window."
  (let* ((svg (or svg (svg-load file)))
         (buf (get-buffer-create (or name "*temp*")))
         (cur (current-buffer))
         w h)
    (switch-to-buffer-other-window buf)
    (with-current-buffer buf
      (erase-buffer)
      ;; Resize to fit new window
      (setq w (window-pixel-width)
            h (window-pixel-height)
            svg (svg--extract-fragment svg nil w h))
      (insert-image (svg-image svg)))

    ;; Resize to fit current window
    (pop-to-buffer cur)
    (setq w (window-pixel-width)
          h (window-pixel-height)
          svg (car canvas--svg)
          svg (svg--extract-fragment svg nil w h))
    (svg-possibly-update-image svg)
    ))

(defun canvas-crop-image (svg area)
  (let* ((w (- (nth 2 area) (nth 0 area)))
         (h (- (nth 3 area) (nth 1 area)))
         (type "png"))
    ;; (with-temp-buffer
    (with-current-buffer (get-buffer-create "b.png")
      (set-buffer-multibyte nil)
      (image-mode-as-text)
      (erase-buffer)
      (svg-print svg)
      (call-process-region
       (point-min) (point-max) "convert"
       t (list (current-buffer) "*Messages*") nil
       "-crop"
       (format
	;; width x height + left + top
	"%dx%d+%d+%d" w h (nth 0 area) (nth 1 area))
       "+repage"
       "-" (format "%s:-" type))
      ;; (save-buffer)
      (buffer-string)
      )))

(defvar svg--float-left 0)
(defvar svg--float-right 0)
(defun svg-render-tag (svg node x y width height
                           &optional padding margin border)
  (let* ((i canvas--id)
         (padding (or padding 0))
         (margin (or margin 0))
         (border (or border 0))
         (w 0)
         (h 0)
         (left x)
         (right (+ x width))
         (top y)
         (width-max 0)
         (height-max 0)
         g box bbox added break n
         (added1 nil))
    ;; (message "%s" (dom-tag node))
    (pcase (dom-tag node)
      ((or 'body 'div 'h1 'h2 'h3 'p 'a 'ul 'ol 'li 'i 'b 'span
           'code 'pre 'label 'button 'nav 'header 'main 'abbr 'small 'footer
           'sup 'sub 'strong
           'table 'td 'tr 'th 'tbody)
       (if (eq (dom-tag node) 'a)
           (setq g (dom-node 'a
                             `((:id  . ,(number-to-string (+ i 1)))
                               (text . ,(dom-attr node 'text))
                               (href . ,(dom-attr node 'href)))))
         (setq g (svg-group nil nil
                            :id (number-to-string (+ i 1))
                            :data-id (dom-attr node 'id)
                            :data-class (dom-attr node 'class)
                            :class (dom-tag node))))
             ;; height (if (memq (dom-tag node) '(h2 p)) 15 height)
       (setq box (svg-rectangle g x y width height
                                :id (number-to-string (+ i 2))
                                :fill "none"
                                :stroke "black"
                                ;; :stroke-width border
                                :class (dom-attr node 'class)))
       (setq i (+ i 2)))
      ('\#text
       (when (not (string-empty-p (setq node (string-trim node))))
         (setq g (svg-group svg nil
                            :id (number-to-string (+ i 1)))
               added t)
         (let* ((h (or height 15))
                (n (/ (* width) h))
                (size (length node))
                start str)
           (with-temp-buffer
             (insert node)
             (goto-char (point-min))
             (while (not (eobp))
               (setq start (point))
               (if (> (+ start n) size)
                   (goto-char (1+ size))
                 (goto-char (+ start n))
                 (forward-word 1))
               (setq str (buffer-substring-no-properties start (point))
                     y (+ y h)
                     i (1+ i))
               (svg-text g str
                   :x x
                   :y y
                   :font-size h
                   :id (number-to-string (+ i 1)))
               ;; (message "%s %s %s" size n str)
               )))
         ;; (setq added t)
         ;; (svg-text svg node
         ;;           :x x
         ;;           :y (+ y (or height 15))
         ;;           :font-size (or height 15)
         ;;           :id (number-to-string (+ i 1)))
         ))
      )
    (unless (memq (dom-tag node)
                  '(\#text head script noscript style comment form map))
      ;; Need to process children of unknown tag types
      (setq n (if (eq (dom-tag node) 'tr) (1- (length (dom-children node))) 1))
      (mapc (lambda (a)
              (setq break (or (memq (dom-tag a) '(h1 h2 p br ul li tr))))
              (when (setq added1
              ;; (if (setq added1
                        (svg-render-tag
                         g a 0 0
                         (if width  (- (/ width n)  (* 2 (+ border margin padding))))
                         (if height (- height (* 2 (+ border margin padding))))
                         padding margin border))
                  (setq bbox (or (svg-bbox (last g))
                                 '(0 0 0 0))
                        w (if (memq (dom-tag node) '(th td))
                              width
                            (- (nth 2 bbox) (nth 0 bbox)))
                        h (- (nth 3 bbox) (nth 1 bbox))
                        w (+ (* 2 (+ border margin padding)) w)
                        h (+ (* 2 (+ border margin padding)) h)
                        added t
                        i (1+ i))
                  ;; (message "%s %s, %s %s %s %s %s %s"
                  ;;          (dom-tag node) a x y left right height-max w)
                (when (or (and (> x left) break)
                          (> (+ x w) right))
                  (setq width-max (max width-max (+ (- x left)))
                        x left
                        y (+ y height-max)
                        break nil
                        height-max 0))
                (dom-set-attribute (last g) 'transform
                                   (format "translate(%d,%d)"
                                           (+ x border margin padding)
                                           (+ y border margin padding)
                                           ))
                (setq x (+ x w)
                      height-max (max height-max h)))
              )
            (dom-children node))

      (when (and added box)
        (dom-append-child svg g)
        (setq width-max (max width-max (+ (- x left)))
              height-max (+ (max height-max h) (- y top)))
        (dom-set-attribute box 'width (if width
                                          (min width width-max)
                                        width-max))
        (dom-set-attribute box 'height (if height
                                           (min height height-max)
                                         height-max)))
      )
    (setq canvas--id (+ i 1))
    added))

(defun svg-render-html (&optional dom)
  "Render HTML DOM as an SVG image."
  (interactive)
  (let* ((w (window-pixel-width))
         (h (window-pixel-height))
         (svg (svg-create w h :font-family "DejaVu Sans Mono"))
         (canvas--id 0)
         (width w)
         (padding 8)
         (source nil)
         bbox)
    (unless dom
      (setq dom (libxml-parse-html-region (point-min) (point-max))))
    ;; (nconc svg (dom-by-tag dom 'style))

    (svg-render-tag svg (dom-by-tag dom 'body) 0 0 width nil padding)

    (with-current-buffer (get-buffer "a.svg")
      (setq inhibit-modification-hooks t
            bbox (svg-bbox svg))
      (dom-set-attribute svg 'width  (- (nth 2 bbox) (nth 0 bbox)))
      (dom-set-attribute svg 'height (- (nth 3 bbox) (nth 1 bbox)))
      (image-mode-as-text)
      (erase-buffer)

      (when source
        (setq cursor-type t)
      (svg-print svg)
      (goto-char (point-min))
      (while (re-search-forward "background" nil t)
        (replace-match  "fill" nil nil))
      (call-process-region (point-min) (point-max) "xmllint" t t nil
                           "--format" "-")
      (image-mode)
      )

      (unless source
        (setq cursor-type nil)
        (dotimes (i (/ (- (nth 3 bbox) (nth 1 bbox)) h))
          (dom-set-attribute svg 'height h)
          (dom-set-attribute svg 'viewBox (format "0 %d %d %d" (* i h) w h))
          ;; (svg-image-map svg)
          (svg-insert-image svg)
          (svg-possibly-update-image svg)
          (newline)
          ))
      (save-buffer))
    svg))

;; ewp functions copied from https://github.com/larsmagne/ewp/blob/master/ewp.el
(defun ewp-crop-image (&optional square)
  "Crop the image under point.
If SQUARE (the prefix), crop a square from the image."
  (interactive "P")
  (let ((image (get-text-property (point) 'display)))
    (when (or (not image)
	      (not (consp image))
	      (not (eq (car image) 'image)))
      (error "No image under point"))
    ;; We replace the image under point with an SVG image that looks
    ;; just like that image.  That allows us to draw lines over it.
    ;; At the end, we replace that SVG with a cropped version of the
    ;; original image.
    (let* ((data (getf (cdr image) :data))
	   (undo-handle (prepare-change-group))
	   (orig-data data)
	   (type (cond
		  ((getf (cdr image) :format)
		   (format "%s" (getf (cdr image) :format)))
		  (data
		   (ewp-content-type data))))
	   (image-scaling-factor 1)
	   (size (image-size image t))
	   (svg (svg-create (car size) (cdr size)
			    :xmlns:xlink "http://www.w3.org/1999/xlink"
			    :stroke-width 5))
	   (text (buffer-substring (line-beginning-position)
				   (line-end-position)))
	   (inhibit-read-only t))
      (with-temp-buffer
	(set-buffer-multibyte nil)
	(if (null data)
	    (insert-file-contents-literally (getf (cdr image) :file))
	  (insert data))
	;; (let ((ewp-exif-rotate nil))
	;;   (ewp-possibly-rotate-buffer image))
	(setq orig-data (buffer-string))
	(setq type (ewp-content-type orig-data))
	(call-process-region (point-min) (point-max)
			     "convert" t (current-buffer) nil
			     "-resize" "600x"
			     "-"
			     (format "%s:-" (cadr (split-string type "/"))))
	(setq data (buffer-string)))
      (svg-embed svg data type t
		 :width (car size)
		 :height (cdr size))
      (delete-region (line-beginning-position)
		     (line-end-position))
      (svg-insert-image svg)
      (let ((area (condition-case _
		      (save-excursion
			(forward-line 1)
			(ewp-crop-image-1 svg square
					  (car size) (cdr size)))
		    (quit nil))))
	(delete-region (line-beginning-position) (line-end-position))
	(if area
	    (ewp-crop-image-update area orig-data size type)
	  ;; If the user didn't complete the crop, re-insert the
	  ;; original image (and text).
	  (insert text))
	(undo-amalgamate-change-group undo-handle)))))

(defun ewp-crop-image-update (area data size type)
  (let* ((image-scaling-factor 1)
	 (osize (image-size (create-image data (ewp--image-type) t) t))
	 (factor (/ (float (car osize)) (car size))))
    ;; (ewp-insert-image-data
     (with-temp-buffer
       (set-buffer-multibyte nil)
       (insert data)
       (call-process-region
	(point-min) (point-max) "convert"
	t (list (current-buffer) nil) nil
	"+repage" "-crop"
	(format
	 ;; width x height + left + top
	 "%dx%d+%d+%d"
	 (abs (truncate (* factor (- (getf area :right) (getf area :left)))))
	 (abs (truncate (* factor (- (getf area :bottom) (getf area :top)))))
	 (truncate (* factor (min (getf area :left) (getf area :right))))
	 (truncate (* factor (min (getf area :top) (getf area :bottom)))))
	"-" (format "%s:-" (cadr (split-string type "/"))))
       (buffer-string))))

(defun ewp-crop-image-1 (_svg &optional square image-width image-height)
  (track-mouse
    (cl-loop with prompt = (if square "Move square" "Set start point [%s]")
	     and state = (if square 'move-unclick 'begin)
	     and area = (if square
			    (list :left (- (/ image-width 2)
					   (/ image-height 2))
				  :top 0
				  :right (+ (/ image-width 2)
					    (/ image-height 2))
				  :bottom image-height)
			  (list :left 0
				:top 0
				:right 0
				:bottom 0))
	     and corner = nil
	     for event = (read-event (if canvas--hide-prompt
                                         " "
                                       (format prompt (or canvas--mode "selection"))))
             and svg = (car canvas--svg)
             ;; do (message "%s" event)
	     do (if (or (not (consp event))
			(not (consp (cadr event)))
			(not (nth 7 (cadr event)))
			;; Only do things if point is over the SVG being
			;; tracked.
			(not (eq (getf (cdr (nth 7 (cadr event))) :type)
				 'svg)))
                    (setq event (funcall canvas-plugin-fn event svg))
                  ;; (canvas--select-mode event svg)
                  ;; (message "%s %s" event state)
		  (let ((pos (nth 8 (cadr event))))
                    ;; (setf (getf area :right) (car pos)
		    ;;       (getf area :bottom) (cdr pos))
		    (cl-case state
		      ('begin
		       (cond
                        ((eq (car event) 'double-mouse-1)
                         (setq event (funcall canvas-plugin-fn (car event) svg)))
			((eq (car event) 'down-mouse-1)
                         ;; (if canvas--mode
                         ;;     ;; We need updated id since delete
                         ;;     ;; operation can set this out of order.
                         ;;     (setq canvas--id (length (dom-children svg))))
			 (setq state 'stretch
			       prompt "Stretch to end point [%s]")))
			 (setf (getf area :left) (car pos)
			       (getf area :top) (cdr pos)
			       (getf area :right) (car pos)
			       (getf area :bottom) (cdr pos)))
		      ('stretch
		       (cond
			((eq (car event) 'mouse-movement)
			 (setf (getf area :right) (car pos)
			       (getf area :bottom) (cdr pos)))
			((memq (car event) '(mouse-1 drag-mouse-1))
                         ;; (if canvas--mode
                             ;; Mouse release must start a new polyline
                             ;; (setq canvas--id (1+ canvas--id))
                           (funcall canvas-act-in-region-fn svg area)
                            ;; svg (list (getf area :left)
			    ;;           (getf area :top)
                            ;;           (getf area :right)
                            ;;           (getf area :bottom)))
			 (setq state 'begin;'corner
                               prompt "Set start point [%s]"
			       ;; prompt "Choose corner to adjust (RET to crop)"
                               ))))
		      ('corner
		       (cond
			((eq (car event) 'down-mouse-1)
			 ;; Find out what corner we're close to.
			 (setq corner (ewp-find-corner
				       area pos
				       '((:left :top)
					 (:left :bottom)
					 (:right :top)
					 (:right :bottom))))
			 (when corner
			   (setq state 'adjust
				 prompt "Adjust crop")))))
		      ('adjust
		       (cond
			((memq (car event) '(mouse drag-mouse-1))
			 (setq state 'corner
			       prompt "Choose corner to adjust"))
			((eq (car event) 'mouse-movement)
			 (setf (getf area (car corner)) (car pos)
			       (getf area (cadr corner)) (cdr pos)))))
		      ('move-unclick
		       (cond
			((eq (car event) 'down-mouse-1)
			 (setq state 'move-click
			       prompt "Move"))))
		      ('move-click
		       (cond
			((eq (car event) 'mouse-movement)
			 (setf (getf area :left) (car pos)
			       (getf area :right) (+ (car pos) image-height)))
			((memq (car event) '(mouse-1 drag-mouse-1))
			 (setq state 'move-unclick
			       prompt "Click to move"))))))
                  (if (eq state 'stretch)
                      (canvas--draw svg area)
                     (canvas--draw-init svg area)))
	     while (not (member event '(return ?q)))
	     finally (return (or (if canvas-exit-fn (funcall canvas-exit-fn))
                                 (and (eq event 'return)
				      area))))))

(defun ewp-find-corner (area pos corners)
  (cl-loop for corner in corners
	   ;; We accept 10 pixels off.
	   when (and (< (- (car pos) 10)
			(getf area (car corner))
			(+ (car pos) 10))
		     (< (- (cdr pos) 10)
			(getf area (cadr corner))
			(+ (cdr pos) 10)))
	   return corner))

(defun ewp-content-type (image)
  ;; Get the MIME type by running "file" over it.
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert image)
    (call-process-region (point-min) (point-max)
			 "file" t (current-buffer) nil
			 "--mime-type" "-")
    (cadr (split-string (buffer-string)))))

(defun ewp--image-type ()
  (if (or (and (fboundp 'image-transforms-p)
	       (image-transforms-p))
	  (not (fboundp 'imagemagick-types)))
      nil
    'imagemagick))


;; from="0"
;; to="500"
;; dur="5s"
;; values="0;5;0"

;; <polygon points="60,30 90,90 30,90">
;;     <animateTransform
;;       attributeName="transform"
;;       attributeType="XML"
;;       type="rotate"
;;       from="0 60 70"
;;       to="360 60 70"
;;       dur="10s"
;;       repeatCount="indefinite" />
;; </polygon>

;; <rect width="10" height="10">
;;     <animate
;;       attributeName="rx"
;;       values="0;5;0"
;;       dur="10s"
;;       repeatCount="indefinite" />
;; </rect>

;; <circle r="5" fill="red">
;;     <animateMotion
;;       dur="10s"
;;       repeatCount="indefinite"
;;       path="M20,50 C20,-50 180,150 180,50 C180-50 20,150 20,50 z" />
;;   </circle>
(defun svg-animation-metadata (image)
  (let* ((dur 0)
         repeat loop)
    (dolist (node (dom-by-tag image 'animate))
      (setq dur (max dur (string-to-number (dom-attr node 'dur))) ;; 1s
            repeat (dom-attr node 'repeatCount)
            loop (or loop (string= "indefinite" repeat))))
    (cons dur loop)))

(defun svg--value-to-number (v)
  (let* ((names '(("red" . #xff0000) ("green" . #x00ff00) ("blue" . #x0000ff)))
         (res (cdr (assoc v names))))
    (when (not res)
      (setq res (cond ((string-match-p "^#" v)
                       (string-to-number (substring v 1) 16))
                      ((string-match-p "^[+-]?[0-9]" v) (string-to-number v))
                      (t v))))
    res))

(defvar svg-frame-rate 25)
(defun svg--interpolate (n count from to &optional values freeze)
  ;; n is zero based integer value
  (let* ((range count)
         i m intervals value)
    (setq m (% n count))
    (when values
      (setq values (split-string values ";")
            intervals (+ (length values)))
      (cond ((= 1 intervals)
             (setq value (car values)
                   from value
                   to value))
            ((>= intervals count)
             (setq value (nth m values)
                   from value
                   to value))
            (t
             ;; (message "%s %s %s" count intervals m)
             (setq range (/ count intervals)
                   i     (min (/ n range) (- intervals 2))
                   m     (if (>= m intervals) (% m range) m)
                   from  (nth i values)
                   value from ;; default for non-numeric value
                   to    (nth (1+ i) values))
             ;; (message "%s %s %s" range i m)
             (if (= range 2) (setq range 1))
             )))

    (when (number-or-marker-p (svg--value-to-number from))
      (setq from (svg--value-to-number from)
            to   (svg--value-to-number to))
      ;; m is zero based
      (setq value (+ from (* (/ (- to from) range 1.0) (1+ m)))))

    (if (>= n count)
        (setq value (if freeze to from)))
    ;; (message "%s" value)
    value))

(defun test-interpolate ()
  (interactive)
  ;; (dotimes (i 11)
  ;;   (message "%d %06x" (- 5 i)
  ;;            (svg--interpolate (- 5 i) 11 nil nil "red;#000000;green")))
  (assert (= (svg--interpolate 0 3 nil nil "red;#000000;green") #xff0000))
  (assert (= (svg--interpolate 1 3 nil nil "red;#000000;green") #x000000))
  (assert (= (svg--interpolate 2 3 nil nil "red;#000000;green") #x00ff00))

  (setq svg-frame-rate 1)
  (assert (= (svg--interpolate 0 2 nil nil "0") 0))
  (assert (= (svg--interpolate 0 2 "25" "15" "0") 0))
  (assert (= (svg--interpolate 1 2 "25" "15" "0") 0))
  (assert (= (svg--interpolate 2 2 "25" "15" "0") 0))

  (assert (= (svg--interpolate 0 2 "25" "15" "0;5") 0))
  (assert (= (svg--interpolate 1 2 "25" "15" "0;5") 5))
  (assert (= (svg--interpolate 2 2 "25" "15" "0;5") 0))
  ;; (assert (= (svg--interpolate 2 2 "25" "15" "0;5" nil t) 5))

  (assert (= (svg--interpolate 0 3 "25" "15" "0;5;0") 0))
  (assert (= (svg--interpolate 1 3 "25" "15" "0;5;0") 5))
  (assert (= (svg--interpolate 2 3 "25" "15" "0;5;0") 0))
  (assert (= (svg--interpolate 3 3 "25" "15" "0;5;0") 0))

  (assert (string= (svg--interpolate 0 3 "25" "15" "M 256,213;M 212,220;M 212,220")
                   "M 256,213"))
  (assert (string= (svg--interpolate 1 3 "25" "15" "M 256,213;M 212,220;M 212,220")
                   "M 212,220"))
  (assert (string= (svg--interpolate 2 3 "25" "15" "M 256,213;M 212,220;M 212,220")
                   "M 212,220"))
  (assert (string= (svg--interpolate 3 3 "25" "15" "M 256,213;M 212,220;M 212,220")
                   "M 256,213"))
  )

(defun svg-parse-viewBox (image)
  (if (dom-attr image 'viewBox)
      (mapcar 'string-to-number (split-string (dom-attr image 'viewBox)))
    (list 0 0 (dom-attr image 'width) (dom-attr image 'height))))

(defun svg-image-frame (image n)
  (let* ((nodes (dom-by-tag image 'animate))
         parent attr from to repeat freeze value dur values)
    (dolist (node nodes)
      (setq dur  (dom-attr node 'dur)) ;; 1s
    (when dur
      (setq parent (dom-parent image node)
            attr (intern-soft (dom-attr node 'attributeName))
            dur (string-to-number dur)
            repeat (dom-attr node 'repeatCount)
            freeze (string= "freeze" (dom-attr node 'fill))
            values (dom-attr node 'values)
            from (dom-attr node 'from)
            to   (dom-attr node 'to))
      ;; (message "%s %s %s %s %s" n dur from to values repeat)
      (setq value (svg--interpolate n (* dur svg-frame-rate) from to values freeze))
      ;; (message "%s %s %s %s" n value from to)
      (dom-set-attribute parent attr value)
      image))))

(defun svg-image-map (image)
  (let* ((j 0)
         (x 0)
         (y 0)
         (factor-x 1)
         (factor-y 1)
         (view (dom-attr image 'viewBox))
         title bbox map origin w h w1 h1 a1 p transform old)
    (when view
      (setq origin (svg-parse-viewBox image)
            w (dom-attr image 'width)
            h (dom-attr image 'height)
            x (car origin)
            y (cadr origin)
            w1 (nth 2 origin)
            h1 (nth 3 origin)
            a1 (/ w1 h1 1.0)
            factor-x (if (< w h) (/ w w1 1.0) (* 1 a1))
            factor-y (if (> w h) (/ h h1 1.0) (/ 1 a1))
            ;; y axis is inverted
            x (+ (if (> w h) (* -.5 (- w w1)) 0) (* x factor-x))
            y (+ (if (< w h) (* -.5 (- h h1)) 0) (* y factor-y))
            ))
    (dolist (i (dom-by-tag image 'a))
      ;; Accumulate parent transformation
      (setq p i transform nil)
      (while (setq p (dom-parent image p))
        (if (dom-attr p 'transform)
            (setq transform (concat transform " " (dom-attr p 'transform)))))
      (dolist (j (dom-children i))
        (if (memq (dom-tag j) '(g circle rect ellipse polygon))
            (setq p j)))
      (setq old (dom-attr p 'transform))
      ;; Apply accumulated transformation
      (dom-set-attribute p 'transform transform)
      (setq bbox (svg-bbox p)
            j (1+ j)
            title (xml-substitute-special
                   (or (dom-attr i 'text)
                       (dom-attr i 'xlink:title))))
      ;; Restore old transformation
      (dom-set-attribute p 'transform old)

      (when bbox
        (push `((rect . ,(read (format "((%d . %d) . (%d . %d))"
                                       (- (* (nth 0 bbox) factor-x) x)
                                       (- (* (nth 1 bbox) factor-y) y)
                                       (- (* (nth 2 bbox) factor-x) x)
                                       (- (* (nth 3 bbox) factor-y) y))))
                ,(format "%s" (dom-attr i 'id))
                (pointer hand help-echo ,(format "%s" title)))
              map)))
    map))

(defvar-local svg-click-function nil)
(defun svg-on-click (event)
  (interactive "e")
  (let* ((e (event-start event))
         (win (posn-window e))
         (image (posn-image e))
         (id (nth 1 e))
         o href title)
    ;; (message "1 %s" e)
    (when (and image (not (numberp id)))
      (with-selected-window win
        (setq image (svg--animation-init (image-property image :data))
              o (dom-by-id image id)
              href  (dom-attr o 'xlink:href)
              title (dom-attr o 'xlink:title))
        (if svg-click-function (funcall svg-click-function href))))
    ))

(defun svg-on-click-no-map (event)
  (interactive "e")
  (let* ((e (event-start event))
         (win (posn-window e))
         (image (posn-image e))
         xy hit o href title origin x y)
    ;; (message "%s" e)
    (when image
      ;; (select-window win)
      (with-selected-window win
        (setq xy (posn-object-x-y e)
              image (svg--animation-init (image-property image :data))))
      (setq origin (svg-parse-viewBox image)
            x (car origin)
            y (cadr origin))
      ;; (pp xy)

      ;; Check if xy collides with any anchor object in the image
      (dolist (i (dom-by-tag image 'a))
        (setq o (car (dom-children i))
              hit (svg--point-in-region (+ (car xy) x) (+ (cdr xy) y)
                                        (svg-bbox o)))
        ;; (pp (svg-bbox o))
        ;; (pp hit)
        (and (nth 2 o)
             (dom-set-attribute o 'r
                                (string-to-number (dom-attr (nth 2 o) 'from)))
             (setf (nth 2 o) nil))
        (when hit
          (setq href  (dom-attr i 'xlink:href)
                title (dom-attr i 'xlink:title))
          (pp href)
          ;; (pp o)
          (setq o (svg-animate o "r" "1" :from "15" :to "25" :fill "freeze"))
          ;; (pp o)
          (if title (tooltip-show title))
          (with-selected-window win
            (if svg-click-function (funcall svg-click-function href)))
          ;; (ffap href)
          ))
      )))

;; (defstruct point x y r old-x old-y vel-x vel-y mass width height fill title href text)
(defvar simulate--force-x 0)
(defvar simulate--force-y 0)
(defvar simulate--friction 0)
(defvar simulate-screen-width 100)
(defvar simulate-screen-height 100)
(defun simulate-point-update (p dt)
  (when (or (zerop simulate--friction)
            (> (abs simulate--force-x) simulate--friction)
            (> (abs simulate--force-y) simulate--friction))
  (let* ((r (point-r p))
         (acc-x (/ simulate--force-x (point-mass p) 1.0))
         (acc-y (/ simulate--force-y (point-mass p) 1.0))
         (vel-x (+ (point-vel-x p) (* acc-x dt)))
         (vel-y (+ (point-vel-y p) (* acc-y dt))))
    (setf (point-vel-x p) vel-x)
    (setf (point-vel-y p) vel-y)
    (setf (point-x p) (+ (point-x p) (* vel-x dt)))
    (setf (point-y p) (+ (point-y p) (* vel-y dt)))

    ;; Constraints
    ;; (message "1 %s" p)
    (cond ((< (point-x p) r)
           (setf (point-x p) r)
           (setf (point-vel-x p) (- vel-x)))
          ((< (point-y p) r)
           (setf (point-y p) r)
           (setf (point-vel-y p) (- vel-y)))
          ((> (point-x p) (- simulate-screen-width r))
           (setf (point-x p) (- simulate-screen-width r))
           (setf (point-vel-x p) (- vel-x)))
          ((> (point-y p) (- simulate-screen-height r))
           (setf (point-y p) (- simulate-screen-height r))
           (setf (point-vel-y p) (- vel-y))))
    ;; (message "2 %s" p)
    (setq simulate--force-x (if (> simulate--force-x 0)
                               (- simulate--force-x simulate--friction)
                             (+ simulate--force-x simulate--friction)))
    (setq simulate--force-y (if (> simulate--force-y 0)
                               (- simulate--force-y simulate--friction)
                             (+ simulate--force-y simulate--friction)))
    ;; (message "%s %s %s" simulate--force-x simulate--force-y simulate--friction)
    )))

(defun svg-simulation-frame (image dt &optional force-x force-y friction)
  (when (zerop dt)
    ;; Initial conditions
    (setq simulate--force-x  (if force-x force-x 10)
          simulate--force-y  (if force-y force-y 10)
          simulate-screen-width  (dom-attr image 'width)
          simulate-screen-height (dom-attr image 'height)
          simulate--friction (if friction friction (/ (+ force-x force-y) 20))))

  (if (not (or (zerop simulate--friction)
               (> (abs simulate--force-x) simulate--friction)
               (> (abs simulate--force-y) simulate--friction)))
      (svg-animation-cancel)
  (dolist (node (dom-children image))
    (when (memq (dom-tag node) '(circle a))
      (if (eq (dom-tag node) 'a)
          (setq node (car (dom-children node))))
      (let* ((p (make-point :x (dom-attr node 'cx)
                            :y (dom-attr node 'cy)
                            :r (dom-attr node 'r)
                            :vel-x (or (dom-attr node 'vel-x) 0)
                            :vel-y (or (dom-attr node 'vel-y) 0)
                            :mass (or (dom-attr node 'mass) 1)
                            )))
      (simulate-point-update p dt)
      (dom-set-attribute node 'cx (point-x p))
      (dom-set-attribute node 'cy (point-y p))
      (dom-set-attribute node 'vel-x (point-vel-x p))
      (dom-set-attribute node 'vel-y (point-vel-y p))
      )))))

(defvar svg--bubble-nodes nil)
(defun svg-bubble-frame (image &optional init)
  (if init
      (progn
        (setq svg--bubble-nodes nil
              simulate-screen-width  (dom-attr image 'width)
              simulate-screen-height (dom-attr image 'height)
              )
        (dolist (node (dom-children image))
          (when (memq (dom-tag node) '(circle a))
            (if (eq (dom-tag node) 'a)
                (setq node (car (dom-children node))))
            (let* ((p (make-point :x (dom-attr node 'cx)
                                  :y (dom-attr node 'cy)
                                  :r (dom-attr node 'r)
                                  :vel-x (or (dom-attr node 'vel-x) 0)
                                  :vel-y (or (dom-attr node 'vel-y) 0)
                                  :mass (or (dom-attr node 'mass) 1)
                                  )))
              (push (cons p node) svg--bubble-nodes)
              (dom-set-attribute node 'cx (point-x p))
              (dom-set-attribute node 'cy (point-y p))
              (dom-set-attribute node 'vel-x (point-vel-x p))
              (dom-set-attribute node 'vel-y (point-vel-y p))
              ))))

    (let* ((nodes svg--bubble-nodes)
           node p1 p2 x1 y1 x2 y2 r1 r2 d r moved inc ang deg vx vy)
      ;; Move first point in the list
      (setq p1 (car (pop nodes)))
      (while (setq node (pop nodes))
        (setq p2 (car node)
              x1 (point-x p1)
              y1 (point-y p1)
              r1 (point-r p1)
              x2 (point-x p2)
              y2 (point-y p2)
              r2 (point-r p2)
              ang (if (= x2 x1)
                      (* 2 float-pi (cl-random 1.0))
                    (atan (/ (- y2 y1) (- x2 x1))))
              deg (* (/ 360 (* 2 float-pi)) ang)
              vx (cos ang)
              vy (sin ang)
              d (sqrt (+ (* (- y2 y1) (- y2 y1)) (* (- x2 x1) (- x2 x1))))
              r (+ r1 r2))
        (message "b1 %s %s" p1 p2)
        (message "%.1f %s %.1f %.1f %.3f %.3f" d r deg ang vx vy)
        (when (> (abs (- d r)) 1) ;; Account for Floating point error
          (setq moved t
                inc (if (< d r) r (- d r)))
          (setf (point-x p1) (+ x1 (* inc vx)))
          (setf (point-y p1) (+ y1 (* inc vy)))
        (message "b2 %s %s" p1 p2))

      (if (not moved) (svg-animation-cancel))
      ;; Constraints
      (cond ((< (point-x p1) r1)
             (setf (point-x p1) (- r1 (- (point-x p1) r1))))
            ((< (point-y p1) r1)
             (setf (point-y p1) (- r1 (- (point-y p1) r1))))
            ((> (point-x p1) simulate-screen-width)
             (setf (point-x p1) (- (- simulate-screen-width r1)
                                   (- (point-x p1) (- simulate-screen-width r1)))))
            ((> (point-y p1) simulate-screen-height)
             (setf (point-y p1) (- (- simulate-screen-height r1)
                                   (- (point-y p1) (- simulate-screen-height r1))))))
      (message "b3 %s" p1)

      ;; Update DOM node
      (setq node (pop svg--bubble-nodes)
            svg--bubble-nodes (append svg--bubble-nodes (list node)))
      (dom-set-attribute (cdr node) 'cx (point-x (car node)))
      (dom-set-attribute (cdr node) 'cy (point-y (car node)))
      ))
    ))

(defun svg--animation-init (&optional xml)
  (let* ((image (svg-load-from-xml (or xml (buffer-string)))))

    ;; (image-get-display-property)
    ;; (pp (svg-animation-metadata image))
    ;; (pp (svg-image-frame image 2))
    ;; (image-toggle-display)

    ;; (local-set-key [mouse-1] #'svg-on-click)

    (setq image-animate-loop nil)
    (goto-char (1- (point-max)))
    (dom-set-attribute image :image (point-marker))
    image))

(defvar svg--animation-timer nil)
;;;###autoload
(defun svg-animation-cancel ()
  (interactive)
  (if svg--animation-timer (cancel-timer svg--animation-timer)))

;;;###autoload
(defun svg-animation-run (&optional image)
  (interactive)
  (let* ((image (or image (svg--animation-init)))
         (delay (/ 1 svg-frame-rate 1.0))
         (i 0)
         (buf (current-buffer))
         ;; (m (image-get-display-property))
         metadata count loop)
    (setq metadata (svg-animation-metadata image)
          count (1+ (* (car metadata) svg-frame-rate))
          loop (cdr metadata))
    (svg-animation-cancel)
    (setq svg--animation-timer
          (run-with-timer 0 delay
                          (lambda ()
                            (if (with-current-buffer buf (image-get-display-property))
                                (svg-possibly-update-image image)
                              (svg-animation-cancel))
                            (svg-image-frame image i)

                            (if (or loop (< i count))
                                (setq i (1+ i))
                              (svg-animation-cancel))
                            )))
    ;; (dotimes (i 5)
    ;; (while (or (< i count)
    ;;            (if image-animate-loop (setq i 0)))
    ;;   (svg-image-frame image i)
    ;;   (svg-possibly-update-image image)
    ;;   (setq i (1+ i))
    ;;   (sit-for delay))
    ))

(defun svg-animate1 ()
  "Circle packing"
  (interactive)
  (let* ((image nil); (svg--animation-init))
         (n 7)
         (graph-draw-padding 10)
         (padding graph-draw-padding)
         (text "The quick brown fox jumps over the lazy dog")
         (points nil)
         (w (window-pixel-width))
         (h (window-pixel-height))
         (inhibit-read-only t)
         x y r word words e bbox j p)
    (setq image (svg-create w h))
    (erase-buffer)
    (dom-set-attribute image 'viewBox (format "-%d -%d %d %d" (/ w 2) (/ h 2) w h))
    (svg-insert-image image)

    (setq words (split-string text))
    ;; (setq n (length words))
    ;; (dotimes (i n)
    ;;   (setq j i
    ;;         j (random (length words))
    ;;         word (nth i words)
    ;;         e (svg-text nil word
    ;;                     :id 1
    ;;     	        :font-size (+ j 15))
    ;;         bbox (svg-bbox e)
    ;;         ;; r (- (nth 2 bbox) (nth 0 bbox))
    ;;         ;; r (string-pixel-width word)
    ;;         r 50
    ;;         x (- w r)
    ;;         y (- h r))
    ;;   (or word (error "error"))
    ;;   (push (make-point :x x :y y :r r :fill (random-color-html)
    ;;                     :height (* 50)
    ;;                     :width  (+ 50 j)
    ;;                     :title word)
    ;;         points))

    (dotimes (i n)
      (setq j 0
            ;; j (random n)
            word (number-to-string i)
            r (+ 15 j padding)
            x (- w r)
            y (- h r))
      (push (make-point :x x :y y :r r :fill (random-color-html)
                        :href word :title word)
            points))

    (setq p (graph-enclose (graph-pack points image))
          x (point-x p)
          y (point-y p)
          r (point-r p))

    (svg-circle image x y r
                  :stroke-width 2
                  :stroke "red"
                  :fill "none")

    (svg-possibly-update-image image)
    ))

(defun svg-animate2 ()
  "Bubble graph"
  (interactive)
  (let* ((image nil)
         (n 4)
         (graph-draw-padding 0)
         (padding graph-draw-padding)
         (text1 "Google Reddit Amazon Mail Finance Blog Web" )
         (text2 "Notes Reminder Local" )
         (text3 "Database")
         (points nil)
         (w (window-pixel-width))
         (h (window-pixel-height))
         (inhibit-read-only t)
         x y r word words j root)
    (setq image (svg-create w h))
    (erase-buffer)
    (dom-set-attribute image 'viewBox (format "-%d -%d %d %d" (/ w 2) (/ h 3) w h))
    (svg-insert-image image)

    (dolist (k (list text1 text2 text3))
      (setq words (split-string k)
            points nil
            n (length words))
      (dotimes (i n)
        (setq j 0
              ;; j (random n)
              ;; word (number-to-string i)
              word (nth i words)
              r (+ 35 j padding)
              x (- w r)
              y (- h r))
        (push (make-point :x x :y y :r r :fill (random-color-html)
                          :old-x 0 :old-y 0
                          :href word :title word)
              points))
      (push points root))
    (setq graph-draw-group nil)

    ;; Small incircle
    ;; (setq p (car points))
    ;; (setf (point-r p) (* .2 (point-r p)))

    (graph-draw-tree root image)
    (svg-animation-run image)
    (svg-possibly-update-image image)
    ))

(defun svg-animate3 (prefix)
  "Stocks heatmap"
  (interactive "P")
  (let* ((image nil)
         (mul 10)
         (graph-draw-padding 0)
         (points nil);svg--bubble-nodes)
         (w (window-pixel-width))
         (h (window-pixel-height))
         (inhibit-read-only t)
         x y r word words i j root group children child point)
    (setq image (svg-create w h))
    (erase-buffer)
    (dom-set-attribute image 'viewBox (format "-%d -%d %d %d" (/ w 2) (/ h 2) w h))
    (svg-insert-image image)

    (setq group 'sector)
    (unless points
      (with-current-buffer "nifty.txt"
        (goto-char (point-min))
        (setq j (point))
        (while (re-search-forward "\n" (point-max) t)
          ;; (pp word)
          (setq words (split-string (buffer-substring-no-properties j (1- (point))) "\t"))
          (when (> (length words) 1)
            (setq word  (nth 0 words)
                  r     (* mul (string-to-number (nth 2 words)))
                  x 0 y 0
                  i (random-num 5 -5)
                  j (point))
            (push (make-point :x x :y y :r r
                              ;; :fill (random-color-html)
                              :fill (format "#%06x"
                                            (if (> i 0)
                                                (svg--interpolate i 6 nil nil
                                                                  "#ffffff;#00ff00")
                                              (svg--interpolate (abs i) 6 nil nil
                                                                "#ffffff;#ff0000")))
                              :old-x 0 :old-y 0
                              :text (format "%s\n%.2f%%" (nth 1 words) i)
                              :href word :title (format "%s\n%.2f%%" word i))
                  points)
            (when (eq group 'sector)
              (if (setq child (assoc (nth 3 words) children))
                  (setcdr child (cons (car points) (cdr child)))
                (push (list (nth 3 words) (car points)) children))
              ;; (message "%s" children)
              )
            ))
        ;; (setq points (sort points (lambda (a b) (> (point-r a) (point-r b)))))
        (pcase prefix
          (1
           (setq root (mapcar 'cdr children)
                 group (mapcar 'car children)))
          ;; (setq root (list 1 2 3))
          (_ (setq root points)))
        ))
    (setq graph-draw-group t
          graph-draw-group-fn (lambda (d i)
                                (format "%s" (nth i group))))
    (special-mode)
    (use-local-map (let* ((map (make-sparse-keymap)))
                     (define-key map "i" 'svg-animate-index)
                     (define-key map "s" 'svg-animate-sector)
                     map))

    (setq point (graph-draw-tree root image)
          r (point-r point)
          w (* 2 r))
    ;; (setq bbox (svg-bbox image)
    ;;       mul 2
    ;;       w (- (nth 2 bbox) (nth 0 bbox))
    ;;       h (- (nth 3 bbox) (nth 1 bbox)))
    ;; (pp point)
    (dom-set-attribute image 'viewBox
                       (format "%d %d %d %d"
                               (- (point-x point) r)
                               (- (point-y point) r)
                               w w))

    (svg-possibly-update-image image)
    (svg-animation-run image)
    ))

(defun svg-animate-index ()
  (interactive)
  (setq graph-draw-group nil)
  (svg-animate3 0))

(defun svg-animate-sector ()
  (interactive)
  (setq graph-draw-group t)
  (svg-animate3 1))

(defun svg-animate4 (prefix)
  "3 body problem"
  (interactive "P")
  (let* ((image nil)
         (graph-draw-padding 0)
         (points nil);svg--bubble-nodes)
         (w (window-pixel-width))
         (h (window-pixel-height))
         (inhibit-read-only t)
         r x y word words j root group children child point attributes)
    (setq image (svg-create w h))
    (erase-buffer)
    (dom-set-attribute image 'viewBox (format "-%d -%d %d %d" (/ w 2) (/ h 2) w h))
    (svg-insert-image image)

    (setq group 'sector)
    (unless points
      (with-current-buffer "3body.txt"
        (goto-char (point-min))
        (setq j (point))
        (while (re-search-forward "\n" (point-max) t)
          ;; (pp word)
          (setq words (split-string (buffer-substring-no-properties j (1- (point))) ","))
          (unless attributes
            (setq attributes words
                  words nil))
          (setq j (point))
          (when (> (length words) 1)
            (setq word  (nth 0 words)
                  r     30
                  x 0 y 0)
            (push (make-point :x x :y y :r r
                              :fill (random-color-html)
                              :old-x 0 :old-y 0
                              :text word
                              :image (nth 5 words)
                              :href word :title word)
                  points)
            (when (eq group 'sector)
              (if (setq child (assoc (nth 3 words) children))
                  (setcdr child (cons (car points) (cdr child)))
                (push (list (nth 3 words) (car points)) children))
              ;; (message "%s" children)
              )
            ;; (message "%s" points)
            ))
        ;; (setq points (sort points (lambda (a b) (> (point-r a) (point-r b)))))
        (setq points (nreverse points))
        (pcase prefix
          (1
           (setq root (mapcar 'cdr children)
                 group (mapcar 'car children)))
          (_ (setq root points)))
        ))
    (setq graph-draw-group t
          graph-draw-group-fn (lambda (d i)
                                (if (and (> d 0) (> (length (nth i root)) 1))
                                    (format "%s" (nth i group)))
                                ;; (format "%s, %s" d i)
                                ;; (message "%s, %s, %s %s" d i (nth i group) (nth i root))
                                ))
    (special-mode)
    (use-local-map (let* ((map (make-sparse-keymap)))
                     (define-key map "i" 'svg-animate4-1)
                     map))

    (setq point (graph-draw-tree root image)
          r (point-r point)
          w (* 2 r))
    (dom-set-attribute image 'viewBox
                       (format "%d %d %d %d"
                               (- (point-x point) r)
                               (- (point-y point) r)
                               w w))

    (svg-possibly-update-image image)
    ;; (svg-animation-run image)
    ))

(defun svg-animate4-1 ()
  (interactive)
  (svg-animate4 1))

(provide 'svg)))))


(define graphical-browser-configuration
  (home-emacs-configuration
   (packages (list (specification->package "icecat")
		   (specification->package "ublock-origin-icecat")
		   (specification->package "passff-icecat")
		   (specification->package "emacs-atomic-chrome")
		   ))
   (init '(#;(use-package exwm-firefox
	   :config (exwm-firefox-mode))
	   (use-package atomic-chrome
			:config (atomic-chrome-start-server))))))

(define pdf-tools-configuration
  (home-emacs-configuration
   (packages (list (specification->package "emacs-pdf-tools")
		   (specification->package "emacs-nov-el")))
   (init '((pdf-tools-install)
	   (defvar *current-mode* 'light)
	   (defun pdf-view-redisplay (&optional window)
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
   (packages (list (specification->package "pinentry")
		   (specification->package "emacs-pinentry")
		   (specification->package "pinentry-emacs")
		   (specification->package "password-store")
		   pass-import
		   (specification->package "pass-otp")
		   (specification->package "emacs-pass")
		   (specification->package "emacs-password-store")
		   (specification->package "emacs-password-store-otp")
		   (specification->package "gnupg")
		   (specification->package "openssh")
		   (specification->package "openconnect")))
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
	      (specification->package "mpv")
	      (specification->package "yt-dlp")
	      (specification->package "emacs-elfeed")
	      ((options->transformation '((with-branch . "emacs-elfeed-tube=master")))
	       emacs-elfeed-tube)
	      (specification->package "curl")))
   (init '((setq elfeed-feeds '(("https://almostsuremath.com/feed/" math almost-sure)
				("https://karthinks.com/index.xml" crafter karthinks)
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

				("https://www.youtube.com/feeds/videos.xml?channel_id=UCAiiOTio8Yu69c3XnR7nQBQ" crafter david)
				("https://www.youtube.com/feeds/videos.xml?channel_id=UCuj_loxODrOPxSsXDfJmpng" crafter andrew)
				
				("https://www.youtube.com/feeds/videos.xml?channel_id=UCkdmU8hGK4Fg3LghTVtKltQ" japanese cure-dolly)
				("https://www.youtube.com/feeds/videos.xml?channel_id=UC2_krAagEXVPftDXZCDiVZA" japanese kanamenaito)
				("https://www.youtube.com/feeds/videos.xml?channel_id=UCiX01KrL5XyKsxhjRhCC7oA" japanese takashi)

				("https://www.youtube.com/feeds/videos.xml?channel_id=UC2Zs9v2hL2qZZ7vsAENsg4w" learning sung)
				("https://www.youtube.com/feeds/videos.xml?channel_id=UCjmynbA3C3Tm0koVy_8pfLw" learning sung)
				
				("https://www.youtube.com/feeds/videos.xml?channel_id=UCm0MFprGs8VWcfsq743FJ7A" lecture machine-learning washington intro)
				("https://www.youtube.com/feeds/videos.xml?playlist_id=PL5B3KLQNAC5j46Ro64xF7hLV6Uf-gHUHL" lecture continuum-mechanics)
				("https://www.youtube.com/feeds/videos.xml?playlist_id=PLoROMvodv4rMiGQp3WXShtMGgzqpfVfbU" lecture machine-learning statistics ng)
				("https://www.youtube.com/feeds/videos.xml?playlist_id=PLhhyoLH6IjfxVOdVC1P1L5z5azs0XjMsb" lecture machine-learning tensorflow intro)
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
	   (require 'elfeed-tube)
	   (require 'elfeed-tube-fill)
	   (elfeed-tube-setup)
	   (setq-default elfeed-search-filter "")
	   (setq-default elfeed-search-title-max-width 100)
	   (setq-default elfeed-search-title-min-width 100)

	   (defun browse-url-mpv (url &optional new-window)
	     (start-process "mpv" "*mpv*" "mpv" "--ytdl-format=\"bestvideo[height<?720]\"" url))

	   (defun browse-url-mpv (url &optional new-window)
	     (start-process "mpv" "*mpv*" "mpv" "--ytdl-format=mp4" url))

	   (defun browse-url-mpv (url &optional new-window)
	     (start-process "mpv" "*mpv*" "mpv" "--ytdl-format=bestvideo[height<=?720]+bestaudio/best" url))

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
	      (specification->package "alsa-utils")
	      (specification->package "pavucontrol")
	      (specification->package "ffmpeg")
	      (specification->package "emacs-alsamixer-el")
	      (specification->package "emacs-bluetooth")
	      (specification->package "emacs-emms")))
   (init '((require 'emms-setup)
	   (emms-all)
	   (setq emms-player-list '(emms-player-mpv)
		 emms-info-functions '(emms-info-native))

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
	   (emms-add-directory-tree "~/music/persona_3_reload/")
	   (emms-shuffle)
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
	   (global-set-key (kbd "<f1>") 'alsamixer-toggle-mute)))))

(define email-configuration
  (home-emacs-configuration
   (packages (list
	      ((options->transformation
		'((with-branch . "emacs-org-msg=1.12")
		  (with-git-url . "emacs-org-msg=https://github.com/danielfleischer/org-msg.git")))
	       (specification->package "emacs-org-msg"))
	      (specification->package "isync")
	      (specification->package "mu")
	      (specification->package "emacs-mu4e-alert")
	      (specification->package "msmtp")))
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
	   (setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil title:nil email:nil tex:imagemagick"
		 org-msg-startup "hidestars indent inlineimages"
		 org-msg-greeting-fmt "\nAloha%s,\n\n"
		 org-msg-recipient-names '(("zaijab2000@gmail.com" . "Zain")
					   ("pyw@hawaii.edu" . "Dr. Washington")
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
	      (specification->package "emacs-org-roam")
	      emacs-org-roam-ui
	      (specification->package "emacs-org-roam-bibtex")
	      (specification->package "emacs-org-fc")
	      (specification->package "emacs-org-drill")
	      (specification->package "emacs-kanji")))
   (init '((use-package org)

	   (use-package org-roam
			:after org
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
			(add-hook 'org-fc-review-flip-mode-hook (function meow-motion-mode))
			(add-hook 'org-fc-after-review-hook (function meow-normal-mode))
			(defun org-fc-review-cram (context)
			  "Start a review session for all cards in CONTEXT.
Called interactively, prompt for the context.
Valid contexts:
- 'all, all cards in `org-fc-directories'
- 'buffer, all cards in the current buffer
- a list of paths"
			  (interactive (list (org-fc-select-context)))
			  (if org-fc-review--session
			      (when (yes-or-no-p "Flashcards are already being reviewed. Resume? ")
				(org-fc-review-resume))
			      (let* ((index (org-fc-index context))
				     (cards (identity index))
				     (order
				      (or
				       (plist-get context :order)
				       (if org-fc-shuffle-positions 'shuffled 'ordered)))
				     (scheduler
				      (cl-case order
					       (ordered (org-fc-scheduler))
					       (shuffled (org-fc-scheduler-shuffled))
					       (t (error "Unknown review order %s" order)))))
				(if (null cards)
				    (message "No cards due right now")
				    (progn
				     (org-fc-scheduler-init scheduler cards)
				     (setq org-fc-review--session
					   (org-fc-make-review-session scheduler))
				     (run-hooks 'org-fc-before-review-hook)
				     (org-fc-review-next-card)))))))
	   
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
				 (org-fc-type-cloze-init 'deletion)))

	   ))))

(define website-configuration
  (home-emacs-configuration
   (packages (list 
	      (specification->package "python-pygments")
	      (specification->package "emacs-engrave-faces")))
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
	   (setq texmathp-tex-commands '(("lflalign" env-on)))
	   (setq org-latex-compiler "xelatex")
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
		    :include ("20220925152629-index.org"
			      "20220925155207-about.org"
			      "20230225143306-posts.org"
			      "20230225142818-notation.org"
			      "20230225142533-category_theory.org") 
		    :with-toc nil
		    :exclude-tags ("draft")
		    :html-head
		    "<title></title><link rel=\"stylesheet\" href=\"static/css/site.css\" type=\"text/css\"/>\n<header><div class=\"menu\"><ul>\n<li><a href=\"/\">/</a></li>\n<li><a href=\"/about\">/about</a></li>\n<li><a href=\"/posts\">/posts</a></li></ul></div></header><script src=\"static/js/nastaliq.js\"></script>"
		    :recursive t
		    :html-postamble nil
		    :html-mathjax-template "
<script>
  MathJax = {
    loader: {
      load: ['[custom]/xypic.js'],
      paths: {custom: 'https://cdn.jsdelivr.net/gh/sonoisa/XyJax-v3@3.0.1/build/'}
    },
    tex: {
      packages: {'[+]': ['xypic']},
      macros: {
        R: \"{\\\\bf R}\"
      }
    }
  };
</script>
<script id=\"MathJax-script\" async src=\"https://cdn.jsdelivr.net/npm/mathjax@3.2.2/es5/tex-chtml-full.js\"></script>
"
		    )
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
   (packages (list (specification->package "emacs-org-fragtog")
		   (specification->package "emacs-org-modern")
		   (specification->package "emacs-cdlatex")
		   (specification->package "font-latin-modern")
		   (specification->package "emacs-tempel")
		   (specification->package "emacs-valign")
		   (specification->package "emacs-org-present")
		   (specification->package "emacs-org-tree-slide")
		   (specification->package "emacs-consult-org-roam")
		   (specification->package "emacs-calfw")
		   ((options->transformation
		     '((with-branch . "emacs-calfw-blocks=master")))
		    emacs-calfw-blocks)
		   emacs-phscroll
		   (specification->package "texlive")
		   (specification->package "texlive-bin")
		   (specification->package "imagemagick")
		   ((options->transformation '((with-source . "enchant=https://github.com/AbiWord/enchant/releases/download/v2.3.1/enchant-2.3.1.tar.gz")))
		    (specification->package "emacs-jinx"))

		   
		   (specification->package "nuspell")
		   (specification->package "aspell")
		   (specification->package "hunspell")
		   (specification->package "hunspell-dict-en")
		   
		   ))
   (init '((require 'org)
	   (require 'org-tree-slide)
	   (setq org-tree-slide-cursor-init nil)
	   (require 'ox)
	   (require 'calfw)
	   (require 'calfw-org)
	   (require 'calfw-blocks)
	   (setq org-startup-folded t)
	   
	   (add-hook 'emacs-startup-hook (function global-jinx-mode))
	   (keymap-global-set "M-$" (function jinx-correct))
	   (keymap-global-set "C-M-$" (function jinx-languages))
	   
	   ;; (setq ispell-program-name (executable-find "hunspell")
	   ;; 	 ispell-dictionary   "en_US")
					;(setq ispell-alternate-dictionary "/home/zjabbar/.guix-home/profile/share/hunspell/en_US.dic")

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
	   (global-org-modern-mode)
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


	   (custom-set-variables '(org-modern-table nil))
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
	   
	   (setq org-agenda-files '("/home/zjabbar/notes/20211224040925-todo.org"))
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
	   (setq org-confirm-babel-evaluate nil)
	   (setq org-startup-with-latex-preview t)
	   (setq org-preview-latex-default-process 'dvisvgm)
	   (add-hook 'org-mode-hook 'org-fragtog-mode)
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
	   
	   (setq org-format-latex-options '(:foreground default
					    :background default
					    :scale 2
					    :html-foreground "Black"
					    :html-background "Transparent"
					    :html-scale 1.0
					    :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))

	   (setq org-latex-pdf-process '("xelatex -interaction nonstopmode -output-directory %o %f"))
	   (setf
	    (plist-get
	     (alist-get 'dvisvgm org-preview-latex-process-alist)
	     :latex-compiler)
	    '("xelatex -no-pdf -interaction -nonstopmode -shell-escape -output-directory %o %f")
	    (plist-get
	     (alist-get 'dvisvgm org-preview-latex-process-alist)
	     :image-input-type)
	    "xdv"
	    (plist-get
	     (alist-get 'dvisvgm org-preview-latex-process-alist)
	     :image-converter)
	    '("dvisvgm %f --no-fonts --exact-bbox -n -b min -c %S -o %O"))

	   (setf (alist-get :title org-export-options-alist) '("TITLE" nil "Maybe, में भि میں بھی, 明媚." t))
	   (setf (alist-get :with-latex org-export-options-alist) '("t" "tex" (function org-export-with-latex)))))))

(define python-configuration
  (home-emacs-configuration
   (packages (list
	      (specification->package "python")
	      (specification->package "jupyter")

	      ((options->transformation '((with-git-url . "emacs-jupyter=https://github.com/emacs-jupyter/jupyter.git")))
	       (specification->package "emacs-jupyter"))
	      
	      (specification->package "emacs-pydoc")
	      (specification->package "python-lsp-server")
	      (specification->package "tree-sitter")
	      (specification->package "tree-sitter-python")

	      (specification->package "emacs-csv-mode")
	      (specification->package "emacs-py-isort")
	      (specification->package "emacs-python-black")

	      (specification->package "pandoc")
	      
	      (specification->package "python-numpy")	      
	      (specification->package "python-pandas")
	      (specification->package "python-matplotlib")
	      (specification->package "python-scipy")
	      (specification->package "python-sympy")
	      (specification->package "python-scikit-learn")))
   (init '((require 'jupyter)
	   (defun gm/jupyter-api-request-xsrf-cookie-error-advice (func &rest args)
	     (condition-case nil
			     (apply func args)
			     (jupyter-api-http-error nil)))
	   (advice-add 'jupyter-api-request-xsrf-cookie :around (function gm/jupyter-api-request-xsrf-cookie-error-advice))
	   (setq jupyter-use-zmq nil)
	   (setq org-babel-python-command "python3"
		 python-interpreter "python3"
		 python-shell-interpreter "python3"
		 treesit-extra-load-path '("/home/zjabbar/.guix-home/profile/lib/tree-sitter"))
	   
	   (add-hook 'python-mode-hook (function run-python))
	   (add-hook 'python-mode-hook (function python-black-on-save-mode))
	   (add-hook 'python-mode-hook (function eglot-ensure))
	   
	   (org-babel-do-load-languages 'org-babel-load-languages '((scheme .t)
								    (python . t)
								    (sql . t)
								    (eshell . t)
								    (shell . t)
								    (jupyter . t)))
	   (add-to-list 'org-src-lang-modes (cons "python3" 'python))))))

(define lisp-configuration
  (home-emacs-configuration
   (packages (list 
	      (specification->package "sicp")
	      (specification->package "xdot")
	      (specification->package "emacs-guix")
	      (specification->package "tree-sitter-scheme")
	      (specification->package "emacs-srfi")
	      (specification->package "emacs-arei")
	      (specification->package "guile-ares-rs")
	      (specification->package "guile-next")))
   (init '(#;(with-eval-after-load 'guix-repl
				 (setq guix-guile-program  '("guix" "repl")
				       guix-config-scheme-compiled-directory  nil
				       guix-repl-use-latest  nil
				       guix-repl-use-server  nil))

	   (require 'guix)
	   (global-guix-prettify-mode)

	   (setq geiser-mode-auto-p nil)

	   (defun arei-server-start () "Start Arei with Default Port" (interactive)
	     (async-shell-command "guix shell guile-next guile-ares-rs -- guile -c '(begin (use-modules (guix gexp)) ((@ (ares server) run-nrepl-server) #:port 7888))'"))

	   (defun arei-server-start-guix-repl () "Start Arei with Default Port" (interactive)
	     (async-shell-command "guix shell guile-next guile-ares-rs -- echo '(begin (use-modules (guix gexp)) ((@ (ares server) run-nrepl-server) #:port 7888))' | guix repl"))

	   (defun auto-start-arei ()
	     (if (string= "" (shell-command-to-string "sudo ss -tulpn | grep LISTEN.*7888"))
		 (progn (arei-server-start))))

	   (add-hook 'scheme-mode-hook (function auto-start-arei))
	   (add-hook 'scheme-mode-hook (function arei-mode))
	   (remove-hook 'scheme-mode-hook (function geiser-mode--maybe-activate))

	   
	   (setq user-full-name "Zain Jabbar")
	   (setq user-mail-address "zaijab2000@gmail.com")
	   (add-hook 'scheme-mode-hook 'guix-devel-mode)
	   (setq safe-local-variable-values '((eval modify-syntax-entry 43 "'")
					      (eval modify-syntax-entry 36 "'")
					      (eval modify-syntax-entry 126 "'")))
	   (add-hook 'after-init-hook 'envrc-global-mode)
	   (with-eval-after-load 'envrc (define-key envrc-mode-map (kbd "C-c e") 'envrc-command-map))))))


(define blight-configuration '())
(if (string= (read-delimited "\n" (open-input-pipe "echo $HOSTNAME")) "euler")
    (set! blight-configuration
	  (home-emacs-configuration
	   (packages (list (specification->package "emacs-blight")))
	   (init '((shell-command "sudo chmod 777 /sys/class/backlight/amdgpu_bl0/brightness")
		   (require 'blight)
		   (setq my/blight (blight-sysfs))
		   (blight-sysfs :min 0)
		   (global-set-key (kbd "<XF86MonBrightnessDown>") (blight-step my/blight -10))
		   (global-set-key (kbd "<XF86MonBrightnessUp>") (blight-step my/blight 10))
		   (global-set-key (kbd "<f5>") (blight-step my/blight -10))
		   (global-set-key (kbd "<f6>") (blight-step my/blight 10)))))))

(define exwm-configuration
  (home-emacs-configuration
   (packages (list
	      (specification->package "jami")
	      (specification->package "emacs-exwm")
	      (specification->package "emacs-windsize")
	      (specification->package "emacs-vterm")
	      (specification->package "unclutter")
	      (specification->package "xhost")
	      (specification->package "xrandr")
	      (specification->package "arandr")))
   (init '((require 'exwm)
	   (require 'xelb)
	   (require 'windsize)
	   
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
		((and (string-match-p (regexp-quote "scratch") (buffer-name)) (< 1 (length buffer-list))) (tabspaces-switch-to-buffer (cadr buffer-list)))
		((and (not (string-match-p (regexp-quote "scratch") (buffer-name))) (< 1 (length buffer-list))) (kill-current-buffer))
		((and (string-match-p (regexp-quote "scratch") (buffer-name)) (= 1 (length buffer-list))) (set-buffer-modified-p nil) (erase-buffer))
		((and (not (string-match-p (regexp-quote "scratch") (buffer-name))) (= 1 (length buffer-list))) (let ((buf (current-buffer))) (scratch-buffer) (kill-buffer buf))))))

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

	   (global-set-key (kbd "s-e") (function
					(lambda () (interactive)
						(start-process-shell-command
						 "icecat"
						 nil
						 "icecat"))))
	   (global-set-key (kbd "s-E") (function
					(lambda () (interactive)
						(start-process-shell-command
						 "icecat --private-window http://localhost:8080"
						 nil
						 "icecat --private-window http://localhost:8080"))))
	   (global-set-key (kbd "s-v") (function
					(lambda () (interactive)
					(start-process-shell-command "Kanji Dojo" nil "guix shell jbr coreutils --preserve='^LD_LIBRARY_PATH$' --preserve='^DISPLAY$' -- java -jar /home/zjabbar/notes/data/kanji-linux-x64-2.0.7.jar"))))
	   (global-set-key (kbd "s-r") (function eshell))
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
	   (global-set-key (kbd "s-b") (function
					(lambda () (interactive)
						(find-file (read-file-name "" "~/books/")))))
	   (global-set-key (kbd "s-n") 'org-roam-node-find)
	   (global-set-key (kbd "s-i") 'org-roam-node-insert)
	   (global-set-key (kbd "s-N") 'org-roam-dailies-capture-today)
	   (global-set-key (kbd "C-x C-t") 'vterm)
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
	   
	   (setq exwm-input-prefix-keys (append super-keys '(XF86AudioRaiseVolume
							     XF86AudioLowerVolume
							     XF86AudioNext
							     XF86AudioPlay
							     XF86AudioPrev
							     XF86AudioMute
							     XF86MonBrightnessDown
							     XF86MonBrightnessUp)))
					;(define-key exwm-mode-map (kbd "C-c") nil)
	   
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

(define theme-configuration
  (home-emacs-configuration
   (init '((load-theme 'modus-operandi t)))))

(define font-configuration
  (home-emacs-configuration
   (init '((set-face-attribute 'default nil :font "Iosevka-14")
	   (set-fontset-font nil 'tibetan "Iosevka")
	   (set-fontset-font nil 'symbol "Iosevka")
	   (set-fontset-font nil 'han "IPAmjMincho")
	   (set-fontset-font nil 'kana "IPAmjMincho")
	   (set-fontset-font nil 'cjk-misc "IPAmjMincho")))))

(define ui-configuration
  (home-emacs-configuration
   (packages (list (specification->package "emacs-rainbow-delimiters")
                   (specification->package "emacs-which-key")))
   (early-init '((setq gc-cons-threshold 800000
		       package-enable-at-startup nil
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
   (init '(
	   (require 'phscroll)
	   (setq org-startup-truncated nil)
	   (with-eval-after-load "org" (require 'org-phscroll))

	   (tab-bar-mode)
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
	   (which-key-mode)))))

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

