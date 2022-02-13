(use-modules (guix transformations))

(define transform1
  (options->transformation
   '((with-branch . "emacs-next=master"))))

(define transform2
  (options->transformation
   '((with-commit . "emacs-next=5627693ce3f6ff7ef928a902bfab59731a63fbd4"))))

(packages->manifest
 (list
  ;; X Apps
  (specification->package "pavucontrol")
  (specification->package "ungoogled-chromium")
  (specification->package "ublock-origin-chromium")
  (specification->package "nyxt")

  ;; X Resources
  (specification->package "fontconfig")
  (specification->package "font-fira-code")
  ;; (specification->package "font-ipa-mj-mincho")
  ;; (specification->package "font-lohit")
  (specification->package "xauth")

  ;; Programming
  (specification->package "git")

  ;; Python
  (specification->package "python")
  (specification->package "python-pandas")
  (specification->package "python-matplotlib")
  ;; (specification->package "jupyter")
  
  ;; LISP
  (specification->package "sbcl")
  (specification->package "sbcl-iterate")
  
  ;; Emacs
  (transform2 (specification->package "emacs-next"))
  
  (specification->package "emacs-org-roam")
  (specification->package "emacs-guix")
  (specification->package "emacs-exwm")
  (specification->package "emacs-pdf-tools")
  (specification->package "emacs-lispyville")
  (specification->package "emacs-lispy")
  (specification->package "emacs-sly")
  (specification->package "emacs-doom-modeline")
  (specification->package "emacs-doom-themes")
  (specification->package "emacs-vterm")
  (specification->package "emacs-which-key")
  (specification->package "emacs-undo-tree")
  (specification->package "emacs-helm")
  (specification->package "emacs-yasnippet")
  (specification->package "emacs-rainbow-delimiters")
  (specification->package "emacs-google-translate")
  (specification->package "emacs-company")
  (specification->package "emacs-evil")
  (specification->package "emacs-projectile")
  (specification->package "emacs-no-littering")))
