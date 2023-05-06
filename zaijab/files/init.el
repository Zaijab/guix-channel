(setq custom-file (concat user-emacs-directory "/custom.el"))

(require 'system-packages)

(use-package system-packages
  :ensure t)

(use-package use-package-ensure-system-package
  :ensure t)

(use-package orderless
  :ensure-system-package emacs-orderless
  :init (setq completion-styles '(orderless basic)
	      completion-category-overrides '((file (styles basic partial-completion)))
	      orderless-smart-case nil
	      completion-ignore-case t
	      read-file-name-completion-ignore-case t
	      read-buffer-completion-ignore-case t))

