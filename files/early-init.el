;; -*- lexical-binding: t; -*-

(setq gc-cons-threshold most-positive-fixnum
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
      initial-scratch-message nil
					;message-log-max nil
      )
;(kill-buffer "*Messages*")

(tooltip-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

(fset #'yes-or-no-p #'y-or-n-p)

(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq user-emacs-directory "~/.config/emacs")
(setq byte-compile-warnings '(cl-functions))
(setq make-backup-files nil)
