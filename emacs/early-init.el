;;; -*- lexical-binding: t -*-

;; Set garbage collection
(setq gc-cons-threshold (* 128 1024 1024))
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 64 1024 1024))))

(setq package-enable-at-startup nil)
(advice-add #'package--ensure-init-file :override #'ignore)

;; Turn these off early to avoid seeing them at all
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Inhibit frame resizing when font size is different from system default
(setq frame-inhibit-implied-resize t)

