;;; -*- lexical-binding: t -*-

;;; init.el -- emacs meets vim

;; Path to user directory
(add-to-list 'load-path (expand-file-name "user" user-emacs-directory))

;; Load configurations
(require 'packages)
(require 'keybinds)
(require 'mode-line)
(require 'lang)
(require 'misc-configs)
(require 'user-env)
