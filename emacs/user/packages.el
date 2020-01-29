;;; -*- lexical-binding: t -*-

(setq straight-repository-branch "develop")

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t
      straight-check-for-modifications 'never
      use-package-always-defer t)

;; Packages
(use-package evil
  :demand t
  :config
  (evil-mode)
  (setq undo-tree-enable-undo-in-region nil))

(use-package base16-theme
  :demand t
  :init
  (add-to-list 'custom-theme-load-path (expand-file-name "pkgs/base16-theme/build" user-emacs-directory))
  :config
  (load-theme 'base16-gruvbox-dark-pale t))

(use-package ivy
  :demand t
  :config
  (ivy-mode)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 10))

(use-package counsel
  :commands
  (counsel-describe-variable
   counsel-describe-function
   counsel-describe-face
   counsel-git-grep
   counsel-ag
   counsel-rg)
  :config
  (setq counsel-ag-base-command "ag --nocolor --nogroup --width 200 %s")
  (setq counsel-rg-base-command "rg -i -M 200 --no-heading --line-number --color never %s ."))

(use-package avy
  :commands
  (evil-avy-goto-char-2
   evil-avy-goto-word-1
   evil-avy-goto-line)
  :config
  (setq avy-background t))

(use-package ace-window
  :commands (ace-window ace-swap-window)
  :config
  (setq aw-scope 'frame))

(use-package which-key
  :demand t
  :config
  (which-key-mode)
  (setq which-key-idle-secondary-delay 0))

(use-package projectile
  :demand t
  :config
  (setq projectile-completion-system 'ivy)
  (projectile-mode))

(use-package smartparens
  :demand t
  :config
  (smartparens-global-mode))

(use-package editorconfig
  :demand t
  :config (editorconfig-mode))

(use-package ox-hugo
  :after ox)

(use-package dumb-jump
  :commands (dumb-jump-go dumb-jump-back dumb-jump-quick-look)
  :config
  (setq dumb-jump-selector 'ivy)
  (setq dumb-jump-force-searcher 'rg))

(use-package lsp-mode
  :commands lsp
  :hook ((python-mode rust-mode) . lsp)
  :config
  (setq lsp-enable-snippet nil
        lsp-auto-guess-root t
        lsp-keep-workspace-alive nil
        lsp-rust-server 'rust-analyzer))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-flycheck-enable t
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-show-symbol nil
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-doc-enable nil
        lsp-ui-doc-position 'at-point))

(use-package flycheck
  :hook (lsp-mode . flycheck-mode))

(use-package company
  :commands company-mode
  :hook (lsp-mode . company-mode)
  :config
  (setq company-dabbrev-downcase 0
        company-idle-delay 0
        company-tooltip-align-annotations t))

(use-package company-lsp
  :after company
  :config
  (push 'company-lsp company-backends)
  (setq company-lsp-cache-candidates 'auto))

(use-package esup
  :commands esup)

(provide 'packages)
