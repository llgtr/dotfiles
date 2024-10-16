;;; -*- lexical-binding: t -*-

(setq straight-repository-branch "develop")

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
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
  (evil-set-undo-system 'undo-redo)
  (setq undo-tree-enable-undo-in-region nil))

(use-package evil-magit
  :hook (magit-mode . evil-magit-init)
  :init
  (setq evil-magit-state 'normal))

(use-package base16-theme
  :demand t
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
  (setq aw-scope 'frame
        aw-dispatch-alist
        '((?d aw-delete-window "Delete Window")
          (?s aw-swap-window "Swap Windows")
          (?m aw-move-window "Move Window")
          (?D delete-other-windows "Delete Other Windows")
          (?h aw-show-dispatch-help))))

(use-package which-key
  :demand t
  :config
  (which-key-mode)
  (setq which-key-idle-secondary-delay 0))

(use-package projectile
  :demand t
  :init (recentf-mode)
  :config
  (setq projectile-completion-system 'ivy
        projectile-project-root-files-functions '(projectile-root-bottom-up))
  (projectile-mode))

(use-package editorconfig
  :demand t
  :config (editorconfig-mode))

(use-package dumb-jump
  :commands (dumb-jump-go dumb-jump-back dumb-jump-quick-look)
  :config
  (setq dumb-jump-selector 'ivy)
  (setq dumb-jump-force-searcher 'rg))

(use-package hl-todo
  :demand t
  :config (global-hl-todo-mode))

(use-package magit
  :commands magit-status magit-blame)

(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("*.el"))
  :bind (:map copilot-completion-map ("TAB" . copilot-accept-completion)))

(provide 'packages)
