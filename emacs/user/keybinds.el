;;; -*- lexical-binding: t -*-

(defvar main-leader-key "SPC")
(defvar alt-leader-key "M-SPC")
(defvar main-major-mode-leader-key ",")

(use-package general
  :demand t
  :config
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix main-leader-key
   :non-normal-prefix alt-leader-key ; When in insert or emacs mode
   :keymaps 'override

   ;; General binds
   "SPC" '(execute-extended-command :wk "counsel-M-x")
   "TAB" 'ace-window
   "!" 'shell-command
   "&" 'async-shell-command
   "?" 'emacs-init-time
   "0" 'text-scale-adjust
   ":" 'eval-expression

   ;; Applications
   "a" '(:ignore t :wk "Applications")
   "au" 'undo-tree-visualize
   "at" 'tetris

   ;; Buffers
   "b" '(:ignore t :wk "Buffers")
   "bb" 'ivy-switch-buffer
   "bd" 'kill-buffer
   "bw" 'read-only-mode
   "bl" 'list-buffers
   "bp" 'previous-buffer
   "bn" 'next-buffer

   ;; Comments
   "c" '(:ignore t :wk "Comments")
   "cr" 'comment-region
   "cl" 'comment-line

   ;; Errors
   "e" '(:ignore t :wk "Errors")
   "en" 'flycheck-next-error
   "ep" 'flycheck-previous-error
   "el" 'flycheck-list-errors

   ;; Files
   "f" '(:ignore t :wk "Files")
   "fr" 'recentf-open-files

   ;; Help
   "h" '(:ignore t :wk "Help")
   "hw" 'which-key-show-top-level
   "hm" 'which-key-show-major-mode
   "hdv" 'counsel-describe-variable
   "hdf" 'counsel-describe-function
   "hdF" 'counsel-describe-face
   "hdk" 'describe-key
   "hdp" 'describe-package
   "hdm" 'describe-mode

   ;; Insert
   "i" '(:ignore t :wk "Insert")
   "ic" 'insert-char
   "iy" 'yas-insert-snippet

   ;; Jump
   "j" '(:ignore t :wk "Jump")
   "jc" 'evil-avy-goto-char-2
   "jw" 'evil-avy-goto-word-1
   "jl" 'evil-avy-goto-line
   "jd" 'dumb-jump-go
   "jb" 'dumb-jump-back
   "jq" 'dumb-jump-quick-look

   ;; LSP
   "l" '(:ignore t :wk "LSP")
   "ld" 'lsp-describe-session
   "le" 'lsp-execute-code-action
   "lfd" 'lsp-find-defition
   "lfr" 'lsp-find-references
   "lF" 'lsp-format-buffer
   "lg" 'lsp-ui-doc-glance
   "lr" 'lsp-rename
   "ls" 'lsp

   ;; Projectile / project
   "p" '(:ignore t :wk "Project")
   "p!" 'projectile-run-shell-command-in-root
   "p&" 'projectile-run-async-shell-command-in-root
   "pd" 'projectile-dired
   "pf" 'projectile-find-file
   "pr" 'projectile-recentf
   "pR" 'projectile-replace
   "pp" 'projectile-switch-project
   "pk" 'projectile-kill-buffers

   ;; Registers
   "r" '(:ignore t :wk "Registers")
   "re" 'evil-show-registers
   "ry" 'counsel-yank-pop

   ;; Search
   "s" '(:ignore t :wk "Registers")
   "sa" 'counsel-ag
   "sr" 'counsel-rg
   "sg" 'counsel-git-grep

   ;; Toggles
   "t" '(:ignore t :wk "Toggles")
   "tw" 'whitespace-mode

   "T" '(:ignore t :wk "UI toggles")
   "Tt" 'tool-bar-mode
   "Tm" 'menu-bar-mode

   ;; Windows
   "w" '(:ignore t :wk "Windows")
   "w\"" 'split-window-below
   "w%" 'split-window-right
   "w=" 'balance-windows
   "wd" 'delete-window
   "wD" 'delete-other-windows
   "ws" 'ace-swap-window

   ;; Quit
   "q" '(:ignore t :wk "Quit")
   "qq" 'evil-quit
   "qQ" 'evil-quit-all)

  ;; Org-mode
  (general-define-key
   :states '(normal visual)
   :prefix main-major-mode-leader-key
   :keymaps 'org-mode-map

   "." 'org-time-stamp
   "," 'org-priority
   ";" 'org-toggle-comment
   "<" 'org-date-from-calendar
   "d" 'org-deadline
   "e" 'org-export-dispatch
   "l" 'org-insert-link
   "s" 'org-schedule
   "T" 'org-todo
   "#" 'org-update-statistics-cookies
   "i" 'org-clock-in
   "o" 'org-clock-out
   "r" 'org-clock-report
   "b" 'org-toggle-checkbox

   ;; Tables
   "tc" 'org-table-create-or-convert-from-region
   "tk" 'org-table-move-row-up
   "tj" 'org-table-move-row-down
   "th" 'org-table-move-column-left
   "tl" 'org-table-move-column-right
   "tir" 'org-table-insert-row
   "tic" 'org-table-insert-column
   "tdr" 'org-table-kill-row
   "tdc" 'org-table-delete-column)

  ;; Clojure(script)-mode
  (general-define-key
   :states '(normal visual)
   :prefix main-major-mode-leader-key
   :keymaps 'clojure-mode-map

   "c" 'cider-jack-in
   ;; TODO: cider bindings
   )

  ;; Editable dired
  (general-define-key
   :states '(normal visual)
   :prefix main-major-mode-leader-key
   :keymaps 'wdired-mode-map

   "s" 'wdired-finish-edit)

  ;; Web-mode
  (general-define-key
   :states '(normal visual)
   :prefix main-major-mode-leader-key
   :keymaps 'web-mode-map

   "f" 'web-mode-fold-or-unfold
   "w" 'web-mode-whitespaces-show
   "r" 'web-mode-reload
   "i" 'web-mode-buffer-indent
   "ei" 'web-mode-element-insert
   "ek" 'web-mode-element-kill
   "en" 'web-mode-element-next
   "ep" 'web-mode-element-previous
   "ec" 'web-mode-element-close))

(provide 'keybinds)
