;;; -*- lexical-binding: t -*-

;; Set defaults
(setq-default
 ;; Scroll like vim
 scroll-margin 10
 scroll-conservatively 10000
 scroll-step 1
 ;; Indent with spaces rather than tabs by default
 indent-tabs-mode nil
 ;; Follow symlinked files without confirmation
 vc-follow-symlinks t
 ;; Whitespace mode config
 whitespace-style '(face tabs newline trailing lines)
 ;; Recentf
 recentf-max-menu-items 25
 ;; Truncate instead of wrapping
 truncate-lines t)

;; Recent files
(recentf-mode)

;; Better backup configurations
;; (Be careful with this if editing sensitive files via emacs)
(let ((backup-directory (expand-file-name "backup/" user-emacs-directory))
      (auto-save-directory (expand-file-name "auto-save-list/" user-emacs-directory)))
  ; https://www.gnu.org/software/emacs/manual/html_node/elisp/Backquote.html
  (setq backup-directory-alist `(("." . ,backup-directory))
        auto-save-file-name-transforms `((".*" ,auto-save-directory t))
        backup-by-copying t
        version-control t
        vc-make-backup-files t
        delete-old-versions -1))

;; Set font
(add-to-list 'default-frame-alist '(font . "Iosevka Term 12"))

;; Appearance
(blink-cursor-mode 0)
(set-fringe-mode 0)
(show-paren-mode 1)
(setq-default display-line-numbers t)
(setq inhibit-startup-message t)
(setq initial-scratch-message
      (format ";;
;;                        Welcome to emacs %s
;;           ___     ___     //                        /             \\
;;          /   \\~~~/   \\   //                        ((__-^^-,-^^-__))
;;    ,----(     .  .    )                             '-_---' '---_-'
;;   /      \\___     ___/                               '--|o' 'o|--'
;;  /|          (\\   |(                                    \\  '  /
;; ^ \\   /___\\   /\\  |                                      ): :(
;;    |__|    |__|-''                                       :o_o:
;;                                                           \"-\"
"
              emacs-version))

;; Start in fullscreen mode
(custom-set-variables '(initial-frame-alist '((fullscreen . maximized))))

;; Prompt y/n instead of yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; MacOS things
(when (eq system-type 'darwin)
  (setq mac-option-key-is-meta t)
  (setq mac-right-option-modifier nil)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)))

(use-package exec-path-from-shell
  :if (eq system-type 'darwin)
  :defer 1
  :config (exec-path-from-shell-initialize))

(provide 'misc-configs)
