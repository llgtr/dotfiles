;;; -*- lexical-binding: t -*-

;; Set defaults
(setq-default
 ;; Scroll like vim
 scroll-margin 0
 scroll-conservatively 101
 scroll-step 1
 auto-window-vscroll nil
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
(add-to-list 'default-frame-alist '(font . "Iosevka Facile 12"))

;; Appearance
(blink-cursor-mode 0)
(set-fringe-mode 0)
(show-paren-mode 1)
(setq-default display-line-numbers t)
(setq inhibit-startup-message t
      initial-major-mode 'fundamental-mode)
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

;; Prompt y/n instead of yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; MacOS things
(when (eq system-type 'darwin)
  (setq mac-option-key-is-meta t)
  (setq mac-right-option-modifier nil)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)))

(electric-pair-mode)

(provide 'misc-configs)
