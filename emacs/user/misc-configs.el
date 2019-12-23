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

;; Prompt y/n instead of yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Helper function that reads envfile created by emacs-env.sh. Mostly copied
;; from doom-emacs.
(defun read-env-file ()
  (let ((env-file (expand-file-name "envfile" user-emacs-directory)))
    (if (file-exists-p env-file)
        (let (envvars environment)
          (with-temp-buffer
            (insert-file-contents env-file)
            (while (re-search-forward "\n *\\([^#= \n]*\\)=" nil t)
              (push (match-string 1) envvars)
              (push (buffer-substring
                     (match-beginning 1)
                     (1- (or (save-excursion
                               (when (re-search-forward "^\\([^= ]+\\)=" nil t)
                                 (line-beginning-position)))
                             (point-max))))
                    environment)
              (when environment
                (setq process-environment
                      (append (nreverse environment) process-environment)
                      exec-path
                      (if (member "PATH" envvars)
                          (append (parse-colon-path (getenv "PATH"))
                                  (list exec-directory))
                        exec-path)
                      shell-file-name
                      (if (member "SHELL" envvars)
                          (setq shell-file-name
                                (or (getenv "SHELL") shell-file-name))
                        shell-file-name))
                envvars)
              )))
      (error (format "%s does not exist. Run emacs-env.sh" env-file))))
  )

;; MacOS things
(when (eq system-type 'darwin)
  (setq mac-option-key-is-meta t)
  (setq mac-right-option-modifier nil)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (read-env-file))

(provide 'misc-configs)
