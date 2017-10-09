;;; -*- lexical-binding: t -*-

;; Scroll like vim
(setq scroll-margin 10
      scroll-conservatively 10000
      scroll-step 1)

;; Indent with spaces rather than tabs by default
(setq-default indent-tabs-mode nil)

;; Set font
(add-to-list 'default-frame-alist '(font . "Hack 11"))

;; Appearance
(blink-cursor-mode 0)
(set-fringe-mode 0)
(global-linum-mode t)
(show-paren-mode 1)
(setq inhibit-startup-message t)
(setq initial-scratch-message
      (format ";;
;;                        Welcome to emacs %s       ,           ,
;;           ___     ___     //                        /             \\
;;          /   \\~~~/   \\   //                        ((__-^^-,-^^-__))
;;    ,----(     .  .    )                             `-_---' `---_-'
;;   /      \\___     ___/                               `--|o` 'o|--'
;;  /|          (\\   |(                                    \\  `  /
;; ^ \\   /___\\   /\\  |                                      ): :(
;;    |__|    |__|-''                                       :o_o:
;;                                                           \"-\"
"
              emacs-version))

;; Padding for linum
(defun linum-format-func (line)
  (let ((w (length (number-to-string (count-lines (point-min) (point-max))))))
    (propertize (format (format "%%%dd " w) line) 'face 'linum)))
(setq linum-format 'linum-format-func)

;; Familiar behaviour for alt-keys
(when (eq system-type 'darwin)
  (setq mac-option-key-is-meta t)
  (setq mac-right-option-modifier nil))

(provide 'init-misc-configs)
