;;; -*- lexical-binding: t -*-

;; General

;; Rust
(use-package rust-mode
  :mode ("\\.rs\\'"   . rust-mode)
        ("\\.rlib\\'" . rust-mode))

;; Clojure
(use-package clojure-mode
  :mode ("\\.clj\\'"  . clojure-mode)
        ("\\.cljs\\'" . clojurescript-mode)
        ("\\.cljc\\'" . clojurec-mode))

(use-package rainbow-delimiters
  :after (:any clojure-mode lisp-mode)
  :hook ((clojure-mode . rainbow-delimiters-mode)
         (emacs-lisp-mode . rainbow-delimiters-mode)))

(use-package cider
  :after clojure-mode)

;; Haskell
(use-package haskell-mode
  :mode ("\\.hs\\'" . haskell-mode)
  :hook (haskell-mode . interactive-haskell-mode))

;; Elm
(use-package elm-mode
  :mode ("\\.elm\\'" . elm-mode))

;; Web mode
(use-package web-mode
  :mode
  ("\\.html?\\'" . web-mode)
  ("\\.js[x]?\\'" . web-mode)
  ("\\.ts[x]?\\'" . web-mode)
  ("\\.json?\\'" . web-mode)
  :config
  (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil))
  (setq web-mode-enable-auto-quoting nil)
  (setq web-mode-content-types-alist
    '(("jsx" . "/\\(rn\\|component\\)[s]?/.*\\.js[x]?\\'"))))

;; Markdown
(use-package markdown-mode
  :mode
  ("\\.md?\\'" . markdown-mode))

(provide 'lang)
