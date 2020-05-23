;;; -*- lexical-binding: t -*-

;; General

;; LSP and friends
(use-package lsp-mode
  :commands lsp
  :hook ((python-mode rust-mode dart-mode web-mode) . lsp)
  :init
  (setq read-process-output-max (* 1024 1024))
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

(use-package lsp-dart
  :after (dart-mode lsp-mode))

(use-package flycheck
  :hook (lsp-mode . flycheck-mode))

(use-package company
  :commands company-mode
  :hook (lsp-mode . company-mode)
  :config
  (setq company-dabbrev-downcase 0
        company-idle-delay 0
        company-tooltip-align-annotations t)
  :bind
  (:map company-active-map
        ("<tab>" . company-complete-selection)))

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

(use-package dart-mode
  :mode
  ("\\.dart?\\'" . dart-mode))

(provide 'lang)
