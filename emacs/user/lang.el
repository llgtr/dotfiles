;;; -*- lexical-binding: t -*-

;; General

;; LSP and friends
(use-package lsp-mode
  :commands lsp
  :hook ((python-mode rust-mode dart-mode typescript-mode web-mode js-mode) . lsp)
  :init
  (setq read-process-output-max (* 1024 1024)
        lsp-headerline-breadcrumb-enable nil
        lsp-completion-provider :capf
        lsp-enable-snippet nil
        lsp-auto-guess-root t
        lsp-keep-workspace-alive nil
        lsp-eldoc-hook nil
        lsp-enable-on-type-formatting nil
        lsp-enable-indentation nil
        lsp-enable-folding nil
        lsp-enable-file-watchers nil
        lsp-enable-text-document-color nil
        lsp-enable-semantic-highlighting nil
        lsp-signature-auto-activate nil
        lsp-log-io nil
        lsp-rust-server 'rust-analyzer))

(use-package lsp-ui
  :after lsp-mode
  :init
  (setq lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-show-code-actions nil
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-delay 0.5
        lsp-ui-sideline-enable nil
        lsp-ui-doc-enable nil))

(use-package flycheck
  :hook (lsp-mode . flycheck-mode))

(use-package company
  :commands company-mode
  :hook (lsp-mode . company-mode)
  :config
  (setq company-idle-delay 0.0
        company-minimum-prefix-length 2
        company-tooltip-limit 5
        company-tooltip-align-annotations t
        company-require-match 'never
        company-auto-complete-chars nil
        company-dabbrev-other-buffers nil
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil)
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

;; Haskell
(use-package haskell-mode
  :mode ("\\.hs\\'" . haskell-mode)
  :hook (haskell-mode . interactive-haskell-mode))

;; Web mode
(use-package web-mode
  :mode
  ("\\.html?\\'" . web-mode)
  ("\\.tsx?\\'" . web-mode)
  :config
  (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-ternary" . nil))
  (setq web-mode-enable-auto-quoting nil))

(use-package typescript-mode
  :mode ("\\.ts?\\'" . typescript-mode))

(use-package js-mode
  :mode ("\\.js[x]?\\'" . js-mode)
        ("\\.json?\\'" . js-mode)
  :straight (:type built-in))

;; Markdown
(use-package markdown-mode
  :mode
  ("\\.md?\\'" . markdown-mode))

(provide 'lang)
