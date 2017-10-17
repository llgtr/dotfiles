;;; -*- lexical-binding: t -*-

;; General
(use-package flycheck
  :load-path "lock/flycheck"
  :defer 5)

;; Rust
(use-package rust-mode
  :load-path "lock/rust-mode"
  :mode ("\\.rs\\'"   . rust-mode)
        ("\\.rlib\\'" . rust-mode))

(use-package flycheck-rust
  :load-path "lock/flycheck-rust"
  :defer t
  :after rust-mode
  :config
  (add-hook 'rust-mode-hook #'flycheck-mode)
  (add-hook 'rust-mode-hook #'flycheck-rust-setup))

;; Clojure
(use-package clojure-mode
  :load-path "lock/clojure-mode"
  :mode ("\\.clj\\'"  . clojure-mode)
        ("\\.cljs\\'" . clojure-mode)
        ("\\.cljc\\'" . clojure-mode))

(use-package rainbow-delimiters
  :load-path "lock/rainbow-delimiters"
  :defer t
  :after clojure-mode
  :config (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode))

(use-package cider
  :load-path "lock/cider"
  :defer t
  :after clojure-mode)

;; Haskell
(use-package haskell-mode
  :load-path "lock/haskell-mode"
  :mode ("\\.hs\\'" . haskell-mode)
  :config (add-hook 'haskell-mode-hook #'interactive-haskell-mode))

(use-package flycheck-haskell
  :load-path "lock/flycheck-haskell"
  :defer t
  :after haskell-mode
  :config
  (add-hook 'haskell-mode-hook #'flycheck-mode)
  (add-hook 'haskell-mode-hook #'flycheck-haskell-setup))

(provide 'init-lang)
