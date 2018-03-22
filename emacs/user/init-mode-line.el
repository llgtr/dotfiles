;;; -*- lexical-binding: t -*-

(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
               (output ""))
       (when (and path (equal "" (car path)))
         (setq path (cdr path)))
       (while (and path (< (length output) (- max-length 4)))
         (setq output (concat (car path) "/" output))
         (setq path (cdr path)))
       (when path
         (setq output (concat ".../" output)))
       output))

(defvar mode-line-directory
  '(:propertize
    (:eval (if (buffer-file-name)
               (concat " " (shorten-directory default-directory 20)) " "))
                face mode-line-directory)
  "Formats the current directory.")
(put 'mode-line-directory 'risky-local-variable t)

;; Override default value (%12b)
(setq-default mode-line-buffer-identification
  (propertized-buffer-identification "%b"))

(defun simple-mode-line-render (left right)
  "Return a string of `window-total-width' length containing LEFT, and RIGHT
 aligned respectively."
  (let* ((available-width (- (window-total-width) (length left) 2)))
    (format (format " %%s %%%ds " available-width) left right)))

(set-face-attribute 'mode-line nil :foreground "#393939" :background "#515151" :weight 'ultra-bold)
(set-face-attribute 'mode-line-buffer-id nil :foreground "#99cc99")
(set-face-attribute 'mode-line-highlight nil :box nil :foreground "#bb77bb")
(set-face-attribute 'mode-line-inactive  nil :inherit 'default)

(defface mode-line-directory
  '((t :foreground "#99cc99" :weight light))
  ""
  :group 'mode-line-faces
  :group 'basic-faces)

(defface mode-line-basic
  '((t :foreground "#f99157" :weight normal))
  ""
  :group 'mode-line-faces
  :group 'basic-faces)

(defface mode-line-accent1
  '((t :foreground "#ffcc66" :weight bold))
  ""
  :group 'mode-line-faces
  :group 'basic-faces)

(defface mode-line-accent2
  '((t :foreground "#6699cc" :weight normal))
  ""
  :group 'mode-line-faces
  :group 'basic-faces)

(setq-default
 mode-line-format
 '((:eval (simple-mode-line-render
           (concat (format-mode-line
                    (upcase (prin1-to-string evil-state)) "mode-line-accent1")
                   " |"
                   (format-mode-line vc-mode "mode-line-accent2")
                   " |"
                   (format-mode-line minor-mode-alist "mode-line-basic")
                   " | "
                   (format-mode-line
                    (mapconcat 'format-mode-line
                               '(mode-line-mule-info
                                 mode-line-modified
                                 mode-line-remote) "") "mode-line-basic")
                   " |"
                   (mapconcat 'format-mode-line
                              '(mode-line-directory
                                mode-line-buffer-identification) ""))
           (concat (format-mode-line
                    mode-name "mode-line-accent2")
                   " | "
                   (format-mode-line
                    (format-mode-line "%l:%c ")
                    "mode-line-accent1"))))))

(provide 'init-mode-line)
