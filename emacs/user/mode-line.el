;;; -*- lexical-binding: t -*-

;; Override default value (%12b)
(setq-default mode-line-buffer-identification
  (propertized-buffer-identification "%b"))

(set-face-attribute 'mode-line-buffer-id nil
                    :foreground "#d3d0c8"
                    :weight 'ultra-bold)

(set-face-attribute 'mode-line-highlight nil
                    :foreground "#e8e6df")

(defface mode-line-bold
  '((t :weight ultra-bold))
  ""
  :group 'mode-line-faces
  :group 'basic-faces)

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

(defun mode-line-directory ()
  (if (buffer-file-name)
      (concat " " (shorten-directory default-directory 20)) " "))

;; The functions below are heavily borrowed from doom-emacs
(defun buffer-info ()
  "Show info about current buffer's state"
  (cond (buffer-read-only
                  "")
                 ((buffer-modified-p)
                  "*")
                 ((and buffer-file-name
                       (not (file-exists-p buffer-file-name)))
                  "∄")
                 ("-")))

(defun eol-info ()
  "Show info about encoding and line endings"
  (concat " "
          (if (memq
               (plist-get (coding-system-plist buffer-file-coding-system) :category)
               '(coding-category-undecided coding-category-utf-8))
              "U" "∅")
          " "
          (pcase (coding-system-eol-type buffer-file-coding-system)
            (0 "LF")
            (1 "CRLF")
            (2 "CR")
            (_ "N/A"))))

;; This accommodates git only
(defun vc-info ()
  "Show vc state"
  (let ((state (vc-state buffer-file-name (vc-backend buffer-file-name))))
    (concat
     " "
     (substring vc-mode 5)
     " "
     (if (memq state '(edited added)) "✘" "✔"))))

;; Count errors while handling more dynamic error levels
;; Borrowed from doom-modeline
(defun custom--flycheck-count-errors ()
  (let ((info 0) (warning 0) (error 0))
    (mapc
     (lambda (item)
       (let ((count (cdr item)))
         (pcase (flycheck-error-level-compilation-level (car item))
           (0 (cl-incf info count))
           (1 (cl-incf warning count))
           (2 (cl-incf error count)))))
     (flycheck-count-errors flycheck-current-errors))
    `((info . ,info) (warning . ,warning) (error . ,error))))

(defun flycheck-error-info ()
  "Show Flycheck's state"
  (pcase flycheck-last-status-change
    ('finished (if flycheck-current-errors
                   (let-alist (custom--flycheck-count-errors)
                     (let ((infos (or .info 0))
                           (warnings (or .warning 0))
                           (errors (or .error 0)))
                       (format "%s, %s, %s"
                               (propertize (number-to-string infos) 'font-lock-face 'success)
                               (propertize (number-to-string warnings) 'font-lock-face 'warning)
                               (propertize (number-to-string errors) 'font-lock-face 'error))))
                 "✔"))
    ('running "~")
    ('errored "✘")
    ('interrupted "!")
    (_ "∅")))

(setq-default
 mode-line-format
 (list '(:eval (concat " "
                       (format-mode-line (upcase (prin1-to-string evil-state)) "mode-line-bold")
                       " |"
                       (if vc-mode (concat (vc-info) " |"))
                       (eol-info)
                       " "
                       (buffer-info)
                       " |"
                       (mode-line-directory)
                       (format-mode-line 'mode-line-buffer-identification)
                       " | "
                       (cond ((listp mode-name) (car mode-name))
                             (t mode-name))
                       " | "
                       (format-mode-line "%l:%c")
                       (when (and (boundp 'flycheck-last-status-change)
                                  (not (eq flycheck-last-status-change 'not-checked)))
                         (concat " | " (flycheck-error-info)))))))

(provide 'mode-line)
