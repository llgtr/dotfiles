;;; -*- lexical-binding: t -*-

(defvar env-file (expand-file-name "envfile" user-emacs-directory))

;; Reads envfile created by emacs-env.sh. Mostly copied from doom-emacs.

(if (file-exists-p env-file)
    (with-temp-buffer
      (insert-file-contents env-file)
      (while (re-search-forward "\n *\\([^#= \n]*\\)=" nil t)
        (let* ((env-var-val (split-string
                             (buffer-substring
                              (match-beginning 1)
                              (- (or (save-excursion
                                       (when (re-search-forward "^\\([^= ]+\\)=" nil t)
                                         (line-beginning-position)))
                                     (point-max)) 1))
                             "="))
               (env-var (car env-var-val))
               (env-val (cadr env-var-val)))
          (if (and (>= (length env-var-val) 2)
                   (not (seq-empty-p env-val))
                   (not (string-match "\n" env-val)))
              (progn
                (setenv env-var env-val)
                (if (string-equal "PATH" env-var)
                    (setq exec-path (append (parse-colon-path (getenv "PATH"))
                                            (list exec-directory))))
                (if (string-equal "SHELL" env-var)
                    (setq shell-file-name
                          (or (getenv "SHELL") shell-file-name))))
            (error (format "%s is malformed. Run `emacs-env.sh` and overwrite the envfile." env-file))))))
  (error (format "%s does not exist. Run `emacs-env.sh`." env-file)))

(setq default-directory "~")

(provide 'user-env)
