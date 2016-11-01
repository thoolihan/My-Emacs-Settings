(package-initialize)

(server-start)
(require 'cl)
(defvar emacs-directory "~/emacs/")
(defvar emacs-root emacs-directory)

(set-keyboard-coding-system nil)

(defun add-path (p)
  (add-to-list 'load-path
               (concat emacs-directory p)))

(setq exec-path (append exec-path '("/usr/local/bin")))

(add-path "")
(add-path "config")
(add-path "misc")
(let ((myconfig '(
                 "local-settings"
                 "config"
                 "bindings"
                 "config-auto-complete"
                 "config-ess"
                 "config-markdown"
                 "config-r"
                 "config-ssh"
                 "config-yegge"
                 "config-tempfiles"
                 "visual"
                 "misc"
                 )))

  (loop for config-file in myconfig do
        (load-library config-file)))
