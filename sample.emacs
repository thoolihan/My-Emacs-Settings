(server-start)
(require 'cl)
(defvar emacs-directory "~/emacs/")
(defvar emacs-root emacs-directory)

(defun add-path (p)
  (add-to-list 'load-path
               (concat emacs-directory p)))

(add-path "")
(add-path "config")
(add-path "misc")
(let ((myconfig '(
                 "local-settings"
                 "config"
                 "config-neotree"
                 "config-abbrev"
                 "config-dash-at-point"
                 "config-options"
                 "bindings"
                 "config-auto-complete"
                 "config-coffeescript"
                 "config-info"
                 "config-markdown"
                 "config-nxhtml"
                 "config-ssh"
                 "config-style"
                 "config-yasnippet"
                 "config-yegge"
                 "config-tempfiles"
		             "visual"
                 "misc"
                 )))

  (loop for config-file in myconfig do
        (load-library config-file)))
