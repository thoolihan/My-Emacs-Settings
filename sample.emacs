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
                 "config-abbrev"
                 "config-options"
                 "bindings"
                 "config-auto-complete"
                 "config-coffeescript"
                 "config-csharp.el"
                 "config-haml"
                 "config-info"
                 "config-magit"
                 "config-markdown"
                 "config-nxhtml"
                 "config-shell"
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

