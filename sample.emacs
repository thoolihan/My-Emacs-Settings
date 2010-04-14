(defvar emacs-directory "~/emacs/")
(defvar emacs-root emacs-directory)
(server-start)
(require 'calendar)
(require 'cl)

(defun add-path (p)	
  (add-to-list 'load-path	
               (concat emacs-directory p)))

(add-path "")
(add-path "config")
(let ((myconfig (list "local-settings"
                      "visual"
                      "config"
                      "config-abbrev"
                      "config-options"
                      "config-calendar"
                      "bindings"
                      "config-auto-complete"
                      "config-csharp"
                      "config-css"                      
                      "config-erc"
                      "config-info"
                      "config-javascript"
                      "config-nxhtml"
                      "config-powershell"
                      "config-php"
                      "config-sql"
                      "config-ruby-flymake"
                      "config-ri"
                      "config-rinari"
                      "config-vb"
                      "config-magit"
                      "config-shell"
                      "config-ssh"
                      "config-yasnippet"
                      "config-yegge"
                      "config-tempfiles"
                      "config-cedet"
                      "misc"
                      "server"
                      "config-options")))
  (loop for config-file in myconfig do (load-library config-file)))

(if (eq t use-ecb)  (load-library "config-ecb"))
