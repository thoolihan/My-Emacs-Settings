(server-start)
(require 'cl)
(defvar emacs-directory "~/emacs/")
(defvar emacs-root emacs-directory)

(defun add-path (p)	
  (add-to-list 'load-path	
               (concat emacs-directory p)))

(add-path "")
(add-path "config")
(let ((myconfig '("local-settings"
                  "config"
                  "config-abbrev"
                  "config-options"
                  "bindings"
                  "config-auto-complete"
                  "config-clojure"
                  "config-csharp"
                  "config-css"                      
                  "config-info"
                  "config-javascript"
                  "config-nxhtml"
                  "config-powershell"
                  "config-sql"
                  "config-ruby-flymake"
                  "config-ri"
                  "config-rinari"
                  "config-version-control"
                  "config-shell"
                  "config-ssh"
                  "config-style"
                  "config-yasnippet"
                  "config-yegge"
                  "config-tempfiles"
                  "config-cedet"
		          "visual"
                  "misc")))

  (loop for config-file in myconfig do 
        (load-library config-file)))

(if (eq t use-ecb)  (load-library "config-ecb"))

