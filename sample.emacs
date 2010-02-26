(defvar emacs-directory "~/emacs/")
(defvar emacs-root "~/emacs/")
(server-start)
(require 'calendar)
(require 'cl)

(defun add-path (p)	
  (add-to-list 'load-path	
	       (concat emacs-directory p)))
(let ((mypaths (list ""
                     "color-theme/"
                     "color-theme/themes/"
                     "config"
                     "misc/"
                     "semantic/"
                     "slime/"
                     "speedbar/")))
  (loop for path in mypaths do (add-path path)))
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
                      "config-powershell"
                      "config-php"
                      "config-nxml"
                      "config-sql"
                      "config-ruby-flymake"
                      "config-ruby"
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
(c-subword-mode t)
(shell)
(if (fboundp 'menu-bar-mode) (menu-bar-mode 1)) ; temporary
