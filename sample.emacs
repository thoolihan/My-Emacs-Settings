(defvar emacs-directory "~/emacs/")
(server-start)
(require 'calendar)
(require 'cl)

(defvar emacs-root "~/emacs/")
(defun add-path (p)	
  (add-to-list 'load-path	
	       (concat emacs-directory p)))

(add-path "") ; "/emacs"
(add-path "clojure-mode/")
(add-path "color-theme/")
(add-path "color-theme/themes/")
(add-path "config/")
(add-path "csharp-mode/")
(add-path "cedet-1.0pre6/")
(add-path "cedet-1.0pre6/common/")
(add-path "ecb-2.40/")
(add-path "eieio/")
(add-path "ej/")
(add-path "git-emacs")
(add-path "jabber/")
(add-path "javascript/")
(add-path "misc/")
(add-path "nxml/")
(add-path "semantic/")
(add-path "shell/")
(add-path "slime/")
(add-path "speedbar/")
(add-path "sql/")
(add-path "svn/")
(add-path "swank-clojure/")
(add-path "twitter/")
(add-path "twittering-mode/")
(add-path "twit/")
(add-path "vbnet-mode/")

(load-library "local-settings")
(load-library "visual")
(load-library "config")
(load-library "config-abbrev")
(load-library "config-calendar")
(load-library "config-options")
(load-library "bindings")
(load-library "config-auto-complete")
;(load-library "config-clojure")
(load-library "config-csharp")
(load-library "config-erc")
(load-library "config-info")
(load-library "config-javascript")
(load-library "config-powershell")
(load-library "config-nxml")
(load-library "config-sql")
;(load-library "config-ruby-flymake")
(load-library "config-ruby")
;(load-library "config-vb")
(load-library "config-magit")
(load-library "config-shell")
(load-library "config-ssh")
(load-library "config-svn")
(load-library "config-yasnippet")
(load-library "config-yegge")
(load-library "config-tempfiles")
(if (eq t use-twitter) 
    (lambda (load-library "config-twittering-mode") (twittering-mode)))
(load-library "config-cedet")
(if (eq t use-ecb) (load-library "config-ecb"))
(load-library "misc")
(load-library "server")
(load-library "config-options")
(c-subword-mode t)
(shell)
(switch-to-buffer "*scratch*")