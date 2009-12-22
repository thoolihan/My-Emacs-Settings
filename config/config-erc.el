(require 'erc)
(defun irc-freenode ()
  "Connect to Freenode irc"
  (interactive)
  (erc :server "irc.freenode.net" :port 6667
       :nick "thoolihan" :full-name "Tim Hoolihan"))

(setq erc-hide-list '("JOIN" "PART" "QUIT"))