(defun ext-tools-launch (path)
  "launch an external tool async"
  (shell-command (concat path " &")))

(defun ext-tools-start-visual-studio
  "launch visual studio"
  (interactive
   (ext-tools-launch ext-tools-path-ide-net)))

(defun ext-tools-start-eclipse
  "launch eclipse"
  (interactive
   (ext-tools-launch ext-tools-path-ide-java)))

(defun ext-tools-start-browser
  "launch browser"
  (interactive
   (ext-tools-launch ext-tools-path-browser)))

(defun ext-tools-start-mail
  "launch mail"
  (interactive
   (ext-tools-launch ext-tools-path-mail)))

(defun ext-tools-start-music
  "launch music"
  (interactive
   (ext-tools-launch ext-tools-path-music)))

(defun ext-tools-start-git-bash
  "launch git-bash"
  (interactive
   (ext-tools-launch ext-tools-path-gitbash)))