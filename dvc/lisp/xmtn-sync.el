;;; xmtn-sync.el --- database sync handling for DVC backend for monotone
;;
;; Copyright (C) 2010 Stephen Leake
;;
;; Author: Stephen Leake
;; Keywords: tools
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
;; Boston, MA  02110-1301  USA.

(eval-when-compile
  ;; these have macros we use
  )

(eval-and-compile
  ;; these have functions we use
  (require 'xmtn-automate)
  )

;;; User variables
(defvar xmtn-sync-branch-file "~/.dvc/branches"
  "File associating branch name with workspace root")

(defvar xmtn-sync-executable
  (cond
    ((equal system-type 'windows-nt)
     ;; Native MinGW does not support file: or ssh: - assume Cygwin is
     ;; installed, but not first in path
     "c:/bin/mtn")
    (t
     ;; Unix or Cygwin; assume mtn is in path
     "mtn"))
  "Executable for running sync command on local db; overrides xmtn-executable.")

(defvar xmtn-sync-config "xmtn-sync-config"
  "File to store `xmtn-sync-branch-alist' and `xmtn-sync-remote-exec-alist'; relative to `dvc-config-directory'.")

;;; Internal variables
(defconst xmtn-sync-required-command-version '(0 46)
  "Minimum mtn version for automate sync; overrides xmtn--minimum-required-command-version.")

(defconst xmtn-sync-remote-exec-default "mtn"
  "Default executable command to run on remote host for file: or ssh:; see `xmtn-sync-remote-exec-alist'.")

;; loaded from xmtn-sync-config
(defvar xmtn-sync-branch-alist nil
  "Alist associating branch name with workspace root")

(defvar xmtn-sync-remote-exec-alist
  (list
   (list "file://" xmtn-sync-executable))
  "Alist of host and remote command. Overrides `xmtn-sync-remote-exec-default'.")

;; buffer-local
(defvar xmtn-sync-local-db nil
  "Absolute path to local database.")
(make-variable-buffer-local 'xmtn-sync-local-db)

(defvar xmtn-sync-remote-db nil
  "Absolute path to remote database.")
(make-variable-buffer-local 'xmtn-sync-remote-db)

(defstruct (xmtn-sync-branch
            (:copier nil))
  ;; ewoc element; data for a branch that was received
  name)

(defun xmtn-sync-set-hf ()
  "Set ewoc header and footer."
  (ewoc-set-hf
   xmtn-sync-ewoc
   (concat
    (format " local database : %s\n" xmtn-sync-local-db)
    (format "remote database : %s\n" xmtn-sync-remote-db)
    )
   ""))

(defun xmtn-sync-printer (branch)
  "Print an ewoc element; BRANCH must be of type xmtn-sync-branch."
  (insert "branch:   ")
  (insert (xmtn-sync-branch-name branch))
  (insert "\n")
  )

(defvar xmtn-sync-ewoc nil
  "Buffer-local ewoc for displaying sync.
All xmtn-sync functions operate on this ewoc.
The elements must all be of type xmtn-sync-sync.")
(make-variable-buffer-local 'xmtn-sync-ewoc)

(defun xmtn-sync-status ()
  "Start xmtn-status-one for current ewoc element."
  (let* ((data (ewoc-data (ewoc-locate xmtn-sync-ewoc)))
         (branch (xmtn-sync-branch-name data))
         (work (assoc branch xmtn-sync-branch-alist)))
    (if (not work)
        (progn
          (setq work (read-directory-name (format "workspace root for %s: " branch)))
          (push (list branch work) xmtn-sync-branch-alist)))
    (xmtn-status-one work)))

(defvar xmtn-sync-ewoc-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?0]  '(menu-item "0) status"
                                      'xmtn-sync-status))
    map)
  "Keyboard menu keymap for xmtn-sync-ewoc.")

(defvar xmtn-sync-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [?q]  'dvc-buffer-quit)
    (define-key map "\M-d" xmtn-sync-ewoc-map)
    map)
  "Keymap used in `xmtn-sync-mode'.")

(easy-menu-define xmtn-sync-mode-menu xmtn-sync-mode-map
  "`xmtn-sync' menu"
  `("Xmtn-sync"
    ["Do the right thing"  xmtn-sync-ewoc-map t]
    ["Quit"                dvc-buffer-quit t]
    ))

;; derive from nil causes no keymap to be used, but still have self-insert keys
;; derive from fundamental-mode causes self-insert keys
(define-derived-mode xmtn-sync-mode fundamental-mode "xmtn-sync"
  "Major mode to specify conflict resolutions."
  (setq dvc-buffer-current-active-dvc 'xmtn)
  (setq buffer-read-only nil)
  (setq xmtn-sync-ewoc (ewoc-create 'xmtn-sync-printer))
  (setq dvc-buffer-refresh-function nil)
  (dvc-install-buffer-menu)
  (setq buffer-read-only t)
  (buffer-disable-undo)
  (set-buffer-modified-p nil))

;;;###autoload
(defun xmtn-sync-sync (local-db remote-host remote-db)
  "Sync LOCAL-DB with REMOTE-HOST REMOTE-DB, display sent and received branches.
Remote-db should include branch pattern in URI syntax."
  (interactive "flocal db: \nMremote-host: \nMremote-db: ")
  (pop-to-buffer (get-buffer-create "*xmtn-sync*"))
  (let ((xmtn-executable xmtn-sync-executable)
        (xmtn--minimum-required-command-version xmtn-sync-required-command-version))

    ;; pass remote command to mtn via Lua hook get_mtn_command; see
    ;; xmtn-hooks.lua
    (setenv "XMTN_SYNC_MTN"
            (or (cadr (assoc remote-host xmtn-sync-remote-exec-alist))
                xmtn-sync-remote-exec-default))

    (xmtn-automate-command-output-buffer
     default-directory ; root
     (current-buffer) ; output-buffer
     (list (list
            "ticker" "count"
            "db" local-db
            ) ;; options
           "sync" (concat remote-host remote-db)) ;; command, args
     )))

(provide 'xmtn-sync)

;; end of file
