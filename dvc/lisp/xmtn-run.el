;;; xmtn-run.el --- Functions for runnning monotone commands

;; Copyright (C) 2008 - 2010 Stephen Leake
;; Copyright (C) 2006, 2007 Christian M. Ohler

;; Author: Christian M. Ohler
;; Keywords: tools

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

;;; Commentary:

;; This file provides functions for running monotone commands.  See
;; xmtn-automate.el for more sophisticated access to monotone's
;; automate interface.

;;; Code:

;;; There are some notes on the design of xmtn in
;;; docs/xmtn-readme.txt.

(eval-and-compile
  (require 'cl)
  (require 'dvc-unified)
  (when (featurep 'xemacs)
    (condition-case nil
        (require 'un-define)
      (error nil)))
  (require 'xmtn-base))

(define-coding-system-alias 'xmtn--monotone-normal-form 'utf-8-unix)

(defun* xmtn--run-command-sync (root arguments)
  (xmtn--check-cached-command-version)
  (let ((default-directory (file-truename (or root default-directory))))
    (dvc-run-dvc-sync
     'xmtn
     `(,@xmtn-additional-arguments
       ;; We don't pass the --root argument here; it is not
       ;; necessary since default-directory is set, and it
       ;; confuses the Cygwin version of mtn when run with a
       ;; non-Cygwin Emacs.
       ,@arguments))))

;;; The `dvc-run-dvc-*' functions use `call-process', which, for some
;;; reason, spawns the subprocess with a working directory with all
;;; symlinks expanded.  (Or maybe it's the shell that expands the
;;; symlinks.)  If the path to the root directory looks different from
;;; the current working directory, monotone rejects it even if it is
;;; the same via symlinks.  Therefore, we need to resolve symlinks
;;; here in strategic places.  Hence the calls to `file-truename'.

(defun* xmtn--run-command-async (root arguments &rest dvc-run-keys &key)
  (xmtn--check-cached-command-version)
  (let ((default-directory (file-truename (or root default-directory))))
    (apply #'dvc-run-dvc-async
           'xmtn
           `(,@xmtn-additional-arguments
             ;; We don't pass the --root argument here; it is not
             ;; necessary since default-directory is set, and it
             ;; confuses the Cygwin version of mtn when run with a
             ;; non-Cygwin Emacs.
             ,@arguments)
           dvc-run-keys)))

(defun xmtn--command-output-lines (root arguments)
  "Run mtn in ROOT with ARGUMENTS and return its output as a list of strings."
  (xmtn--check-cached-command-version)
  (let ((accu (list)))
    (let ((default-directory (file-truename (or root default-directory))))
      (dvc-run-dvc-sync
       'xmtn
       `(,@xmtn-additional-arguments
         ,@(if root `(,(concat "--root=" (file-truename root))))
         ,@arguments)
       :finished (lambda (output error status arguments)
                   (with-current-buffer output
                     (save-excursion
                       (goto-char (point-min))
                       (while (not (eobp))
                         (push (buffer-substring-no-properties
                                (point)
                                (progn (end-of-line) (point)))
                               accu)
                         (forward-line 1)))))))
    (setq accu (nreverse accu))
    accu))

(defun xmtn--command-output-line (root arguments)
  "Run mtn in ROOT with ARGUMENTS and return the one line of output as string.

Signals an error if more (or fewer) than one line is output."
  (let ((lines (xmtn--command-output-lines root arguments)))
    (unless (eql (length lines) 1)
      (error "Expected precisely one line of output from monotone, got %s: %s %S"
             (length lines)
             xmtn-executable
             arguments))
    (first lines)))

(defconst xmtn--minimum-required-command-version '(0 46))
;; see also xmtn-sync.el xmtn-sync-required-command-version
(defconst xmtn--required-automate-format-version "2")

(defvar xmtn--*cached-command-version* nil
  ;; compare with (xmtn-version-<= required)
  "(MAJOR MINOR REVISION VERSION-STRING).")

(defvar xmtn--*command-version-cached-for-executable* nil)

(defun xmtn-version-<= (required)
  "Nonnil if REQUIRED (list of major, minor) is <= cached version."
  (version-list-<= required (butlast (xmtn--cached-command-version) 2)))

(defun xmtn--clear-command-version-cache ()
  (setq xmtn--*command-version-cached-for-executable* nil
        ;; This is redundant but neater.
        xmtn--*cached-command-version* nil))

(defun xmtn--cached-command-version ()
  "Return mtn version as a list (MAJOR MINOR REVISION VERSION-STRING).
Sets cache if not already set."
  (if (equal xmtn--*command-version-cached-for-executable* xmtn-executable)
      xmtn--*cached-command-version*
    (let ((executable xmtn-executable))
      (prog1 (setq xmtn--*cached-command-version* (xmtn--command-version
                                                   executable))
        (setq xmtn--*command-version-cached-for-executable* executable)
        (xmtn--check-cached-command-version)))))

(defun xmtn--command-version (executable)
  "Return EXECUTABLE's version as a list (MAJOR MINOR REVISION VERSION-STRING).

VERSION-STRING is the string printed by mtn --version (with no
trailing newline).  MAJOR and MINOR are integers, a parsed
representation of the version number.  REVISION is the revision
id."
  (let (
        ;; Cache a fake version number to avoid infinite mutual
        ;; recursion.
        (xmtn--*cached-command-version*
         (append xmtn--minimum-required-command-version
                 '("xmtn-dummy" "xmtn-dummy")))
        (xmtn--*command-version-cached-for-executable* executable)
        (xmtn-executable executable))
    (let ((string (xmtn--command-output-line nil '("--version"))))
      (unless (string-match
               (concat "\\`monotone \\([0-9]+\\)\\.\\([0-9]+\\)\\(dev\\)?"
                       " (base revision: \\(unknown\\|\\([0-9a-f]\\{40\\}\\)\\))\\'")
               string)
        (error (concat "Version output from monotone --version"
                       " did not match expected pattern: %S")
               string))
      (let ((major (parse-integer string (match-beginning 1) (match-end 1)))
            (minor (parse-integer string (match-beginning 2) (match-end 2)))
            (revision (match-string 4 string)))
        (list major minor revision string)))))

(defun xmtn--check-cached-command-version ()
  (let ((minimum-version xmtn--minimum-required-command-version)
        (string (nth 3 (xmtn--cached-command-version))))
    (unless (xmtn-version-<= xmtn--minimum-required-command-version)
      ;; Clear cache now since the user is somewhat likely to
      ;; upgrade mtn (or change the value of `xmtn-executable')
      ;; after this message.
      (xmtn--clear-command-version-cache)
      (error (concat "xmtn does not work with mtn versions below %s.%s"
                     " (%s is %s)")
             (car minimum-version) (cadr minimum-version)
             xmtn-executable string)))
  nil)

(provide 'xmtn-run)

;;; xmtn-run.el ends here
