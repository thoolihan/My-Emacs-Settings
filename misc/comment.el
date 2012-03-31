;;; comment.el --- comment/uncomment line or region
;; Version 2.0

;; Copyright (C) 2008  Andreas RÅˆhler

;; Author: Andreas RÅˆhler <andreas.roehler@online.de>
;; Keywords: wp, lisp

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Major changes to previous version:

;; With `c-toggle-comment-style' both C comment-styles
;; are available. Try `comment-c-test-lor' for the results.

;;  `comment-dwim-lor' or `comment-uncomment-lor' from
;;  inside a comment which spans over several lines
;;  will detect that and uncomment.

;; With universal arg, line or region is copied and
;; inserted over commented/uncommented part, thus
;; preserving old state

;;; Commentary:

;; Set out to fix a bug occuring in all Emacsen, reported at

;; http://tracker.xemacs.org/XEmacs/its/issue232

;; it ended up with an alternative comment.el

;; The bug of `indent-new-comment-line' should be gone
;; with the following key:

;; (global-set-key [(meta j)] 'comment-indent-new-line-lor)

;; Comment.el should not disturb newcomment.el; it's
;; suffix `-lor' means `line-or-region', designing a
;; new feature: If no active region exists, all
;; commands operate from current line.

;; Other main diffs to newcomment.el are:

;; Styles will not be chosen by customization, but
;; called with respective functions.

;; Check if a comment-char found is inside a string no
;; longer relies on fontifying.

;; The following commands should be available:
;; (displayed here without suffix `lor':

;;                         comment-uncomment
;;                                 /\
;; (copy/do-not-copy)             /  \
;;                               /    \
;;                        comment      uncomment
;;                       /\                    /\
;;                      /  \                  /  \
;;                     /    \                /    \
;;                plain      indent       all    not-all
;;               /\               /\
;;              /  \             /  \
;;             /    \           /    \
;;     span-line  single    span-line single 
;;        /\        /\        /\        /\
;;       /  \      /  \      /  \      /  \
;;      /    \    /    \    /    \    /    \
;;
;;  ------------ boxed not-boxed ----------------

;; ToDo: Comment-uncomment-lor detects only selected
;; comment-style, i.e. "/* */" or "//" in C. When
;; uncommenting, make sure the appropriate style is
;; switched on.

;;; Code:

(defvar comment-padding " ")
(defvar comment-column 0
  "A var of this name now is used internal only, here set to avoid errors. ")

(defvar comment-add 0
  "A var of this name now is used internal only, here set to avoid errors. ")

(defcustom comment-fontify-lor t
  "If t, comment-functions call font-lock-fontify-buffer . "
  :type '(choice (const :tag "No" nil)
	  (const :tag "Yes" t)
  :group 'comment))

(defcustom comment-empty-lines-lor nil
  "If nil, `comment-' does not comment out empty lines. "
  :type '(choice (const :tag "No" nil)
	  (const :tag "Yes" t)
  :group 'comment))

(defcustom comment-stay-on-line-lor  nil
 "If after commenting/uncommenting cursor stay on present line. Default is nil"

 :type 'boolean
 :group 'comment)

(defcustom comment-function-lor  'comment-indent-lor
 "Function to use from comment-uncomment-lor" 

:type 'symbol
:group 'comment)

(defalias 'comment-lor 'comment-indent-lor)
(defalias 'uncomment-function-lor 'uncomment-lor)

(defvar comment-pad-boxes-lor nil
  "If comment-start and end in headers and footers are padded")

;;; Code:

;; Group as is from newcomment.el
(defgroup comment nil
  "Indenting and filling of comments."
  :prefix "comment-"
  :group 'fill)

;; The following should be set in C-mode, other modes
;; could implement that respective
;;;###autoload
(defun c-toggle-comment-style ()
  "Change the C default comment style"
  (interactive)
  (if (comment-end-stringp (< 0 (length comment-end)))
      (progn
	(setq comment-start "/")
	(setq comment-end "")
        (setq comment-add 1))
    (setq comment-start "/* ")
    (setq comment-end " */")))

(defalias  'comment-region 'comment-indent-lor)

;;;###autoload
(defun comment-plain-lor (&optional copy beg end)
  "Comment line or region at beginning of line. "
  (interactive "*P")
  (let ((copy (when copy (prefix-numeric-value copy))))
    (comment-base-lor nil nil nil beg end copy)))

;;;###autoload
(defun comment-indent-lor (&optional copy beg end)
  "Comment line or region, indent comments. "
  (interactive "*P")
  (let ((copy (when copy (prefix-numeric-value copy))))
    (comment-base-lor t nil nil beg end copy)))

;;;###autoload 
(defun comment-indent-single-lor (&optional copy beg end)
  "Comment line or region,  at beginning of lines. "
  (interactive "*P")
  (let ((copy (when copy (prefix-numeric-value copy))))
    (comment-base-lor t nil nil beg end copy)))

;;;###autoload
(defun comment-indent-span-lor (&optional copy beg end)
  "Comment line or region,  at beginning of lines. "
  (interactive "*P")
  (let ((copy (when copy (prefix-numeric-value copy))))
    (comment-base-lor t nil t beg end copy)))

;;;###autoload 
(defun comment-plain-single-lor (&optional copy beg end)
  "Comment line or region. Use with start-end pairs every line. "
  (interactive "*P")
  (let ((copy (when copy (prefix-numeric-value copy))))
    (comment-base-lor nil nil nil beg end copy)))

;;;###autoload
(defun comment-plain-span-lor (&optional copy beg end)
  "Comment line or region, span with one start-end over lines. "
  (interactive "*P")
  (let ((copy (when copy (prefix-numeric-value copy))))
    (comment-base-lor nil nil t beg end copy)))

;;;###autoload
(defun comment-indent-box-lor (&optional copy beg end)
  "Comment out line or region, putting it inside a box. "
  (interactive "*P")
  (let ((copy (when copy (prefix-numeric-value copy))))
    (comment-base-lor t t nil beg end copy)))


;;;###autoload
(defun comment-plain-box-lor (&optional copy beg end)
  "Comment out line or region, putting it inside a box. "
  (interactive "*P")
  (let ((copy (when copy (prefix-numeric-value copy))))
    (comment-base-lor nil t nil beg end copy)))

;;;###autoload
(defun comment-base-lor (indent box span &optional beg end copy)
  (let* ((beg (cond ((and beg end) beg)
		    ((region-active-p)
		     (region-beginning))
		    (t (line-beginning-position))))
	 (end (cond ((and beg end) (copy-marker end))
		    ((region-active-p)
		     (copy-marker (region-end)))
		    (t (copy-marker (line-end-position)))))
	 (span span)
	 (line (count-lines (point-min) (point)))
	 (comment-end-stringp (< 0 (length comment-end)))
	 (maxline 0)
	 (comment-fill-char
	  (substring (string-strip comment-start) -1))
	 ;; comment-add should be set by progmodes exclusively
	 (add (if comment-end-stringp 0 comment-add))
	 first)
    (when (and copy (eq 4 copy))
      (copy-region-as-kill beg end))
    (when (and span (not comment-end-stringp))
      (error "%s" "No valid command in this context:
Comments spanning multiple lines need comment-end string"))
    ;; heritage from newcomment.el, kept for transition
    (when (integerp comment-padding)
      (setq comment-padding (make-string comment-padding ? )))
    (comment-intern-lor beg end indent box span comment-column first add comment-end-stringp)
    ;; (unless (< line (count-lines (point-min) (point))))
    (unless comment-stay-on-line-lor
      (forward-line 1)))
  ;; (save-excursion
  (when copy
    (insert (car kill-ring))
    (unless (looking-at "[ \t]*$")
      (newline-and-indent)))
  ;;)
  (when comment-fontify-lor (font-lock-fontify-buffer)))

;;;###autoload
(defun comment-intern-lor (beg end indent box span comment-column first add comment-end-stringp)
  ;; if line after end isn't empty, newline
  (goto-char end)
  (unless (or comment-end-stringp (bolp) (looking-at "[ \t\f\n\r]*$"))
    (newline))
  (goto-char beg)
  (when (and (empty-line-p)
	     (not comment-empty-lines-lor)
	     (< (line-end-position) (1- end)))
    (forward-line 1)
    (comment-intern-lor (line-beginning-position) end indent box span comment-column first add))
  (when (or (not (empty-line-p))
	    comment-empty-lines-lor)
    (if indent
	(progn (back-to-indentation)
	       (indent-according-to-mode))
      (fixup-whitespace))
    (unless (and span first)
      (insert comment-start)
      (let ((add add))
	(while
	    (< 0 add)
          (skip-chars-backward " \t")
	  (insert comment-start)
	  (setq add (1- add))))
      (unless (looking-back "[ \n\t\f\r]") (insert " "))
      (setq first t)
      (skip-chars-backward " \t")
      (fixup-whitespace)
      (unless (looking-at comment-padding)
	(insert comment-padding)))
    (when (< 0 (length comment-end))
      (if (<= end (line-end-position))
	  (goto-char end)
	(goto-char (line-end-position)))
      (let ((pos (point)))
	(unless span
	  (unless (looking-at comment-padding)
	    (insert comment-padding))
	  (insert comment-end)
	  (save-excursion
	    (goto-char pos)
	    (fixup-whitespace)
	    (unless (looking-at comment-padding)
	      (when (looking-at " ")
		(replace-match ""))
	      (insert comment-padding))))))
    (if (< (line-end-position) (1- end))
	(progn
	  (forward-line 1)
	  (comment-intern-lor (line-beginning-position) end indent box span comment-column first add comment-end-stringp))
      (when span
	(unless (looking-at comment-padding)
	  (insert comment-padding))
	(let ((pos (point)))
	  (insert comment-end)
	  (save-excursion
	    (goto-char pos)
	    (fixup-whitespace)))))
    (when box
      (comment-box-intern-lor beg end comment-end-stringp comment-fill-char add indent))))

;;;###autoload
(defun comment-box-intern-lor (beg end comment-end-stringp comment-fill-char add indent)
  (let* ((comment-end-stringp comment-end-stringp)
	 (maxline (progn
		    (when indent
		      (back-to-indentation)
		      (indent-according-to-mode))
		    (comment-set-line-max-lor beg end comment-end-stringp)))
	 (comment-fill-column
	  (if comment-end-stringp
	      (- maxline (length comment-end))
	    (+ maxline add))))
    (if comment-end-stringp
	(comment-endstring-boxes-lor beg end comment-fill-column)
      (comment-set-boxes-lor beg end comment-end-stringp maxline comment-fill-column comment-fill-char add))
    (comment-set-head-foot-lor beg end comment-end-stringp maxline comment-fill-column comment-fill-char indent))
  (forward-line 1))

;;;###autoload
(defun comment-set-head-foot-lor (beg end comment-end-stringp maxline comment-fill-column comment-fill-char indent)
  (let ((start-col 0))
    (goto-char beg)
    (newline)
    (forward-line -1)
    (when indent (indent-according-to-mode))
    (insert comment-start)
    (fixup-whitespace)
    (setq start-col (progn (save-excursion (back-to-indentation) (current-column))))
    (comment-fill-form-lor comment-end-stringp comment-fill-column comment-fill-char)
    (goto-char end)
    (if (bolp)
	(progn
	  (newline)
	  (forward-line -1)
	  (indent-to start-col))
      (progn
	(end-of-line)
	(newline)
	(indent-to start-col))))
  (insert comment-start)
  (fixup-whitespace)
  (comment-fill-form-lor comment-end-stringp comment-fill-column comment-fill-char))

;;;###autoload
(defun comment-set-line-max-lor (beg end comment-end-stringp &optional line-length)
  (let ((line-length (or line-length 0)))
    (goto-char beg)
    (if comment-end-stringp
	(search-forward comment-end (line-end-position) t 1)
      (end-of-line)
      (skip-chars-backward " \t"))
    (untabify (line-beginning-position) (point))
    (setq line-length (- (point) (line-beginning-position)))
    (when (< maxline line-length)
      (setq maxline line-length))
    (forward-line 1))
  (when (<= (line-end-position) end)
    (comment-set-line-max-lor (line-beginning-position) end comment-end-stringp line-length))
  (message "%s" maxline)
  maxline)

;;;###autoload
(defun comment-endstring-boxes-lor (beg end comment-fill-column)
  (goto-char beg)
  (while (<= (line-end-position) end)
    (search-forward comment-end)
    (goto-char (match-beginning 0))
    (while (< (current-column) comment-fill-column)
      (insert " "))
    (forward-line 1)
    (comment-endstring-boxes-lor (line-beginning-position) end comment-fill-column)))

;;;###autoload
(defun comment-set-boxes-lor (beg end comment-end-stringp maxline comment-fill-column comment-fill-char add)
  (goto-char beg)
  (while (<= (line-end-position) end)
    (end-of-line)
    (skip-chars-backward " \t")
    (unless (looking-at comment-padding)
      (insert comment-padding))
    (while (< (current-column) comment-fill-column)
      (insert " "))
    (if comment-end-stringp
	(insert comment-end)
      (insert comment-start)
      (skip-chars-backward " \t")
      (while (and (empty-stringp comment-end) (< 0 add))
	(insert comment-start)
        (skip-chars-backward " \t")
	(setq add (1- add))))
    (forward-line 1)
    (comment-set-boxes-lor (line-beginning-position) end comment-end-stringp maxline comment-fill-column comment-fill-char add)))

;;;###autoload
(defun comment-fill-form-lor (comment-end-stringp comment-fill-column comment-fill-char)
  (when comment-pad-boxes-lor
    (if (looking-at comment-padding)
	(forward-char 1)
      (insert comment-padding)))
  (while (< (current-column) comment-fill-column)
    (cond ((looking-at "[\n\r\f]")
	   (insert comment-fill-char))
	  ((looking-at "[ \t]")
	   (replace-match comment-fill-char))))
  (save-excursion
    (when comment-end-stringp
      (insert comment-end)))
  (unless comment-pad-boxes-lor
    (when (looking-at comment-padding)
      (replace-match comment-fill-char)))
  (unless comment-end-stringp (insert (concat comment-fill-char comment-start))))

;;;###autoload
(defun comment-right-margin-lor (&optional span)
  "Insert a comment after code on current line or region. "
  (interactive "*")
  (let ((add comment-add))
    (end-of-line)
    (skip-chars-backward " \t")
    (insert " ")
    (insert comment-start)
    (skip-chars-backward " \t")
    (while (and (empty-stringp comment-end) (< 0 add))
      (insert comment-start)
      (skip-chars-backward " \t")
      (setq add (1- add))))
  (fixup-whitespace)
  (forward-char 1)
  (comment-inl-endform-lor))

;;;###autoload
(defun comment-indent-new-line-lor (&optional span)
  "Break line at point and indent, continuing comment if within one. "
  (interactive "*")
  (let ((pos (condition-case nil (search-backward (string-strip comment-start) (line-beginning-position) nil)
	       (error "Can't see a comment in this line!")))
	(col (current-column))
	(add comment-add)
	npos)
    (while (eq (char-before) (char-after))
      (forward-char -1))
    (setq col (current-column))
    (setq npos (point))
    (end-of-line)
    (newline)
    (indent-to-column col)
    (insert comment-start)
    (skip-chars-backward " \t")
    (while (and (empty-stringp comment-end) (< 0 add))
      (insert comment-start)
      (skip-chars-backward " \t")
      (setq add (1- add)))
    (when (and (looking-back "[^ ]") comment-padding)
      (insert comment-padding))
    (comment-inl-endform-lor)))

;;;###autoload
(defun comment-inl-endform-lor ()
  (save-excursion
    (when (and (comment-end-stringp) (not span))
      (insert comment-end)))
  (unless (or (looking-at comment-padding)
	      (looking-back comment-padding))
    (insert comment-padding)))

(defalias 'uncomment-region-lor 'uncomment-lor)

;;;###autoload
(defun uncomment-lor (&optional copy not-all beg end)
  (interactive "*P")
  (let ((beg (cond ((and beg end) beg)
		   ((region-active-p)
		    (region-beginning))
		   (t (line-beginning-position))))
	(end (cond ((and beg end) (copy-marker end) end)
		   ((region-active-p)
		    (copy-marker end))
		   (t (copy-marker (line-end-position)))))
	(all (not not-all))
	(start-replace (make-string (length comment-start) ? ))
	(end-replace (when (comment-end-stringp)
		       (make-string (length comment-end) ? )))
	first (maxline 0))
    (when (and copy (eq 4 copy))
      (copy-region-as-kill beg end))
    (uncomment-intern-lor beg end all start-replace end-replace)
    ;; (goto-char end)
    (unless comment-stay-on-line-lor
      (end-of-line)
      (forward-line 1))
    (save-excursion
      (when copy
	(goto-char beg)
	(insert (car kill-ring))
	(newline-and-indent)))
    (when comment-fontify-lor (font-lock-fontify-buffer))
    (indent-region beg (point))))

;;;###autoload
(defun uncomment-intern-lor (beg end all start-replace end-replace)
  (goto-char beg)
  (cond ((looking-at (regexp-quote comment-start))
	 (comment-startreplace-intern-lor))
	((search-comment-start-n-in-stringp beg end)
	 (goto-char beg)
	 (when (search-forward comment-start end t 1)
	   (goto-char (match-beginning 0))
	   (comment-startreplace-intern-lor)))
	((and (< beg (point)) (in-string-p (point)))
	 (uncomment-intern-lor (point) end all start-replace end-replace)))
  (when (and (< beg (point)) (< (point) end) all)
    (uncomment-intern-lor (point) end all start-replace end-replace))
  (let ((indent (save-excursion (back-to-indentation) (current-column))))
    (when (and (< (point) end) (comment-end-stringp) (search-forward comment-end nil t 1))
      (replace-match end-replace)
      (when (< end (point)) (setq end (point))
	    (indent-region beg end indent))))
  (when (< (line-end-position) end)
    (forward-line 1)
    (uncomment-intern-lor (line-beginning-position) end all start-replace end-replace)))

;;;###autoload
(defun comment-dwim-lor (&optional copy beg end)
  "Modifies `comment-or-uncomment-lor'.
Calls `uncomment' with arg `not-all'.
Thus if a region contains lines with one or more comments,
only lines with one comment are uncommented completly.
Calls `comment' at the end of line as `comment-right-margin-lor'.
Comments empty lines ignoring value of `comment-empty-lines-lor'"
  (interactive "*P")
  (let ((copy (when copy (prefix-numeric-value copy)))
	(beg (cond ((and beg end) beg)
		   ((region-active-p)
		    (region-beginning))
		   (t (line-beginning-position))))
	(end (cond ((and beg end) (copy-marker end) end)
		   ((region-active-p)
		    (copy-marker (region-end)))
		   (t (copy-marker (line-end-position))))))
    (cond ((empty-line-p)
	   (let ((comment-empty-lines-old comment-empty-lines-lor))
	     (setq comment-empty-lines-lor t)
	     (comment-indent-lor)
	     (setq comment-empty-lines-lor comment-empty-lines-old)))
	  ;; at a single line
	  ((and (not (multiline-region-p)) (not (region-active-p)))
	   (comment-right-margin-lor))
	  (t (comment-or-uncomment-lor copy t beg end)))))

;;;###autoload
(defun comment-or-uncomment-lor (&optional copy not-all beg end)
  "Comment line or region, unless it's already commented:
uncomment then.
Uncommenting: with arg NOT-ALL only first comment is removed.
With region: Only first line is checked for decision.
Thus if first line is not commented, but some lines below are,
region is commented alltogether. "
  (interactive "*P")
  (save-match-data
    (let ((copy (when copy (prefix-numeric-value copy)))
	  (not-all not-all)
	  (beg (cond ((and beg end) beg)
		     ((region-active-p)
		      (region-beginning))
		     (t (line-beginning-position))))
	  (end (cond ((and beg end) (copy-marker end) end)
		     ((region-active-p)
		      (copy-marker (region-end)))
		     (t (copy-marker (line-end-position))))))
      ;; (save-excursion ;; check for already commented region
      (cond (;; maybe inside a multiline-comment
             (progn
               (save-excursion
                 (save-restriction
                   (widen)
                   (nth 4 (parse-partial-sexp (defun-beginning-position) (point))))))
	     (back-to-indentation)
	     (when (or (looking-at comment-start)
		       (search-comment-start-n-in-stringp (point) end)
		       (search-backward comment-start))
	       (setq beg (match-beginning 0))
	       (search-forward comment-end nil t 1)
	       (setq end (point))
	       (funcall 'uncomment-function-lor copy not-all beg end)))
	    ((search-comment-start-n-in-stringp beg end)
	     (funcall 'uncomment-function-lor copy not-all beg end))
	    (t (funcall 'comment-function-lor copy beg end))))))

;;; helpers

;;;###autoload
(defun multiline-region-p (&optional ispec beg end)
  "Return nil or the position, where a \\r, \\n or \\f was found. "
  (interactive "p")
  (save-excursion
    (let* ((beg (cond ((and beg end) beg)
		      ((region-active-p)
		       (region-beginning))
		      (t (line-beginning-position))))
	   (end (cond ((and beg end) (copy-marker end))
		      ((region-active-p)
		       (copy-marker (region-end)))
		      (t (copy-marker (line-end-position)))))
	   (ml (progn (goto-char beg)
		      (re-search-forward "[\r\n\f]" end t 1))))
      (when ispec (message "%s" ml))
    ml)))

;;;###autoload
(defun in-string-p (&optional pos)
  (let ((orig (or pos (point))))
    (save-excursion
      (save-restriction
        (widen)
      (beginning-of-defun)
      (numberp (nth 3 (parse-partial-sexp (point) orig)))))))

;;;###autoload
(defun empty-stringp (string)
  (string= "" string))

;;;###autoload
(defun comment-end-stringp (&optional ispec)
  "Returns t if comment-end in comment-style is a non-empty string.  "
  (interactive "*p")
  (let ((erg (and (not (empty-stringp comment-end))
		  (stringp comment-end))))
    (when ispec
      (message "%s" erg))
    erg))

;;;###autoload
(defun search-comment-start-n-in-stringp (&optional beg end)
  "Search comment start,
make sure, it's not part of a string. "
  (interactive "r")
  (let ((beg (or beg beg
		 (and (when (and (interactive-p) (region-active-p)) beg) beg)
		 (point)))
	(end (or end end
		 (when (and (interactive-p) (region-active-p)) end)
		 (line-end-position)))
	(pos (point)))
    (goto-char beg)
    (while
	(and (search-forward comment-start end t 1) (in-string-p))
      (search-comment-start-n-in-stringp))
    (and (< pos (point)) (not (in-string-p)))
    ))

;; from GNU Emacs subr.el
(when (featurep 'xemacs)
  (defun looking-back (regexp &optional limit greedy)
    "Return non-nil if text before point matches regular expression REGEXP.
    Like `looking-at' except matches before point, and is slower.
    LIMIT if non-nil speeds up the search by specifying a minimum
    starting position, to avoid checking matches that would start
    before LIMIT.
    If GREEDY is non-nil, extend the match backwards as far as possible,
    stopping when a single additional previous character cannot be part
    of a match for REGEXP."
    (let ((start (point))
	  (pos
	   (save-excursion
	     (and (re-search-backward (concat "\\(?:" regexp "\\)\\=") limit t)
		  (point)))))
      (if (and greedy pos)
	  (save-restriction
	    (narrow-to-region (point-min) start)
	    (while (and (> pos (point-min))
			(save-excursion
			  (goto-char pos)
			  (backward-char 1)
			  (looking-at (concat "\\(?:"  regexp "\\)\\'"))))
	      (setq pos (1- pos)))
	    (save-excursion
	      (goto-char pos)
	      (looking-at (concat "\\(?:"  regexp "\\)\\'")))))
      (not (null pos)))))

;;;###autoload
(defun indent-with-spaces  (column)
  "Indent with spaces to column "
  (interactive "*")
  (while (< (current-column) column)
    (insert " "))
  (forward-char 1))

;;;###autoload
(defun comment-startreplace-intern-lor ()
  (let ((add comment-add))
    (while (< 0 (1+ add))
      (setq add (1- add))
      (skip-chars-forward " \t")
      (when (looking-at (regexp-quote comment-start))
	(replace-match start-replace)))))

(defcustom empty-line-p-chars "^[ \t\f\r]*$"
  "Empty-line-p-chars."
 :type 'regexp
 :group 'convenience)

;;;###autoload
(defun empty-line-p (&optional ispec)
  "Returns t if cursor is at an empty line, nil otherwise."
  (interactive "p")
  (save-excursion
    (beginning-of-line)
    (when ispec
      (message "%s" (looking-at empty-line-p-chars)))
    (looking-at empty-line-p-chars)))

(unless (featurep 'xemacs)
  (defun region-active-p ()
    "and mark-active transient-mark-mode
 (not (eq (region-beginning) (region-end)"
    (and mark-active transient-mark-mode
         (not (eq (region-beginning) (region-end))))))

;;; from string-strip.el --- Strip CHARS from STRING

;; (setq strip-chars-before  "[ \t\r\n\f]*")
(defcustom strip-chars-before  "[ \t\r\n\f]*"
 "Regexp indicating which chars shall be stripped before STRING - which is defined by `string-chars-preserve'."

:type 'string
:group 'convenience)

;; (setq strip-chars-after  "[ \t\r\n\f]*")
(defcustom strip-chars-after  "[ \t\r\n\f]*\\'"
  "Regexp indicating which chars shall be stripped after STRING - which is defined by `string-chars-preserve'."

:type 'string
:group 'convenience)

(defcustom string-chars-preserve "\\(.*?\\)"
  "Chars preserved of STRING.
`strip-chars-after' and
`strip-chars-before' indicate what class of chars to strip."
  :type 'string
  :group 'convenience)

;;;###autoload
(defun string-strip (str &optional chars-before chars-after chars-preserve)
  "Return a copy of STR, CHARS removed.
`CHARS-BEFORE' and `CHARS-AFTER' default is \"[ \t\r\n\f]*\",
i.e. spaces, tabs, carriage returns, newlines and newpages.
`CHARS-PRESERVE' must be a parentized expression,
it defaults to \"\\(.*?\\)\""
  (let ((s-c-b (or chars-before
                   strip-chars-before))
        (s-c-a (or chars-after
                   strip-chars-after))
        (s-c-p (or chars-preserve
                   string-chars-preserve)))
    (string-match
     (concat "\\`[" s-c-b"]*" s-c-p "[" s-c-a "]*\\'") str)
    (match-string 1 str)))

;;;###autoload
(defconst comment-version "2.0"
  "Version number of this sh-beg-end.el.")

;;;###autoload
(defun comment-version (&optional ispec)
  "Print and/or return comment-version."
  (interactive "p")
  (when ispec
    (message "%s" comment-version))
  comment-version)

(provide 'comment)
;;; comment.el ends here
