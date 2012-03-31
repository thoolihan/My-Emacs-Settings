(require 'cl)

(defsubst wc-whitespace-char? ()
  "is current character whitespace?"
  (not (looking-at "[A-Za-z0-9]")))

(defun wc-buffer (b)
  "count words in a buffer"
  (interactive "bBuffer to Analyze ")
  (let ((word-count 0) (in-word nil) 
        (char-count 0) (line-count 1))
    (save-excursion
      (set-buffer b)
      (beginning-of-buffer)
      (while (not (eobp))
        (if in-word
            (if (wc-whitespace-char?)
                (setq in-word nil))
          (if (not (wc-whitespace-char?))
              (progn
                (setq in-word t)
                (incf word-count))))
        (incf char-count)
        (if (looking-at "[\r\n]") (incf line-count))
        (forward-char 1))
      (message "%d chars %d words %d lines" 
               char-count word-count line-count))))

(defun wc ()
  "count words in current buffer"
  (interactive)
  (wc-buffer (buffer-name)))
