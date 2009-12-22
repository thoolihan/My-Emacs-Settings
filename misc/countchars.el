(defun count-chars-region (beginning end)
  "Print number of chars in the region."
  (interactive "r")
  (message "Counting chars in region ... ")

;;; 1. Set up appropriate conditions.
  (save-excursion
    (goto-char beginning)
    (let ((count 0))

;;; 2. Run the while loop.
      (while (< (point) end)
        (re-search-forward ".")
        (setq count (1+ count)))

;;; 3. Send a message to the user.
      (cond ((zerop count)
             (message
              "The region does NOT have any chars."))
            (t
             (message
              "The region has %d char(s)." count))))))
