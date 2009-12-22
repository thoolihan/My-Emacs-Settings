(defun google (s)
  "search google"
  (interactive "sSearch Terms: ")
  (browse-url (concat "http://google.com/search?q=" s)))

(defun stackoverflow (s)
  "search stack overflow"
  (interactive "sSearch Terms: ")
  (google (concat s " site:stackoverflow.com")))

(defun emacswiki (s)
  "search emacswiki"
  (interactive "sSearch Terms: ")
  (google (concat s " site:emacswiki.org")))

(defun wiki (s)
  "search wikipedia"
  (interactive "sSearch Terms: ")
  (google (concat s " site:en.wikipedia.org")))