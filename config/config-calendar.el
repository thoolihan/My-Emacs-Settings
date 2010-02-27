(setq calendar-latitude 41.3)
(setq calendar-longitude -81.6)
(setq calendar-location-name "Akron, OH")

(setq calendar-time-zone -300)
(setq calendar-standard-time-zone-name "EST")
(setq calendar-daylight-time-zone-name "EDT")

(add-hook 'diary-hook 'appt-make-list)
;(diary)
