(setq calendar-latitude +60.79)
(setq calendar-longitude +10.69)

(defun get-sunrise-sunset-string ()
  (save-window-excursion
    (let ((regex "[0-9]+:[0-9]+[ap]m")
          (s (sunrise-sunset))
          (buf (get-buffer "*temp*")))
      (unless (and (stringp s)
                   (string-match-p regex s))
        (when buf
          (with-current-buffer buf
            (let* ((s1 (buffer-string))
                   (s2 (if (string-match-p regex s1)
                           s1 nil)))
              (setq s s2)
              (kill-buffer buf)))))
      s)))

(defun convert-time-format-of-sunrise-and-sunset ()
  (let (rise_set a b c d e f)
    (setq rise_set (get-sunrise-sunset-string))
    (if (string-match "0:00 hours daylight" rise_set) ;If polar-night
        (progn
          (setq 24h/sunrise 'polar-night
                24h/sunset 'polar-night))
      (if (string-match "24:00 hours daylight" rise_set) ;If midnight-sun
          (progn
            (setq 24h/sunrise 'midnight-sun
                  24h/sunset 'midnight-sun))
        (progn                          ;Convert 12hr to 24hr
          (string-match "\\([0-9][0-9]?\\):\\([0-9][0-9]\\)\\([ap]m\\)[^0-9]+\\([0-9][0-9]?\\):\\([0-9][0-9]\\)\\([ap]m\\)" rise_set)
          (setq a (string-to-number (match-string 1 rise_set))
                b (string-to-number (match-string 2 rise_set))
                c (match-string 3 rise_set)
                d (string-to-number (match-string 4 rise_set))
                e (string-to-number (match-string 5 rise_set))
                f (match-string 6 rise_set))
          (if (equal c "pm")
              (setq 24h/sunrise (list (+ 12 a) b))
            (setq 24h/sunrise (list a b)))
          (if (equal f "pm")
              (setq 24h/sunset (list (+ 12 d) e))
            (setq 24h/sunset (list d e))))))))

(if (and
     (boundp 'calendar-longitude)
     (boundp 'calendar-latitude))
    (progn
      (convert-time-format-of-sunrise-and-sunset)
      (run-with-timer 0 (* 60 60 24) 'convert-time-format-of-sunrise-and-sunset))
  ())

(defun theme-auto-switch ()
  "Automatically switch between dark and light moe-theme."
  (interactive)
  (let ((now (list (string-to-number (format-time-string "%H"))
                   (string-to-number (format-time-string "%M")))))
    (if (and (or (> (car now) (car 24h/sunrise))
                 (and (= (car now) (car 24h/sunrise))
                      (>= (second now) (second 24h/sunrise))))
             (or (< (car now) (car 24h/sunset))
                 (and (= (car now) (car 24h/sunset))
                      (< (second now) (second 24h/sunset)))))
        (theme-set 'day)
      (theme-set 'night))))

(theme-auto-switch)
(setq moe-timer (run-with-timer 0 (* 1 60) 'theme-auto-switch))
