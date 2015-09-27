(setq calendar-latitude +50.45)
(setq calendar-longitude +30.52)

(defvar daycycle-24h-sunrise nil)
(defvar daycycle-24h-sunset nil)
(defvar daycycle-theme-set-fn nil)

(defun daycycle-get-sunriseset-string ()
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

(defun daycycle-convert-time-format-of-sunriseset ()
  (let (rise_set a b c d e f)
    (setq rise_set (daycycle-get-sunriseset-string))
    (if (string-match "0:00 hours daylight" rise_set) ;If polar-night
        (progn
          (setq daycycle-24h-sunrise 'polar-night
                daycycle-24h-sunset 'polar-night))
      (if (string-match "24:00 hours daylight" rise_set) ;If midnight-sun
          (progn
            (setq daycycle-24h-sunrise 'midnight-sun
                  daycycle-24h-sunset 'midnight-sun))
        (progn                          ;Convert 12hr to 24hr
          (string-match "\\([0-9][0-9]?\\):\\([0-9][0-9]\\)\\([ap]m\\)[^0-9]+\\([0-9][0-9]?\\):\\([0-9][0-9]\\)\\([ap]m\\)" rise_set)
          (setq a (string-to-number (match-string 1 rise_set))
                b (string-to-number (match-string 2 rise_set))
                c (match-string 3 rise_set)
                d (string-to-number (match-string 4 rise_set))
                e (string-to-number (match-string 5 rise_set))
                f (match-string 6 rise_set))
          (if (equal c "pm")
              (setq daycycle-24h-sunrise (list (+ 12 a) b))
            (setq daycycle-24h-sunrise (list a b)))
          (if (equal f "pm")
              (setq daycycle-24h-sunset (list (+ 12 d) e))
            (setq daycycle-24h-sunset (list d e))))))))

(defun daycycle-start-daily-recalc-timer ()
  (when (and (boundp 'calendar-longitude)
             (boundp 'calendar-latitude))
    (daycycle-convert-time-format-of-sunriseset)
    (run-with-timer 0 (* 60 60 24) 'daycycle-convert-time-format-of-sunriseset)))

(defun daycycle-theme-auto-switch ()
  "Automatically switch between dark and light theme."
  (interactive)
  (let ((now (list (string-to-number (format-time-string "%H"))
                   (string-to-number (format-time-string "%M")))))
    (if (and (or (> (car now) (car daycycle-24h-sunrise))
                 (and (= (car now) (car daycycle-24h-sunrise))
                      (>= (second now) (second daycycle-24h-sunrise))))
             (or (< (car now) (car daycycle-24h-sunset))
                 (and (= (car now) (car daycycle-24h-sunset))
                      (< (second now) (second daycycle-24h-sunset)))))
        (funcall daycycle-theme-set-fn 'day)
      (funcall daycycle-theme-set-fn 'night))))

(defun daycycle-init (theme-set-fn arg)
  (setq daycycle-theme-set-fn theme-set-fn)
  (cond ((equal arg 'day)
         (funcall daycycle-theme-set-fn 'day))
        ((equal arg 'night)
         (funcall daycycle-theme-set-fn 'night))
        ((equal arg 'auto)
         (progn
           (daycycle-start-daily-recalc-timer)
           (daycycle-theme-auto-switch)
           (setq daycycle-timer (run-with-timer 0 (* 1 60) 'daycycle-theme-auto-switch))))
        (t (error "Wrong argument:" arg))))

(provide 'daycycle)
