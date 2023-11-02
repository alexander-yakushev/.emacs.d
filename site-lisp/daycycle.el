(require 'solar)

(setq calendar-latitude +50.45)
(setq calendar-longitude +30.52)

(defvar daycycle-24h-sunrise nil)
(defvar daycycle-24h-sunset nil)
(defvar daycycle-theme-set-fn nil)

(require 'solar)

(defun daycycle-convert-time-format-of-sunriseset ()
  (let* ((sss (solar-sunrise-sunset (calendar-current-date)))
         (sunrisef (caar sss))
         (sunsetf (caadr sss))
         (dur (caddr sss)))
    (if (string-match "^0:00$" dur)
        (progn
          (setq daycycle-24h-sunrise 'polar-night
                daycycle-24h-sunset 'polar-night))
      (if (string-match "24:00" dur)
          (progn
            (setq daycycle-24h-sunrise 'midnight-sun
                  daycycle-24h-sunset 'midnight-sun))
        (progn                          ;Convert 12hr to 24hr
          (setq daycycle-24h-sunrise (list (floor sunrisef) (round (* 60 (mod sunrisef 1.0)))))
          (setq daycycle-24h-sunset (list (floor sunsetf) (round (* 60 (mod sunsetf 1.0))))))))))

(defun daycycle-start-daily-recalc-timer ()
  (when (and (boundp 'calendar-longitude)
             (boundp 'calendar-latitude))
    (daycycle-convert-time-format-of-sunriseset)
    (run-with-timer 0 (* 60 60 24) 'daycycle-convert-time-format-of-sunriseset)))

(defvar daycycle-theme-current-theme-type nil)

(defun daycycle-theme-auto-switch ()
  "Automatically switch between dark and light theme."
  (interactive)
  (let* ((hrs (string-to-number (format-time-string "%H")))
         (mins (string-to-number (format-time-string "%M")))
         (now-type (if (and (or (> hrs (car daycycle-24h-sunrise))
                                (and (= hrs (car daycycle-24h-sunrise))
                                     (>= mins (second daycycle-24h-sunrise))))
                            (or (< hrs (car daycycle-24h-sunset))
                                (and (= hrs (car daycycle-24h-sunset))
                                     (< mins (second daycycle-24h-sunset)))))
                       'day 'night)))
    (unless (eql now-type daycycle-theme-current-theme-type)
      (setq daycycle-theme-current-theme-type now-type)
      (funcall daycycle-theme-set-fn now-type))))

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

;; (cancel-timer daycycle-timer)

(provide 'daycycle)
