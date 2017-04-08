;; Different useful functions.
(defun clone-and-comment-line (beg end)
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list nil nil)))
  (save-excursion
   (let ((beg (or beg (progn
                        (move-beginning-of-line 1)
                        (point))))
         (end (or end (progn
                        (move-end-of-line 1)
                        (point)))))
     (copy-region-as-kill beg end)
     (comment-region beg end)
     (goto-char beg)
     (open-line 1)
     (yank))))

(defun backward-delete-word (&optional arg)
  (interactive "p")
  (delete-region (point) (progn (backward-word) (point))))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max) nil))

(lexical-let ((temp-buffer-counter 0))
  (defun create-temp-buffer ()
    (interactive)
    (setf temp-buffer-counter (+ temp-buffer-counter 1))
    (switch-to-buffer (concat "temp" (number-to-string temp-buffer-counter)))))

(defun unfill-block ()
  "Remove ending chars on current paragraph. This command is
similar to a toggle of `fill-paragraph'. When there is a text
selection, act on the region."
  (interactive)

  ;; This command symbol has a property “'stateIsCompact-p”.
  (let ((bigFillColumnVal most-positive-fixnum)
        (deactivate-mark nil))
    (save-excursion
      ;; Determine whether the text is currently compact.
      (let ((fill-column most-positive-fixnum))
        (if (region-active-p)
            (fill-region (region-beginning) (region-end))
          (fill-paragraph nil))))))

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(let ((prev-margin (make-local-variable 'prev-margin)))
  (defun narrow-or-widen-dwim (p)
    "If the buffer is narrowed, it widens. Otherwise, it narrows intelligently.
Intelligently means: region, subtree, or defun, whichever applies
first.

With prefix P, don't widen, just narrow even if buffer is already
narrowed."
    (interactive "P")
    (declare (interactive-only))
    (if (and (buffer-narrowed-p) (not p))
        (progn (widen)
               (when prev-margin
                 (recenter prev-margin)))
      (setq prev-margin (cdr (nth 6 (posn-at-point))))
      (cond ((region-active-p)
             (narrow-to-region (region-beginning) (region-end)))
            ((derived-mode-p 'org-mode) (org-narrow-to-subtree))
            (t (narrow-to-defun))))))

(defun recenter-top-bottom (&optional arg)
  "Move current buffer line to the specified window line.
With no prefix argument, successive calls place point according
to the cycling order defined by `recenter-positions'.

A prefix argument is handled like `recenter':
 With numeric prefix ARG, move current line to window-line ARG.
 With plain `C-u', move current line to window center."
  (interactive "P")
  (cond
   (arg (recenter arg))			; Always respect ARG.
   (t
    (setq recenter-last-op
	  (if (eq this-command last-command)
	      (car (or (cdr (member recenter-last-op recenter-positions))
		       recenter-positions))
	    (car recenter-positions)))
    (let ((this-scroll-margin
	   (min (max 0 scroll-margin)
		(truncate (/ (window-body-height) 4.0)))))
      (cond ((eq recenter-last-op 'middle)
	     (recenter))
	    ((eq recenter-last-op 'top)
	     (recenter this-scroll-margin))
	    ((eq recenter-last-op 'bottom)
	     (recenter (- -1 this-scroll-margin)))
	    ((integerp recenter-last-op)
	     (recenter recenter-last-op))
	    ((floatp recenter-last-op)
	     (recenter (round (* recenter-last-op (window-height))))))))))

(defun git-fix-url ()
  (interactive)
  (let* ((old-url (buffer-substring-no-properties (region-beginning) (region-end)))
         (r (s-match "^https?://\\([^/]+\\)/\\([^/]+\\)/\\([^/]+\\)" old-url))
         (new-url (concat "git@" (cadr r) ":" (caddr r) "/" (cadddr r) ".git")))
    (kill-region (region-beginning) (region-end))
    (insert new-url)))

;; Yegges counting

(defun count-words--message (str start end)
  (let* ((lines (count-lines start end))
         (words (count-words start end))
         (chars (- end start))
         (yegges (sqrt (* (/ words 4000.0) (/ chars 25000.0)))))
    (message "%s has %d line%s, %d word%s, %f Yegge%s and %d character%s."
             str
             lines (if (= lines 1) "" "s")
             words (if (= words 1) "" "s")
             yegges (if (= yegges 1) "" "s")
             chars (if (= chars 1) "" "s"))))

(defun count-yegges ()
  (interactive)
  (cond ((use-region-p)
         (count-words--message "Region" (region-beginning) (region-end)))
        (t
         (count-words--buffer-message))))

(defun usefuls-zone-out ()
  (interactive)
  (let ((zone-programs '(zone-pgm-jitter
                         zone-pgm-putz-with-case
                         zone-pgm-dissolve
                         zone-pgm-explode
                         zone-pgm-rotate
                         zone-pgm-rotate-LR-lockstep
                         zone-pgm-rotate-RL-lockstep
                         zone-pgm-rotate-LR-variable
                         zone-pgm-rotate-RL-variable
                         zone-pgm-drip
                         zone-pgm-drip-fretfully
                         zone-pgm-five-oclock-swan-dive
                         zone-pgm-martini-swan-dive
                         zone-pgm-rat-race
                         zone-pgm-stress
                         zone-pgm-random-life)))
    (zone)))

(defun swap-buffers-in-windows ()
  "Put the buffer from the selected window in next window, and vice versa"
  (interactive)
  (let* ((this (selected-window))
         (other (other-window 1))
         (this-buffer (window-buffer this))
         (other-buffer (window-buffer other)))
    (set-window-buffer other this-buffer)
    (set-window-buffer this other-buffer)))

(defun smart-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.
Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.
If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (move-beginning-of-line nil)
    (search-forward-regexp "^[[:space:]]*" (line-end-position) t)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(provide 'usefuls)
