;; Support functions for `stesla-rotate-buffers'.  From the EmacsWiki.

(defvar stesla-hated-buffers)
(setq stesla-hated-buffers '("KILL" "*Apropos*" "*Completions*" "*grep*"
                               ".newsrc-dribble" ".bbdb" "sent-mail" "*vc*"
                               "*Compile-Log*" "*Help*" "*Messages*" "*Packages*" "Async
                               Shell Command" "*slime-events*" "*elein-swank*" "*swank*"))

(defvar stesla-hated-buffer-regexps)
(setq stesla-hated-buffer-regexps '("^ " "*Buffer" "^\\*trace" "^\\*tramp"
                                    " (Sunrise)" "^\\*magit" "^\\*Customize" "^*Find*"
                                    "^*Quail" "^\\*slime-repl" "^\\*SLIME" "^\\*"
                                    "\\(Sunrise\\)$" "\\(Sunrise\\)<..?>$"))

(setq iswitchb-buffer-ignore (append stesla-hated-buffer-regexps  stesla-hated-buffers))

(defmacro stesla-buffer-regexp-mapcar (regexp buffers)
  "Find BUFFERS whose name matches REGEXP"
  `(mapcar (lambda (this-buffer)
             (if (string-match ,regexp (buffer-name this-buffer))
                 this-buffer))
           ,(if (symbolp buffers) (symbol-value buffers) buffers)))

(defmacro stesla-hated-buffer-from-regexps (regexps)
  "Generate a one-dimensional list of buffers that match REGEXPS"
  (append
   '(append)
   (mapcar (lambda (regexp)
             `(delete nil (stesla-buffer-regexp-mapcar ,regexp
                                                       (buffer-list))))
           (if (symbolp regexps) (symbol-value regexps) regexps))))

(defun stesla-delete-from-list (delete-these from-list)
  "Delete DELETE-THESE from FROM-LIST."
  (cond
   ((car delete-these)
    (if (member (car delete-these) from-list)
        (stesla-delete-from-list (cdr delete-these)
                                 (delete (car delete-these) from-list))
      (stesla-delete-from-list (cdr delete-these) from-list)))
   (t from-list)))

(defun stesla-hated-buffers ()
  "List of buffers I never want to see."
  (delete nil
          (append
           (mapcar 'get-buffer stesla-hated-buffers)
           (stesla-hated-buffer-from-regexps stesla-hated-buffer-regexps))))

;; `stesla-rotate-buffers': Like `bury-buffer' but with the capability to
;; exclude certain specified buffers.

(defun stesla-rotate-buffers (&optional n)
  "Switch to the Nth next buffer.  Negative arguments move backwards."
  (interactive)
  (unless n
    (setq n 1))
  (let ((my-buffer-list
         (stesla-delete-from-list (stesla-hated-buffers)
                                  (buffer-list (selected-frame)))))
    (switch-to-buffer
     (if (< n 0)
         (nth (+ (length my-buffer-list) n)
              my-buffer-list)
       (bury-buffer)
       (nth n my-buffer-list)))))

;; Windows-style C-TAB and C-M-TAB to switch buffers.

(global-set-key (kbd "C-.") 'stesla-rotate-buffers)
(global-set-key (kbd "C-,") (lambda ()
                              (interactive)
                              (stesla-rotate-buffers -1)))

;; This is C-TAB and C-M-TAB for the Linux console.  This requires special
;; setup; namely, you need to load a keymap file with /usr/bin/loadkeys
;; containing the following lines:
;;
;; control keycode 15 = Macro
;; control alt keycode 15 = Pause
;;
;; If you actually -have- a key that generates the Macro or Pause keysyms,  you
;; have a better keyboard than I.  For me, this makes Emacs DWIW.  Credit for
;; this hack goes to Alex Schroeder.

(global-set-key (kbd "ESC [ M") 'stesla-rotate-buffers)
(global-set-key (kbd "ESC [ P") (lambda ()
                                  (interactive)
                                  (stesla-rotate-buffers -1)))
