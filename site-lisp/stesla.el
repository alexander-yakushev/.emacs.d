;; Support functions for `stesla-rotate-buffers'.  From the EmacsWiki.

(defvar stesla-hated-buffers)
(setq stesla-hated-buffers '("KILL" "*Apropos*" "*Completions*" "*grep*"
                               ".newsrc-dribble" ".bbdb" "sent-mail" "*vc*"
                               "*Compile-Log*" "*Help*" "*Messages*" "*Packages*" "Async
                               Shell Command" "*slime-events*" "*elein-swank*" "*swank*"))

(defvar stesla-hated-buffer-regexps)
(setq stesla-hated-buffer-regexps '("^ " "*Buffer" "^\\*trace" "^\\*tramp"
                                    " (Sunrise)" "^\\*magit" "^magit" "^\\*Customize" "^*Find*"
                                    "^*Quail" "^\\*slime-repl" "^\\*SLIME" "^\\*"
                                    "\\(Sunrise\\)$" "\\(Sunrise\\)<..?>$"))

(setq iswitchb-buffer-ignore (append stesla-hated-buffer-regexps stesla-hated-buffers))

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

(defun stesla-project-buffers ()
  (let* ((project (project-current))
         (root (file-truename (file-name-as-directory (project-root project))))
         bufs)
    (dolist (buf (buffer-list))
      (when (string-prefix-p root (file-truename
                                   (buffer-local-value 'default-directory buf)))
        (push buf bufs)))
    (nreverse bufs)))

(defun stesla-rotate-buffers* (buffer-list &optional reverse)
  (let* ((bl (stesla-delete-from-list (stesla-hated-buffers) buffer-list))
         (switch-to (nth (if reverse (- (length bl) 1) 1) bl)))
    (when switch-to
      (when (not reverse)
        (bury-buffer))
      (switch-to-buffer switch-to))))

;; `stesla-rotate-buffers': Like `bury-buffer' but with the capability to
;; exclude certain specified buffers.

(defun stesla-rotate-forward ()
  "Switch to the next buffer."
  (interactive)
  (stesla-rotate-buffers* (buffer-list (selected-frame))))

(defun stesla-rotate-backward ()
  "Switch to the previous buffer."
  (interactive)
  (stesla-rotate-buffers* (buffer-list (selected-frame)) t))

(defun stesla-project-rotate-forward ()
  "Switch to the next buffer of the current project."
  (interactive)
  (stesla-rotate-buffers* (stesla-project-buffers)))

(defun stesla-project-rotate-backward ()
  "Switch to the previous buffer of the current project."
  (interactive)
  (stesla-rotate-buffers* (stesla-project-buffers) t))

(provide 'stesla)
