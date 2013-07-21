(require 's)

(defun clj-ns ()
  (replace-regexp-in-string
   "/" "."
   (cadr (s-match (concat (projectile-project-p) "src/\\(.+\\)\\\.clj")
                  (buffer-file-name)))))

(defun curr-sexp-fn ()
  (condition-case nil
   (let* ((beg (point))
          (_ (search-forward " "))
          (end (point)))
     (goto-char beg)
     (filter-buffer-substring beg end))
   (error "")))

(defun let-refactor (varname &optional initial)
  (let ((initial (or initial (let ((c (point)))
                               (backward-list)
                               c))))
    (condition-case nil
        (if (string-match "(let " (curr-sexp-fn))
            (let* ((beg (point))
                   (_ (progn
                        (paredit-forward-down)
                        (forward-list)
                        (backward-char)))
                   (end (point))
                   (part1 (concat (filter-buffer-substring beg end) "\n"))
                   (_ (progn
                        (goto-char initial)
                        (backward-list)))
                   (part2 (filter-buffer-substring end (point)))
                   (part3 (filter-buffer-substring (point) initial))
                   (_ (progn
                        (goto-char beg)
                        (forward-list)))
                   (part4 (filter-buffer-substring initial (point))))
              (goto-char beg)
              (kill-sexp)
              (insert (concat part1 varname " " part3 part2 varname part4))
              (indent-region beg (point))
              (goto-char end)
              (forward-list))
          (progn
            (paredit-backward-up)
            (let-refactor varname initial)))
      (error (goto-char initial)))))

(defun clj-refactor-let (varname)
  (interactive "sEnter form name: ")
  (let-refactor varname))
