(require 'cl)

(defun clj-ns ()
  (let ((fname (buffer-file-name)))
    (string-match (concat (projectile-project-p) "src/\\(.+\\)\\\.clj") fname)
    (replace-regexp-in-string "/" "." (match-string 1 fname))))

(defun clojure-utils-unique-alist (alist)
  (let ((key-list nil)
        (new-list nil))
    (while alist
      (when (and (car alist) (not (member (caar alist) key-list)))
        (setq new-list (cons (car alist) new-list))
        (setq key-list (cons (caar alist) key-list)))
      (setq alist (cdr alist)))
    new-list))

(defun clojure-utils-partition-binding-alist (alist)
  (let* ((separator-el (car (elt alist 0)))
         (curr-alist (list (elt alist 0)))
         (result ())
         (i 1))
    (while (< i (length alist))
      (when (equal (car (elt alist i)) separator-el)
        (setq result (cons (nreverse curr-alist) result))
        (setq curr-alist nil))
      (setq curr-alist (cons (elt alist i) curr-alist))
      (setq i (+ i 1)))
    (cons (nreverse curr-alist) result)))
 
;; (clojure-utils-partition-binding-vector [a 1 b 2 c 3 a 4 b 5 c 6])

(defun curr-sexp-fn ()
  (interactive)
  (condition-case nil
      (let* ((beg (point))
             (_ (forward-list))
             (end (point)))
        (goto-char beg)
        (filter-buffer-substring beg end))
    (error "")))

(defun clojure-utils-goto-top-sexp-beginning ()
  (condition-case nil
      (progn
        (paredit-backward-up)
        (clojure-utils-goto-top-sexp-beginning))
    (error "")))

(defun clojure-utils-is-let-sexp ()
  (string-match "(let" (curr-sexp-fn)))

(defun clojure-utils-simplify-string (s)
  (set-text-properties 0 (length s) nil s)
  (s-trim s))

(defun clojure-utils-read-binding-vector ()
  (save-excursion
    (let* ((_ (progn
                (paredit-forward-down)
                (forward-list)
                (backward-char)))
           (end (point))
           (_ (progn
                (forward-char)
                (backward-list)
                (paredit-forward-down)))
           (beg (point)))
      (previous-line)
      (mapcar (lambda (bind)
                (next-line)
                (let ((bind (s-trim bind)))
                  (if (s-matches? "^[[{]" bind)
                      (let* ((beg (point))
                             (_ (forward-list))
                             (sep (point))
                             (_ (move-end-of-line nil))
                             (end (min end (point)))
                             (_ (goto-char beg)))
                        (cons (clojure-utils-simplify-string (filter-buffer-substring beg sep))
                              (clojure-utils-simplify-string (filter-buffer-substring sep end))))
                    (let ((sep (s-index-of " " bind)))
                      (cons (clojure-utils-simplify-string (s-left sep bind))
                            (clojure-utils-simplify-string (s-right (- (length bind) sep) bind)))))))
              (s-lines (filter-buffer-substring beg end))))))

(defun clojure-utils-vector-to-alist (v)
  (clojure-utils-unique-alist
   (nreverse
    (loop for i from 0 to (- (length v) 1)
          if (evenp i)
          collect (cons (elt v i) (elt v (+ i 1)))))))

(defun clojure-utils-make-new-testcase-with-last-values (bindings)
  (apply #'s-concat
         (loop for bind in (car (clojure-utils-partition-binding-alist bindings))
               collect (format "\n%s %s" (car bind) (cdr bind)))))

(defun clojure-utils-transform )
(defun clojure-utils-get-unique-let-variables (let-expr)
  )

(mapcar #'car (car (clojure-utils-partition-binding-vector [a 1 b 2 c 3 a 4 b 5 c 6]))) 

(defun clojure-utils-make-another-deflet-testcase ()
  (interactive)
  (clojure-utils-goto-top-sexp-beginning)
  (let* ((bindings (clojure-utils-read-binding-vector))
         (beg (point))
         (edit-start nil))
    (paredit-forward-down)
    (forward-list)
    (paredit-backward-down)
    (setq edit-start (point))
    (insert (clojure-utils-make-new-testcase-with-last-values bindings))
    (indent-region beg (point))
    (goto-char edit-start)
    (next-line)))

(clojure-utils-partition-binding-alist foo)
(setq foo (clojure-utils-read-binding-vector))
(let [[a b] 1
      c 2
      d 3
      [a b] 4
      c 5
      d 6
      [a b] 4
      c 5
      d 6]
  (+ a b))

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
