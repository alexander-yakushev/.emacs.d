;; TeX editing. Disabled for now, needs to be updated.

(require 'tex)
(setq TeX-auto-save t
      TeX-parse-self t)
(setq-default TeX-master nil
              TeX-engine 'xetex
              TeX-PDF-mode t)
(require 'reftex)
(setq reftex-plug-into-AUCTeX t)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

(defun get-bibtex-keys (file)
  (with-current-buffer (find-file-noselect file)
    (mapcar 'car (bibtex-parse-keys))))

(defun LaTeX-add-all-bibitems-from-bibtex ()
  (interactive)
  (mapc 'LaTeX-add-bibitems
        (apply 'append
               (mapcar 'get-bibtex-keys (reftex-get-bibfile-list)))))

;;; Texify everything

(require 'tex-buf)
(defun TeX-command-default (name)
  "Next TeX command to use. Most of the code is stolen from `TeX-command-query'."
  (cond ((if (string-equal name TeX-region)
             (TeX-check-files (concat name "." (TeX-output-extension))
                              (list name)
                              TeX-file-extensions)
           (TeX-save-document (TeX-master-file)))
         TeX-command-default)
        ((and (memq major-mode '(doctex-mode latex-mode))
              (TeX-check-files (concat name ".bbl")
                               (mapcar 'car
                                       (LaTeX-bibliography-list))
                               BibTeX-file-extensions))
         ;; We should check for bst files here as well.
         TeX-command-BibTeX)))

(defcustom TeX-texify-max-runs-same-command 5
  "Maximal run number of the same command"
  :type 'integer :group 'TeX-command)

(defun TeX-texify-sentinel (&optional proc sentinel)
  "Non-interactive! Call the standard-sentinel of the current LaTeX-process.
If there is still something left do do start the next latex-command."
  (set-buffer (process-buffer proc))
  (funcall TeX-texify-sentinel proc sentinel)
  (let ((case-fold-search nil))
    (when (string-match "\\(finished\\|exited\\)" sentinel)
      (set-buffer TeX-command-buffer)
      (unless (plist-get TeX-error-report-switches (intern (TeX-master-file)))
	(TeX-texify)))))

(defun TeX-texify ()
  "Get everything done."
  (interactive)
  (let ((nextCmd (TeX-command-default (TeX-master-file)))
	proc)
    (if (null nextCmd)
	(when  (called-interactively-p 'any)
	  (message "TeX-texify: Nothing to be done."))
      (TeX-command nextCmd 'TeX-master-file)
      (when (or (called-interactively-p 'any)
		(null (boundp 'TeX-texify-count-same-command))
		(null (boundp 'TeX-texify-last-command))
		(null (equal nextCmd TeX-texify-last-command)))
	(mapc 'make-local-variable '(TeX-texify-sentinel TeX-texify-count-same-command TeX-texify-last-command))
	(setq TeX-texify-count-same-command 1))
      (if (>= TeX-texify-count-same-command TeX-texify-max-runs-same-command)
	  (message "TeX-texify: Did %S already %d times. Don't want to do it anymore." TeX-texify-last-command TeX-texify-count-same-command)
	(setq TeX-texify-count-same-command (1+ TeX-texify-count-same-command))
	(setq TeX-texify-last-command nextCmd)
	(and (null (equal nextCmd TeX-command-Show))
	     (setq proc (get-buffer-process (current-buffer)))
	     (setq TeX-texify-sentinel (process-sentinel proc))
	     (set-process-sentinel proc 'TeX-texify-sentinel))))))

(require 'flymake)

(defun flymake-get-tex-args (file-name)
  (list "chktex" (list "-q" "-v0" file-name)))

(add-hook 'LaTeX-mode-hook
          (lambda () (LaTeX-add-environments
                 `("lstlisting")
                 `("dmath"))
            (TeX-add-symbols '("textup" 1))
            (setq TeX-master (buffer-file-name))
            (flymake-mode-on)))

;; Other things

(use-package google-translate :disabled t
  :keys ("C-x M-t" google-translate-fast)
  :config
  (defun google-translate-fast (ask)
    (interactive "P")
    (let* ((langs (if (equal ask nil) (google-translate-read-args nil nil) '("ru" "uk")))
           (src (car langs))
           (dst (cadr langs))
           (text (buffer-substring-no-properties (region-beginning) (region-end)))
           (sliced-text (s-slice-at " " text))
           (final-text (s-join " "
                               (-map (lambda (slice)
                                       (google-translate-json-translation (google-translate-request src dst
                                                                                                    (s-join " " (-map 's-trim slice)))))
                                     (-partition-all 35 sliced-text)))))
      (kill-region (region-beginning) (region-end))
      (insert final-text))))

(use-package smali-mode :disabled t)

(use-package nlinum :disabled t
  :commands nlinum-mode
  :init
  (add-hook 'prog-mode-hook 'nlinum-mode)
  (add-hook 'org-mode-hook 'nlinum-mode))

(use-package slime :ensure t
  :commands slime
  :config
  (setq-default slime-lisp-implementations
                '((sbcl ("sbcl" "--dynamic-space-size" "9500"))))

  (use-package ac-slime :ensure t :demand t
    :init
    (add-hook 'slime-mode-hook 'set-up-slime-ac)
    (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
    (eval-after-load "auto-complete"
      '(add-to-list 'ac-modes 'slime-repl-mode)))
  (setq slime-contribs '(slime-asdf))
  (slime-setup '(slime-autodoc))
  (slime-setup '(slime-fancy slime-scratch slime-editing-commands
                             slime-fuzzy slime-repl slime-fancy-inspector
                             slime-presentations slime-asdf
                             slime-indentation))
  (require 'slime-autoloads))

(use-package ivy :ensure t :demand t
  :bind (("M-x" . counsel-M-x)
         ;; ("C-x C-f" . counsel-find-file)
         )
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t))

(use-package unicode-fonts :ensure t :demand t
  :config
  (unicode-fonts-setup))

(defun cider-decompile-last-form ()
  (interactive)
  (let* ((form (concat "(with-out-str (clj-java-decompiler.core/decompile "
                       (cider-last-sexp)
                       "))"))
         (result (nrepl-dict-get (cider-nrepl-sync-request:eval
                                  form nil (cider-current-ns))
                                 "value")))
    (pop-to-buffer "*cider-decompiler*")
    (read-only-mode -1)
    (delete-region (point-min) (point-max))
    (insert result)
    (let* ((result (car (read-from-string (buffer-substring-no-properties
                                           (point-min) (point-max)))))
           (result (replace-regexp-in-string "__auto__[0-9]+" "" result))
           (classnames (-map 'cadr (s-match-strings-all "class \\([^ ]+\\) " result)))
           (result (-reduce-from (lambda (r classname)
                                   (s-replace (concat classname ".") "" r))
                                 result classnames)))
      (delete-region (point-min) (point-max))
      (insert result)
      (java-mode)
      (whitespace-mode -1)
      (whitespace-cleanup)
      (beginning-of-buffer)
      (read-only-mode 1))))
