;;; Java decompiler

;; (defun javad-buffer ()
;;   "run jad on contents of buffer"
;;   (interactive)
;;   (lexical-let* ((f-name (buffer-file-name))
;;                  (new-f-name (concat temporary-file-directory "jad/" (file-name-base f-name) ".jad"))
;;                  (old-buf (buffer-name))
;;                  (done (lambda (&rest args)
;;                          (interactive)
;;                          (progn
;;                            (kill-buffer (current-buffer))
;;                            (kill-buffer old-buf)))))
;;     (call-process "jad" nil nil nil "-o" "-d" (file-name-directory new-f-name) f-name)
;;     (find-file new-f-name)
;;     (revert-buffer t t)
;;     (javad-mode)
;;     (kill-whole-line 3)
;;     (whitespace-cleanup)
;;     (save-buffer)
;;     (setq buffer-read-only 't)
;;     (local-set-key [(q)] done)))

;; (defun procyon-buffer ()
;;   "run procyon on contents of buffer"
;;   (interactive)
;;   (lexical-let* ((f-name (buffer-file-name)))
;;     (switch-to-buffer "procyon1")
;;     (call-process "java" nil t t "-jar" "/Users/alex/Downloads/procyon-decompiler-0.5.30.jar" "-ci" f-name)))

(defun cfr-buffer ()
  "run CFR on contents of buffer"
  (interactive)
  (lexical-let* ((f-name (buffer-file-name))
                 ;; (new-f-name (concat temporary-file-directory "jad/" (file-name-base f-name) ".jad"))
                 (old-buf (buffer-name))
                 (done (lambda (&rest args)
                         (interactive)
                         (progn
                           (kill-buffer (current-buffer))
                           (kill-buffer old-buf)))))
    (switch-to-buffer (concat old-buf  ".java"))
    (call-process "java" nil t nil "-jar" (concat (getenv "HOME") "/Software/cfr-0.152.jar")
                  "--importfilter" "never"
                  f-name)
    (java-decompiler-mode)
    (goto-char 0)
    (kill-whole-line 3)
    ;; (whitespace-cleanup)
    ;; (save-buffer)
    (setq buffer-read-only 't)
    (local-set-key [(q)] done)))

(define-derived-mode java-decompiler-mode java-mode "Java decompiled"
  "Major mode for Java Decompiled")

(defun java-decompiler-find-class (&rest args)
  (interactive)
  (if (not (string= ".class" (substring (buffer-file-name) -6 nil)))
      nil
    (message "Show class as: [b]ytecode, [d]ecompile, or [i]dentity?")
    (let ((resp (read-char)))
      (cond
       ((= resp 98) (progn (javap-buffer) nil))
       ((= resp 100) (progn (cfr-buffer) nil))
       (t nil))
      (let ((buff (current-buffer)))
        (sr-quit)
        (switch-to-buffer buff)))))

(provide 'java-decompiler)
