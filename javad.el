;;; javad.el --- Java decompiler

(defun javad-buffer ()
  "run jad on contents of buffer"
  (interactive)
  (lexical-let* ((f-name (buffer-file-name))
                 (new-f-name (concat (file-name-base f-name) ".jad"))
                 (old-buf (buffer-name))
                 (done (lambda (&rest args)
                         (interactive)
                         (progn
                           (kill-buffer (current-buffer))
                           (kill-buffer old-buf)))))
    (call-process "jad" nil nil nil f-name)
    (find-file new-f-name)
    (java-mode)
    (kill-whole-line 3)
    (whitespace-cleanup)
    (save-buffer)
    (setq buffer-read-only 't)
    (local-set-key [(q)] done)))

(provide 'javad)
