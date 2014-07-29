;;; javad.el --- Java decompiler

(defun javad-buffer ()
  "run jad on contents of buffer"
  (interactive)
  (lexical-let* ((f-name (buffer-file-name))
                 (new-f-name (concat temporary-file-directory "jad/" (file-name-base f-name) ".jad"))
                 (old-buf (buffer-name))
                 (done (lambda (&rest args)
                         (interactive)
                         (progn
                           (kill-buffer (current-buffer))
                           (kill-buffer old-buf)))))
    (call-process "jad" nil nil nil "-o" "-d" (file-name-directory new-f-name) f-name)
    (find-file new-f-name)
    (revert-buffer t t)
    (javad-mode)
    (kill-whole-line 3)
    (whitespace-cleanup)
    (save-buffer)
    (setq buffer-read-only 't)
    (local-set-key [(q)] done)))

(define-derived-mode javad-mode java-mode "Java dissasembled"
  "Major mode for Javad")

(provide 'javad)
