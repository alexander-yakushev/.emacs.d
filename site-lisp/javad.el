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

(defun javad-find-class (&rest args)
  (interactive)
  (if (not (string= ".class" (substring (buffer-file-name) -6 nil)))
      nil
    (message "Show class as: [b]ytecode, [d]issasembly or [i]dentity?")
    (let ((resp (read-char)))
      (cond
       ((= resp 98) (progn (javap-buffer) nil))
       ((= resp 100) (progn (javad-buffer) nil))
       (t nil))
      (let ((buff (current-buffer)))
        (sr-quit)
        (switch-to-buffer buff)))))

(provide 'javad)
