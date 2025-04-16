;;; -*- lexical-binding: t -*-

(defvar rainbow-numbers-mode--font-lock-keywords
  '(("\\([0-9]\\{3\\}\\)[0-9]\\{3\\}\\b" 1 font-lock-builtin-face t)
    ("\\([0-9]\\{1,3\\}\\)[0-9]\\{6\\}\\b" 1 font-lock-variable-name-face t)
    ("\\([0-9]\\{1,3\\}\\)[0-9]\\{9\\}\\b" 1 font-lock-function-name-face t)
    ("\\([0-9]\\{1,3\\}\\)[0-9]\\{12\\}\\b" 1 font-lock-type-face t)))

(define-minor-mode rainbow-numbers-mode
  "Highlight groupings within numbers to make long numbers easier to read."
  :init-value nil
  :lighter ""
  :keymap nil
  (font-lock-remove-keywords nil rainbow-numbers-mode--font-lock-keywords)
  (when (boundp 'font-lock-flush)
    (font-lock-flush))
  (font-lock-fontify-buffer)
  (when rainbow-numbers-mode
    (font-lock-add-keywords nil rainbow-numbers-mode--font-lock-keywords)
    (when font-lock-mode
      (when (boundp 'font-lock-flush)
        (font-lock-flush))
      (with-no-warnings (font-lock-fontify-buffer)))))

(provide 'rainbow-numbers-mode)
