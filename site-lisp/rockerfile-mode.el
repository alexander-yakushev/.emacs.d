;;; rockerfile-mode.el --- Major mode for editing Rocker's Rockerfiles

(require 'sh-script)
(require 'rx)

(defvar rocker-image-name nil)

(defgroup rockerfile nil
  "rockerfile code editing commands for Emacs."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :prefix "rockerfile-"
  :group 'languages)

(defcustom rockerfile-mode-hook nil
  "*Hook called by `rockerfile-mode'."
  :type 'hook
  :group 'rockerfile)

(defcustom rockerfile-use-sudo nil
  "Runs rocker builder command with sudo.")

(defvar rockerfile-font-lock-keywords
  `(,(cons (rx (or line-start "onbuild ")
               (group (or "from" "maintainer" "run" "cmd" "expose" "env"
                          "add" "copy" "entrypoint" "volume" "user" "workdir" "onbuild"
                          "mount" "import" "export" "push" "attach"))
               word-boundary)
           font-lock-keyword-face)
    ,@(sh-font-lock-keywords)
    ,@(sh-font-lock-keywords-2)
    ,@(sh-font-lock-keywords-1))
  "Default font-lock-keywords for `rockerfile mode'.")

(defvar rockerfile-mode-map
  (let ((map (make-sparse-keymap))
        (menu-map (make-sparse-keymap)))
    (define-key map "\C-c\C-b" 'rockerfile-build-buffer)
    (define-key map "\C-c\M-b" 'rockerfile-build-no-cache-buffer)
    (define-key map "\C-c\C-a" 'rockerfile-build-and-attach-to-current-line)
    (define-key map "\C-c\C-z" 'rockerfile-test-function)
    (define-key map "\C-c\C-c" 'comment-region)
    (define-key map [menu-bar rockerfile-mode] (cons "Rockerfile" menu-map))
    (define-key menu-map [dfc]
      '(menu-item "Comment Region" comment-region
                  :help "Comment Region"))
    (define-key menu-map [dfb]
      '(menu-item "Build" rockerfile-build-buffer
                  :help "Send the Rockerfile to rocker build"))
    (define-key menu-map [dfb]
      '(menu-item "Build without cache" rockerfile-build-no-cache-buffer
                  :help "Send the Rockerfile to rocker build without cache"))
    map))

(defvar rockerfile-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?' "\"" table)
    table)
  "Syntax table for `rockerfile-mode'.")

(define-abbrev-table 'rockerfile-mode-abbrev-table nil
  "Abbrev table used while in `rockerfile-mode'.")

(unless rockerfile-mode-abbrev-table
  (define-abbrev-table 'rockerfile-mode-abbrev-table ()))

;;;###autoload
;; (defun rockerfile-build-buffer (image-name)
;;   "Build an image based upon the buffer"
;;   (interactive
;;    (if (null rocker-image-name)
;;        (list (read-string "image-name: " nil nil))
;;      (list rocker-image-name)))
;;   (save-buffer)
;;   (if (stringp image-name)
;;       (async-shell-command
;;        (format "%s rocker build -t %s -f %s %s" (if rockerfile-use-sudo "sudo" "") image-name (buffer-file-name) (file-name-directory (buffer-file-name)))
;;        "*rocker-build-output*")
;;     (print "rocker-image-name must be a string, consider surrounding it with double quotes")))

(defun rockerfile-build-and-attach-to-current-line ()
  "Build an image based upon the buffer"
  (interactive)
  (end-of-line)
  (newline)
  (insert "ATTACH [\"/bin/bash\"]")
  (save-buffer)
  (save-current-buffer
    (let ((original-file (buffer-file-name)))
      (eshell)
      (eshell-return-to-prompt)
      (insert "rocker build --attach -f " original-file)
      (eshell-send-input)
      (end-of-buffer)))
  (kill-whole-line)
  (previous-line)
  (save-buffer)
  (with-current-buffer "*eshell*"
    (eshell-return-to-prompt)))

(defun rockerfile-build-buffer (arg)
  "Build an image for the current Rockerfile. With prefix
argument, also push the image."
  (interactive "P")
  (save-current-buffer
    (let ((original-file (buffer-file-name)))
      (eshell)
      (eshell-return-to-prompt)
      (if arg
          (insert "rocker build --push -f " original-file)
        (insert "rocker build -f " original-file))
      (eshell-send-input))))

;;;###autoload
(defun rockerfile-build-no-cache-buffer (image-name)
  "Build an image based upon the buffer without cache"
  (interactive
   (if (null rocker-image-name)
       (list (read-string "image-name: " nil nil))
     (list rocker-image-name)))
  (save-buffer)
  (if (stringp image-name)
      (async-shell-command
       (format "%s rocker build --no-cache -t %s -f %s %s" (if rockerfile-use-sudo "sudo" "") image-name (buffer-file-name) (file-name-directory (buffer-file-name)))
       "*rocker-build-output*")
    (print "rocker-image-name must be a string, consider surrounding it with double quotes")))

;; Handle emacs < 24, which does not have prog-mode
(defalias 'rockerfile-parent-mode
  (if (fboundp 'prog-mode) 'prog-mode 'fundamental-mode))

;;;###autoload
(define-derived-mode rockerfile-mode rockerfile-parent-mode "Rockerfile"
  "A major mode to edit Rockerfiles.
\\{rockerfile-mode-map}
"
  (set-syntax-table rockerfile-mode-syntax-table)
  (set (make-local-variable 'require-final-newline) mode-require-final-newline)
  (set (make-local-variable 'comment-start) "#")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-start-skip) "#+ *")
  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  (set (make-local-variable 'font-lock-defaults)
       '(rockerfile-font-lock-keywords nil t))
  (setq local-abbrev-table rockerfile-mode-abbrev-table))

;;;###autoload
(add-to-list 'auto-mode-alist '("Rockerfile.*\\'" . rockerfile-mode))
(add-to-list 'auto-mode-alist '(".*\\.rockerfile" . rockerfile-mode))

(provide 'rockerfile-mode)

;;;; Linting
;; (use-package flycheck :ensure t :demand t)
;; (defconst flycheck-rockerlint-form
;;   (flycheck-prepare-emacs-lisp-form
;;    (require 'package)
;;    (package-initialize)
;;    (require 'rockerlint)

;;    (let ((source (car command-line-args-left))
;;          ;; Remember the default directory of the process
;;          (process-default-directory default-directory))
;;      (rockerlint-lint source))))

;; (flycheck-define-checker rockerlint-checker
;;                          "Rockerfile checker."
;;                          :command ("emacs" "-Q" "-batch" "-L" "~/.emacs.d/site-lisp/"
;;                                    "--eval" (eval flycheck-rockerlint-form)
;;                                    "--" source)
;;                          :error-patterns
;;                          ((error line-start line ":" column ": " (message) line-end))
;;                          :modes (rockerfile-mode))

;; (push 'rockerlint-checker flycheck-checkers)
;; (add-hook 'rockerfile-mode-hook (lambda () (flycheck-mode 1)))

;;; rockerfile-mode.el ends here
