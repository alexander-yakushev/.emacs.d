(progn ; packages initialization
  (require 'package)

  (setq load-prefer-newer t)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/"))
  (add-to-list 'package-archives
               '("melpa-stable" . "https://stable.melpa.org/packages/") t)
  (add-to-list 'package-archives
               '("SC"  . "http://joseito.republika.pl/sunrise-commander/") t)
  (package-initialize)

  (when (not (file-exists-p "~/.emacs.d/.initialized"))
    (package-refresh-contents)
    (load-file "~/.emacs.d/installed-packages.el")
    (mapc
     (lambda (package)
       (or (package-installed-p package)
           (package-install package)))
     unlogic-installed-packages)

    (unless (package-installed-p 'use-package)
      (package-refresh-contents)
      (package-install 'use-package))
    (write-region "" nil "~/.emacs.d/.initialized"))

  (require 'use-package)
  (put 'use-package 'lisp-indent-function 'defun)

  (push :keys (cdr (member :bind use-package-keywords)))

  (defun use-package-handler/:keys
      (name keyword arg rest state &optional override)
    (let ((global-keys nil) (override-keys nil) (curr-group :global)
          (local-map nil) (local-keys nil) (commands nil))
      (while arg
        (cond ((equal (car arg) :local)
               (progn
                 (setq curr-group :local)
                 (setq local-map (intern (concat (symbol-name name) "-mode-map")))
                 (add-to-list 'local-keys (cons local-map nil))
                 (setq arg (cdr arg))))
              ((keywordp (car arg))
               (progn
                 (setq curr-group (car arg))
                 (setq arg (cdr arg))))
              ((symbolp (car arg)) ;; means the mode map for local bindings
               (progn
                 (setq curr-group :local)
                 (setq local-map (car arg))
                 (add-to-list 'local-keys (cons local-map nil))
                 (setq arg (cdr arg))))
              ((stringp (car arg))
               (let ((bind-pair (cons (car arg) (cadr arg))))
                 (case curr-group
                   (:local (push bind-pair (cdr (assoc local-map local-keys))))
                   (:global (add-to-list 'global-keys bind-pair))
                   (:override (add-to-list 'override-keys bind-pair)))
                 (add-to-list 'commands (cadr arg))
                 (setq arg (cddr arg))))
              (t (error (concat "Malformed :keys list, wrong element: "
                                (prin1-to-string (car arg)))))))
      (use-package-concat
       (use-package-process-keywords name
         (use-package-sort-keywords
          (use-package-plist-maybe-put rest :defer t))
         (use-package-plist-append state :commands commands))
       `(,(when local-keys
            `(eval-after-load ',name
               ',(macroexp-progn
                  (mapcar (lambda (bindings)
                            `(bind-keys :map ,(car bindings) ,@(cdr bindings)))
                          local-keys))))
         (ignore (progn
                   ,(when global-keys `(bind-keys ,@global-keys))
                   ,(when override-keys `(bind-keys* ,@override-keys)))))))))

(add-to-list 'load-path "~/.emacs.d/site-lisp")

(load-file "~/.emacs.d/bindings.el") ;; Load bindings

(load-file "~/.emacs.d/esk.el") ;; Load Emacs starter kit leftovers

(load (setq custom-file (expand-file-name (locate-user-emacs-file "custom.el"))))

;; Turn off mouse interface early in startup to avoid momentary display
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

(cd "~") ;; start from userdir

;;; Day-to-day usage

(use-package s :ensure t)

(use-package sudo :commands sudo-find-file)

(use-package stesla
  :keys (:override
         "C-." stesla-rotate-buffers
         "C-," stesla-rotate-backwards))

(use-package sunrise-commander :ensure t
  :keys ("<f7>" sunrise
         "<C-f7>" sunrise-cd-resize

         sr-mode-map
         ";" dired-next-line
         "C-;" sr-advertised-find-file
         "C-h" sr-go-home
         "j" ido-sunrise
         "C-c C-o" sr-open-custom-terminal

         sr-tabs-mode-map
         "C-j" sr-cycle-bookmark
         "C-p" sr-dired-prev-subdir)
  :config
  (use-package sunrise-x-checkpoints :ensure t)
  (use-package sunrise-x-loop :ensure t)
  (use-package sunrise-x-mirror :ensure t)
  (use-package sunrise-x-tabs :ensure t)

  (defun sunrise-cd-resize ()
    (interactive)
    (setq sr-panes-height (* 2 (/ (frame-height) 3)))
    (sunrise-cd))

  (setq bookmarks '("~/.emacs.d/" "~/clojure/" "~/projects/grammarly/"))

  (let ((bookmark-counter 0))
    (defun sr-cycle-bookmark ()
      (interactive)
      (let ((bookmark (nth bookmark-counter bookmarks)))
        (setq bookmark-counter (+ bookmark-counter 1))
        (if (>= bookmark-counter (length bookmarks))
            (setq bookmark-counter 0))
        (sr-goto-dir bookmark))))

  (defun sr-go-home ()
    (interactive)
    (sr-goto-dir "~"))

  (defun sr-open-custom-terminal ()
    (interactive)
    (shell-command (concat "urxvt -cd \"" (expand-file-name (sr-choose-cd-target)) "\" -e zsh")))

  (defun ido-sunrise ()
    "Call `sunrise' the ido way.
    The directory is selected interactively by typing a substring.
    For details on keybindings, see `ido-find-file'."
    (interactive)
    (let ((ido-report-no-match nil)
          (ido-auto-merge-work-directories-length -1))
      (ido-file-internal 'read-only 'sr-advertised-find-file nil "Sunrise: " 'dir)))

  ;; Also auto refresh dired, but be quiet about it
  (setq global-auto-revert-non-file-buffers t)
  (setq auto-revert-verbose nil)

  (use-package javad
    :config
    (use-package javap-mode :ensure t)
    (add-hook 'find-file-hook 'javad-find-class))

  (openwith-mode t))

(use-package multiple-cursors :ensure t
  :keys ("C-M-<mouse-1>" mc/add-cursor-on-click
         "<C-down>" mc/mark-next-like-this
         "<C-M-down>" mc/mark-next-like-this-symbol
         "C-c m" mc/mark-all-like-this-dwim))

(use-package ediff
  :keys ("C-c d" ediff-opened-buffers)
  :commands ediff
  :config
  (add-hook 'ediff-startup-hook (lambda () (ediff-toggle-split)))

  (defun ediff-opened-buffers ()
    "Run Ediff on a pair of buffers, BUFFER-A and BUFFER-B."
    (interactive)
    (let* ((bA (ediff-other-buffer ""))
           (bB (progn
                 ;; realign buffers so that two visible bufs will be
                 ;; at the top
                 (save-window-excursion (other-window 1))
                 (ediff-other-buffer bA))))
      (setq job-name 'ediff-buffers)
      (ediff-buffers-internal bA bB nil '(ediff-toggle-split) nil)))

  (defvar ediff-do-hexl-diff nil
    "variable used to store trigger for doing diff in hexl-mode")

  (defadvice ediff-files-internal (around ediff-files-internal-for-binary-files activate)
    "catch the condition when the binary files differ
the reason for catching the error out here (when re-thrown from the inner advice)
is to let the stack continue to unwind before we start the new diff
otherwise some code in the middle of the stack expects some output that
isn't there and triggers an error"
    (let ((file-A (ad-get-arg 0))
          (file-B (ad-get-arg 1))
          ediff-do-hexl-diff)
      (condition-case err
          (progn
            ad-do-it)
        (error
         (if ediff-do-hexl-diff
             (let ((buf-A (find-file-noselect file-A))
                   (buf-B (find-file-noselect file-B)))
               (with-current-buffer buf-A
                 (hexl-mode 1))
               (with-current-buffer buf-B
                 (hexl-mode 1))
               (ediff-buffers buf-A buf-B))
           (error (error-message-string err)))))))

  (defadvice ediff-setup-diff-regions (around ediff-setup-diff-regions-for-binary-files activate)
    "when binary files differ, set the variable "
    (condition-case err
        (progn
          ad-do-it)
      (error
       (setq ediff-do-hexl-diff
             (and (string-match-p "^Errors in diff output.  Diff output is in.*"
                                  (error-message-string err))
                  (string-match-p "^\\(Binary \\)?[fF]iles .* and .* differ"
                                  (buffer-substring-no-properties
                                   (line-beginning-position)
                                   (line-end-position)))
                  (y-or-n-p "The binary files differ, look at the differences in hexl-mode? ")))
       (error (error-message-string err))))))

(use-package midnight :demand t
  :config (midnight-delay-set 'midnight-delay "11:59pm"))

(use-package smex :ensure t
  :keys ("M-x" smex)
  :config
  (setq smex-save-file (concat user-emacs-directory "var/.smex-items"))
  (smex-initialize))

(use-package saveplace :demand t
  :init
  (setq save-place-file (concat user-emacs-directory "var/places"))
  (setq-default save-place t))

(use-package recentf :demand t :config (recentf-mode 1))

(use-package undo-tree :ensure t :demand t :config (global-undo-tree-mode))

(use-package mainline :demand t ;; custom status line
  :config
  (setq mainline-arrow-shape 'arrow)
  (mainline-activate))

(use-package daycycle :demand t ;; set theme and switch it during the day
  :config
  (defun -theme-set (time)
    (if (eq time 'day)
        (progn
          (setq mainline-color1 "#d6d6d6")
          (setq mainline-color2 "#efefef")
          (setq mainline-color3 "#70c0b1")
          (setq mainline-color-fg "black")
          (custom-set-faces
           '(show-paren-match ((t (:foreground "grey70" :bold nil :background "#008800"))))
           '(show-paren-mismatch ((t (:foreground "grey70" :bold nil :background "#880000")))))
          (color-theme-sanityinc-tomorrow-day))
      (setq mainline-color1 "#444444")
      (setq mainline-color2 "#222222")
      (setq mainline-color3 "#293B3A")
      (setq mainline-color-fg "white")
      (custom-set-faces
       '(show-paren-match ((t (:foreground "#00ff00" :bold t :background unspecified))))
       '(show-paren-mismatch ((t (:foreground "#ff0000" :bold t :background unspecified)))))
      (color-theme-sanityinc-tomorrow-eighties))
    (setq fci-rule-color "sienna")
    (setq-default fci-rule-color "sienna")
    (custom-set-faces
     `(fringe ((t (:background ,(face-attribute 'default :background)))))))

  (daycycle-init '-theme-set 'auto))

(use-package usefuls :demand t
  :keys (:override
         "C-M-q" narrow-or-widen-dwim))

;;; Programming/Version Control

(use-package magit :ensure t
  :keys ("C-x g" magit-status
         "<f8>" magit-blame)
  :commands (magit-show-commit)
  :config
  (setq magit-last-seen-setup-instructions "1.4.0")
  (remove-hook 'magit-status-sections-hook 'magit-insert-stashes)
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-stashes 'magit-insert-untracked-files)

  (defun magit-section-highlight-less (section _)
    (magit-section-case
      ((untracked unstaged staged unpushed unpulled pulls branch)
       (magit-section-make-overlay (magit-section-start   section)
                                   (magit-section-content section)
                                   'magit-section-highlight)
       t)))
  (add-hook 'magit-section-highlight-hook 'magit-section-highlight-less)
  (remove-hook 'magit-status-mode-hook 'whitespace-mode)

  (use-package magit-gh-pulls :ensure t
    :init (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)))

(use-package git-timemachine :ensure t
  :keys ("<C-f8>" git-timemachine
         :local
         "c" git-timemachine-show-commit)
  :config
  (defun git-timemachine-show-commit ()
    (interactive)
    (magit-show-commit (car git-timemachine-revision)))

  (defface git-timemachine-minibuffer-author-face
    '((t (:foreground "firebrick")))
    "How to display the minibuffer detail"
    :group 'git-timemachine)

  ;; Overriden because I don't remember why ¯\_(ツ)_/¯
  (defun git-timemachine--revisions ()
    "List git revisions of current buffers file."
    (if git-timemachine--revisions-cache
        git-timemachine--revisions-cache
      (setq git-timemachine--revisions-cache
            (prog2
                (message "Fetching Revisions...")
                (let ((default-directory git-timemachine-directory)
                      (file git-timemachine-file))
                  (with-temp-buffer
                    (unless (zerop (process-file vc-git-program nil t nil "--no-pager" "log" "--name-only" "--follow" "--date=short" "--pretty=format:%H:%ar:%ad:%an:%s" file))
                      (error "Git log command exited with non-zero exit status for file: %s" file))
                    (goto-char (point-min))
                    (let ((lines)
                          (commit-number (/ (1+ (count-lines (point-min) (point-max))) 3)))
                      (while (not (eobp))
                        (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
                          (string-match "\\([^:]*\\):\\([^:]*\\):\\(.*\\):\\(.*\\):\\(.*\\)" line)
                          (let ((commit (match-string 1 line))
                                (date-relative (match-string 2 line))
                                (date-full (match-string 3 line))
                                (author (match-string 4 line))
                                (subject (match-string 5 line)))
                            (forward-line 1)
                            (let ((file-name (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
                              (push (list commit file-name commit-number date-relative date-full subject author) lines))))
                        (setq commit-number (1- commit-number))
                        (forward-line 2))
                      (nreverse lines))))
              (message "Fetching Revisions...done")))))

  ;; Overriden to customize m
  (defun git-timemachine--show-minibuffer-details (revision)
    "Show details for REVISION in minibuffer."
    (let ((detail (nth 5 revision))
          (date-relative (nth 3 revision))
          (date-full (nth 4 revision))
          (author (nth 6 revision)))
      (message (format "%s (%s) [%s (%s)]" (propertize detail 'face 'git-timemachine-minibuffer-detail-face)
                       (propertize author 'face 'git-timemachine-minibuffer-author-face)
                       date-full date-relative)))))

(use-package git-gutter :ensure t :demand t
  :config
  (global-git-gutter-mode 1)
  (setq-default git-gutter:modified-sign "~"))

(use-package vc-annotate :demand t
  :commands vc-annotate
  :keys (:local
         "c" vc-annotate-show-commit-at-line)
  :config
  (setq vc-ignore-dir-regexp
        (format "\\(%s\\)\\|\\(%s\\)"
                vc-ignore-dir-regexp
                tramp-file-name-regexp))

  (defun vc-annotate-show-commit-at-line ()
    (interactive)
    (let* ((rev (car (vc-annotate-extract-revision-at-line)))
           (rev (if (string= (substring rev 0 1) "^")
                    (substring rev 1)
                  rev)))
      (magit-show-commit rev)))

  (defun vc-git-annotate-command (file buf &optional rev)
    (let ((name (file-relative-name file)))
      (vc-git-command buf 'async nil "blame" "--date=iso" rev "--" name)))

  (defun vc-annotate-get-time-set-line-props ()
    (let ((bol (point))
          (date (vc-call-backend vc-annotate-backend 'annotate-time))
          (inhibit-read-only t))
      (cl-assert (>= (point) bol))
      (put-text-property bol (point) 'invisible 'vc-annotate-annotation)
      (let ((boc (point)))
        (save-excursion
          (search-backward-regexp "[0-9][0-9]:[0-9][0-9]:[0-9][0-9] \\+[0-9][0-9][0-9][0-9] +[0-9]+)")
          (when (< (- boc (point)) 40)
            (put-text-property (point) boc 'invisible t))
          (search-backward-regexp "(")
          (let ((paren-point (point)))
            (beginning-of-line)
            (when (> (- paren-point (point) 10))
              (put-text-property (+ (point) 9) paren-point 'invisible t)))))
      date))

  (defvar --vc-annotate-current-rev nil)

  (defun --vc-annotate-post-hook (file rev &rest rst)
    (setq --vc-annotate-current-rev rev)
    (vc-run-delayed
      (unless (active-minibuffer-window)
        (message (vc-git--run-command-string
                  nil "log" "--pretty=format:[%an] %s (%ar)" "-n 1" --vc-annotate-current-rev)))))

  (add-function :after (symbol-function 'vc-annotate) #'--vc-annotate-post-hook))

;;; Programming/Clojure & Lisps

(use-package clojure-mode :ensure t
  :config

  (defun clojure-pretty-fn ()
    (font-lock-add-keywords nil `(("(\\(partial\\>\\)"
                                   (0 (progn (compose-region (match-beginning 1)
                                                             (match-end 1)
                                                             ?μ) nil)))))
    (font-lock-add-keywords nil `(("(\\(fn\\>\\)"
                                   (0 (progn (compose-region (match-beginning 1)
                                                             (match-end 1)
                                                             ?λ) nil)))))
    (font-lock-add-keywords
     'clojure-mode `(("\\(#\\)("
                      (0 (progn (compose-region (match-beginning 1)
                                                (match-end 1) "ƒ")
                                nil)))))
    (font-lock-add-keywords
     'clojure-mode `(("\\(#\\){"
                      (0 (progn (compose-region (match-beginning 1)
                                                (match-end 1) "∈")
                                nil))))))

  (add-hook 'clojure-mode-hook 'clojure-pretty-fn)
  (add-hook 'clojure-mode-hook
            (lambda ()
              (setq-local
               clojure-defun-indents
               '(set-content-view! on-ui transact fact facts fact-group))
              (put 'defactivity 'clojure-backtracking-indent '(4 (2)))
              (put 's/defn 'clojure-doc-string-elt 4)))

  (use-package clj-refactor :ensure t
    :config
    (cljr-add-keybindings-with-prefix "C-c C-r")
    (add-hook 'clojure-mode-hook (lambda () (clj-refactor-mode 1)))
    (add-hook 'clojure-mode-hook 'yas-minor-mode-on)))

(use-package cider :ensure t
  :keys (:global ;empty
         :local cider-mode-map
         "C-c C-t" cider-toggle-trace-var
         "C-c i" cider-inspect-usual)
  :commands (cider-connect cider-jack-in)
  :config
  (add-hook 'cider-mode-hook 'eldoc-mode)

  (use-package company :ensure t :demand t
    :keys (:global ;empty
           :local company-mode-map
           "TAB" company-indent-or-complete-common
           "M-SPC" company-complete)
    :config
    (add-hook 'cider-repl-mode-hook #'company-mode)
    (add-hook 'cider-mode-hook #'company-mode)

    (use-package company-quickhelp :ensure t :demand t
      :config
      (company-quickhelp-mode 1)))

  (use-package cider-inspector
    :demand t
    :keys (:local
           ";" cider-inspector-next-inspectable-object
           "p" cider-inspector-previous-inspectable-object
           "C-;" cider-inspector-operate-on-point
           "C-p" cider-inspector-pop))

  (defun cider-inspect-usual ()
    (interactive)
    (when-let ((expression (cider-read-from-minibuffer "Inspect expression: "
                                                       (cider-sexp-at-point))))
      (cider-inspect-expr expression (cider-current-ns))))

  ;; (use-package ac-cider :ensure t
  ;;   :config
  ;;   (add-hook 'cider-mode-hook 'ac-flyspell-workaround)
  ;;   (add-hook 'cider-mode-hook 'ac-cider-setup)
  ;;   (add-hook 'cider-repl-mode-hook 'ac-cider-setup)

  ;;   (eval-after-load "auto-complete"
  ;;     '(progn
  ;;        (add-to-list 'ac-modes 'cider-mode)
  ;;        (add-to-list 'ac-modes 'cider-repl-mode)))

  ;;   (defun set-auto-complete-as-completion-at-point-function ()
  ;;     (setq completion-at-point-functions '(auto-complete)))

  ;;   (add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)
  ;;   (add-hook 'cider-mode-hook 'set-auto-complete-as-completion-at-point-function)
  ;;   (add-hook 'cider-repl-mode-hook 'set-auto-complete-as-completion-at-point-function))
  )

(use-package auto-complete-config
  :commands ac-config-default
  :init (add-hook 'prog-mode-hook 'ac-config-default))

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
  (slime-setup '(slime-fancy slime-scratch slime-editing-commands
                             slime-fuzzy slime-repl slime-fancy-inspector
                             slime-presentations slime-asdf
                             slime-indentation))
  (require 'slime-autoloads))

(use-package aggressive-indent :ensure t
  :commands aggressive-indent-mode
  :config
  (add-hook 'lisp-mode-hook 'aggressive-indent-mode)
  (add-hook 'clojure-mode-hook 'aggressive-indent-mode))

(use-package comment-sexp :demand t)

;;; Programming/Other languages

(use-package cc-mode :demand t
  :config
  (defun java-default-formatting ()
    (c-set-style "java")
    (setq ;; c-basic-offset 4
     tab-width 4
     indent-tabs-mode nil))

  (defun java-clojure-compiler-formatting ()
    (c-set-style "whitesmith")
    (setq ;; c-basic-offset 4
     tab-width 4
     indent-tabs-mode t))

  ;; (add-hook 'java-mode-hook 'java-clojure-compiler-formatting)
  (add-hook 'java-mode-hook 'java-default-formatting))

(use-package web-mode :ensure t
  :config
  (add-hook 'web-mode-hook (lambda () (setq  web-mode-markup-indent-offset 2))))

(use-package dockerfile-mode :ensure t :demand t)

(use-package hcl-mode :ensure t :demand t)

(use-package rockerfile-mode :demand t
  :config
  (use-package flycheck :ensure t :demand t)
  (defconst flycheck-rockerlint-form
    (flycheck-prepare-emacs-lisp-form
      (require 'package)
      (package-initialize)
      (require 'rockerlint)

      (let ((source (car command-line-args-left))
            ;; Remember the default directory of the process
            (process-default-directory default-directory))
        (rockerlint-lint source))))

  (flycheck-define-checker rockerlint-checker
    "Rockerfile checker."
    :command ("emacs" "-Q" "-batch" "-L" "~/.emacs.d/site-lisp/"
              "--eval" (eval flycheck-rockerlint-form)
              "--" source)
    :error-patterns
    ((error line-start line ":" column ": " (message) line-end))
    :modes (rockerfile-mode))

  (push 'rockerlint-checker flycheck-checkers)
  (add-hook 'rockerfile-mode-hook (lambda () (flycheck-mode 1))))

(use-package rust-mode :ensure t)

(use-package go-mode :ensure t
  :config
  (add-hook 'go-mode-hook (lambda () (setq whitespace-style '(face trailing empty)
                                      indent-tabs-mode t
                                      tab-width 4)
                            (whitespace-mode -1))))

(use-package toml-mode :ensure t)

(use-package markdown-mode :ensure t
  :keys (markdown-mode-map
         "C-c C-l" markdown-smart-insert-link
         "C-c C-c C-c" markdown-insert-gfm-code-block)
  :config
  (defun markdown-smart-insert-link ()
    (interactive)
    (let (link text)
      (if (use-region-p)
          (let ((bounds (markdown-wrap-or-insert "[" "]")))
            (setq link (read-string "Link: "))
            (goto-char (cdr bounds))
            (insert (concat "("
                            (if (string= link "")
                                (buffer-substring-no-properties
                                 (1+ (car bounds)) (1- (cdr bounds)))
                              link)
                            ")")))
        (setq link (read-string "Link: "))
        (setq text (read-string "Text: "))
        (insert (concat "[" (if (string= text "") link text) "](" link ")"))))))

(use-package terraform-mode :ensure t)

(use-package zencoding-mode :ensure t)

(use-package sass-mode :ensure t)

;;; Programming/Miscellaneous

(use-package rainbow-mode :ensure t
  :commands rainbow-turn-on
  :init
  (add-hook 'prog-mode-hook 'rainbow-turn-on)
  (add-hook 'nxml-mode-hook 'rainbow-turn-on)
  (add-hook 'sgml-mode-hook 'rainbow-turn-on)
  (add-hook 'web-mode-hook 'rainbow-turn-on)
  (add-hook 'css-mode-hook 'rainbow-turn-on))

(use-package projectile :ensure t
  :keys ("M-f" projectile-find-file)
  :config
  (projectile-global-mode)
  (add-hook 'find-file-hook
            (lambda ()
              (when (file-remote-p default-directory)
                (setq-local projectile-mode-line "Projectile")))))

(use-package helm-ag :ensure t ;; helm-grep with silver searcher
  :keys (:override
         "M-h" helm-do-ag-project-root-custom
         "M-H" helm-do-ag

         helm-ag-map
         "C-;" helm-next-line
         "M-;" helm-goto-next-file
         "M-p" helm-goto-precedent-file
         "<right>" helm-execute-persistent-action)
  :config
  (defun helm-do-ag-project-root-custom (sym-at-p)
    (interactive "P")
    (let ((helm-ag-insert-at-point (when sym-at-p 'symbol)))
      (helm-do-ag-project-root))))

(use-package hippie-exp
  :keys ("M-/" hippie-expand)
  :config
  (dolist (f '(try-expand-line try-expand-list try-complete-file-name-partially))
    (delete f hippie-expand-try-functions-list))

  ;; Add this back in at the end of the list.
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-file-name-partially t))

(use-package idle-highlight-mode :ensure t :demand t
  :commands idle-highlight-mode
  :config
  ;; Hack idle-highlight to support symbols like $SOMETHING.
  (defun idle-highlight-word-at-point ()
    "Highlight the word under the point."
    (if idle-highlight-mode
        (let* ((target-symbol (symbol-at-point))
               (target (symbol-name target-symbol)))
          (idle-highlight-unhighlight)
          (when (and target-symbol
                     (not (in-string-p))
                     (looking-at-p "\\s_\\|\\sw") ;; Symbol characters
                     (not (member target idle-highlight-exceptions)))
            (setq idle-highlight-regexp (concat "\\_<" (regexp-quote target) "\\_>"))
            (highlight-regexp idle-highlight-regexp 'idle-highlight)))))
  (add-hook 'prog-mode-hook 'idle-highlight-mode))

(use-package paren :demand t :config (show-paren-mode 1))

(use-package wakatime-mode :ensure t :demand t
  :config (global-wakatime-mode))

(use-package fill-column-indicator :ensure t :demand t
  :config
  (add-hook 'prog-mode-hook 'fci-mode)
  (add-hook 'clojure-mode-hook 'fci-mode))

(use-package hideshow :ensure t :demand t
  :config
  (add-hook 'c-mode-common-hook   'hs-minor-mode)
  (add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
  (add-hook 'java-mode-hook       'hs-minor-mode)
  (add-hook 'lisp-mode-hook       'hs-minor-mode)
  (add-hook 'perl-mode-hook       'hs-minor-mode)
  (add-hook 'sh-mode-hook         'hs-minor-mode)
  (add-hook 'clojure-mode-hook    'hs-minor-mode))

;;; Writing

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :config
  (use-package ox-reveal :ensure t
    :demand t
    :config
    (setq org-reveal-root (expand-file-name "~/Software/reveal-js"))))

(use-package thesaurus :ensure t
  :keys ("C-x t" thesaurus-choose-synonym-and-replace)
  :config (thesaurus-set-bhl-api-key-from-file "~/.bighugelabsapi.key"))

(use-package centered-window-mode :ensure t
  :keys ("<f12>" serenity-mode)
  :config
  (defvar-local hidden-mode-line-mode nil)

  (define-minor-mode hidden-mode-line-mode
    "Minor mode to hide the mode-line in the current buffer."
    :init-value nil
    :global t
    :variable hidden-mode-line-mode
    :group 'editing-basics
    (if (not (null mode-line-format))
        (progn
          (setq hide-mode-line mode-line-format
                mode-line-format nil)
          (setq-default mode-line-format nil))
      (setq-default mode-line-format hide-mode-line)
      (setq mode-line-format hide-mode-line
            hide-mode-line nil))
    (force-mode-line-update)
    ;; Apparently force-mode-line-update is not always enough to
    ;; redisplay the mode-line
    (redraw-display)
    (when (and (called-interactively-p 'interactive)
               hidden-mode-line-mode)
      (run-with-idle-timer
       0 nil 'message
       (concat "Hidden Mode Line Mode enabled.  "
               "Use M-x hidden-mode-line-mode to make the mode-line appear."))))

  (defun serenity-mode ()
    (interactive)
    (if (null mode-line-format)
        (progn
          (centered-window-mode -1)
          (hidden-mode-line-mode -1)
          (set-fringe-mode "default"))
      (progn
        (centered-window-mode 1)
        (hidden-mode-line-mode 1)
        (set-fringe-mode "no-fringes")))))

(use-package flyspell :ensure t
  :config
  (flyspell-mode 0) ;; Off flyspell by default
  (add-hook 'org-mode-hook (lambda () (flyspell-mode 1) (flyspell-buffer)))
  (add-hook 'LaTeX-mode-hook (lambda () (flyspell-mode 1) (flyspell-buffer)))

  ;; Enable flyspell-prog-mode for programming languages
  (add-hook 'clojure-mode-hook 'flyspell-prog-mode)
  (add-hook 'java-mode-hook 'flyspell-prog-mode)
  (add-hook 'lua-mode-hook 'flyspell-prog-mode)
  (add-hook 'lisp-mode-hook 'flyspell-prog-mode))

(use-package langtool :ensure t :demand t
  :config
  (setq langtool-java-classpath "/usr/share/languagetool:/usr/share/java/languagetool/*"))

(use-package ascii-one-liners :demand t)
;; Customizations

(progn ;; Smooth scrolling
  (setq scroll-conservatively 101) ;; move minimum when cursor exits view, instead of recentering
  (setq mouse-wheel-progressive-speed nil) ;; on a long mouse scroll keep scrolling by 1 line
  )

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Don't litter my fs tree
(setq backup-directory-alist '(("." . "~/.local/share/emacs-saves")))

;; Advice yanking to auto-indent yank content
(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command (after indent-region activate)
           (and (not current-prefix-arg)
                (member major-mode '(emacs-lisp-mode lisp-mode
                                                     clojure-mode    scheme-mode
                                                     haskell-mode    ruby-mode
                                                     rspec-mode      python-mode
                                                     c-mode          c++-mode
                                                     objc-mode       latex-mode
                                                     plain-tex-mode  lua-mode))
                (let ((mark-even-if-inactive transient-mark-mode))
                  (indent-region (region-beginning) (region-end) nil))))))

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'ido-exit-minibuffer 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; XWidget Webkit
(defun xwidget-webkit-new-session (url)
  "Create a new webkit session buffer with URL."
  (let*
      ((bufname (generate-new-buffer-name "*xwidget-webkit*"))
       xw)
    (setq xwidget-webkit-last-session-buffer (switch-to-buffer
                                              (get-buffer-create bufname)))
    (insert " ")
    (setq xw (xwidget-insert 0 'webkit  bufname 2560 1600))
    (xwidget-put xw 'callback 'xwidget-webkit-callback)
    (xwidget-webkit-mode)
    (xwidget-webkit-goto-uri (xwidget-webkit-last-session) url)))

;; Local Variables:
;; eval: (hs-hide-all)
;; End:
