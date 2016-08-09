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

;; Configure packages

(use-package s :ensure t)

(use-package sudo :commands sudo-find-file)

(use-package stesla
  :keys (:override
         "C-." stesla-rotate-buffers
         "C-," stesla-rotate-backwards))

(use-package multiple-cursors :ensure t
  :keys ("C-M-<mouse-1>" mc/add-cursor-on-click
         "<C-down>" mc/mark-next-like-this
         "C-c m" mc/mark-all-like-this-dwim))

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

  (defvar bookmark-counter 0)
  (setq bookmarks '("~/.emacs.d/" "~/clojure/android/lein-droid" "~/clojure/android/neko"
                    "~/projects/grammarly/core-metrics" "~/projects/grammarly/clj-core-metrics"))

  (defun sunrise-cd-resize ()
    (interactive)
    (setq sr-panes-height (* 2 (/ (frame-height) 3)))
    (sunrise-cd))

  (defun sr-cycle-bookmark ()
    (interactive)
    (let ((bookmark (nth bookmark-counter bookmarks)))
      (setq bookmark-counter (+ bookmark-counter 1))
      (if (>= bookmark-counter (length bookmarks))
          (setq bookmark-counter 0))
      (sr-goto-dir bookmark)))

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

(use-package web-mode :ensure t
  :config
  (add-hook 'web-mode-hook (lambda () (setq  web-mode-markup-indent-offset 2))))

(use-package rainbow-mode :ensure t
  :commands rainbow-turn-on
  :init
  (add-hook 'prog-mode-hook 'rainbow-turn-on)
  (add-hook 'nxml-mode-hook 'rainbow-turn-on)
  (add-hook 'sgml-mode-hook 'rainbow-turn-on)
  (add-hook 'web-mode-hook 'rainbow-turn-on)
  (add-hook 'css-mode-hook 'rainbow-turn-on))

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

(use-package projectile :ensure t
  :keys ("M-f" projectile-find-file)
  :config
  (projectile-global-mode)
  (add-hook 'find-file-hook
            (lambda ()
              (when (file-remote-p default-directory)
                (setq-local projectile-mode-line "Projectile")))))

(use-package auto-complete-config
  :commands ac-config-default
  :init (add-hook 'prog-mode-hook 'ac-config-default))

(use-package slime :ensure t
  :commands slime
  :init
  (defvar grammarly-patterns-path-to-project nil)
  (defun grammarly-patterns-set-path (path-to-patterns)
    (setq grammarly-patterns-path-to-project path-to-patterns)
    (add-to-list 'load-path (expand-file-name (concat path-to-patterns "/emacs")))
    (condition-case _ (require 'patterns-modes)
      (error (display-warning :error "Could not find patterns-mode."))))

  (defun допобачення ()
    (interactive)
    (slime-quit-lisp))

  (defun grammarly-patterns-slime-init ()
    (interactive)
    (slime-repl-send-string
     (format "#+sbcl (pushnew :glove *features*)
(pushnew :dev *features*)
(load \"%s/init.lisp\")
(named-readtables:in-readtable plang:patterns)
(in-package :plang)
(use-package :ptools)
(declaim (sb-ext:muffle-conditions sb-ext:compiler-note))"
grammarly-patterns-path-to-project)))
  :config
  (grammarly-patterns-set-path "~/projects/grammarly/patterns")
  (setq-default slime-lisp-implementations
              '((sbcl ("sbcl" "--dynamic-space-size" "9500"))
                (ccl ("ccl"))))

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

(use-package midnight :demand t
  :config (midnight-delay-set 'midnight-delay "11:59pm"))

(use-package helm-git-grep :ensure t
  :keys (:override
         "M-h" helm-git-grep

         helm-git-grep-map
         "C-;" helm-next-line
         "M-;" helm-goto-next-file
         "M-p" helm-goto-precedent-file)
  :config
  (defun helm-git-grep (sym-at-p)
    (interactive "P")
    (if sym-at-p
        (helm-git-grep-at-point nil nil)
      (helm-git-grep-1))))

(use-package helm-ag :ensure t
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

  (defun git-timemachine--show-minibuffer-details (revision)
    "Show details for REVISION in minibuffer."
    (setq ---foo  revision)
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

(use-package smex :ensure t
  :keys ("M-x" smex)
  :config
  (setq smex-save-file (concat user-emacs-directory "var/.smex-items"))
  (smex-initialize))

(use-package saveplace :demand t
  :init
  (setq save-place-file (concat user-emacs-directory "var/places"))
  (setq-default save-place t))

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

(use-package gradle-mode :ensure t)

(use-package groovy-mode :ensure t)

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

;; Smooth scrolling
(progn
  (setq scroll-conservatively 101) ;; move minimum when cursor exits view, instead of recentering
  (setq mouse-wheel-progressive-speed nil) ;; on a long mouse scroll keep scrolling by 1 line
  )

(use-package paren :demand t
  :config (show-paren-mode 1))

;; Autocomplete in Clojure

;; (require 'company)
;; (require 'company-quickhelp)

;; (company-quickhelp-mode 1)

;; (add-hook 'cider-repl-mode-hook #'company-mode)
;; (add-hook 'cider-mode-hook #'company-mode)

;; Default SSH for tramp

;; Auto undo-tree
(global-undo-tree-mode)

;; Clojure

(use-package clojure-mode :ensure t
  :config
  (add-hook 'clojure-mode-hook 'esk-pretty-fn)
  (add-hook 'clojure-mode-hook
            #'(lambda ()
                (put 'defactivity 'clojure-backtracking-indent '(4 (2)))))

  (use-package clj-refactor :ensure t
    :config
    (cljr-add-keybindings-with-prefix "C-c C-r")
    (add-hook 'clojure-mode-hook (lambda () (clj-refactor-mode 1)))
    (add-hook 'clojure-mode-hook 'yas-minor-mode-on))

  (put 's/defn 'clojure-doc-string-elt 4))

(use-package cider :ensure t
  :keys (:global ;empty
         :local cider-mode-map
         "C-c C-t" cider-toggle-trace-var
         "C-c i" cider-inspect-usual)
  :commands (cider-connect cider-jack-in)
  :config
  (add-hook 'cider-mode-hook 'eldoc-mode)
  (setq nrepl-log-messages t)

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

(use-package aggressive-indent :ensure t
  :commands aggressive-indent-mode
  :config
  (add-hook 'lisp-mode-hook 'aggressive-indent-mode)
  (add-hook 'clojure-mode-hook 'aggressive-indent-mode))

;; Autocomplete
;; (require 'auto-complete-config)
;; (ac-config-default)

;; Openwith mode

;; HideShow mode
(add-hook 'c-mode-common-hook   'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'java-mode-hook       'hs-minor-mode)
(add-hook 'lisp-mode-hook       'hs-minor-mode)
(add-hook 'perl-mode-hook       'hs-minor-mode)
(add-hook 'sh-mode-hook         'hs-minor-mode)
(add-hook 'clojure-mode-hook    'hs-minor-mode)

;; Disable flashes on errors
(setq ring-bell-function 'ignore)

;; count-words with Yegges
(defun count-words--message (str start end)
  (let* ((lines (count-lines start end))
         (words (count-words start end))
         (chars (- end start))
         (yegges (sqrt (* (/ words 4000.0) (/ chars 25000.0)))))
    (message "%s has %d line%s, %d word%s, %f Yegge%s and %d character%s."
             str
             lines (if (= lines 1) "" "s")
             words (if (= words 1) "" "s")
             yegges (if (= yegges 1) "" "s")
             chars (if (= chars 1) "" "s"))))

(defun count-yegges ()
  (interactive)
  (cond ((use-region-p)
         (count-words--message "Region" (region-beginning) (region-end)))
        (t
         (count-words--buffer-message))))

;; Pretty lambdas

(defun esk-pretty-fn ()
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

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Off flyspell
(flyspell-mode 0)
(add-hook 'org-mode-hook (lambda () (flyspell-mode 1) (flyspell-buffer)))
(add-hook 'LaTeX-mode-hook (lambda () (flyspell-mode 1) (flyspell-buffer)))

;; Eval and replace
(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

;; Enable recentf
(require 'recentf)
(recentf-mode 1)

;; Configure main-line
(use-package mainline :demand t
  :config
  (setq mainline-arrow-shape 'arrow)
  (mainline-activate))

(use-package daycycle :demand t
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

(put 'ido-exit-minibuffer 'disabled nil)

(setq backup-directory-alist '(("." . "~/.local/share/emacs-saves"))    ; don't litter my fs tree
)

(defun indent-buffer ()
  "indent whole buffer"
  (interactive)
  (indent-region (point-min) (point-max) nil))

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

(use-package zencoding-mode :ensure t)

(use-package fill-column-indicator :ensure t :demand t
  :config
  (add-hook 'prog-mode-hook 'fci-mode)
  (add-hook 'clojure-mode-hook 'fci-mode))

(defun unlogic-git-fix-url ()
  (interactive)
  (let* ((old-url (buffer-substring-no-properties (region-beginning) (region-end)))
         (r (s-match "^https?://\\([^/]+\\)/\\([^/]+\\)/\\([^/]+\\)" old-url))
         (new-url (concat "git@" (cadr r) ":" (caddr r) "/" (cadddr r) ".git")))
    (kill-region (region-beginning) (region-end))
    (insert new-url)))

(defun clone-and-comment-line (beg end)
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list nil nil)))
  (let ((p (point))
        (beg (or beg (progn
                       (move-beginning-of-line 1)
                       (point))))
        (end (or end (progn
                       (move-end-of-line 1)
                       (point)))))
    (copy-region-as-kill beg end)
    (comment-region beg end)
    (goto-char beg)
    (open-line 1)
    (yank)))

;; Enable flyspell-prog-mode for programming languages
(add-hook 'clojure-mode-hook 'flyspell-prog-mode)
(add-hook 'java-mode-hook 'flyspell-prog-mode)
(add-hook 'lua-mode-hook 'flyspell-prog-mode)
(add-hook 'lisp-mode-hook 'flyspell-prog-mode)

;; TeX editing

;; (require 'tex)
;; (setq TeX-auto-save t
;;       TeX-parse-self t)
;; (setq-default TeX-master nil
;;               TeX-engine 'xetex
;;               TeX-PDF-mode t)
;; (require 'reftex)
;; (setq reftex-plug-into-AUCTeX t)
;; (add-hook 'LaTeX-mode-hook 'turn-on-reftex)

;; (defun get-bibtex-keys (file)
;;   (with-current-buffer (find-file-noselect file)
;;     (mapcar 'car (bibtex-parse-keys))))

;; (defun LaTeX-add-all-bibitems-from-bibtex ()
;;   (interactive)
;;   (mapc 'LaTeX-add-bibitems
;;         (apply 'append
;;                (mapcar 'get-bibtex-keys (reftex-get-bibfile-list)))))

;; ;;; Texify everything

;; (require 'tex-buf)
;; (defun TeX-command-default (name)
;;   "Next TeX command to use. Most of the code is stolen from `TeX-command-query'."
;;   (cond ((if (string-equal name TeX-region)
;;              (TeX-check-files (concat name "." (TeX-output-extension))
;;                               (list name)
;;                               TeX-file-extensions)
;;            (TeX-save-document (TeX-master-file)))
;;          TeX-command-default)
;;         ((and (memq major-mode '(doctex-mode latex-mode))
;;               (TeX-check-files (concat name ".bbl")
;;                                (mapcar 'car
;;                                        (LaTeX-bibliography-list))
;;                                BibTeX-file-extensions))
;;          ;; We should check for bst files here as well.
;;          TeX-command-BibTeX)))

;; (defcustom TeX-texify-max-runs-same-command 5
;;   "Maximal run number of the same command"
;;   :type 'integer :group 'TeX-command)

;; (defun TeX-texify-sentinel (&optional proc sentinel)
;;   "Non-interactive! Call the standard-sentinel of the current LaTeX-process.
;; If there is still something left do do start the next latex-command."
;;   (set-buffer (process-buffer proc))
;;   (funcall TeX-texify-sentinel proc sentinel)
;;   (let ((case-fold-search nil))
;;     (when (string-match "\\(finished\\|exited\\)" sentinel)
;;       (set-buffer TeX-command-buffer)
;;       (unless (plist-get TeX-error-report-switches (intern (TeX-master-file)))
;; 	(TeX-texify)))))

;; (defun TeX-texify ()
;;   "Get everything done."
;;   (interactive)
;;   (let ((nextCmd (TeX-command-default (TeX-master-file)))
;; 	proc)
;;     (if (null nextCmd)
;; 	(when  (called-interactively-p 'any)
;; 	  (message "TeX-texify: Nothing to be done."))
;;       (TeX-command nextCmd 'TeX-master-file)
;;       (when (or (called-interactively-p 'any)
;; 		(null (boundp 'TeX-texify-count-same-command))
;; 		(null (boundp 'TeX-texify-last-command))
;; 		(null (equal nextCmd TeX-texify-last-command)))
;; 	(mapc 'make-local-variable '(TeX-texify-sentinel TeX-texify-count-same-command TeX-texify-last-command))
;; 	(setq TeX-texify-count-same-command 1))
;;       (if (>= TeX-texify-count-same-command TeX-texify-max-runs-same-command)
;; 	  (message "TeX-texify: Did %S already %d times. Don't want to do it anymore." TeX-texify-last-command TeX-texify-count-same-command)
;; 	(setq TeX-texify-count-same-command (1+ TeX-texify-count-same-command))
;; 	(setq TeX-texify-last-command nextCmd)
;; 	(and (null (equal nextCmd TeX-command-Show))
;; 	     (setq proc (get-buffer-process (current-buffer)))
;; 	     (setq TeX-texify-sentinel (process-sentinel proc))
;; 	     (set-process-sentinel proc 'TeX-texify-sentinel))))))

;; preview-TeX-style-dir

;;; Texify everything ends

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

;; Temp buffers

(defvar temp-buffer-counter 0)

(defun create-temp-buffer ()
  (interactive)
  (setf temp-buffer-counter (+ temp-buffer-counter 1))
  (switch-to-buffer (concat "temp" (number-to-string temp-buffer-counter))))

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(defun unfill-block ()
  "Remove ending chars on current paragraph. This command is
similar to a toggle of `fill-paragraph'. When there is a text
selection, act on the region."
  (interactive)

  ;; This command symbol has a property “'stateIsCompact-p”.
  (let ((bigFillColumnVal most-positive-fixnum)
        (deactivate-mark nil))
    (save-excursion
      ;; Determine whether the text is currently compact.
      (let ((fill-column most-positive-fixnum))
        (if (region-active-p)
            (fill-region (region-beginning) (region-end))
          (fill-paragraph nil))))))

;;; Javadev

;; ;; While hacking on Clojure
;; (remove-hook 'java-mode-hook (lambda ()
;;                             (c-set-style "whitesmith")
;;                             (setq ;; c-basic-offset 4
;;                              tab-width 4
;;                              indent-tabs-mode t)
;;                             ))

;;Default
(add-hook 'java-mode-hook (lambda ()
                            (c-set-style "java")
                            (setq ;; c-basic-offset 4
                             tab-width 4
                             indent-tabs-mode nil)
                            ))

;; Customize narrow
(defun narrow-or-widen-dwim (p)
  "If the buffer is narrowed, it widens. Otherwise, it narrows intelligently.
Intelligently means: region, subtree, or defun, whichever applies
first.

With prefix P, don't widen, just narrow even if buffer is already
narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning) (region-end)))
        ((derived-mode-p 'org-mode) (org-narrow-to-subtree))
        (t (narrow-to-defun))))

(use-package langtool :ensure t :demand t)
(setq langtool-java-classpath
      "/usr/share/languagetool:/usr/share/java/languagetool/*")

(use-package wakatime-mode :ensure t :demand t
  :config (global-wakatime-mode))

;; Sexp commenting
(defun uncomment-sexp (&optional n)
  "Uncomment a sexp around point."
  (interactive "P")
  (let* ((initial-point (point-marker))
         (p)
         (end (save-excursion
                (when (elt (syntax-ppss) 4)
                  (re-search-backward comment-start-skip
                                      (line-beginning-position)
                                      t))
                (setq p (point-marker))
                (comment-forward (point-max))
                (point-marker)))
         (beg (save-excursion
                (forward-line 0)
                (while (= end (save-excursion
                                (comment-forward (point-max))
                                (point)))
                  (forward-line -1))
                (goto-char (line-end-position))
                (re-search-backward comment-start-skip
                                    (line-beginning-position)
                                    t)
                (while (looking-at-p comment-start-skip)
                  (forward-char -1))
                (point-marker))))
    (unless (= beg end)
      (uncomment-region beg end)
      (goto-char p)
      ;; Indentify the "top-level" sexp inside the comment.
      (while (and (ignore-errors (backward-up-list) t)
                  (>= (point) beg))
        (skip-chars-backward (rx (syntax expression-prefix)))
        (setq p (point-marker)))
      ;; Re-comment everything before it.
      (ignore-errors
        (comment-region beg p))
      ;; And everything after it.
      (goto-char p)
      (forward-sexp (or n 1))
      (skip-chars-forward "\r\n[:blank:]")
      (if (< (point) end)
          (ignore-errors
            (comment-region (point) end))
        ;; If this is a closing delimiter, pull it up.
        (goto-char end)
        (skip-chars-forward "\r\n[:blank:]")
        (when (= 5 (car (syntax-after (point))))
          (delete-indentation))))
    ;; Without a prefix, it's more useful to leave point where
    ;; it was.
    (unless n
      (goto-char initial-point))))

(defun comment-sexp--raw ()
  "Comment the sexp at point or ahead of point."
  (pcase (or (bounds-of-thing-at-point 'sexp)
             (save-excursion
               (skip-chars-forward "\r\n[:blank:]")
               (bounds-of-thing-at-point 'sexp)))
    (`(,l . ,r)
     (goto-char r)
     (skip-chars-forward "\r\n[:blank:]")
     (comment-region l r)
     (skip-chars-forward "\r\n[:blank:]"))))

(defun comment-or-uncomment-sexp (&optional n)
  "Comment the sexp at point and move past it.
If already inside (or before) a comment, uncomment instead.
With a prefix argument N, (un)comment that many sexps."
  (interactive "P")
  (if (or (elt (syntax-ppss) 4)
          (< (save-excursion
               (skip-chars-forward "\r\n[:blank:]")
               (point))
             (save-excursion
               (comment-forward 1)
               (point))))
      (uncomment-sexp n)
    (dotimes (_ (or n 1))
      (comment-sexp--raw))))

;;; Temporaririly unused things

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

;; ;; Screen layout
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

;; ;; Modes
;; (add-to-list 'load-path "~/projects/lisp/patterns/emacs/")
;; (require 'patterns-modes)

;; (defconst query-replace-highlight t)
;; (defconst search-highlight t)

;; (defun slime-init ()
;;   (slime-repl-send-string "
;; (pushnew :ling *features*)
;; #+sbcl (pushnew :glove *features*)
;; (ql:quickload :gr-patterns)
;; #+ccl
;; (setf ccl:*default-external-format*
;;       (ccl:make-external-format :character-encoding :utf-8
;;                                 :line-termination :unix))
;; (named-readtables:in-readtable plang:patterns)
;; (in-package :plang)
;; (use-package :ptools)"))

;; (setq slime-net-coding-system 'utf-8-unix)

;; (slime)

;; Local Variables:
;; eval: (hs-hide-all)
;; End:
(put 'narrow-to-region 'disabled nil)
