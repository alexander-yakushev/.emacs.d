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

  (eval-when-compile
    (require 'use-package))
  (setq use-package-verbose t)

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

;; Load bindings
(add-to-list 'load-path "~/.emacs.d/site-lisp")

(load-file "~/.emacs.d/bindings.el")

(load-file "~/.emacs.d/esk.el")

(require 'centered-window-mode)

(put 'use-package 'lisp-indent-function 'defun)

(load (setq custom-file (expand-file-name (locate-user-emacs-file "custom.el"))))

;; Turn off mouse interface early in startup to avoid momentary display
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

(cd "~") ;; start from userdir

;; Configure packages

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
         "<C-f7>" sunrise-cd

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
  (setq bookmarks '("~/.emacs.d/" "~/clojure/android/lein-droid" "~/clojure/android/neko"))

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

  (use-package javad
    :config
    (use-package javap-mode :ensure t)
    (add-hook 'find-file-hook 'javad-find-class))

  (openwith-mode t))

(use-package rainbow-mode :ensure t
  :commands rainbow-turn-on
  :init
  (add-hook 'prog-mode-hook 'rainbow-turn-on)
  (add-hook 'nxml-mode-hook 'rainbow-turn-on))

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
  (projectile-global-mode))

(use-package auto-complete-config
  :commands ac-config-default
  :init (add-hook 'prog-mode-hook 'ac-config-default))

(use-package slime :ensure t
  :config
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

(use-package s :ensure t :demand t)

;; Initialize smex
(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

(require 'paren)
(show-paren-mode 1)
(set-face-foreground 'show-paren-match "#00ff00")
(set-face-attribute 'show-paren-match nil :weight 'extra-bold)
(set-face-background 'show-paren-match (face-background 'default))

;; Autocomplete in Clojure

;; (require 'company)
;; (require 'company-quickhelp)

;; (company-quickhelp-mode 1)

;; (add-hook 'cider-repl-mode-hook #'company-mode)
;; (add-hook 'cider-mode-hook #'company-mode)

(defun kill-buffer-and-its-windows ()
  "Kill BUFFER and delete its windows.  Default is `current-buffer'.
BUFFER may be either a buffer or its name (a string)."
  (interactive) ; (list (read-buffer "Kill buffer: " (current-buffer) 'existing)))
  (setq buffer (get-buffer (current-buffer)))
  (cond ((buffer-live-p buffer)         ; Kill live buffer only.
         (let ((wins (get-buffer-window-list buffer nil t))) ; On all frames.
           (when (and (buffer-modified-p buffer)
                      (fboundp '1on1-flash-ding-minibuffer-frame))
             (1on1-flash-ding-minibuffer-frame t)) ; Defined in `oneonone.el'.
           (when (kill-buffer buffer)   ; Only delete windows if buffer killed.
             (dolist (win wins)         ; (User might keep buffer if modified.)
               (when (window-live-p win) (delete-window win))))))
        ((interactive-p)
         (error "Cannot kill buffer.  Not a live buffer: `%s'" buffer))))

;; Sudo-save


;; Default SSH for tramp

(setq tramp-default-method "ssh")

;; Auto undo-tree
(global-undo-tree-mode)

;; Clojure

(use-package clojure-mode :ensure t
  :mode ("\\(?:build\\|profile\\)\\.boot\\'" . clojure-mode)
  :mode ("\\.\\(clj[csx]?\\|dtm\\|edn\\)\\'" . clojure-mode)
  :config
  (add-hook 'clojure-mode-hook 'esk-pretty-fn)
  (add-hook 'clojure-mode-hook
            #'(lambda ()
                (put 'defactivity 'clojure-backtracking-indent '(4 (2)))))

  (use-package clj-refactor :ensure t
    :config
    (cljr-add-keybindings-with-prefix "C-c C-r")
    (add-hook 'clojure-mode-hook (lambda () (clj-refactor-mode 1)))
    (add-hook 'clojure-mode-hook 'yas-minor-mode-on)))

(use-package cider :ensure t
  :keys (:global ;empty
         :local cider-mode-map
         "C-c C-t" cider-toggle-trace-var
         "C-c i" cider-inspect)
  :commands (cider-connect cider-jack-in)
  :config
  (add-hook 'cider-mode-hook 'eldoc-mode)
  (setq nrepl-log-messages t)

  (use-package cider-inspector
    :demand t
    :keys (:local
           ";" cider-inspector-next-inspectable-object
           "p" cider-inspector-previous-inspectable-object
           "C-;" cider-inspector-operate-on-point
           "C-p" cider-inspector-pop))

  (use-package ac-cider :ensure t
    :config
    (add-hook 'cider-mode-hook 'ac-flyspell-workaround)
    (add-hook 'cider-mode-hook 'ac-cider-setup)
    (add-hook 'cider-repl-mode-hook 'ac-cider-setup)

    (eval-after-load "auto-complete"
      '(progn
         (add-to-list 'ac-modes 'cider-mode)
         (add-to-list 'ac-modes 'cider-repl-mode)))

    (defun set-auto-complete-as-completion-at-point-function ()
      (setq completion-at-point-functions '(auto-complete)))

    (add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)
    (add-hook 'cider-mode-hook 'set-auto-complete-as-completion-at-point-function)
    (add-hook 'cider-repl-mode-hook 'set-auto-complete-as-completion-at-point-function)))

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

;; Enable Tomorrow Eighties theme
(color-theme-sanityinc-tomorrow-eighties)

;; Disable flashes on errors
(setq ring-bell-function 'ignore)

;; Open files with stupid cp1251
(defun stupid-encoding ()
  (interactive)
  (revert-buffer-with-coding-system 'cp1251 t))

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

;; Configure sunrise

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Configure midnight mode
(require 'midnight)
(midnight-delay-set 'midnight-delay "11:59pm")

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

(use-package smooth-scrolling :ensure t :demand t)

;; Configure main-line

(defun center-format (str c)
  (let* ((l (length str)))
    (if (< l c)
        (let ((p (/ (- c l) 2)))
          (concat (make-string p 32) str (make-string (- c p l) 32)))
      str)))

(defun my/percentage-from-top (padding)
  (let ((p (round (/ (* 100.0 (point)) (point-max)))))
    (replace-regexp-in-string "|" "%%"
                              (format (concat "%" (number-to-string padding) "s")
                                      (cond ((= p 0) "Top")
                                            ((> p 98) "Bot")
                                            (t (concat (number-to-string p) "|")))))))

(load "~/.emacs.d/mainline.el")
(require 'mainline)
(setq mainline-arrow-shape 'arrow)

(defun theme-set (time)
  (if (eq time 'day)
      (progn
        (setq mainline-color1 "#d6d6d6")
        (setq mainline-color2 "#efefef")
        (setq mainline-color3 "#70c0b1")
        (setq mainline-color-fg "black")
        (color-theme-sanityinc-tomorrow-day))
    (setq mainline-color1 "#444444")
    (setq mainline-color2 "#222222")
    (setq mainline-color3 "#293B3A")
    (setq mainline-color-fg "white")
    (color-theme-sanityinc-tomorrow-eighties)))

(load "~/.emacs.d/sunriseset.el")

;; ;; 075E5D
(set-face-attribute 'mode-line nil
                    :background "#444444"
                    :box nil)
;; (propertize " %* " 'face '(:foreground "#ffffff" :background
;; "#293B3A"))

(defun get-interesting-minor-modes ()
  (let ((mms (format-mode-line minor-mode-alist)))
    (propertize
     (replace-regexp-in-string
      " $" ""
      (replace-regexp-in-string
       " +" " "
       (reduce (lambda (s mode)
                 (replace-regexp-in-string mode "" s))
               '("Undo-Tree" "Projectile\\[.+\\]" "WS" "Fill" "hs"
                 "SliNav" "Paredit" "ElDoc" "Hi")
               :initial-value
               mms)))
     'help-echo mms)))

(defun trimmed-buffer-name (bn n)
  (let* ((l (length bn)))
    (if (> l n)
        (concat ".." (substring bn (- l (- n 2))))
      bn)))

(defun get-project-and-branch ()
  (condition-case err
      (let ((pn (and (projectile-project-p) (projectile-project-name))))
        (cond ((and vc-mode pn) (concat pn ":" (replace-regexp-in-string ".+[:-]" "" vc-mode)))
              (vc-mode (replace-regexp-in-string ".+[:-]" "" vc-mode))
              (pn pn)))
    (error "")))

(setq-default mode-line-format
              '("%e" (:eval
                      (let* ((classic-bn-length 20)
                             (ww (window-width))
                             (full-buffer-name (center-format (buffer-name) classic-bn-length))
                             (position-length 16)
                             (vc (get-project-and-branch))
                             (vc-length (if vc (+ 3 (length vc)) 2))
                             (mms (get-interesting-minor-modes))
                             (mms-length (length mms))
                             (mms-length (if (> mms-length 0) (+ 3 mms-length) 2))
                             (total-length (+ 3 (length full-buffer-name) 3 position-length 3
                                              (length mode-name) 1
                                              mms-length vc-length 3 -2))
                             (cut-down (< ww total-length))
                             (space-for-buffer-name (+ (length full-buffer-name)
                                                       (- ww (- total-length mms-length
                                                                vc-length))))
                             (real-buffer-name (if cut-down
                                                   (center-format (trimmed-buffer-name (buffer-name) space-for-buffer-name)
                                                                  (min classic-bn-length space-for-buffer-name))
                                                 full-buffer-name))
                             (total-length (if cut-down
                                               (+ (- total-length mms-length
                                                     vc-length (length full-buffer-name))
                                                  (length real-buffer-name))
                                             total-length))
                             (skip-space (- ww total-length)))
                        (concat
                         (mainline-rmw 'left mainline-color3)
                         (mainline-make 'left real-buffer-name mainline-color3 mainline-color1)
                         (mainline-make 'left (my/percentage-from-top 3) mainline-color1)
                         (mainline-make 'left "(%4l : %3c)" mainline-color1 mainline-color2)
                         (mainline-make 'left (format-mode-line mode-name) mainline-color2)
                         (mainline-make 'center (make-string skip-space 32) mainline-color2)
                         (if cut-down
                             (mainline-make 'center " " mainline-color2)
                           (concat
                            (mainline-make 'right mms mainline-color2)
                            (mainline-make 'right vc mainline-color1 mainline-color2)))
                         (mainline-make 'right (or current-input-method-title "EN")
                                        mainline-color3 (if cut-down
                                                            mainline-color2
                                                          mainline-color1))
                         (mainline-make 'right "%z     " mainline-color3)
                         )))))

;; bs-show for mouse

(put 'ido-exit-minibuffer 'disabled nil)

(setq backup-directory-alist '(("." . "~/.local/share/emacs-saves"))    ; don't litter my fs tree
)

(defun indent-buffer ()
  "indent whole buffer"
  (interactive)
  (indent-region (point-min) (point-max) nil))

(require 'saveplace)
(setq-default save-place t)

;; Multiple cursors


;; (require 'use-package)

;; (use-package nlinum
;;   :defer 1
;;   :init
;;   (define-globalized-minor-mode global-linum-mode nlinum-mode (lambda () (nlinum-mode 1)))
;;   (global-linum-mode 1)
;;   ;; (add-hook 'prog-mode-hook (lambda () (nlinum-mode 1)))
;;   ;; (add-hook 'dired-mode-hook (lambda () (nlinum-mode 1)))
;;   ;; (add-hook 'nxml-mode-hook (lambda () (nlinum-mode 1)))
;;   )

;; (if (daemonp)
;;     (add-hook 'window-setup-hook
;;               (lambda ()
;;                 (message ">> Daemon mode")
;;                 (require 'nlinum)
;;                 (define-globalized-minor-mode global-linum-mode nlinum-mode
;;                   (lambda () (nlinum-mode 1)))
;;                 (global-linum-mode 1)))
;;   (message ">> Non daemon mode")
;;   (define-globalized-minor-mode global-linum-mode nlinum-mode
;;     (lambda () (nlinum-mode 1)))
;;   (global-linum-mode 1)
;;   (require 'nlinum))


;; (global-linum-mode 1)

(setq lua-indent-level 3)

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

;; Configure helm

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
        (helm-git-grep-at-point)
      (helm-git-grep-1))))

;; Configure magit

(use-package magit :ensure t
;  :load-path "~/.emacs.d/site-lisp/magit/lisp"
  :commands (magit-status magit-blame-mode)
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

;;   (use-package magit-gh-pulls :ensure t
;;     :pin melpa
;; ;    :load-path "~/.emacs.d/site-lisp/magit-gh-pulls/"
;;     :init (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls))
  )

(defun rev-at-line ()
  (vc-annotate-extract-revision-at-line))

(defun vc-annotate-show-commit-at-line ()
  (interactive)
  (let* ((rev (car (rev-at-line)))
         (rev (if (string= (substring rev 0 1) "^")
                  (substring rev 1)
                rev)))
    (magit-show-commit rev)))

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
      (ediff-buffers-internal bA bB nil nil nil)
      (ediff-toggle-split))))

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

(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Thesaurus

;; Unfill

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

;; Modeline hiding

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
        (hidden-mode-line-mode -1)
        (centered-window-mode -1))
    (progn
      (hidden-mode-line-mode 1)
      (centered-window-mode 1))))

(require 'aggressive-indent)
;; (global-aggressive-indent-mode)
(add-hook 'lisp-mode-hook 'aggressive-indent-mode)
(add-hook 'clojure-mode-hook 'aggressive-indent-mode)

;; Binary diff

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
     (error (error-message-string err)))))

(use-package langtool :ensure t :demand t)
(setq langtool-java-classpath
      "/usr/share/languagetool:/usr/share/java/languagetool/*")

;; (require 'wakatime-mode)
;; (global-wakatime-mode)

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

(end-of-buffer)

;; Local Variables:
;; eval: (hs-hide-all)
;; End:
