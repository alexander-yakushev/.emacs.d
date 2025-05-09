(when (string-equal system-type "darwin") ;; Emacs Mac Port config
  ;; If menu bar is off, Emacs will always stay on top and frustrate the shit
  ;; out of you.
  (menu-bar-mode 1)
  ;; mac switch meta key
  ;; (setq mac-option-modifier 'meta)
  ;; (setq mac-command-modifier 'super)
  ;; (setq mac-mouse-wheel-smooth-scroll nil)
  ;; (mac-auto-operator-composition-mode)
  )

(progn             ; packages initialization
  (require 'package)

  (setq load-prefer-newer t)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
  (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
  (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
  (package-initialize)

  (when (not (file-exists-p "~/.emacs.d/.initialized"))
    (package-refresh-contents)
    (unless (package-installed-p 'use-package)
      (package-install 'use-package))
    (write-region "" nil "~/.emacs.d/.initialized"))

  (require 'use-package)
  (put 'use-package 'lisp-indent-function 'defun))

(add-to-list 'load-path "~/.emacs.d/site-lisp")
(add-to-list 'load-path "~/.emacs.d/site-lisp/sunrise-commander/")

(load-file (locate-user-emacs-file "bindings.el")) ;; Load bindings
(load (setq custom-file (expand-file-name (locate-user-emacs-file "custom.el"))))

(progn             ; misc initialization
  (cd "~") ;; start from userdir

  ;; Turn off mouse interface early in startup to avoid momentary display
  (dolist (mode '(tool-bar-mode scroll-bar-mode))
    (when (fboundp mode) (funcall mode -1)))

  (when window-system
    (tooltip-mode 1)
    (mouse-wheel-mode t)
    (setq scroll-conservatively 101) ;; move minimum when cursor exits view, instead of recentering
    (setq mouse-wheel-progressive-speed nil) ;; on a long mouse scroll keep scrolling by 1 line
    (blink-cursor-mode -1))

  (random t) ;; Seed the random-number generator

  (defun add-hook-for-modes (hook mode-list)
    (dolist (mode mode-list)
      (add-hook (intern (concat (symbol-name mode) "-mode-hook")) hook)))

  ;; Set GC threshold high, but periodically autocollect when idle.
  (setq gc-cons-threshold (* 50 1024 1024))
  (setq gc-cons-percentage 0.2)
  (run-with-idle-timer 5 t (lambda () (garbage-collect)))

  ;; Auto refresh buffers
  (global-auto-revert-mode 1)

  (add-hook 'text-mode-hook 'turn-on-auto-fill)

  (put 'downcase-region 'disabled nil)
  (put 'upcase-region 'disabled nil)
  (put 'ido-exit-minibuffer 'disabled nil)
  (put 'narrow-to-region 'disabled nil))

;;; Day-to-day usage

(use-package sudo :commands sudo-find-file)

(use-package stesla
  ;; Intentionally reversed
  :bind* (("C-." . stesla-rotate-backward)
          ("C-," . stesla-rotate-forward)
          ;; ("C-M-." . stesla-project-rotate-forward)
          ;; ("C-M-," . stesla-project-rotate-backward)
          ))

(use-package sunrise-commander
  :bind (("<f7>" . sunrise)
         ("<C-f7>" . sunrise-cd-resize)

         :map sr-mode-map
         (";" . dired-next-line)
         ("C-;" . sr-advertised-find-file)
         ("C-h" . sr-go-home)
         ("j" . ido-sunrise)
         ("M-r" . sr-toggle-date-order)
         ("C-c C-o" . sr-open-custom-terminal)
         ("o" . sr-dired-xdg-open)

         :map sr-tabs-mode-map
         ("C-j" . sr-cycle-bookmark)
         ("C-p" . sr-dired-prev-subdir)
         ("C-M-;" . sr-tabs-add)
         ("C-M-l" . sr-tabs-prev)
         ("C-M-'" . sr-tabs-next))
  :init
  (defun sr-detect-switch ()) ;; To fix weird intermittent bug.

  :config
  (use-package sunrise-x-checkpoints)
  (use-package sunrise-x-loop)
  (use-package sunrise-x-mirror)
  (use-package sunrise-x-tabs)

  (defun sunrise-cd-resize ()
    (interactive)
    (setq sr-panes-height (* 2 (/ (frame-height) 3)))
    (sunrise-cd))

  (setq bookmarks '("~/.emacs.d/" "~/clojure/" "~/grammarly/"
                    "~/Games/World of Warcraft/_classic_/Interface/AddOns/"))

  (lexical-let ((bookmark-counter 0))
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

  (defun sr-toggle-date-order ()
    (interactive)
    (if (equal (get sr-selected-window 'sorting-order) "NAME")
        (sr-sort-by-time)
      (sr-sort-by-name))
    (beginning-of-buffer) (next-line) (next-line))

  (defun sr-nova ()
    (interactive)
    (sr-goto-dir "/sshx:nova:/hdd/"))

  (defun sr-osprey ()
    (interactive)
    (sr-goto-dir "/sshx:osprey:~"))

  (defun sr-condor ()
    (interactive)
    (sr-goto-dir "/sshx:condor:~"))

  (defun sr-stella ()
    (interactive)
    (sr-goto-dir "/sshx:deck@192.168.1.172:/home/deck/"))

  (defun sr-open-custom-terminal ()
    (interactive)
    (shell-command (concat "osascript -e 'tell application \"iTerm2\" to activate' -e 'tell application \"iTerm2\"' -e 'tell current window' -e 'create tab with default profile' -e 'tell current session' -e 'write text \"cd \\\"" (expand-file-name (sr-choose-cd-target)) "\\\"\"' -e 'end tell' -e 'end tell' -e 'end tell'"))
    ;; (shell-command (concat "urxvt -cd \"" (expand-file-name (sr-choose-cd-target)) "\" -e zsh"))
    )

  (defun sr-dired-xdg-open ()
    (interactive)
    (dired-do-shell-command "open" nil (dired-get-marked-files t nil nil nil t)))

  (defun ido-sunrise ()
    "Call `sunrise' the ido way.
    The directory is selected interactively by typing a substring.
    For details on keybindings, see `ido-find-file'."
    (interactive)
    (let ((ido-report-no-match nil)
          (ido-auto-merge-work-directories-length -1))
      (ido-file-internal 'read-only 'sr-advertised-find-file nil "Sunrise: " 'dir)))

  (defun sunrise-reset-directories ()
    "To be used if the remembered directories are non-existent."
    (interactive)
    (setq sr-left-directory "~")
    (setq sr-right-directory "~")
    (sunrise))

  (use-package java-decompiler
    :config
    (use-package javap-mode :ensure t)
    (add-hook 'find-file-hook 'java-decompiler-find-class))

  (openwith-mode t))

(use-package multiple-cursors :ensure t :demand t
  :bind (("C-M-<mouse-1>" . mc/add-cursor-on-click)
         ("<C-down>" . mc/mark-next-like-this)
         ("<C-M-down>" . mc/mark-next-like-this-symbol)
         ("C-c m" . mc/mark-all-like-this-dwim)

         :map mc/keymap
         ("C-'" . forward-char)
         ("C-h" . mc-hide-unmatched-lines-mode)))

(use-package visual-regexp :ensure t
  :bind (("M-%" . vr/query-replace))
  :init
  ;; Redefine dired search-and-replace
  (defun dired-do-find-regexp-and-replace (from to arg)
    (interactive
     (let ((common
            (query-replace-read-args
             "Query replace regexp in marked files" t t)))
       (list (nth 0 common) (nth 1 common) current-prefix-arg)))
    (save-current-buffer
      (dolist (file (dired-get-marked-files))
        (find-file file)
        (if arg
            (replace-regexp from to nil (point-min) (point-max))
          (query-replace-regexp from to nil (point-min) (point-max)))
        ;; (vr/query-replace from to (point-min) (point-max))
        (save-buffer)
        (kill-buffer)))))

(use-package ediff
  :bind (("C-c d" . ediff-opened-buffers))
  :commands ediff
  :config
  (add-hook 'ediff-startup-hook 'ediff-toggle-split)

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

  (defun ediff-files-internal-advice (orig-fun &rest args)
    "catch the condition when the binary files differ
the reason for catching the error out here (when re-thrown from the inner advice)
is to let the stack continue to unwind before we start the new diff
otherwise some code in the middle of the stack expects some output that
isn't there and triggers an error"
    (let ((file-A (nth args 0))
          (file-B (nth args 1))
          ediff-do-hexl-diff)
      (condition-case err
          (apply orig-fun args)
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
  (advice-add 'ediff-files-internal :around #'ediff-files-internal-advice)

  (defun ediff-setup-diff-regions-advice (orig-fun &rest args)
    "when binary files differ, set the variable "
    (condition-case err
        (apply orig-fun args)
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
  (advice-add 'ediff-setup-diff-regions :around #'ediff-setup-diff-regions-advice))

(use-package midnight :demand t
  :config (midnight-delay-set 'midnight-delay "11:59pm"))

(use-package calendar
  :config
  (calendar-set-date-style 'european)
  (setq calendar-week-start-day 1))

(use-package smex :ensure t
  :bind (("M-x" . my-smex))
  :config
  (smex-initialize)

  (defun my-smex ()
    (interactive)
    ;; Ensure that GC doesn't happen in SMEX.
    (let ((gc-cons-threshold most-positive-fixnum))
      (smex))))

(use-package saveplace :demand t
  :init
  (save-place-mode 1))

(use-package recentf :demand t
  :config
  (recentf-mode 1))

(use-package ido :demand t
  :bind (:map ido-file-dir-completion-map
              ("M-f" . ido-project-find-file))
  :config
  (defun ido-project-find-file (&optional file)
    (interactive)
    (setq ido-exit 'fallback)
    (setq ido-fallback 'project-find-file)
    (setq ido-text-init file)
    (setq ido-rotate-temp t)
    (exit-minibuffer))

  (ido-mode t)
  (ido-ubiquitous-mode))

(use-package bs
  :bind (:map bs-mode-map
              ("M-;" . bs-down-next-project)
              ("M-p" . bs-up-next-project))
  :config
  (font-lock-add-keywords
   'bs-mode '(("\\[.+\\]" . font-lock-function-name-face)))

  (font-lock-add-keywords
   nil `(("(?\\(lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil)))))

  (defun bs--get-buffer-project (buffer)
    (let ((fname (buffer-file-name buffer)))
      (if (and fname
               (not (s-starts-with? "/sshx" fname)))
          (if-let (p (project-current nil (file-name-directory fname)))
              (project-name p)
            "<none>")
        "<none>")))

  ;; (defun bs--sort-by-project (b1 b2)
  ;;   "Compare buffers B1 and B2 by buffer name."
  ;;   (string< (bs--get-buffer-project b1)
  ;;            (bs--get-buffer-project b2)))

  (defun advise-bs-buffer-list (orig &rest _)
    (let ((og (funcall orig))
          (prev nil)
          (res nil))
      (dolist (buf og)
        (let ((pr (bs--get-buffer-project buf)))
          (when (not (equal pr prev))
            (when (not (null prev))
              (push "" res))
            (push (format "[%s]" pr) res)
            (setq prev pr))
          (push buf res)))
      (nreverse res)))

  (advice-add 'bs-buffer-list :around #'advise-bs-buffer-list)

  (defun bs-show-in-buffer (list)
    "Display buffer list LIST in buffer *buffer-selection*.
Select buffer *buffer-selection* and display buffers according to current
configuration `bs-current-configuration'.  Set window height, fontify buffer
and move point to current buffer."
    (setq bs-current-list list)
    (let* ((window-combination-limit 'window-size)
           (bs-buf (get-buffer-create "*buffer-selection*"))
           (bs-win (progn
                     (pop-to-buffer bs-buf bs-default-action-list)
                     (selected-window))))
      ;; Delete other windows showing *buffer-selection*.
      ;; Done after pop-to-buffer, instead of just calling delete-windows-on,
      ;; to allow display-buffer-reuse(-mode)?-window to be used in ALIST.
      (dolist (w (get-buffer-window-list bs-buf 'not t))
        (unless (eq w bs-win)
          (with-demoted-errors "Error deleting window: %S"
            (delete-window w)))))
    (bs-mode)
    (text-scale-set -1)
    (let* ((inhibit-read-only t)
           (map-fun (lambda (entry)
                      (if (bufferp entry)
                          (string-width (buffer-name entry))
                        0)))
           (max-length-of-names (apply 'max
                                       (cons 0 (mapcar map-fun list))))
           (name-entry-length (min bs-maximal-buffer-name-column
                                   (max bs-minimal-buffer-name-column
                                        max-length-of-names))))
      (erase-buffer)
      (setq bs--name-entry-length name-entry-length)
      (bs--show-header)
      (let ((fst t))
        (dolist (buffer list)
          (if (stringp buffer)
              (let ((str (format "   %s" buffer)))
                (put-text-property 0 (length str) 'face 'bold str)
                (insert str)
                (insert "\n"))
            (bs--insert-one-entry buffer)
            (insert "\n"))
          (setq fst nil)))
      (delete-char -1)
      (bs--set-window-height)
      (bs--goto-current-buffer)
      (font-lock-ensure)
      (bs-apply-sort-faces)
      (set-buffer-modified-p nil)))

  (defun advise-bs-up-down (orig &rest args)
    (apply orig args)
    (while (stringp (bs--current-buffer))
      (apply orig args)))

  (advice-add 'bs-down :around #'advise-bs-up-down)
  (advice-add 'bs-up :around #'advise-bs-up-down)

  (advice-add 'bs-show :after #'recenter-top-bottom)

  (defun bs-down-next-project ()
    (interactive)
    (bs--down)
    (while (not (stringp (bs--current-buffer)))
      (bs--down))
    (while (stringp (bs--current-buffer))
      (bs--down)))

  (defun bs-up-next-project ()
    (interactive)
    (bs--up)
    (while (not (stringp (bs--current-buffer)))
      (bs--up))
    (while (stringp (bs--current-buffer))
      (bs--up))
    (while (not (stringp (bs--current-buffer)))
      (bs--up))
    (bs--down)))

(use-package mainline :demand t
  :config
  (use-package diminish :ensure t :demand t
    :config
    (dolist (mode '(eldoc-mode auto-fill-function auto-revert-mode hi-lock-mode
                               global-whitespace-mode))
      (diminish mode)))

  (mainline-activate))

(use-package color-theme-sanityinc-tomorrow :ensure t)

(use-package daycycle :demand t ;; set theme and switch it during the day
  :config
  (defun -theme-set (time)
    (cond ((eq time 'day)
           (set-face-attribute 'mainline-face1 nil :foreground "black" :background "#d6d6d6")
           (set-face-attribute 'mainline-face2 nil :foreground "black" :background "#efefef")
           (set-face-attribute 'mainline-face3 nil :foreground "black" :background "#70c0b1")
           (custom-set-faces
            '(show-paren-match ((t (:foreground "grey70" :bold nil :background "#008800"))))
            '(show-paren-mismatch ((t (:foreground "grey70" :bold nil :background "#880000"))))
            '(mode-line ((t (:background "#d6d6d6" :box nil)))))
           (color-theme-sanityinc-tomorrow-day))

          ((eq time 'night)
           (set-face-attribute 'mainline-face1 nil :foreground "white" :background "#444444")
           (set-face-attribute 'mainline-face2 nil :foreground "white" :background "#222222")
           (set-face-attribute 'mainline-face3 nil :foreground "white" :background "#293B3A")
           (custom-set-faces
            '(show-paren-match ((t (:foreground "#00ff00" :bold t :background unspecified))))
            '(show-paren-mismatch ((t (:foreground "#ff0000" :bold t :background unspecified))))
            '(mode-line ((t (:background "#444444" :box nil)))))
           (color-theme-sanityinc-tomorrow-eighties)))
    (setq fci-rule-color "sienna")
    (setq-default fci-rule-color "sienna")
    (custom-set-faces
     `(fringe ((t (:background ,(face-attribute 'default :background)))))))

  (daycycle-init '-theme-set 'auto))

(use-package usefuls :demand t
  :bind* (("C-M-q" . narrow-or-widen-dwim)
          ("C-c c" . clone-and-comment-line)
          ("C-+" . inc-frame-font-size)
          ("C-_" . dec-frame-font-size)
          ("C-M-+" . set-frame-font-size-monitor)
          ("C-M-_" . set-frame-font-size-laptop))
  :config
  (advice-yank-auto-indent))

;; (use-package vlf :ensure t)

(use-package calc
  :config
  (setq math-additional-units '((TiB "1024 * GiB" "Tera Byte")
                                (GiB "1024 * MiB" "Giga Byte")
                                (MiB "1024 * KiB" "Mega Byte")
                                (KiB "1024 * B" "Kilo Byte")
                                (B nil "Byte")
                                (Gib "1024 * Mib" "Giga Bit")
                                (Mib "1024 * Kib" "Mega Bit")
                                (Kib "1024 * b" "Kilo Bit")
                                (b "B / 8" "Bit"))))

(use-package time
  :config
  (defvar-local world-clock-constant-time nil)

  ;; Redefining it to add support for setting time.
  (defun world-clock-display (alist)
    "Replace current buffer text with times in various zones, based on ALIST."
    (let ((inhibit-read-only t)
          (buffer-undo-list t)
          (now (or world-clock-constant-time (current-time)))
          (max-width 0)
          result fmt)
      (erase-buffer)
      (dolist (zone alist)
        (let* ((label (cadr zone))
               (width (string-width label)))
          (push (cons label
                      (format-time-string world-clock-time-format
                                          now (car zone)))
                result)
          (when (> width max-width)
            (setq max-width width))))
      (setq fmt (concat "%-" (int-to-string max-width) "s %s\n"))
      (dolist (timedata (nreverse result))
        (insert (format fmt
                        (propertize (car timedata)
                                    'face 'world-clock-label)
                        (cdr timedata))))
      (delete-char -1))
    (goto-char (point-min)))

  (defun world-clock-set-time ()
    (interactive)
    (let* ((input (read-string "Local time to display: "))
           (in-time (parse-time-string input))
           (new-time (mapcar* (lambda (x y) (or x y))
                              in-time
                              (parse-time-string (current-time-string)))))
      (setq world-clock-constant-time (apply 'encode-time new-time))
      (world-clock)))

  (define-key world-clock-mode-map (kbd "RET") 'world-clock-set-time))

(use-package bm :ensure t
  :bind (("C-S-SPC" . bm-toggle)
         ("C-9" . bm-previous)
         ("C-0" . bm-next)))

(use-package deft :ensure t :pin melpa-stable
  :bind (("<f9>" . deft)
         ("<M-f9>" . deft-convert-buffer-into-note)
         :map deft-mode-map
         ("C-q" . kill-this-buffer))
  :config
  ;; Auto-close deft window when jumping to other window.
  (defun deft--close-deft-buffer ()
    (kill-matching-buffers "\*Deft\*" t t))

  (defun deft-convert-buffer-into-note (slug)
    (interactive "sNew filename (without extension): ")
    (let ((curr-buf (current-buffer)))
      ;; Save current buffer text into kill ring.
      (kill-ring-save (point-min) (point-max))
      (deft-new-file-named slug) ;; Create new note.
      ;; Paste the killed text there and save.
      (yank)
      (save-buffer)
      (setq kill-ring (cdr kill-ring)) ;; Clear the kill ring.
      (kill-buffer curr-buf)))

  (add-hook 'deft-open-file-hook 'deft--close-deft-buffer))

(use-package keyfreq :demand t
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(use-package goto-chg :ensure t :demand t
  :bind (("C-x C-x" . goto-last-change)))

(use-package anzu :ensure t :demand t
  :config
  (setq anzu-cons-mode-line-p nil)
  (setcar (cdr (assq 'isearch-mode minor-mode-alist))
          '(:eval (anzu--update-mode-line)))
  (global-anzu-mode +1)
  (diminish 'anzu-mode))

;;; Programming/Version Control

(use-package magit :ensure t
  :bind (("C-x g" . magit-status)
         ("M-g" . magit-status)
         ("<f8>" . magit-blame-addition)
         ("<M-f8>" . magit-blame)

         :map magit-mode-map
         (";" . magit-section-forward)
         ("M-;" . magit-section-forward-sibling)
         ("X" . magit-reset-hard)

         :map magit-refs-mode-map
         (";" . magit-section-forward)

         :map magit-log-mode-map
         ("p" . previous-line)
         (";" . next-line)

         :map magit-blame-mode-map
         (";" . magit-blame-next-chunk))
  :commands (magit-show-commit)
  :config
  (setq magit-last-seen-setup-instructions "1.4.0")
  (remove-hook 'magit-status-sections-hook 'magit-insert-stashes)
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-stashes 'magit-insert-untracked-files)
  ;; Don't show recent commits
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-unpushed-to-upstream
                          'magit-insert-unpushed-to-upstream-or-recent
                          'replace)

  (defun magit-hunk-recenter-top (section &rest _)
    (goto-char (--if-let (magit-section-goto-successor-1 section)
                   (if (eq (oref it type) 'button)
                       (point-min)
                     (oref it start))
                 (point-min)))
    (recenter)
    t)

  (defun magit-section-highlight-less (section _)
    (magit-section-case
     ((untracked unstaged staged unpushed unpulled pulls branch)
      nil
      t)))

  (add-hook 'magit-section-highlight-hook 'magit-section-highlight-less)
  (setq magit-section-goto-successor-hook #'magit-hunk-recenter-top))

(use-package git-timemachine :ensure t
  :bind (("<C-f8>" . git-timemachine)
         :map git-timemachine-mode-map
         ("c" . git-timemachine-show-commit))
  :config
  (defun git-timemachine-show-commit ()
    (interactive)
    (magit-show-commit (car git-timemachine-revision)))

  (defface git-timemachine-minibuffer-author-face
    '((t (:foreground "firebrick")))
    "How to display the minibuffer detail"
    :group 'git-timemachine)

  ;; Show shorter date in the minibuffer.
  (defun advise-git-timemachine--process-file (orig &rest args)
    (if (equal (car args) "log")
        (let* ((flags-end (-elem-index "--" args))
               (new-args (-insert-at flags-end "--date=short" args)))
          (apply orig new-args))
      (apply orig args)))
  (advice-add 'git-timemachine--process-file :around #'advise-git-timemachine--process-file)

  ;; Overriden to customize minibuffer message.
  (defun git-timemachine--show-minibuffer-details (revision)
    "Show details for REVISION in minibuffer."
    (let ((commit (subseq (nth 0 revision) 0 7))
          (subject (nth 5 revision))
          (date-relative (nth 3 revision))
          (date-full (nth 4 revision))
          (author (nth 6 revision)))
      (message (format "%s: %s (%s) [%s (%s)]"
                       (propertize commit 'face 'git-timemachine-commit)
                       (propertize subject 'face 'git-timemachine-minibuffer-detail-face)
                       (propertize author 'face 'git-timemachine-minibuffer-author-face)
                       date-full date-relative)))))

(use-package git-gutter :ensure t :demand t
  :bind (("C-M-." . git-gutter:next-hunk)
         ("C-M-," . git-gutter:previous-hunk))
  :config
  (global-git-gutter-mode 1)
  (diminish 'git-gutter-mode)
  (setq-default git-gutter:modified-sign "~"))

(use-package vc-annotate
  :commands vc-annotate
  :bind (:map vc-annotate-mode-map
              ("c" . vc-annotate-show-commit-at-line))
  :config
  (defun vc-annotate-show-commit-at-line ()
    (interactive)
    (let* ((rev (car (vc-annotate-extract-revision-at-line)))
           (rev (if (string= (substring rev 0 1) "^")
                    (substring rev 1)
                  rev)))
      (magit-show-commit rev))))

;;; Programming/Clojure & Lisps

(use-package clojure-mode :ensure t)

(use-package paredit :ensure t :demand t
  :bind (:map
         paredit-mode-map
         ("M-(" . paredit-wrap-sexp)
         ("M-[" . paredit-wrap-square)
         ("M-{" . paredit-wrap-curly)
         ("M-p" . paredit-backward-down)
         ("M-;" .  paredit-forward-down)
         ("C-M-p" . paredit-backward-up)
         ("C-M-;" . paredit-forward-up)
         ("C-M-'" . forward-sexp)
         ("C-M-l" . backward-sexp)
         ;; ("M-a" . highlight-symbol-prev)
         ("M-k" . kill-line)
         ("M-d" . kill-region)
         ("C-M-d" . delete-region)
         ("C-w" . paredit-backward-kill-word)
         ("<C-backspace>" . paredit-backward-kill-word))
  :config
  (diminish 'paredit-mode "Par")
  (add-hook-for-modes 'paredit-mode '(scheme emacs-lisp lisp clojure clojurescript)))

(use-package cider :ensure t
  :bind (:map
         cider-mode-map
         ("C-c t" . cider-toggle-trace-var)
         ("C-c i" . cider-inspect)
         ("C-c j" . clj-decompiler-decompile)
         ("C-c C-e" . cider-eval-last-sexp-in-context)
         ("C-c C-t M-." . cider-test-jump-to-function-test)
         ("C-c C-t a" . cider-test-macroexpand-are)
         ("C-c C-s w" . sesman-pop-browser)

         :map
         cider-repl-mode-map
         ("C-c C-l" . cider-clear-repl-buffer-with-shift))
  :commands (cider-connect cider-jack-in)
  :config
  (add-hook 'cider-mode-hook 'eldoc-mode)

  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-mode-hook #'company-mode)

  (use-package cider-inspector :demand t
    :bind (:map cider-inspector-mode-map
                (";" . cider-inspector-next-inspectable-object)
                ("p" . cider-inspector-previous-inspectable-object)
                ;; ("C-;" . cider-inspector-operate-on-point)
                ;; ("C-p" . cider-inspector-pop)
                ("r" . cider-reinspect)))

  (use-package sesman
    :bind (:map sesman-browser-mode-map
                ("q" . sesman-browser-close-browser)
                ("k" . sesman-browser-quit-session))
    :config
    (defvar sesman--window-config-coming-from nil)
    (defun sesman--restore-window-config ()
      (when sesman--window-config-coming-from
        (let ((frame (selected-frame)))
          (unwind-protect
              (set-window-configuration sesman--window-config-coming-from)
            (select-frame frame)))
        (setq bs--window-config-coming-from nil)))

    (defun sesman-browser ()
      "Display an interactive session browser.
See `sesman-browser-mode' for more details."
      (interactive)
      (let* ((system (sesman--system))
             (pop-to (called-interactively-p 'any))
             (sessions (sesman-sessions system))
             (cur-session (when pop-to
                            (sesman-current-session 'CIDER)))
             (buff (get-buffer-create (format "*sesman %s browser*" system))))
        (with-current-buffer buff
          (setq-local sesman-system system)
          (sesman-browser-mode)
          (cursor-sensor-mode 1)
          (let ((inhibit-read-only t)
                (sessions (pcase sesman-browser-sort-type
                            ('name (seq-sort (lambda (a b) (string-greaterp (car b) (car a)))
                                             sessions))
                            ('relevance (sesman--sort-sessions system sessions))
                            (_ (error "Invalid `sesman-browser-sort-type'"))))
                (i 0))
            (erase-buffer)
            (insert "\n ")
            (insert (propertize (format "%s Sessions:" system)
                                'face '(bold font-lock-keyword-face)))
            (insert "\n\n")
            (dolist (ses sessions)
              (setq i (1+ i))
              (sesman-browser--insert-session system ses i))
            ;; (when pop-to
            ;;   (pop-to-buffer buff)
            ;;   (sesman-browser--goto-stop (car cur-session)))
            (sesman-browser--sensor-function)))))

    (defun sesman-pop-browser ()
      (interactive)
      (sesman--restore-window-config)
      (setq sesman--window-config-coming-from (current-window-configuration))
      (when (> (window-height) 7)
        (ignore-errors (select-window (split-window-below))))
      (sesman-browser)
      (switch-to-buffer (get-buffer-create (format "*sesman %s browser*"
                                                   (sesman--system)))))

    (defun sesman-browser-close-browser ()
      "Quite session at point."
      (interactive)
      (kill-buffer (current-buffer))
      (sesman--restore-window-config)))

  (defun cider-clear-repl-buffer-with-shift ()
    "Needed to side-step https://github.com/clojure-emacs/cider/issues/2595."
    (interactive)
    (cider-repl-clear-buffer)
    (insert "(symbol \"\")")
    (cider-repl-return))

  (defvar cider-inspect-last-inspected-expr nil)

  (defun cider-inspect-last-sexp ()
    "Inspect the result of the the expression preceding point."
    (interactive)
    (let ((expr (cider-last-sexp)))
      (setq cider-inspect-last-inspected-expr expr)
      (cider-inspect-expr expr (cider-current-ns))))

  (defun cider-reinspect ()
    "Like refresh, but re-evaluates the last expression."
    (interactive)
    (cider-popup-buffer-quit-function)
    (cider-inspect-expr cider-inspect-last-inspected-expr (cider-current-ns)))

  (defun cider-toggle-warn-on-reflection ()
    (interactive)
    (let* ((form (concat "(set! *warn-on-reflection* (not *warn-on-reflection*))"))
           (result (nrepl-dict-get (cider-nrepl-sync-request:eval
                                    form nil (cider-current-ns))
                                   "value")))
      (message "*warn-on-reflection*: %s" result)))

  ;; Prevent CIDER from jumping to source in other window.
  (defun cider--jump-to-loc-from-info-always-same-window (orig-fn info &rest _)
    (funcall orig-fn info))

  (advice-add 'cider--jump-to-loc-from-info :around #'cider--jump-to-loc-from-info-always-same-window)

  (defun cider--switch-to-nrepl-server-when-jack-in (orig-fn params &rest _)
    (let ((process (funcall orig-fn params)))
      (switch-to-buffer (process-buffer process))
      (insert "\n\n===\n\nPlease wait...\n")
      (beginning-of-buffer)))

  (advice-add 'cider-jack-in-clj :around #'cider--switch-to-nrepl-server-when-jack-in)
  ;; (advice-remove 'cider-jack-in-clj 'cider--jump-to-nrepl-server-when-jack-in)

  (defun cider-test-jump-to-function-test ()
    (interactive)
    (cider-try-symbol-at-point
     "Symbol"
     (lambda (var)
       (let* ((info (cider-var-info var))
              (ns (nrepl-dict-get info "ns"))
              (var (nrepl-dict-get info "name")))
         (cider-find-var nil (concat ns "-test/" var "-test") nil)))))

  (defun cljr-require ()
    (interactive)
    (when-let (aliases (cljr--magic-requires-lookup-alias))
      (let ((short (cl-first aliases)))
        (when-let (long (cljr--prompt-user-for "Require " (cl-second aliases)))
          (save-excursion
            (cljr--insert-in-ns ":require")
            (let ((libspec (format "[%s :as %s]" long short)))
              (insert libspec)
              (ignore-errors (cljr--maybe-eval-ns-form))
              (cljr--indent-defun)
              (cljr--post-command-message "Required %s" libspec)))))))

  (defun cider-test-macroexpand-are ()
    (interactive)
    (cider-macroexpand-1-inplace)
    (cider-macroexpand-1-inplace))

  (defun clojure-lein-to-tools-deps-dependency ()
    (interactive)
    (unless (eq (char-after) ?\[)
      (search-backward "["))
    (forward-char)
    (paredit-splice-sexp)
    (backward-char)
    (search-forward "\"")
    (backward-char)
    (paredit-wrap-curly)
    (insert ":mvn/version "))

  (defun clojure-capture-args ()
    (interactive)
    (let ((args (buffer-substring-no-properties (region-beginning)
                                                (region-end))))
      (forward-sexp)
      (paredit-C-j)
      (insert "#_(reset! -args [])")
      (paredit-C-j)
      (insert (format "(-/capture -args %s)" args))))

  (defun cider-require-ns-at-point ()
    "Like refresh, but re-evaluates the last expression."
    (interactive)
    (save-excursion
      (let* ((bounds (bounds-of-thing-at-point 'symbol))
             (symbol (when bounds
                       (buffer-substring-no-properties (car bounds) (cdr bounds))))
             (ns (when (and symbol (string-match "\\(.+\\)/" symbol))
                   (match-string 1 symbol))))
        (if ns
            (cider-interactive-eval (format "(require '%s)" ns))
          (user-error "No namespace found in symbol at point"))))))

(use-package clj-refactor :ensure t
  :config
  (cljr-add-keybindings-with-prefix "C-c C-r")
  (add-hook 'clojure-mode-hook 'clj-refactor-mode)
  (add-hook 'clojure-mode-hook 'yas-minor-mode-on)
  ;; (add-hook 'clojure-ts-mode-hook #'clj-refactor-mode)
  ;; (add-hook 'clojure-ts-mode-hook #'yas-minor-mode-on)
  )

(use-package company :ensure t :demand t
  :bind (:map company-mode-map
              ("TAB" . company-indent-or-complete-must-have-prefix)
              ("M-SPC" . company-complete)

              :map company-active-map
              ("TAB" . company-complete-selection)
              ("<tab>" . company-complete-selection))
  :config
  (defun company-indent-or-complete-must-have-prefix ()
    "Indent the current line or region, or complete the common
part if there is prefix."
    (interactive)
    (if (looking-at "\\_>")
        (company-indent-or-complete-common nil)
      (call-interactively #'indent-for-tab-command)))

  (use-package company-quickhelp :ensure t :demand t
    :config
    (company-quickhelp-mode 1)))

(use-package clj-decompiler :ensure t)

(use-package comment-sexp :demand t)

(use-package highlight-parentheses :ensure t
  :config
  (set-face-attribute 'hl-paren-face nil :weight 'ultra-bold)
  (diminish 'highlight-parentheses-mode)
  (add-hook 'lisp-mode-hook 'highlight-parentheses-mode)
  (add-hook 'clojure-mode-hook 'highlight-parentheses-mode)
  ;; (add-hook 'clojure-ts-mode-hook 'highlight-parentheses-mode)
  )

(use-package elisp-mode
  :bind (:map
         emacs-lisp-mode-map
         ("C-c C-e" . eval-defun)
         ("C-c M-e" . eval-and-replace)

         :map read-expression-map
         ("TAB" . lisp-complete-symbol))
  :config
  (use-package elisp-slime-nav :ensure t
    :config
    (diminish 'elisp-slime-nav-mode)
    (add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode))

  (defun elisp-mode-pretty-lambdas ()
    (font-lock-add-keywords
     nil `(("(?\\(lambda\\>\\)"
            (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                      ,(make-char 'greek-iso8859-7 107))
                      nil))))))

  (add-hook 'emacs-lisp-mode-hook 'company-mode)
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'emacs-lisp-mode-hook 'elisp-mode-pretty-lambdas))

(use-package paren-face :ensure t :demand t
  :config
  (add-hook-for-modes 'paren-face-mode '(scheme emacs-lisp lisp))
  (add-hook 'clojure-mode-hook
            (lambda ()
              (setq-local paren-face-regexp "[][(){}]")
              (paren-face-mode 1)))
  (add-hook 'clojurescript-mode-hook
            (lambda ()
              (setq-local paren-face-regexp "[][(){}]")
              (paren-face-mode 1)))
  ;; (add-hook 'clojure-ts-mode-hook
  ;;           (lambda ()
  ;;             (setq-local paren-face-regexp "[][(){}]")
  ;;             (paren-face-mode 1)))
  )

;;; Programming/Other languages

(use-package java-mode
  :bind (:map java-mode-map
              ("M-SPC" . yas/expand)))

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
  (add-hook 'java-mode-hook 'java-default-formatting)

  (use-package java-snippets :ensure t
    :init
    (add-hook 'java-mode-hook 'yas-minor-mode)))

(use-package web-mode :ensure t)

(use-package go-mode :ensure t
  :config
  (add-hook 'go-mode-hook (lambda () (setq whitespace-style '(face trailing empty)
                                      indent-tabs-mode t
                                      tab-width 4)
                            (whitespace-mode -1))))

(use-package rust-mode :ensure t
  :config
  (add-hook 'rust-mode-hook 'yas-minor-mode)

  (defun rust-wrap-debug-println ()
    (interactive)
    (goto-char (region-beginning))
    (insert "println!(\"{:?}\", ")
    (goto-char (region-end))
    (insert ");")))

(use-package markdown-mode :ensure t
  :bind (:map markdown-mode-map
              ("C-c C-l" . markdown-smart-insert-link)
              ("C-c C-c C-c" . markdown-insert-gfm-code-block))
  :config
  (use-package markdown-preview-mode :ensure t
    :config
    (setq markdown-preview-stylesheets (list "http://thomasf.github.io/solarized-css/solarized-light.min.css")))

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
        (insert (concat "[" (if (string= text "") link text) "](" link ")")))))

  (defvar github-link-to-issue-history ())
  (defun github-link-to-issue ()
    (interactive)
    (let* ((choices '())
           (minibuffer-completion-table choices)
           (ido-max-prospects 10))
      (let* ((issue (buffer-substring-no-properties (region-beginning) (region-end)))
             (number (substring issue 1))
             (repo
              (ido-completing-read "Github repository: " nil nil nil
                                   nil 'github-link-to-issue-history (car github-link-to-issue-history))))
        (delete-region (region-beginning) (region-end))
        (insert (format "[%s](https://github.com/%s/issues/%s)" issue repo number))))))

(use-package zencoding-mode :ensure t
  :bind (:map zencoding-mode-keymap
         ("C-j" . electric-newline-and-maybe-indent)
         ("C-c C-z" . zencoding-expand-line)))

;;; Programming/Miscellaneous

(use-package yasnippet :demand t
  :init
  (diminish 'yas-minor-mode)
  (define-key yas-minor-mode-map [(tab)] nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil))

(use-package rainbow-numbers-mode :demand t)

(use-package prog-mode
  :config
  (defun prog-mode-local-comment-auto-fill ()
    (set (make-local-variable 'comment-auto-fill-only-comments) t)
    (auto-fill-mode t))

  (defun prog-mode-add-watchwords ()
    (font-lock-add-keywords
     nil '(("\\<\\(TODO\\|FIXME\\)"
            1 font-lock-warning-face t))))

  (add-hook 'prog-mode-hook 'prog-mode-local-comment-auto-fill)
  (add-hook 'prog-mode-hook 'hl-line-mode)
  (add-hook 'prog-mode-hook 'prog-mode-add-watchwords)
  ;; (add-hook 'prog-mode-hook 'rainbow-numbers-mode)
  )

(use-package rainbow-mode :ensure t
  :commands rainbow-turn-on
  :init
  (add-hook-for-modes 'rainbow-turn-on '(prog nxml sgml web css)))

(use-package project :demand t
  :bind (("M-f" . project-find-file))
  ;; To replace in 'project--read-file-cpd-relative
  ;; (concat (format "[%s] " (cadr (s-match ".+/\\([^/]+\\)/?$" common-parent-directory))) prompt))
  )

(use-package helm-ag :ensure t
  :bind* (("M-h" . helm-do-ag-project-root-custom)
          ("M-H" . helm-do-ag-non-parallel))
  :bind (:map helm-ag-map
              ("C-;" . helm-next-line)
              ("M-;" . helm-goto-next-file)
              ("M-p" . helm-goto-precedent-file)
              ("<right>" . helm-execute-persistent-action))
  :config
  (defun helm-ag--insert-thing-at-point (thing)
    "Not documented, THING."
    (helm-aif (thing-at-point thing)
        (s-replace-all '(("-" . "\\-")) (regexp-quote (substring-no-properties it)))
      ""))

  (defun helm-do-ag-project-root-custom (sym-at-p)
    (interactive "P")
    (let ((helm-ag-base-command (if (equal current-prefix-arg '(16))
                                    helm-ag-base-command
                                  (concat helm-ag-base-command " -j1")))
          (helm-ag-insert-at-point (when sym-at-p 'symbol)))
      (helm-do-ag-project-root)))

  (defun helm-do-ag-non-parallel (sym-at-p)
    (interactive "P")
    (let ((helm-ag-base-command (if (equal current-prefix-arg '(16))
                                    helm-ag-base-command
                                  (concat helm-ag-base-command " -j1")))
          (helm-ag-insert-at-point (when sym-at-p 'symbol)))
      (helm-do-ag))))

(use-package helm-swoop :ensure t)

(use-package hippie-exp
  :bind (("M-/" . hippie-expand))
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
  :config
  (diminish 'wakatime-mode)
  (global-wakatime-mode))

(use-package display-fill-column-indicator :demand t
  :config
  (add-hook 'prog-mode-hook 'display-fill-column-indicator--turn-on))

(use-package hideshow :demand t
  :hook ((prog-mode          . hs-minor-mode)
         (clojure-mode       . hs-clojure-hide-namespace-and-folds)
         ;; (clojure-ts-mode    . hs-clojure-hide-namespace-and-folds)
         (clojurescript-mode . hs-clojure-hide-namespace-and-folds))
  :config
  (defun hs-clojure-hide-namespace-and-folds ()
    "Hide the first (ns ...) expression in the file, and also all
the (^:fold ...) expressions."
    (interactive)
    (hs-life-goes-on
     (save-excursion
       (goto-char (point-min))
       (when (ignore-errors (re-search-forward "^(ns "))
         (hs-hide-block))

       (while (ignore-errors (re-search-forward "\\^:fold"))
         (hs-hide-block)
         (next-line))

       (beginning-of-buffer)
       (while (ignore-errors (re-search-forward "^(s/fdef"))
         (hs-hide-block)
         (next-line)))))

  (diminish 'hs-minor-mode))

(use-package dumb-jump :ensure t
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;;; Writing

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :bind (:map org-mode-map
              ("C-'" . forward-char))
  :config
  (add-hook 'org-mode-hook #'org-indent-mode)
  (setq org-inhibit-startup-visibility-stuff t))

(use-package centered-window-mode :ensure t
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

;; Nova related

(defun tramp-nova-docker-compose ()
  (interactive)
  (find-file "/sshx:nova:/hdd/raid/services/docker-compose.yml"))

;; Grammarly-related
;; (use-package grammarly :demand t)

;; Local Variables:
;; eval: (hs-hide-all)
;; End:
