(require 'hippie-exp)

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("SC"  . "http://joseito.republika.pl/sunrise-commander/") t)
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(clean-buffer-list-delay-general 1)
 '(clojure-defun-indents (quote (on-ui\ set-content-view!)))
 '(clojure-swank-command "echo \"lein2 jack-in %s\" | $SHELL -l")
 '(default-input-method "ukrainian-computer")
 '(dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\..+")
 '(font-lock-maximum-decoration (quote ((dired-mode) (sunrise) (t . t))))
 '(inferior-lisp-program "sbcl")
 '(ispell-program-name "/usr/bin/aspell")
 '(ls-lisp-verbosity (quote (links uid gid)))
 '(midnight-mode t nil (midnight))
 '(minimap-width-fraction 0.1)
 '(minimap-window-location (quote right))
 '(nrepl-lein-command "lein2")
 '(nrepl-server-command "echo \"lein2 repl :headless\" | $SHELL -l")
 '(openwith-associations (quote (("\\.pdf\\'" "evince" (file)) ("\\.mp3\\'" "xmms" (file)) ("\\.\\(?:mpe?g\\|avi\\|wmv\\)\\'" "mplayer" ("-idx" file)) ("\\.\\(?:jp?g\\|png\\)\\'" "feh" (file)) ("\\.odt\\'" "lowriter" (file)) ("\\.docx?\\'" "lowriter" (file)) ("\\.xlsx?\\'" "localc" (file)))))
 '(openwith-mode t)
 '(package-archives (quote (("gnu" . "http://elpa.gnu.org/packages/") ("marmalade" . "http://marmalade-repo.org/packages/") ("SC" . "http://joseito.republika.pl/sunrise-commander/") ("melpa" . "http://melpa.milkbox.net/packages/"))))
 '(pop-up-windows nil)
 '(recentf-auto-cleanup (quote never))
 '(recentf-max-saved-items 200)
 '(sr-attributes-display-mask (quote (nil nil nil nil t nil nil nil t)))
 '(sr-avfs-root "/avfs")
 '(sr-listing-switches "-alh")
 '(sr-show-file-attributes nil)
 '(yas/trigger-key "C-o"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "unknown" :family "Droid Sans Mono"))))
 '(minimap-active-region-background ((t (:background "#494949"))) t)
 '(sr-active-path-face ((t (:foreground "#00CCCC" :weight bold :height 120))))
 '(sr-passive-path-face ((t (:foreground "#008888" :weight bold :height 120)))))

(load-file "~/.emacs.d/installed-packages.el")
(mapc
 (lambda (package)
   (or (package-installed-p package)
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package))))
 unlogic-installed-packages)

(load-file "~/.emacs.d/bindings.el")

;; Start from userdir
(cd "~")

;; SLIME
;; (add-to-list 'load-path "~/.emacs.d/slime")
;; (require 'slime)
;; (slime-setup '(slime-fancy))

;; ;; Autocomplete-SLIME
;; (require 'ac-slime)
;; (add-hook 'slime-mode-hook 'set-up-slime-ac)
;; (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
;; (eval-after-load "auto-complete" '(add-to-list 'ac-modes 'slime-repl-mode))

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
(load "~/.emacs.d/sudo.el")
(require 'sudo)

;; Default SSH for tramp

(setq tramp-default-method "ssh")

;; Minimap

(defun minimap-toggle ()
  "Show minimap if hidden, hide if present."
  (interactive)
  (if (and (boundp 'minimap-bufname)
           minimap-bufname
           (get-buffer minimap-bufname)
           (get-buffer-window (get-buffer minimap-bufname)))
      (minimap-kill)
    (minimap-create)))

;; Autocomplete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
(setq ac-auto-start 4)

;; Auto whitespace
(global-whitespace-mode)

;; Auto undo-tree
(global-undo-tree-mode)

;; Initialize Stesla
(load-file "~/.emacs.d/stesla.el")

;; nREPL mode
(load-file "~/.emacs.d/nrepl.el/nrepl.el")
(setq nrepl-popup-stacktraces nil)

(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)

;; Openwith mode
(openwith-mode t)

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

;; Setup kibit for Clojure

;; Teach compile the syntax of the kibit output
(require 'compile)
(add-to-list 'compilation-error-regexp-alist-alist
             '(kibit "At \\([^:]+\\):\\([[:digit:]]+\\):" 1 2 nil 0))
(add-to-list 'compilation-error-regexp-alist 'kibit)

;; A convenient command to run "lein kibit" in the project to which
;; the current emacs buffer belongs to.
(defun kibit ()
  "Run kibit on the current project.
Display the results in a hyperlinked *compilation* buffer."
  (interactive)
  (compile "lein kibit"))

(defun left-meaningful-word ()
  (interactive)
  (let ((ip (point)))
    (while (string-match "[A-Za-z0-9\-_]" (string (char-before)))
      (backward-char))))

(defun right-meaningful-word ()
  (interactive)
  (let ((ip (point)))
    (while (string-match "[A-Za-z0-9\-_]" (string (char-after)))
      (forward-char))))

;; Occur word under the cursor
(defun occur-at-point ()
  (interactive)
  (let ((ip (point)))
    (forward-char)
    (left-meaningful-word)
    (let ((p (point)))
      (right-meaningful-word)
      (let ((pe (point)))
        (goto-char ip)
        (occur (filter-buffer-substring p pe) 1)))))

;; Open files with stupid cp1251

(defun stupid-encoding ()
  (interactive)
  (revert-buffer-with-coding-system 'cp1251 t))

;; count-words with Yegges

(defun count-words--message (str start end)
  (let* ((lines (count-lines start end))
         (words (count-words start end))
         (chars (- end start))
         (yegges (/ (+ (/ words 4000.0) (/ chars 25000.0)) 2)))
    (message "%s has %d line%s, %d word%s, %f Yegge%s and %d character%s."
	     str
	     lines (if (= lines 1) "" "s")
	     words (if (= words 1) "" "s")
             yegges (if (= yegges 1) "" "s")
	     chars (if (= chars 1) "" "s"))))

;; Pretty lambdas

(defun esk-pretty-fn ()
  (font-lock-add-keywords nil `(("(\\(comp\\>\\)"
                                 (0 (progn (compose-region (match-beginning 1)
                                                           (match-end 1)
                                                           ?ξ) nil)))))
  (font-lock-add-keywords nil `(("(\\(partial\\>\\)"
                                 (0 (progn (compose-region (match-beginning 1)
                                                           (match-end 1)
                                                           ?μ) nil)))))
  (font-lock-add-keywords nil `(("(\\(fn\\>\\)"
                                 (0 (progn (compose-region (match-beginning 1)
                                                           (match-end 1)
                                                           ?λ) nil))))))

(add-hook 'clojure-mode-hook 'esk-pretty-fn)

;; Configure sunrise
(defvar bookmark-counter 0)
(defvar bookmarks)

(setq bookmarks '("~/Dropbox/KPI/sem10/"
                  "~/work/uni/10sem/"))

(defun sr-cycle-bookmark ()
  (interactive)
  (let ((bookmark (nth bookmark-counter bookmarks)))
    (setq bookmark-counter (+ bookmark-counter 1))
    (if (>= bookmark-counter (length bookmarks))
        (setq bookmark-counter 0))
    (sr-goto-dir bookmark)))

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
    (ido-file-internal 'sr-dired 'sr-dired nil "Sunrise: " 'dir)))

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

;; Init YASnippet
(yas/load-directory "~/.emacs.d/snippets")
(load "~/.emacs.d/clojure-utils.el")

;; Projectile
(projectile-global-mode)

(require 'eproject)
;; Configure eproject
(define-project-type clojure (generic-git) (look-for "project.clj")
  :irrelevant-files ("target/" "bin/"))

;; Enable recentf
(require 'recentf)
(recentf-mode 1)

;; General config
(setq require-final-newline t)

(setq-default scroll-margin 1
              scroll-conservatively 0
              scroll-up-aggressively 0.01
              scroll-down-aggressively 0.01)

;; Configure main-line

(defun center-format (str c)
  (let* ((l (length str)))
    (if (< l c)
        (let ((p (/ (- c l) 2)))
          (concat (make-string p 32) str (make-string (- c p l) 32)))
      str)))
(/ 1 2.0)

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
(setq mainline-color1 "#444444")
(setq mainline-color2 "#222222")
(setq mainline-color3 "#293B3A")
;; 075E5D
(set-face-attribute 'mode-line nil
                    :background "#444444"
                    :box nil)
;; (propertize " %* " 'face '(:foreground "#ffffff" :background
;; "#293B3A"))

(defun get-interesting-minor-modes ()
  (replace-regexp-in-string
   " $" ""
   (replace-regexp-in-string
    "^ " ""
    (replace-regexp-in-string
     " +" " "
     (reduce (lambda (s mode)
               (replace-regexp-in-string mode "" s))
             '("Undo-Tree" "Projectile" "WS" "Fill" "hs")
             :initial-value
             (format-mode-line minor-mode-alist))))))

(setq-default mode-line-format
              '("%e" (:eval (concat
                             (mainline-rmw 'left mainline-color3)
                             (mainline-make 'left (center-format (buffer-name) 20) mainline-color3 mainline-color1)
                             (mainline-make 'left (my/percentage-from-top 4) mainline-color1)
                             (mainline-make (quote left) "(%4l : %3c)" mainline-color1 mainline-color2)
                             (mainline-make 'left mode-name mainline-color2)
                             (let* ((magic 25)
                                    (vcskip (if vc-mode
                                                (length vc-mode)
                                              -1))
                                    (mms (get-interesting-minor-modes))
                                    (skip (- (window-width) (length mms) (length mode-name)
                                             vcskip (max (length (buffer-name)) 20) magic 12)))
                               (concat
                                (mainline-make 'center (make-string skip 32) mainline-color2)
                                (mainline-make 'right mms mainline-color2)
                                (mainline-vc 'right mainline-color1 mainline-color2)))
                             (mainline-make 'right (or current-input-method-title "EN") mainline-color3 mainline-color1)
                             (mainline-make 'right "%z     " mainline-color3)
                             ))))

;; bs-show for mouse

(require 'bs)

(defun mouse-bs-show (event)
  "Pop up a menu of buffers for selection with the mouse.
This switches buffers in the window that you clicked on,
and selects that window."
  (interactive "e")
  (mouse-minibuffer-check event)
  (let ((buffers (bs-buffer-list))  alist menu split-by-major-mode sum-of-squares)
    ;; Make an alist of elements that look like (MENU-ITEM . BUFFER).
    (dolist (buf buffers)
      ;; Divide all buffers into buckets for various major modes.
      ;; Each bucket looks like (MODE NAMESTRING BUFFERS...).
      (with-current-buffer buf
        (let* ((adjusted-major-mode major-mode) elt)
          (dolist (group mouse-buffer-menu-mode-groups)
            (when (string-match (car group) (format-mode-line mode-name))
              (setq adjusted-major-mode (cdr group))))
          (setq elt (assoc adjusted-major-mode split-by-major-mode))
          (unless elt
            (setq elt (list adjusted-major-mode
                            (if (stringp adjusted-major-mode)
                                adjusted-major-mode
                              (format-mode-line mode-name nil nil buf)))
                  split-by-major-mode (cons elt split-by-major-mode)))
          (or (memq buf (cdr (cdr elt)))
              (setcdr (cdr elt) (cons buf (cdr (cdr elt))))))))
    ;; Compute the sum of squares of sizes of the major-mode buckets.
    (let ((tail split-by-major-mode))
      (setq sum-of-squares 0)
      (while tail
	(setq sum-of-squares
	      (+ sum-of-squares
		 (let ((len (length (cdr (cdr (car tail)))))) (* len len))))
	(setq tail (cdr tail))))
    (if (< (* sum-of-squares mouse-buffer-menu-mode-mult)
	   (* (length buffers) (length buffers)))
	;; Subdividing by major modes really helps, so let's do it.
	(let (subdivided-menus (buffers-left (length buffers)))
	  ;; Sort the list to put the most popular major modes first.
	  (setq split-by-major-mode
		(sort split-by-major-mode
		      (function (lambda (elt1 elt2)
				  (> (length elt1) (length elt2))))))
	  ;; Make a separate submenu for each major mode
	  ;; that has more than one buffer,
	  ;; unless all the remaining buffers are less than 1/10 of them.
	  (while (and split-by-major-mode
		      (and (> (length (car split-by-major-mode)) 3)
			   (> (* buffers-left 10) (length buffers))))
	    (let ((this-mode-list (mouse-buffer-menu-alist
				   (cdr (cdr (car split-by-major-mode))))))
	      (and this-mode-list
		   (setq subdivided-menus
			 (cons (cons
				(nth 1 (car split-by-major-mode))
				this-mode-list)
			       subdivided-menus))))
	    (setq buffers-left
		  (- buffers-left (length (cdr (car split-by-major-mode)))))
	    (setq split-by-major-mode (cdr split-by-major-mode)))
	  ;; If any major modes are left over,
	  ;; make a single submenu for them.
	  (if split-by-major-mode
	      (let ((others-list
		     (mouse-buffer-menu-alist
		      ;; we don't need split-by-major-mode any more,
		      ;; so we can ditch it with nconc.
		      (apply 'nconc (mapcar 'cddr split-by-major-mode)))))
		(and others-list
		     (setq subdivided-menus
			   (cons (cons "Others" others-list)
				 subdivided-menus)))))
	  (setq menu (cons "Buffer Menu" (nreverse subdivided-menus))))
      (progn
	(setq alist (mouse-buffer-menu-alist buffers))
	(setq menu (cons "Buffer Menu"
			 (mouse-buffer-menu-split "Select Buffer" alist)))))
    (let ((buf (x-popup-menu event menu))
	  (window (posn-window (event-start event))))
      (when buf
	(select-window
	 (if (framep window) (frame-selected-window window)
	   window))
	(switch-to-buffer buf)))))
(put 'ido-exit-minibuffer 'disabled nil)

(setq backup-by-copying t      ; don't clobber symlinks
      backup-directory-alist
      '(("." . "~/.local/share/emacs-saves"))    ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)       ; use versioned backups

(defun indent-buffer ()
  "indent whole buffer"
  (interactive)
  (indent-region (point-min) (point-max) nil))

(require 'saveplace)
(setq-default save-place t)

(require 'legalese)

(require 'multiple-cursors)

(global-linum-mode t)
(require 'linum-relative)
(setq lua-indent-level 4)

(require 'key-chord)
(key-chord-mode 1)
(key-chord-define-global "';" 'goto-last-change)

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
                                                     plain-tex-mode))
                (let ((mark-even-if-inactive transient-mark-mode))
                  (indent-region (region-beginning) (region-end) nil))))))

;; Configure helm
(defun helm-do-projectile-grep ()
  (interactive)
  (helm-do-grep-1 (list (projectile-project-p))
                  t nil '("*.*")))

;; Configure magit
;;; Add magit-gh-pulls plugin
(load "~/.emacs.d/magit-gh-pulls.el")
(add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)

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
