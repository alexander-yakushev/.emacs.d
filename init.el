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
 '(LaTeX-command-style (quote (("" "%(PDF)%(latex) -file-line-error %S%(PDFout)"))))
 '(ac-auto-show-menu 0.0)
 '(ac-auto-start nil)
 '(ac-quick-help-delay 0.5)
 '(ac-trigger-key "TAB")
 '(ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector (vector "#c5c8c6" "#cc6666" "#b5bd68" "#f0c674" "#81a2be" "#b294bb" "#8abeb7" "#1d1f21"))
 '(bibtex-dialect (quote biblatex))
 '(clean-buffer-list-delay-general 1)
 '(clojure-defun-indents (quote (on-ui\ set-content-view!\ let-mutable)))
 '(create-lockfiles nil)
 '(custom-enabled-themes (quote (sanityinc-tomorrow-eighties)))
 '(custom-safe-themes (quote ("bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "c5207e7b8cc960e08818b95c4b9a0c870d91db3eaf5959dd4eba09098b7f232b" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" default)))
 '(default-input-method "ukrainian-computer")
 '(dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\..+")
 '(eldoc-idle-delay 1)
 '(fci-rule-color "#282a2e")
 '(fill-column 80)
 '(find-directory-functions (quote (cvs-dired-noselect dired-noselect sr-dired)))
 '(flymake-start-syntax-check-on-newline nil)
 '(font-lock-maximum-decoration (quote ((dired-mode) (sunrise) (t . t))))
 '(global-whitespace-mode t)
 '(inferior-lisp-program "sbcl")
 '(ispell-program-name "/usr/bin/aspell")
 '(latex-block-names (quote ("dmath" "lstlisting")))
 '(ls-lisp-verbosity (quote (links uid gid)))
 '(magit-time-format-string "%b %d, %Y %T")
 '(midnight-mode t nil (midnight))
 '(minimap-width-fraction 0.1)
 '(minimap-window-location (quote right))
 '(openwith-associations (quote (("\\.pdf\\'" "evince" (file)) ("\\.mp3\\'" "xmms" (file)) ("\\.\\(?:mpe?g\\|avi\\|wmv\\)\\'" "mplayer" ("-idx" file)) ("\\.\\(?:jpe?g\\|png\\)\\'" "feh" (file)) ("\\.odt\\'" "lowriter" (file)) ("\\.docx?\\'" "lowriter" (file)) ("\\.xlsx?\\'" "localc" (file)) ("\\.svg\\'" "display" (file)))))
 '(openwith-mode t)
 '(org-directory "~/Documents/Notes/")
 '(org-mobile-directory "~/ownCloud/Notes")
 '(org-mobile-files (quote (org-agenda-files "~/Documents/Notes/gsoc.org" "~/Documents/Notes/life.org")))
 '(org-mobile-inbox-for-pull "~/Documents/Notes/from-mobile.org")
 '(package-archives (quote (("gnu" . "http://elpa.gnu.org/packages/") ("marmalade" . "http://marmalade-repo.org/packages/") ("SC" . "http://joseito.republika.pl/sunrise-commander/") ("melpa" . "http://melpa.milkbox.net/packages/"))))
 '(persp-auto-save-opt 0)
 '(persp-auto-save-persps-to-their-file nil)
 '(pop-up-windows nil)
 '(projectile-enable-caching nil)
 '(recentf-auto-cleanup (quote never))
 '(recentf-max-saved-items 20000)
 '(refheap-token "39bee1d0-8aa8-4168-ba05-24627adb1c9b")
 '(refheap-user "alexyakushev")
 '(sr-attributes-display-mask (quote (nil nil nil nil t nil nil nil t)))
 '(sr-avfs-root "/avfs")
 '(sr-listing-switches "-alh")
 '(sr-show-file-attributes nil)
 '(vc-annotate-background nil)
 '(vc-annotate-color-map (quote ((20 . "#cc6666") (40 . "#de935f") (60 . "#f0c674") (80 . "#b5bd68") (100 . "#8abeb7") (120 . "#81a2be") (140 . "#b294bb") (160 . "#cc6666") (180 . "#de935f") (200 . "#f0c674") (220 . "#b5bd68") (240 . "#8abeb7") (260 . "#81a2be") (280 . "#b294bb") (300 . "#cc6666") (320 . "#de935f") (340 . "#f0c674") (360 . "#b5bd68"))))
 '(vc-annotate-very-old-color nil)
 '(warning-suppress-types (quote ((undo discard-info))))
 '(yas/trigger-key "C-o"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 144 :width normal :foundry "unknown" :family "Droid Sans Mono"))))
 '(minimap-active-region-background ((t (:background "#494949"))))
 '(preview-reference-face ((t (:foreground "#00CCCC" :background "#CCCCCC"))) t)
 '(sr-active-path-face ((t (:foreground "#00CCCC" :weight bold :height 120))))
 '(sr-passive-path-face ((t (:foreground "#008888" :weight bold :height 120)))))

(load-file "~/.emacs.d/installed-packages.el")
(mapc
 (lambda (package)
   (or (package-installed-p package)
       (package-install package)))
 unlogic-installed-packages)

(load-file "~/.emacs.d/bindings.el")

;; Start from userdir
(cd "~")

;; Autocomplete-nrepl-compliment

;; (load "~/.emacs.d/ac-nrepl-compliment/ac-cider-compliment.el")

;; (require 'ac-nrepl)
;; (add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
;; (add-hook 'cider-mode-hook 'ac-nrepl-setup)
;; (eval-after-load "auto-complete"
;;   '(add-to-list 'ac-modes 'cider-repl-mode))

(require 'ac-cider-compliment)
(add-hook 'cider-mode-hook 'ac-flyspell-workaround)
(add-hook 'cider-mode-hook 'ac-cider-compliment-setup)
;; (add-hook 'cider-interaction-mode-hook 'ac-cider-compliment-repl-setup)
(add-hook 'cider-repl-mode-hook 'ac-cider-compliment-repl-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'cider-mode))

(defun set-auto-complete-as-completion-at-point-function ()
  (setq completion-at-point-functions '(auto-complete)))

(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'cider-mode-hook 'set-auto-complete-as-completion-at-point-function)
(add-hook 'cider-repl-mode-hook 'set-auto-complete-as-completion-at-point-function)

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

;; Auto undo-tree
(global-undo-tree-mode)

;; Initialize Stesla
(load-file "~/.emacs.d/stesla.el")

;; CIDER mode
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

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
  (occur (thing-at-point 'symbol) 1))

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

(add-hook 'clojure-mode-hook 'esk-pretty-fn)

(put-clojure-indent 'let-mutable 'defun)
(put-clojure-indent 'set-content-view! 'defun)
(put-clojure-indent 'fact 'defun)
(put-clojure-indent 'facts 'defun)

;; Configure sunrise
(require 'sunrise-commander)
(defvar bookmark-counter 0)
(defvar bookmarks)

(setq bookmarks '("~/work/uni/12sem/"))

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
    (ido-file-internal 'read-only 'sr-advertised-find-file nil "Sunrise: " 'dir)))

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

;; Init YASnippet
(yas/load-directory "~/.emacs.d/snippets")
(load "~/.emacs.d/clojure-utils.el")

;; Projectile
(projectile-global-mode)

;; Enable recentf
(require 'recentf)
(recentf-mode 1)

;; General config
(setq-default require-final-newline t)

(setq-default redisplay-dont-pause t
              scroll-margin 1
              scroll-step 1
              scroll-conservatively 10000
              scroll-preserve-screen-position 1)

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
  (let ((mms (format-mode-line minor-mode-alist)))
    (propertize
     (replace-regexp-in-string
      " $" ""
      (replace-regexp-in-string
       " +" " "
       (reduce (lambda (s mode)
                 (replace-regexp-in-string mode "" s))
               '("Undo-Tree" "Projectile" "WS" "Fill" "hs"
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
  (let ((pn (and (projectile-project-p) (projectile-project-name))))
    (cond ((and vc-mode pn) (concat pn ":" (replace-regexp-in-string ".+[:-]" "" vc-mode)))
          (vc-mode (replace-regexp-in-string ".+[:-]" "" vc-mode))
          (pn pn))))

(defun org-clock-working-on-project ()
  (org-propertize
   (concat (if org-clock-mode-line-timer "+" "-") org-clock-heading)
   'local-map org-clock-mode-line-map
   'mouse-face 'mode-line-highlight))

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
(setq lua-indent-level 3)

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
                                                     plain-tex-mode  lua-mode))
                (let ((mark-even-if-inactive transient-mark-mode))
                  (indent-region (region-beginning) (region-end) nil))))))

;; Configure helm

;; Configure magit
;;; Add magit-gh-pulls plugin
(load "~/.emacs.d/magit-gh-pulls/magit-gh-pulls.el")
(add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)

(defun rev-at-line ()
  (vc-annotate-extract-revision-at-line))

(defun vc-annotate-show-commit-at-line ()
  (interactive)
  (let* ((rev (car (rev-at-line)))
         (rev (if (string= (substring rev 0 1) "^")
                  (substring rev 1)
                rev)))
    (magit-show-commit rev)))

(defun magit-read-tag (prompt &optional require-match)
  (magit-completing-read prompt nil nil
                         require-match nil 'magit-read-rev-history))

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

(load "~/.emacs.d/smali-mode.el")
(require 'smali-mode)

(add-hook 'ediff-startup-hook (lambda () (ediff-toggle-split)))

(defun org-insert-codeblock ()
  (interactive)
  (insert "#+begin_src clojure\n\n#+end_src")
  (previous-line))

;; TeX editing

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(setq-default TeX-engine 'xetex)
(setq-default TeX-PDF-mode t)

;;; Texify everything

(require 'tex-buf)
(defun TeX-command-default (name)
  "Next TeX command to use. Most of the code is stolen from `TeX-command-query'."
  (cond ((if (string-equal name TeX-region)
             (TeX-check-files (concat name "." (TeX-output-extension))
                              (list name)
                              TeX-file-extensions)
           (TeX-save-document (TeX-master-file)))
         TeX-command-default)
        ((and (memq major-mode '(doctex-mode latex-mode))
              (TeX-check-files (concat name ".bbl")
                               (mapcar 'car
                                       (LaTeX-bibliography-list))
                               BibTeX-file-extensions))
         ;; We should check for bst files here as well.
         TeX-command-BibTeX)))

(defcustom TeX-texify-max-runs-same-command 5
  "Maximal run number of the same command"
  :type 'integer :group 'TeX-command)

(defun TeX-texify-sentinel (&optional proc sentinel)
  "Non-interactive! Call the standard-sentinel of the current LaTeX-process.
If there is still something left do do start the next latex-command."
  (set-buffer (process-buffer proc))
  (funcall TeX-texify-sentinel proc sentinel)
  (let ((case-fold-search nil))
    (when (string-match "\\(finished\\|exited\\)" sentinel)
      (set-buffer TeX-command-buffer)
      (unless (plist-get TeX-error-report-switches (intern (TeX-master-file)))
	(TeX-texify)))))

(defun TeX-texify ()
  "Get everything done."
  (interactive)
  (let ((nextCmd (TeX-command-default (TeX-master-file)))
	proc)
    (if (null nextCmd)
	(when  (called-interactively-p 'any)
	  (message "TeX-texify: Nothing to be done."))
      (TeX-command nextCmd 'TeX-master-file)
      (when (or (called-interactively-p 'any)
		(null (boundp 'TeX-texify-count-same-command))
		(null (boundp 'TeX-texify-last-command))
		(null (equal nextCmd TeX-texify-last-command)))
	(mapc 'make-local-variable '(TeX-texify-sentinel TeX-texify-count-same-command TeX-texify-last-command))
	(setq TeX-texify-count-same-command 1))
      (if (>= TeX-texify-count-same-command TeX-texify-max-runs-same-command)
	  (message "TeX-texify: Did %S already %d times. Don't want to do it anymore." TeX-texify-last-command TeX-texify-count-same-command)
	(setq TeX-texify-count-same-command (1+ TeX-texify-count-same-command))
	(setq TeX-texify-last-command nextCmd)
	(and (null (equal nextCmd TeX-command-Show))
	     (setq proc (get-buffer-process (current-buffer)))
	     (setq TeX-texify-sentinel (process-sentinel proc))
	     (set-process-sentinel proc 'TeX-texify-sentinel))))))

;;; Texify everything ends

(require 'flymake)

(defun flymake-get-tex-args (file-name)
  (list "chktex" (list "-q" "-v0" file-name)))

(add-hook 'LaTeX-mode-hook
          (lambda () (LaTeX-add-environments
                      `("lstlisting")
                      `("dmath"))
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

(require 'thesaurus)
(thesaurus-set-bhl-api-key-from-file "~/.bighugelabsapi.key")

;; (require 'persp-mode)
;; (persp-mode 1)

;; (define-globalized-minor-mode global-persp-mode
;;   persp-mode (lambda () (persp-mode 1)))
                                        ;(global-persp-mode)


(defmacro with-persp-buffer-list (&rest body)
  (let ((buffers (gensym)))
    `(let ((,buffers (safe-persp-buffers (get-frame-persp))))
       (flet ((buffer-list (&optional frame) ,buffers))
         ,@body))))

(global-set-key (kbd "C-x l") #'(lambda (arg)
                                  (interactive "P")
                                  (with-persp-buffer-list (bs-show arg))))

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


;; Javadev

(load "~/.emacs.d/javad.el")
(require 'javap-mode)

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

(add-hook 'find-file-hook 'javad-find-class)
