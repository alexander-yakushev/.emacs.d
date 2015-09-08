(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector (vector "#c5c8c6" "#cc6666" "#b5bd68" "#f0c674" "#81a2be" "#b294bb" "#8abeb7" "#1d1f21"))
 '(clean-buffer-list-delay-general 1)
 '(create-lockfiles nil)
 '(debug-on-error nil)
 '(default-input-method "ukrainian-computer")
 '(fill-column 80)
 '(global-whitespace-mode t)
 '(package-archives (quote (("gnu" . "http://elpa.gnu.org/packages/") ("marmalade" . "http://marmalade-repo.org/packages/") ("melpa" . "http://melpa.milkbox.net/packages/"))))
 '(pop-up-windows nil)
 '(recentf-auto-cleanup (quote never))
 '(recentf-max-saved-items 20000)
 '(warning-suppress-types (quote ((undo discard-info)))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 110 :width normal :foundry "unknown" :family "Droid Sans Mono")))))

(defvar minimal-installed-packages
  '(
    dash
    goto-last-change
    highlight
    highlight-symbol
    idle-highlight-mode
    ido-ubiquitous
    key-chord
    s
    smex
    starter-kit
    starter-kit-bindings
    starter-kit-eshell
    ))

(mapc (lambda (package)
        (or (package-installed-p package)
            (package-install package)))
      minimal-installed-packages)

(require 'cl)
(require 'dash)

(defun define-keys (args)
  (lexical-let ((defkeys (lambda (map args)
                           (-map (lambda (x)
                                   (if map
                                       (define-key map (kbd (car x)) (cadr x))
                                     (global-set-key (kbd (car x)) (cadr x))))
                                 (-partition 2 args)))))
    (-map (lambda (modelist)
            (cond
             ((eq (car modelist) :global) (funcall defkeys nil (cdr modelist)))
             (t (lexical-let ((modelist modelist))
                  (if (stringp (caddr modelist))
                      (eval-after-load (caddr modelist)
                        `(-map (lambda (x)
                                 (define-key ,(cadr modelist) (kbd (car x)) (cadr x)))
                               (-partition 2 ',(cdddr modelist))))
                    (add-hook (caddr modelist)
                              (lambda ()
                                (funcall defkeys (eval (cadr modelist)) (cdddr modelist)))))))))
          (-partition-by-header #'keywordp args))))

(define-keys
  '(;;; Navigation
    :global
    "C-l" backward-char
    "C-'" forward-char
    "C-p" previous-line
    "C-;" next-line

    "M-l" backward-word
    "M-'" forward-word

    "C-M-'" forward-list
    "C-M-l" backward-list

    "C-q" move-beginning-of-line

    "M-q" highlight-symbol-prev
    "M-e" highlight-symbol-next
    "C-o" recenter-top-bottom

    ;;; Editing
    "C-w" backward-kill-word
    "M-d" kill-region
    "C-b" kill-whole-line
    "C-c C-q" join-line
    "C-=" comment-or-uncomment-region
    "C-f" fill-paragraph
    "<f6>" whitespace-cleanup
    "C-c C-r" eval-and-replace
    "M-\\" indent-region
    "C-c TAB" quoted-insert
    "M-c" just-one-space

    ;;; Buffer manipulation
    "C-x <C-return>" other-window
    "C-\\" kill-buffer-and-its-windows

    ;;; Miscellaneous
    "C-/" toggle-input-method
    "C-c M-r" query-replace
    "C-c d" ediff-opened-buffers

    ;; bs mode
    :global
    "C-x C-l" bs-show

    :local Buffer-menu-mode-map Buffer-menu-mode-hook
    ";" bs-down

    :local bs-mode-map bs-mode-hook
    ";" bs-down

    ;; Package mode
    :local package-menu-mode-map package-menu-mode-hook
    ";" next-line

    :local octave-mode-map octave-mode-hook
    "C-c C-c" octave-send-region
    "C-x C-e" octave-send-line
    ))

(cd "~")

(defun kill-buffer-and-its-windows ()
  "Kill BUFFER and delete its windows.  Default is `current-buffer'.
BUFFER may be either a buffer or its name (a string)."
  (interactive)
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

(setq ring-bell-function 'ignore)

(global-auto-revert-mode 1)

(require 'recentf)
(recentf-mode 1)

(setq-default require-final-newline t)

(setq-default redisplay-dont-pause t
              scroll-margin 1
              scroll-step 1
              scroll-conservatively 10000
              scroll-preserve-screen-position 1)

(put 'ido-exit-minibuffer 'disabled nil)

(setq backup-by-copying t      ; don't clobber symlinks
      backup-directory-alist
      '(("." . "~/.local/share/emacs-saves"))    ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(require 'ediff)
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
    (ediff-toggle-split)))
