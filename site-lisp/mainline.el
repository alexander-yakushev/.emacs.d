;;; mainline.el --- modeline replacement forked from powerline.el, much simplified

(require 'projectile)

(defcustom mainline-color1 "#123550"
  "Mainline color 1 info blocks background")

(defcustom mainline-color2 "#112230"
  "Mainline color 2 vcs info middle block background")

(defun mainline-make-face
    (bg &optional fg)
  (if bg
      (let ((fg mainline-color-fg)
            (cface (intern (concat "mainline-"
                                   bg
                                   "-"
                                   (if fg
                                       (format "%s" fg)
                                     "white")))))
        (make-face cface)
        (if fg
            (if (eq fg 0)
                (set-face-attribute cface nil
                                    :background bg
                                    :box nil)
              (set-face-attribute cface nil
                                  :foreground fg
                                  :background bg
                                  :box nil))
          (set-face-attribute cface nil
                              :foreground "white"
                              :background bg
                              :box nil))
        cface)
    nil))

(defun mainline-make (string color1)
  (let ((plface (mainline-make-face color1)))
    (if (or (not string) (string= string ""))
        ""
      (propertize (concat " " string) 'face plface))))

(defmacro defmainline (name string)
  `(defun ,(intern (concat "mainline-" (symbol-name name)))
       (color1)
     (mainline-make ,string color1)))

(defmainline major-mode
  (propertize (cond ((stringp mode-name) mode-name)
                    ((listp mode-name) (car mode-name))
                    (t "SGML"))
              'help-echo "Major mode\n\ mouse-1: Display major mode menu\n\ mouse-2: Show help for major mode\n\ mouse-3: Toggle minor modes"
              'local-map (let ((map (make-sparse-keymap)))
                           (define-key map [mode-line down-mouse-1]
                             `(menu-item ,(purecopy "Menu Bar") ignore
                                         :filter (lambda (_) (mouse-menu-major-mode-map))))
                           (define-key map [mode-line mouse-2] 'describe-mode)
                           (define-key map [mode-line down-mouse-3] mode-line-mode-menu)
                           map)))

(defvar mms-cache (make-hash-table :test 'equal))


(defun mainline-interesting-minor-modes ()
  (let ((full-mode-line (format-mode-line minor-mode-alist)))
    (or (gethash full-mode-line mms-cache)
        (let ((mms (propertize
                    (format-mode-line
                     (remove-if (lambda (mm)
                                  (let ((mm-sym (car mm)))
                                    (or (eq mm-sym 'auto-fill-function)
                                        (eq mm-sym 'auto-revert-mode)
                                        (eq mm-sym 'global-whitespace-mode)
                                        (eq mm-sym 'projectile-mode)
                                        (eq mm-sym 'eldoc-mode)
                                        (eq mm-sym 'elisp-slime-nav-mode)
                                        (eq mm-sym 'git-gutter-mode)
                                        (eq mm-sym 'hi-lock-mode)
                                        (eq mm-sym 'wakatime-mode)
                                        (eq mm-sym 'yas-minor-mode)
                                        (eq mm-sym 'highlight-parentheses-mode)
                                        (eq mm-sym 'hs-minor-mode))))
                                minor-mode-alist)))))
          (puthash full-mode-line (if (> (length mms) 0)
                                      (substring mms 1)
                                    mms)
                   mms-cache)))))

(defun mainline-center-format (str c)
  (let* ((l (length str)))
    (if (< l c)
        (let ((p (/ (- c l) 2)))
          (concat (make-string p 32) str (make-string (- c p l) 32)))
      str)))

(defun mainline-percentage (padding)
  (let ((p (round (/ (* 100.0 (point)) (point-max)))))
    (replace-regexp-in-string "|" "%%"
                              (format (concat "%" (number-to-string padding) "s")
                                      (cond ((= p 0) "Top")
                                            ((> p 98) "Bot")
                                            (t (concat (number-to-string p) "|")))))))

(defun mainline-trimmed-buffer-name (bn n)
  (let* ((l (length bn)))
    (if (> l n)
        (concat ".." (substring bn (- l (- n 2))))
      bn)))

(defun mainline-activate ()
  (setq-default
   mode-line-format
   '("%e" (:eval
           (let* ((classic-bn-length 24)
                  (ww (window-width))
                  (full-buffer-name (mainline-center-format (buffer-name) classic-bn-length))
                  (position-length 16)
                  (mms (mainline-interesting-minor-modes))
                  (mms-length (if (> (length mms) 0) (length mms) -1))
                  (total-length (+ 3 (length full-buffer-name) 3 position-length 3
                                   (length mode-name)
                                   mms-length 2))
                  (compact? (< ww total-length))
                  (space-for-buffer-name (+ (length full-buffer-name)
                                            (- ww (- total-length mms-length))))
                  (real-buffer-name (if compact?
                                        (mainline-center-format (mainline-trimmed-buffer-name (buffer-name) space-for-buffer-name)
                                                                (min classic-bn-length space-for-buffer-name))
                                      full-buffer-name)))
             (concat
              (mainline-make current-input-method-title mainline-color3)
              (mainline-make "%*" mainline-color3)
              (mainline-make real-buffer-name mainline-color3)
              (mainline-make (mainline-percentage 4) mainline-color1)
              (mainline-make "(%4l : %3c) " mainline-color1)
              (mainline-major-mode mainline-color2)
              (mainline-make "﻿" mainline-color2)
              (if compact? ""
               (mainline-make mms mainline-color1))))))))
;; (mainline-activate)

(provide 'mainline)
