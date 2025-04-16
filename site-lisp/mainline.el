;;; mainline.el --- modeline replacement forked from powerline.el, much simplified

(defface mainline-face1
  `((t :foreground "black" :background "#123550" :box nil))
  "")
(defface mainline-face2
  `((t :foreground "black" :background "#112230" :box nil))
  "")
(defface mainline-face3
  `((t :foreground "black" :background "#112230" :box nil))
  "")

(defun mainline-make (string plface)
  (if (or (not string) (string= string ""))
      ""
    (propertize (concat " " string) 'face plface)))

(defun mainline-major-mode (face)
  (mainline-make
   (propertize (cond ((stringp mode-name) mode-name)
                     ((listp mode-name) (if (listp (car mode-name))
                                            (cadar mode-name)
                                          (car mode-name)))
                     (t "SGML"))
               'help-echo "Major mode\n\ mouse-1: Display major mode menu\n\ mouse-2: Show help for major mode\n\ mouse-3: Toggle minor modes"
               'local-map (let ((map (make-sparse-keymap)))
                            (define-key map [mode-line down-mouse-1]
                                        `(menu-item ,(purecopy "Menu Bar") ignore
                                                    :filter (lambda (_) (mouse-menu-major-mode-map))))
                            (define-key map [mode-line mouse-2] 'describe-mode)
                            (define-key map [mode-line down-mouse-3] mode-line-mode-menu)
                            map))
   face))

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
                                            ((> p 99) "Bot")
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
                  (mms (format-mode-line minor-mode-alist))
                  (mms (if (> (length mms) 0) (substring mms 1) mms))
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
              (mainline-make current-input-method-title 'mainline-face3)
              (mainline-make "%*" 'mainline-face3)
              (mainline-make real-buffer-name 'mainline-face3)
              (mainline-make (mainline-percentage 4) 'mainline-face1)
              (mainline-make "(%4l : %3c) " 'mainline-face1)
              (mainline-major-mode 'mainline-face2)
              (mainline-make (spinner-print spinner-current) 'mainline-face2)
              (mainline-make "﻿" 'mainline-face2)
              (if compact? ""
                (mainline-make mms 'mainline-face1))))))))

;; (mainline-activate)

(provide 'mainline)
