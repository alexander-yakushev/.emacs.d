;;; mainline.el --- modeline replacement forked from powerline.el

;; Author: Jason Milkins
;; Version: 1.0.2
;; Keywords: statusline / modeline

;;; Add a require to .emacs (or install from elpa/marmalade which will
;;; setup autoloads)
;;;
;;;     (require 'mainline)
;;;
;;; You can customize the separator graphic by setting the custom variable
;;;
;;;     mainline-arrow-shape
;;;
;;; possible values:
;;;
;;;     arrow
;;;     slant
;;;     curve

(require 'projectile)

(defcustom mainline-color1 "#123550"
  "Mainline color 1 info blocks background")

(defcustom mainline-color2 "#112230"
  "Mainline color 2 vcs info middle block background")

(defcustom mainline-arrow-shape 'arrow
  "Mainline graphic shape")

(set-face-attribute 'mode-line nil
                    :box nil)

(set-face-attribute 'mode-line-inactive nil
                    :box nil)

(scroll-bar-mode -1)

(defun slant-left-xpm
  (color1 color2)
  "Return an XPM left slant string representing."
  (create-image
   (format "/* XPM */
static char * slant_left[] = {
\"12 18 2 1\",
\"@ c %s\",
\"  c %s\",
\"@@@@         \",
\"@@@@         \",
\"@@@@@        \",
\"@@@@@        \",
\"@@@@@@       \",
\"@@@@@@       \",
\"@@@@@@@      \",
\"@@@@@@@      \",
\"@@@@@@@@     \",
\"@@@@@@@@     \",
\"@@@@@@@@@    \",
\"@@@@@@@@@    \",
\"@@@@@@@@@@   \",
\"@@@@@@@@@@   \",
\"@@@@@@@@@@@  \",
\"@@@@@@@@@@@  \",
\"@@@@@@@@@@@@ \",
\"@@@@@@@@@@@@\"};"
           (if color1 color1 "None")
           (if color2 color2 "None"))
   'xpm t :ascent 'center))

(defun slant-right-xpm
  (color1 color2)
  "Return an XPM right slant string representing@"
  (create-image
   (format "/* XPM */
static char * slant_right[] = {
\"12 18 2 1\",
\"@ c %s\",
\"  c %s\",
\"        @@@@\",
\"        @@@@\",
\"       @@@@@\",
\"       @@@@@\",
\"      @@@@@@\",
\"      @@@@@@\",
\"     @@@@@@@\",
\"     @@@@@@@\",
\"    @@@@@@@@\",
\"    @@@@@@@@\",
\"   @@@@@@@@@\",
\"   @@@@@@@@@\",
\"  @@@@@@@@@@\",
\"  @@@@@@@@@@\",
\" @@@@@@@@@@@\",
\" @@@@@@@@@@@\",
\"@@@@@@@@@@@@\",
\"@@@@@@@@@@@@\"};"
           (if color2 color2 "None")
           (if color1 color1 "None"))
   'xpm t :ascent 'center))

(defun arrow-left-xpm
  (color1 color2)
  "Return an XPM left arrow string representing@"
  (create-image
   (format "/* XPM */
static char * arrow_left[] = {
\"12 18 2 1\",
\"@ c %s\",
\"  c %s\",
\"@           \",
\"@@          \",
\"@@@         \",
\"@@@@        \",
\"@@@@@       \",
\"@@@@@@      \",
\"@@@@@@@     \",
\"@@@@@@@@    \",
\"@@@@@@@@@   \",
\"@@@@@@@@@   \",
\"@@@@@@@@    \",
\"@@@@@@@     \",
\"@@@@@@      \",
\"@@@@@       \",
\"@@@@        \",
\"@@@         \",
\"@@          \",
\"@           \"};"
           (if color1 color1 "None")
           (if color2 color2 "None"))
   'xpm t :ascent 'center))

(defun arrow-right-xpm
  (color1 color2)
  "Return an XPM right arrow string representing@"
  (create-image
   (format "/* XPM */
static char * arrow_right[] = {
\"12 18 2 1\",
\"@ c %s\",
\"  c %s\",
\"           @\",
\"          @@\",
\"         @@@\",
\"        @@@@\",
\"       @@@@@\",
\"      @@@@@@\",
\"     @@@@@@@\",
\"    @@@@@@@@\",
\"   @@@@@@@@@\",
\"   @@@@@@@@@\",
\"    @@@@@@@@\",
\"     @@@@@@@\",
\"      @@@@@@\",
\"       @@@@@\",
\"        @@@@\",
\"         @@@\",
\"          @@\",
\"           @\"};"
           (if color2 color2 "None")
           (if color1 color1 "None"))
   'xpm t :ascent 'center))

(defun curve-right-xpm
  (color1 color2)
  "Return an XPM right curve string representing@"
  (create-image
   (format "/* XPM */
static char * curve_right[] = {
\"12 18 2 1\",
\"@ c %s\",
\"  c %s\",
\"           @\",
\"         @@@\",
\"         @@@\",
\"       @@@@@\",
\"       @@@@@\",
\"       @@@@@\",
\"      @@@@@@\",
\"      @@@@@@\",
\"      @@@@@@\",
\"      @@@@@@\",
\"      @@@@@@\",
\"      @@@@@@\",
\"       @@@@@\",
\"       @@@@@\",
\"       @@@@@\",
\"         @@@\",
\"         @@@\",
\"           @\"};"
           (if color2 color2 "None")
           (if color1 color1 "None"))
   'xpm t :ascent 'center))

(defun curve-left-xpm
  (color1 color2)
  "Return an XPM left curve string representing@"
  (create-image
   (format "/* XPM */
static char * curve_left[] = {
\"12 18 2 1\",
\"@ c %s\",
\"  c %s\",
\"@           \",
\"@@@         \",
\"@@@         \",
\"@@@@@       \",
\"@@@@@       \",
\"@@@@@       \",
\"@@@@@@      \",
\"@@@@@@      \",
\"@@@@@@      \",
\"@@@@@@      \",
\"@@@@@@      \",
\"@@@@@@      \",
\"@@@@@       \",
\"@@@@@       \",
\"@@@@@       \",
\"@@@         \",
\"@@@         \",
\"@           \"};"
           (if color1 color1 "None")
           (if color2 color2 "None"))
   'xpm t :ascent 'center))

;; from memoize.el @ http://nullprogram.com/blog/2010/07/26/
(defun memoize (func)
  "Memoize the given function. If argument is a symbol then
install the memoized function over the original function."
  (typecase func
    (symbol (fset func (memoize-wrap (symbol-function func))) func)
    (function (memoize-wrap func))))

(defun memoize-wrap (func)
  "Return the memoized version of the given function."
  (let ((table-sym (gensym))
        (val-sym (gensym))
        (args-sym (gensym)))
    (set table-sym (make-hash-table :test 'equal))
    `(lambda (&rest ,args-sym)
       ,(concat (documentation func) "\n(memoized function)")
       (let ((,val-sym (gethash ,args-sym ,table-sym)))
         (if ,val-sym
             ,val-sym
           (puthash ,args-sym (apply ,func ,args-sym) ,table-sym))))))

(memoize 'slant-left-xpm)
(memoize 'slant-right-xpm)
(memoize 'arrow-left-xpm)
(memoize 'arrow-right-xpm)
(memoize 'curve-left-xpm)
(memoize 'curve-right-xpm)

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

(defun mainline-make-left
  (string color1 &optional color2 localmap)
  (let ((plface (mainline-make-face color1))
        (arrow  (and color2 (not (string= color1 color2)))))
    (concat
     (if (or (not string) (string= string ""))
         ""
       (propertize " " 'face plface))
     (if string
         (if localmap
             (propertize string 'face plface 'mouse-face plface 'local-map localmap)
           (propertize string 'face plface))
       "")
     (if arrow
         (propertize " " 'face plface)
       "")
     (if arrow
         (propertize " " 'display
                     (cond ((eq mainline-arrow-shape 'arrow)
                            (arrow-left-xpm color1 color2))
                           ((eq mainline-arrow-shape 'slant)
                            (slant-left-xpm color1 color2))
                           ((eq mainline-arrow-shape 'curve)
                            (curve-left-xpm color1 color2))
                           (t
                            (arrow-left-xpm color1 color2)))
                     'local-map (make-mode-line-mouse-map
                                 'mouse-1 (lambda () (interactive)
                                            (setq mainline-arrow-shape
                                                  (cond ((eq mainline-arrow-shape 'arrow)       'slant)
                                                        ((eq mainline-arrow-shape 'slant)       'curve)
                                                        ((eq mainline-arrow-shape 'curve)       'arrow)
                                                        (t                                       'arrow)))
                                            (redraw-modeline))))
       ""))))

(defun mainline-make-right
  (string color2 &optional color1 localmap)
  (let ((plface (mainline-make-face color2))
        (arrow  (and color1 (not (string= color1 color2)))))
    (concat
     (if arrow
         (propertize " " 'display
                     (cond ((eq mainline-arrow-shape 'arrow)
                            (arrow-right-xpm color1 color2))
                           ((eq mainline-arrow-shape 'slant)
                            (slant-right-xpm color1 color2))
                           ((eq mainline-arrow-shape 'rounded)
                            (rounded-xpm color1 color2))
                           ((eq mainline-arrow-shape 'curve)
                            (curve-right-xpm color1 color2))
                           (t
                            (arrow-right-xpm color1 color2)))
                     'local-map (make-mode-line-mouse-map
                                 'mouse-1 (lambda () (interactive)
                                            (setq mainline-arrow-shape
                                                  (cond ((eq mainline-arrow-shape 'arrow)       'slant)
                                                        ((eq mainline-arrow-shape 'slant)       'curve)
                                                        ((eq mainline-arrow-shape 'curve)       'arrow)
                                                        (t                                      'arrow)))
                                            (redraw-modeline))))
       "")
     (if arrow
         (propertize " " 'face plface)
       "")
     (if string
         (if localmap
             (propertize string 'face plface 'mouse-face plface 'local-map localmap)
           (propertize string 'face plface))
       "")
     (if (or (not string) (string= string ""))
         ""
       (propertize " " 'face plface)))))

(defun mainline-make-text
  (string color &optional fg localmap)
  (let ((plface (mainline-make-face color)))
    (if string
        (if localmap
            (propertize string 'face plface 'mouse-face plface 'local-map localmap)
          (propertize string 'face plface))
      "")))

(defun mainline-make (side string color1 &optional color2 localmap)
  (cond ((and (eq side 'right) color2) (mainline-make-right  string color1 color2 localmap))
        ((and (eq side 'left) color2)  (mainline-make-left   string color1 color2 localmap))
        ((eq side 'left)               (mainline-make-left   string color1 color1 localmap))
        ((eq side 'right)              (mainline-make-right  string color1 color1 localmap))
        (t                             (mainline-make-text   string color1 localmap))))

(defmacro defmainline (name string)
  `(defun ,(intern (concat "mainline-" (symbol-name name)))
     (side color1 &optional color2)
     (mainline-make side
                     ,string
                     color1 color2)))

(defmainline major-mode
  (propertize mode-name
              'help-echo "Major mode\n\ mouse-1: Display major mode menu\n\ mouse-2: Show help for major mode\n\ mouse-3: Toggle minor modes"
              'local-map (let ((map (make-sparse-keymap)))
                           (define-key map [mode-line down-mouse-1]
                             `(menu-item ,(purecopy "Menu Bar") ignore
                                         :filter (lambda (_) (mouse-menu-major-mode-map))))
                           (define-key map [mode-line mouse-2] 'describe-mode)
                           (define-key map [mode-line down-mouse-3] mode-line-mode-menu)
                           map)))

(defun mainline-interesting-minor-modes ()
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

(defun mainline-get-project-and-branch ()
  (condition-case err
      (let ((pn (and (projectile-project-p) (projectile-project-name))))
        (cond ((and vc-mode pn) (concat pn ":" (replace-regexp-in-string ".+[:-]" "" vc-mode)))
              (vc-mode (replace-regexp-in-string ".+[:-]" "" vc-mode))
              (pn pn)))
    (error "")))

(defun mainline-activate ()
  (setq-default
   mode-line-format
   '("%e" (:eval
           (let* ((classic-bn-length 20)
                  (ww (window-width))
                  (full-buffer-name (mainline-center-format (buffer-name) classic-bn-length))
                  (position-length 16)
                  (vc (mainline-get-project-and-branch))
                  (vc-length (if vc (length vc) -1))
                  (mms (mainline-interesting-minor-modes))
                  (mms-length (if (> (length mms) 0) (length mms) -1))
                  (total-length (+ 3 (length full-buffer-name) 3 position-length 3
                                   (length mode-name)
                                   mms-length 3 vc-length 3 2))
                  (compact? (< ww total-length))
                  (space-for-buffer-name (+ (length full-buffer-name)
                                            (- ww (- total-length mms-length
                                                     vc-length))))
                  (real-buffer-name (if compact?
                                        (mainline-center-format (mainline-trimmed-buffer-name (buffer-name) space-for-buffer-name)
                                                       (min classic-bn-length space-for-buffer-name))
                                      full-buffer-name))
                  (total-length (if compact?
                                    (+ (- total-length mms-length
                                          vc-length 3 (length full-buffer-name))
                                       (length real-buffer-name))
                                  total-length))
                  (skip-space (- ww total-length)))
             (concat
              (mainline-make 'left "%*" mainline-color3)
              (mainline-make 'left real-buffer-name mainline-color3 mainline-color1)
              (mainline-make 'left (mainline-percentage 3) mainline-color1)
              (mainline-make 'left "(%4l : %3c)" mainline-color1 mainline-color2)
              (mainline-major-mode 'left mainline-color2)
              (mainline-make 'center (make-string skip-space 32) mainline-color2)
              (if compact?
                  (mainline-make 'center " " mainline-color2)
                (concat
                 (mainline-make 'right mms mainline-color2)
                 (mainline-make 'right vc mainline-color1 mainline-color2)))
              (mainline-make 'right (concat (or current-input-method-title "EN") " ")
                             mainline-color3 (if compact?
                                                 mainline-color2
                                               mainline-color1))))))))

(provide 'mainline)
