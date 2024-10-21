;;; -*- lexical-binding: t -*-

(setq rainbow-numbers-number-grouping-length 6)

(defun rainbow-numbers--get-number-of-groupings (start-pos end-pos)
  (let ((length (- end-pos start-pos)))
    (if (= (mod length 3) 0)
        (/ length 3)
      (+ 1 (/ length 3)))))

(defun rainbow-numbers--char-is-number (c)
  (and (not (null c))
       (>= c ?0)
       (<= c ?9)))

(defun rainbow-numbers--number-match-maybe-jump-to-number (limit)
  "Jump to number if one exists, unless already in a
number. Return non-nil if a number is found, otherwise return nil."
  (if (and (rainbow-numbers--char-is-number (char-before))
           (rainbow-numbers--char-is-number (char-after)))
      t
    (let ((word-found (re-search-forward
                       "\\b[0-9]\\{7,\\}\\b"
                       ;; "\\(^\\|[[:blank:]]\\)[0-9]\\{7,\\}\\($\\|[[:blank:]]\\|[\\.,]\\)"
                       limit
                       t)))
      (backward-word)
      word-found)))

(defun rainbow-numbers--get-nearest-k-grouping (start-pos end-pos k)
  "Return start and end of nearest k-parity grouping and move point
  to after this grouping."
  (let* ((grouping-num (rainbow-numbers--get-number-of-groupings start-pos end-pos))
         (out-of-bounds (rainbow-numbers--forward-k-groupings start-pos end-pos k grouping-num)))
    (if (null out-of-bounds)
        '()
      (cons (if (< (point) (+ start-pos 3))
                start-pos
              (- (point) 3))
            (point)))))

(defun rainbow-numbers--forward-k-groupings (start-pos end-pos k cur-grouping-num)
  "Goes to end of number if out of bounds.
Needs to order k-groupings from right to left. Currently doing the opposite.
Indexing issues here"
  (let* ((k2 (+ (mod (- cur-grouping-num 1 k) rainbow-numbers-number-grouping-length)
                1))
         (offset (mod (- end-pos start-pos) 3))
         (new-pos start-pos))
    (unless (= offset 0)
      (setq k2 (- k2 1))
      (setq new-pos (+ new-pos offset)))
    (let ((step-forward (+ new-pos (* k2 3))))
      (goto-char (min end-pos step-forward))
      (if (<= step-forward end-pos)
          t
        '()))))


(defun rainbow-numbers--set-match (start-pos end-pos)
  "This is the lazy way of setting the match."
  (save-excursion
    (goto-char start-pos)
    (re-search-forward (concat ".\\{"
                               (number-to-string (- end-pos start-pos))
                               "\\}"))))

(defun make-rainbow-numbers-number-match (k)
  (lambda (limit)
    (let ((keep-running t)
          (output '()))
      (while keep-running
        (let ((in-number (rainbow-numbers--number-match-maybe-jump-to-number limit)))
          (if (null in-number)
              (setq keep-running '())
            (let* ((end-pos (save-excursion
                              (forward-word)
                              (point)))
                   (start-pos (point))
                   (nearest-grouping (rainbow-numbers--get-nearest-k-grouping start-pos end-pos k)))
              (unless (null nearest-grouping)
                (setq keep-running '())
                (rainbow-numbers--set-match (car nearest-grouping)
                                            (cdr nearest-grouping))
                (setq output t))))))
      output)))

(defun rainbow-numbers-number-match-0 (limit)
  (funcall (make-rainbow-numbers-number-match 0) limit))

(defun rainbow-numbers-number-match-1 (limit)
  (funcall (make-rainbow-numbers-number-match 1) limit))

(defun rainbow-numbers-number-match-2 (limit)
  (funcall (make-rainbow-numbers-number-match 2) limit))

(defun rainbow-numbers-number-match-3 (limit)
  (funcall (make-rainbow-numbers-number-match 3) limit))

(defun rainbow-numbers-number-match-4 (limit)
  (funcall (make-rainbow-numbers-number-match 4) limit))

(defun rainbow-numbers-number-match-5 (limit)
  (funcall (make-rainbow-numbers-number-match 5) limit))

(setq rainbow-numbers--font-lock-keywords
      (list (cons (function rainbow-numbers-number-match-0)
                  'font-lock-builtin-face)
            (cons (function rainbow-numbers-number-match-1)
                  'font-lock-variable-name-face)
            (cons (function rainbow-numbers-number-match-2)
                  'font-lock-function-name-face)
            (cons (function rainbow-numbers-number-match-3)
                  'font-lock-preprocessor-face)
            (cons (function rainbow-numbers-number-match-4)
                  'font-lock-warning-face)
            (cons (function rainbow-numbers-number-match-5)
                  'font-lock-type-face)))

(define-minor-mode rainbow-numbers-mode
  "Highlight groupings within numbers to make long numbers easier to read."
  :init-value nil
  :lighter ""
  :keymap nil
  (font-lock-remove-keywords nil rainbow-numbers--font-lock-keywords)
  (when (boundp 'font-lock-flush)
    (font-lock-flush))
  (font-lock-fontify-buffer)
  (when rainbow-numbers-mode
    (font-lock-add-keywords nil
                            rainbow-numbers--font-lock-keywords)
    (when font-lock-mode
      (when (boundp 'font-lock-flush)
        (font-lock-flush))
      (with-no-warnings (font-lock-fontify-buffer)))))

(provide 'rainbow-numbers-mode)
