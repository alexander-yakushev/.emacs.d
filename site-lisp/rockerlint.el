(require 'cl)
(require 's)
(require 'dash)

(defun rockerlint-tokenize-line (s)
  "Takes a line and returns a list of parsed tokens. If error
occurs, returns an error list."
  (let ((result ())
        (line s)
        (index 0))
    (block nil
      (while (and line (not (s-blank? line)))
        (let ((is-space (= (aref line 0) 32))
              (is-quote (= (aref line 0) ?\"))
              (next-space (string-match " " line))
              (next-quote (string-match "\"" line)))
          (cond        
           (is-space (let ((first-nonspace (string-match "[^ ]" line 1)))
                       (setf index (+ index first-nonspace))
                       (setf line (substring line first-nonspace))))
           (is-quote (let ((closing-quote (string-match "[^\\][\"]" line 1)))
                       (if closing-quote
                           (progn
                             (push (list :token (substring line 1 (+ closing-quote 1))
                                         :type :string
                                         :index index)
                                   result)
                             (setf line (substring line (+ closing-quote 2)))
                             (setf index (+ index (+ closing-quote 2))))
                         (return (list :error :unclosed-quote :index index)))))
           ((and next-space next-quote
                 (< next-quote next-space)) (return (list :error :missing-space :index (+ index next-quote))))
           (next-space (progn
                         (push (list :token (substring line 0 next-space)
                                     :type :normal
                                     :index index)
                               result)
                         (setf index (+ index (+ next-space 1)))
                         (setf line (substring line (+ next-space 1)))))
           (t (return (reverse (cons (list :token line
                                           :type :normal
                                           :index index) result))))))))))

(defun rockerlint-list-lines (file)
  (let ((real-lines (with-temp-buffer
                      (insert-file-contents file)
                      (split-string (buffer-string) "\n")))
        (logical-lines ())
        (index 0))
    (while real-lines
      (let* ((line (car real-lines))
             (line-trimmed (s-trim-left line)))
        (cond ((or (s-blank? line-trimmed)
                   (s-starts-with? "#" line-trimmed)) (progn (setf index (+ 1 index))
                                                             (setf real-lines (cdr real-lines))))
              ((s-ends-with? "\\" line) (progn (setf index (+ 1 index))
                                               (setf real-lines (cdr real-lines))
                                               (push (list line index) logical-lines)
                                               (while (s-ends-with? "\\" (car real-lines))
                                                 (setf index (+ 1 index))
                                                 (setf real-lines (cdr real-lines)))
                                               (setf index (+ 1 index))
                                               (setf real-lines (cdr real-lines))))
              (t (progn (setf index (+ 1 index))
                        (setf real-lines (cdr real-lines))
                        (push (list line index) logical-lines))))))
    (reverse logical-lines)))

(defun rockerlint-verify-legal-commands (token-list)
  "Takes a parsed token list and returns itself if it is alright,
  otherwise returns an error list."
  (block nil
    (let* ((first-token (car token-list))
           (ft-name (getf first-token :token))
           (ft-index (getf first-token :index)))
      (cond ((-some? (lambda (token) (string= (getf token :token) "{{")) token-list) (return token-list))
            ((string= ft-name "FROM") (if (not (= (length token-list) 2))
                                          (return (list :error :wrong-args-num :index ft-index :command "FROM" :exact 1))))
            ((string= ft-name "WORKDIR") (if (not (= (length token-list) 2))
                                             (return (list :error :wrong-args-num :index ft-index :command "WORKDIR" :exact 1))))
            ((string= ft-name "RUN") (if (< (length token-list) 2)
                                         (return (list :error :wrong-args-num :index ft-index :command "RUN" :min 1))))
            ((string= ft-name "CMD") (if (< (length token-list) 2)
                                         (return (list :error :wrong-args-num :index ft-index :command "CMD" :min 1))))
            ((string= ft-name "PUSH") (if (not (= (length token-list) 2))
                                          (return (list :error :wrong-args-num :index ft-index :command "PUSH" :exact 1))))
            ((string= ft-name "MOUNT") (if (not (= (length token-list) 2))
                                           (return (list :error :wrong-args-num :index ft-index :command "MOUNT" :exact 1))))
            ((string= ft-name "ADD") (if (< (length token-list) 2)
                                         (return (list :error :wrong-args-num :index ft-index :command "ADD" :min 1))))
            ((string= ft-name "IMPORT") (if (not (<= 2 (length token-list) 3))
                                            (return (list :error :wrong-args-num :index ft-index :command "IMPORT" :min 1 :max 2))))
            ((string= ft-name "EXPORT") (if (not (<= 2 (length token-list) 3))
                                            (return (list :error :wrong-args-num :index ft-index :command "EXPORT" :min 1 :max 2))))
            ((not (member ft-name
                          '("MAINTAINER" "EXPOSE" "ENTRYPOINT" "VOLUME"
                            "USER" "ONBUILD" "ATTACH" "ENV"))) (return (list :error :unknown-op :index ft-index :command ft-name)))
            (t token-list)))))

(defun rockerlint-lint (file)
  (let* ((lines (rockerlint-list-lines file))
         (tokenized (-map (lambda (l) (list (rockerlint-tokenize-line (car l)) (cadr l))) lines)))
    ;; tokenized
    (->> tokenized
         (-map (lambda (l)
                 (if (eql (caar l) :error)
                     l
                   (list (rockerlint-verify-legal-commands (car l)) (cadr l)))))
         (-filter (lambda (l) (eql (caar l) :error)))
         (-map (lambda (l)
                 (let ((err-type (getf (car l) :error))
                       (index 0 ;(getf (car l) :index)
                        )
                       (line (cadr l)))
                   (cond ((eql err-type :unclosed-quote) (message "%d:%d: Unmatched doublequote." line index))
                         ((eql err-type :missing-space) (message "%d:%d: Missing space between a token and doublequote." line index))
                         ((eql err-type :wrong-args-num) (cond ((getf (car l) :exact) (message "%d:%d: %s takes %d arguments(s)." line index
                                                                                               (getf (car l) :command) (getf (car l) :exact)))
                                                               ((getf (car l) :max) (message "%d:%d: %s takes %d-%d arguments." line index
                                                                                             (getf (car l) :command) (getf (car l) :min)
                                                                                             (getf (car l) :max)))
                                                               (t (message "%d:%d: %s takes at least %d argument(s)." line index
                                                                           (getf (car l) :command) (getf (car l) :min)))))
                         ((eql err-type :unknown-op) (message "%d:%d: %s command is not recognized." line index (getf (car l) :command)))
                         (t (message "%d:%d: Unknown error: %s." line index (car l))))))))))

(provide 'rockerlint)
