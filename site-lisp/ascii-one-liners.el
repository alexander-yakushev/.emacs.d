;;; magit-gh-pulls.el --- GitHub pull requests extension for Magit

;; Copyright (C) 2011-2015  Yann Hodique, Alexander Yakushev

;; Author: Yann Hodique <yann.hodique@gmail.com>
;; Keywords: ascii art
;; Version: 0.1.0
;; URL: https://github.com/sigma/magit-gh-pulls
;; Package-Requires: ((s "1.6.1") (dash "2.13.0"))

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This extension scrapes http://www.copypastatroll.com/one-line-ascii-art/ and
;; allows to paste one-line ASCII art by its name.

(defvar ascii-one-liners-site-url "http://www.copypastatroll.com/one-line-ascii-art/")

(defun ascii-one-liners-dom->alist (parsed-dom)
  (thread-last parsed-dom
    (assq 'html) (assq 'body)
    (assq 'div) (assq 'div) (assq 'div) (assq 'div)
    (assq 'article) (assq 'div)
    (assq 'table) (assq 'tbody) (assq 'tr) cddr
    (-mapcat (lambda (x) (when (listp x) (cddr x))))
    (-remove (lambda (x) (if (listp x)
                        (or (s-blank? (s-trim (caddr x))) (equal (caddr x) " "))
                      (or (s-blank? (s-trim x)) (equal x " ")))))
    (-partition 2)
    (-map (lambda (row) (destructuring-bind (key val) row
                     (cons (concat (s-trim (if (listp key) (caddr key) key))
                                   " | " (caddr val))
                           (caddr val)))))))

(defun ascii-one-liners-get-webpage ()
  (let ((url-request-method "GET"))
    (with-current-buffer (url-retrieve-synchronously ascii-one-liners-site-url)
      (dotimes (_ 13) (kill-whole-line))
      (libxml-parse-html-region (point-min) (point-max)))))

(defvar ascii-one-liners-saved-table nil)

(defun ascii-one-liners-load-table ()
  (setq ascii-one-liners-saved-table
        (ascii-one-liners-dom->alist (ascii-one-liners-get-webpage))))

(defun ascii-one-liners-insert ()
  (interactive)
  (unless ascii-one-liners-saved-table
    (ascii-one-liners-load-table))
  (let* ((choices (-map 'car ascii-one-liners-saved-table))
         (minibuffer-completion-table choices)
         (ido-max-prospects 10)
         (selected (ido-completing-read "One-liner: " choices nil nil
                                        nil 'extended-command-history (car choices))))
    (insert (cdr (assoc selected ascii-one-liners-saved-table)))))

(provide 'ascii-one-liners)
