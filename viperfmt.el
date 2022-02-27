;;; viperfmt.el --- Foramtting helpers for Viper code. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Gavin Gray
;;
;; Author: Gavin Gray <https://github.com/gavingray>
;; Maintainer: Gavin Gray <gavinleroy6@gmail.com>
;; Created: February 24, 2022
;; Modified: February 24, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/gavingray/viperfmt
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

;;;###autoload
(defun viperfmt-reformat-buffer ()
  "Reformat the whole buffer."
  (interactive)
  (viperfmt-reformat-region (point-min)
                            (point-max)))

;;;###autoload
(defun viperfmt-reformat-region (beg end)
  "Reformat the region from BEG to END, accounting for indentation."
  (interactive "r")
  (let ((inhibit-read-only t))
    (if (= (save-excursion (goto-char beg)
                           (line-beginning-position))
           beg)
        (viperfmt-reformat-region-as-is beg end)
      (let* ((column (- beg (line-beginning-position)))
             (string (buffer-substring-no-properties beg end))
             (new-string (with-temp-buffer
                           (insert (make-string column ? ) string)
                           (viper-reformat-region-as-is (point-min)
                                                        (point-max))
                           (delete-region (point-min) (1+ column))
                           (buffer-substring (point-min)
                                             (point-max)))))
        (save-excursion
          (goto-char beg)
          (delete-region beg end)
          (insert new-string))))))

;; Internal functions

(defun viperfmt-reformat-region-as-is (beg end)
  "Reformat the given region from `BEG' to `END' as-is."
  (let ((buffer-contents (buffer-substring beg end)))
    (viperfmt-format-string buffer-contents)))

(defun viperfmt-format-string (str)
  "Reformat `STR' as a Viper program."
  (viperfmt-format-tokens (viperfmt-tokenize str)))

;; Various helper utilities

(defmacro incq (v) `(setq ,v (1+ ,v)))
(defmacro consq (c ls) `(setq ,ls (cons ,c ,ls)))
(defmacro concatq (ls a) `(setq ,ls (concat ,ls ,a)))
(defmacro inner (&rest body)
  `(progn (when (< 0 (length token))
            (consq token res)
            (setq token ""))
          ,@body))
(defun substringp (haystack needle)
  (string-match-p (regexp-quote needle) haystack))
(defmacro reset-if-eq (expected pair prdc)
  `(if (string= ,pair ,expected)
       (progn (consq token res)
              (setq token "")
              (consq ,pair res)
              (setq ,prdc nil))
     (concatq token curr)))

(defun whitespacep (c)
  (or (string= c " ") (string= c "\t")
      (string= c "\r") (string= c "\n")))

(defun get-indent (tab ind nxt)
  (let ((add 0))
    (when (or (string= nxt "requires")
              (string= nxt "ensures")
              (string= nxt "invariant"))
      (incq add))
    (make-string (+ ind add) tab)))

(defun get-index (a idx &optional v)
  "Get element at `IDX' from `A' or `V' if index doesn't exist."
  (if (and (<= 0 idx) (< idx (length a)))
      (char-to-string (aref a idx))
    v))

;; FIXME this is a terrible copy of https://github.com/viperproject/viper-ide/blob/master/client/src/ViperFormatter.ts
(defun viperfmt-tokenize (str)
  "Tokenize a Viper program `STR'."
  (let ((i 0)
        (token "")
        (s-max (length str))
        (line-comment nil)
        (multi-comment nil)
        (res '()))
    (while (<= i s-max)
      (let* ((curr (get-index str (- i 1) ""))
             (next (get-index str i ""))
             (next-next (get-index str (1+ i) ""))
             (peek-two (concat curr next))
             (peek-three (concat peek-two next-next)))
        (cond
         (line-comment
          (reset-if-eq "\n" curr line-comment))
         (multi-comment
          (reset-if-eq "*/" peek-two multi-comment))
         ((string= peek-two "//")
          (inner (consq "//" res)
                 (incq i)
                 (setq line-comment t)))
         ((string= peek-two "/*")
          (inner (consq "/*" res)
                 (incq i)
                 (setq multi-comment t)))
         ((string= peek-three "==>")
          (inner (consq peek-three res)
                 (setq i (+ i 2))))
         ((substringp "==:=>=<=!=" peek-two)
          (inner (consq peek-two res)
                 (incq i)))
         ((or (whitespacep curr) (substringp "()[]{}:,+-\\*><!" curr))
          (when (< 0 (length token))
            (consq token res)
            (setq token ""))
          (when (or (string= curr "\n")
                    (and (< 0 (length curr))
                         (substringp "()[]{}:,+-\\*>=<=!=" curr)))
            (consq curr res)))
         (t (concatq token curr)))
        (incq i)))
    (when (< 0 (length token))
      (consq token res))
    ;; NOTE the extra "" is needed to buffer the tokens at the end.
    (reverse (cons "" res))))

(defun viperfmt-format-tokens (toks)
  (letrec ((loopo (lambda (res indent space ts)
                    (let* ((curr (car ts))
                           (next (cadr ts))
                           (k-norm (lambda (r i s)
                                     (funcall loopo
                                              (concat r curr s) i
                                              (if (string= "\n" curr)
                                                  (get-indent ?\t i next)
                                                s) (cdr ts)))))
                      (cond
                       ((not (and curr next)) res)
                       ((string= "//" curr)
                        (funcall loopo (concat res curr next)
                                 indent space (cddr ts)))
                       ((string= "/*" curr)
                        (funcall loopo (concat res next (caddr ts))
                                 indent space (cdddr ts)))
                       ((and (not (string= "returns" curr))
                             (or (substringp "([" curr)
                                 (substringp "())]:," curr)
                                 (and (string= "{" next)
                                      (string-suffix-p ")" curr))))
                        (funcall k-norm res indent ""))
                       ((string= "{" curr)
                        (funcall k-norm res (1+ indent)
                                 (concat (if (string= "\n" next) "" "\n")
                                         (get-indent ?\t indent next))))
                       ((string= "}" curr)
                        (funcall k-norm res (- indent 1)
                                 (concat (if (string= "\n" curr) "" "\n")
                                         (get-indent ?\t indent next))))
                       (t (funcall k-norm res indent space)))))))
    (funcall loopo "" 0 " " toks)))

;;;; _

(provide 'viperfmt)

;;; viperfmt.el ends here
