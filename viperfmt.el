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

(defcustom viper-format-on-save nil
  "Format future viper buffers before saving."
  :type 'boolean
  :safe #'booleanp
  :group 'viper-mode)

(defun viper-enable-format-on-save ()
  "Enable formatting when saving buffer."
  (interactive)
  (setq-local viper-format-on-save t))

(defun viper-disable-format-on-save ()
  "Disable formatting when saving buffer."
  (interactive)
  (setq-local viper-format-on-save nil))

(defun viper-before-save-method ()
  (when viper-format-on-save
    (condition-case e
        (viperfmt-reformat-buffer)
      (message (format "viper-before-save-hook %S %S")
               (car e)
               (cdr e)))))

;;;###autoload
(defun viperfmt-reformat-buffer ()
  "Reformat the whole buffer."
  (interactive)
  (save-excursion
    (if (version<= "26.2" emacs-version)
        (viperfmt--reformat-buffer)
      (error "You really should be using Emacs version >= 26.2"))))

;;;###autoload
(defun viperfmt--reformat-buffer ()
  "Reformat and replace current buffer."
  (save-excursion
    (let* ((filename buffer-file-name)
           (formatted-buffer (get-buffer-create "*viperfmt*"))
           (formatted-contents (viperfmt--reformat-region-as-is (point-min)
                                                                (point-max))))
      (unwind-protect
          (progn (with-current-buffer formatted-buffer
                   (erase-buffer)
                   (insert formatted-contents))
                 (erase-buffer)
                 (insert-buffer formatted-buffer))
        (kill-buffer formatted-buffer)))))

;; Internal functions

(defun viperfmt--reformat-region-as-is (beg end)
  "Reformat the given region from `BEG' to `END' as-is."
  (with-current-buffer (current-buffer)
    (let* ((buffer-contents (buffer-substring-no-properties beg end))
           (s (viperfmt--format-string buffer-contents)))
      s)))

(defun viperfmt--format-string (str)
  "Reformat `STR' as a Viper program."
  (viperfmt--format-tokens (viperfmt--tokenize str)))

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

(defun whitespacep (c)
  (or (string= c " ") (string= c "\t")
      (string= c "\r") (string= c "\n")))

(defun get-indent (ind nxt)
  (let ((add 0))
    (when (or (string= nxt "requires")
              (string= nxt "ensures")
              (string= nxt "invariant"))
      (incq add))
    (make-string (* 2 (+ ind add)) ? )))

(defun get-index (a idx &optional v)
  "Get element at `IDX' from `A' or `V' if index doesn't exist."
  (if (and (<= 0 idx) (< idx (length a)))
      (char-to-string (aref a idx))
    v))

;; FIXME this is a terrible copy of https://github.com/viperproject/viper-ide/blob/master/client/src/ViperFormatter.ts
(defun viperfmt--tokenize (str)
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
         (if (string= "\n" curr)
             (progn (consq token res)
                    (setq token "")
                    (consq "\n" res)
                    (setq line-comment nil))
           (concatq token curr)))
         (multi-comment
          (if (string= "*/" peek-two)
             (progn (consq token res)
                    (setq token "")
                    (consq "*/" res)
                    (incq i)
                    (setq multi-comment nil))
           (concatq token curr)))
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

(defun viperfmt--format-tokens (toks)
  (letrec ((loopo (lambda (res indent ts)
                    (let* ((curr (car ts))
                           (next (cadr ts))
                           (k-norm (lambda (r i s)
                                     (funcall loopo (concat r curr
                                                            (if (string= "\n" curr)
                                                                (get-indent i next)
                                                              s))
                                              i (cdr ts)))))
                      (cond
                       ((not (and curr next)) res)
                       ((string= "//" curr)
                        (funcall loopo (concat res curr next)
                                 indent (cddr ts)))
                       ((string= "/*" curr)
                        (funcall loopo (concat res curr next (caddr ts))
                                 indent (cdddr ts)))
                       ((and (not (string= "returns" curr))
                             (or (substringp "([" curr)
                                 (substringp "())]:," next)
                                 (and (string= "{" next)
                                      (string-suffix-p ")" curr))))
                        (funcall k-norm res indent ""))
                       ((string= "{" curr)
                        (let ((ind (1+ indent)))
                          (funcall k-norm res ind
                                   (concat (if (string= "\n" next) "" "\n")
                                           (get-indent ind next)))))
                       ((string= "}" next)
                        (let ((ind (1- indent)))
                          (funcall k-norm res ind
                                   (concat (if (string= "\n" curr) "" "\n")
                                           (get-indent ind next)))))
                       (t (funcall k-norm res indent " ")))))))
    (funcall loopo "" 0 toks)))

;;;; _

(provide 'viperfmt)

;;; viperfmt.el ends here
