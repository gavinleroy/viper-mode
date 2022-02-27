;;; viper-mode.el --- An Emacs major mode for editing Viper files -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Gavin Gray
;;
;; Author: Gavin Gray <https://github.com/gavingray>
;; Maintainer: Gavin Gray <gavinleroy6@gmail.com>
;; Created: February 23, 2022
;; Modified: February 23, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/gavingray/viper-mode
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(eval-when-compile (require 'rx))
(eval-when-compile (require 'viperfmt))

;; Load dependencies and optional libraries

(defvar viper-prettify-symbols-alist
  '(("&&" . ?∧) ("||" . ?∨) ("<=" . ?≤)
    (">=" . ?≥) ("!=" . ?≠) ("->" . ?→)
    ("=>" . ?⇒))
  "Alist of symbol prettifications used for `prettify-symbols-alist'.")

;;;; Customization

(defgroup viper-mode nil
  "Support for Viper code."
  :link '(url-link "https://www.pm.inf.ethz.ch/research/viper.html")
  :group 'languages)

(defcustom viper-before-save-hook 'viper-before-save-method
  "Function for formatting before save."
  :type 'function
  :group 'viper-mode)

(defcustom viper-indent-offset 2
  "Indent Viper code by this number of spaces."
  :type 'integer
  :group 'viper-mode
  :safe #'integerp)

(defface viper-operator-face
  '((t :inherit default))
  "Face for Viper operators."
  :group 'viper-mode)

(defvar viper-mode-syntax-table
  (let ((table (make-syntax-table)))

    ;; Operators
    (dolist (i '(?+ ?- ?* ?/ ?% ?& ?| ?^ ?! ?< ?> ?~ ?@))
      (modify-syntax-entry i "." table))

    ;; Strings
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\\ "\\" table)

    ;; Comments
    (modify-syntax-entry ?/  ". 124b" table)
    (modify-syntax-entry ?*  ". 23n"  table)
    (modify-syntax-entry ?\n "> b"    table)
    (modify-syntax-entry ?\^m "> b"   table)

    table))

;; Mode

(defvar viper-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for Viper major mode.")

;;;;###autoload
(define-derived-mode viper-mode prog-mode "Viper"
  "Major mode for Viper code.

\\{viper-mode-map}"
  :group 'viper-mode
  :syntax-table viper-mode-syntax-table

  ;; Fonts
  (setq-local font-lock-defaults
              '(viper-font-lock-keywords
                nil nil nil nil))

  ;; Misc
  (setq-local comment-start "// ")
  (setq-local comment-end   "")

  (add-hook 'before-save-hook viper-before-save-hook nil t)

  (setq prettify-symbols-alist viper-prettify-symbols-alist))

;;;;###autoload
(add-to-list 'auto-mode-alist '("\\.vpr" . viper-mode))

(defconst viper-single-comment
  (eval-when-compile
    "(//).*$\n?")
  "Regular expression matching Viper single-line comments.")

(defconst viper-multi-comment
  (eval-when-compile
    (rx "/*" (* (| (not "*") (: "*" (not "/")))) (+ "*") "/"))
  "Regular expression matching Viper multi-line comments.")

(defconst viper-language-constants
  '("false" "true"
    "null" "write"
    "none" "wildcard"))

(defconst viper-numeric-const
  (eval-when-compile
    "(?&lt;![\w$])((\+|-)?[0-9]+)\b")
  "Regular expression matching a Viper numeric constant.")

(defconst viper-primitive-type
  '("Bool" "Int" "Ref"
    "Perm" "Rational" "Set"
    "Seq" "Multiset" "Map"))

(defconst viper-other-keywords
  '("function" "axiom"
    "import" "predicate"
    "method" "new"))

;; \b(function)\b(\s+([$\w]+(\.\[$\w])?))? ;; Silver meta-function
;; (defconst viper-function
;;   (eval-when-compile
;;     "\b(function)\b(\s+([$\w]+(\.\[$\w])?))?")
;;   "Regular expression matching Viper function.")

;; \b(axiom)\b(\s+([$\w]+(\.[$\w]+)?))? ;; Silver Axiom
;; (defconst viper-axiom
;;   (eval-when-compile
;;     "\b(axiom)\b(\s+([$\w]+(\.[$\w]+)?))?")
;;   "Regular expression matching Viper axiom.")

;; \b(import)\b(\s+(".*?"|<.*?>))? ;; Silver meta imports
;; (defconst viper-import
;;   (eval-when-compile
;;     "\b(import)\b(\s+(\".*?\"|<.*?>))?")
;;   "Regular expression matching Viper imports.")

;; \b(predicate)\b(\s+([$\w]+(\.[$\w]+)?))? ;; Silver predicate
;; (defconst viper-predicate
;;   (eval-when-compile
;;     "\b(predicate)\b(\s+([$\w]+(\.[$\w]+)?))?")
;;   "Regular expression matching Viper predicates.")

;; \b(method)\b(\s+([$\w]+(\.[$\w]+)?))? ;; Silver method
;; (defconst viper-method
;;   (eval-when-compile
;;     "\b(method)\b(\s+([$\w]+(\.[$\w]+)?))?")
;;   "Regular expression matching Viper methods.")

;; \b(domain|range)\b\s*\( ;; Silver verification expression
(defconst viper-domains
  '("domain" "range"))

;; :: ;; TODO forall separator

(defconst viper-variable-declarations
  '("var" "returns" "field"))

(defconst viper-named-assertions
  '("define"))

(defconst viper-control-flows
  '("if" "else" "elseif" "while"
    "statelabel" "label" "goto"))

(defconst viper-verification-symbols
  '("acc" "forperm" "forall" "exists"
    "folding" "unfolding" "packaging"
    "applying" "old" "perm" "result"
    "let"))

(defconst viper-statements
  '("assert" "inhale" "exhale" "assume"
    "fold" "unfold" "package" "apply"
    "wand" "fresh" "constraining"))

(defconst viper-contracts
  '("requires" "ensures" "invariant"))

(defconst viper-operations
  '("union" "in" "intersection"
    "setminus" "\+\+" "subset"))

(defvar viper-font-lock-keywords
  (append
   `(
     ;; Language constants
     (,(regexp-opt viper-language-constants 'symbols) . font-lock-keyword-face)

     ;; Numeric constants
     (,viper-numeric-const 1 font-lock-type-face)

     ;; Primitive types
     (,(regexp-opt viper-primitive-type 'symbols) . font-lock-type-face)

     ;; Other keywords
     (,(regexp-opt viper-other-keywords 'symbols) . font-lock-keyword-face)

     (,(regexp-opt viper-domains 'symbols) . font-lock-keyword-face)

     (,(regexp-opt viper-variable-declarations 'symbols) . font-lock-variable-name-face)

     (,(regexp-opt viper-named-assertions 'symbols) . font-lock-keyword-face)

     (,(regexp-opt viper-control-flows 'symbols) . font-lock-keyword-face)

     (,(regexp-opt viper-verification-symbols 'symbols) . font-lock-keyword-face)

     (,(regexp-opt viper-statements 'symbols) . font-lock-keyword-face)

     (,(regexp-opt viper-contracts 'symbols) . font-lock-keyword-face)

     (,(regexp-opt viper-operations 'symbols) . viper-operator-face)

     (,viper-single-comment 1 font-lock-comment-face)

     (,viper-multi-comment 1 font-lock-comment-face)

     ;; FIXME many more things

     )))

;;; _

(defun viper-mode-reload ()
  (interactive)
  (unload-feature 'viper-mode)
  (require 'viper-mode)
  (viper-mode))

(provide 'viper-mode)

;;; viper-mode.el ends here
