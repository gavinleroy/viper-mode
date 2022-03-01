;;; lsp-viper.el --- The lsp-mode client for Viper -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Gavin Gray
;;
;; Author: Gavin Gray <https://github.com/gavingray>
;; Maintainer: Gavin Gray <gavinleroy6@gmail.com>
;; Created: February 28, 2022
;; Modified: February 28, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/gavingray/lsp-viper
;; Package-Requires: ((emacs "26.2"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

;; (require 'json)
;; (require 'projectile nil 'noerror)
;; (require 'find-file-in-project nil 'noerror)
(require 'cl-lib)
(require 'lsp-mode)

(defgroup lsp-viper nil
  "LSP support for Viper using viper-tools installation."
  :group lsp-mode
  :link '(url-link github.com/gavinleroy/viper-mode))

;;;###autoload
(defcustom lsp-viper-server-command "racket"
  "The command to launch the Viper language server."
  :group 'lsp-viper
  :type 'file)

;;;###autoload
(defcustom lsp-viper-server-args '()
  "Extra arguments for the Viper language server."
  :group 'lsp-viper
  :type '(repeat string))

(defcustom lsp-viper-dir
  (f-join
   ;; lsp-server-install-dir
   "~/dev/prj/viper-mode/" ;; TMP while debugging is in place
   "viper-server/"
   )
  "The directory of the Viper Server."
  :type 'directory
  :group 'lsp-viper)

(defcustom lsp-viper-executable
  (concat lsp-viper-dir "main.rkt")
  "Path to the Viper server lib executable."
  :type '(file :must-match t)
  :group 'lsp-viper)

(defcustom lsp-viper-logging t
  ;; FIXME change to nil after debugging
  "Flag for allowing request logging."
  :type 'boolean
  :group 'lsp-viper)

;; TODO add a function to automatically download and install viper-tools

(defun lsp-viper--gen-server-command ()
  "Generate the Viper language server startup command."
  `(,lsp-viper-server-command ;; racket
    ,@lsp-viper-server-args   ;; ...
    ,lsp-viper-executable))   ;; .../viper-server/viper-server

(defun lsp-viper--log (format &rest args)
  "Log LSP Viper request with `FORMAT'ed `ARGS'."
  (when lsp-viper-logging
    (apply #'lsp-log format args)))

(defun lsp-viper--execute-command (command &optional args)
  "Send an executeCommand request for `COMMAND' and `ARGS'."
  (lsp--cur-workspace-check)
  (lsp-send-execute-command command (apply #'vector args)))

(defun lsp-viper-verify-buffer ()
  "Verify the current buffer with running Viper server."
  (interactive)
  (-let* ((root (lsp-workspace-root default-directory))
          (uri (buffer-string)))
    (lsp-request "Verify" (list :uri uri
                                :manuallyTriggered t
                                :workspace root)))
  )

(add-to-list 'lsp-language-id-configuration '(viper-mode . "viper"))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection 'lsp-viper--gen-server-command)
  :major-modes '(viper-mode)
  :activation-fn (lsp-activate-on "viper")
  :priority 1
  :server-id 'viper-server))

(lsp-consistency-check lsp-viper)

(provide 'lsp-viper)

;;; lsp-viper.el ends here
