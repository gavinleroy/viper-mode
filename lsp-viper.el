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

(require 'cl-lib)
(require 'lsp-mode)
(require 'ht)
(require 'lsp-viper-protocol)
(require 'lsp-viper-settings)

(defgroup lsp-viper nil
  "LSP support for Viper using viper-tools installation."
  :group lsp-mode
  :link '(url-link github.com/gavinleroy/viper-mode))

;;;###autoload
(defcustom lsp-viper-server-command "node"
  "The command to launch the Viper language server."
  :group 'lsp-viper
  :type 'file)

;;;###autoload
(defcustom lsp-viper-server-args '("--stdio")
  "Extra arguments for the Viper language server.

`STDIO' is required to work with lsp-viper."
  :group 'lsp-viper
  :type '(repeat string))

(defcustom lsp-viper-dir
  (f-join
   ;; lsp-server-install-dir
   "~/dev/prj/"
   "viper-ide/server/dist/" ;; TMP while debugging is in place
   )
  "The directory of the Viper Server."
  :type 'directory
  :group 'lsp-viper)

(defcustom lsp-viper-executable
  (concat lsp-viper-dir "server.js")
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
  `(,lsp-viper-server-command
    ,lsp-viper-executable
    ,@lsp-viper-server-args))

;; --------------------

(defcustom lsp-viper--backend-ready nil
  "Is the Viper Backend ready?."
  :type 'boolean
  :group 'lsp-viper)

;; --------------------
;; Notification handlers

(defun lsp-viper--file-opened (workspace fn)
  "Message user that `FN' was opened in `WORKSPACE'."
  (with-lsp-workspace workspace
    (message (format "File opened: %s" fn))))

(defun lsp-viper--file-closed (workspace fn)
  "Message user that `FN' was opened in `WORKSPACE'."
  (with-lsp-workspace workspace
    (message (format "File closed: %s" fn))))


(defun lsp-viper--notification-log (workspace params)
  "Handle the viper/Log extension notification in `WORKSPACE'."
  (with-lsp-workspace workspace
    (lsp-viper--logit 'info params)))

(defun lsp-viper--notification-error (workspace params)
  "Handle the viper/Error extension notification in `WORKSPACE'."
  (with-lsp-workspace workspace
    (lsp-viper--logit 'error params)))

(lsp-defun lsp-viper--notification-backend-ready
  (workspace (&BackendReadyParams
              :name :restarted :is-viper-server))
  (with-lsp-workspace workspace
    (progn (setq lsp-viper--backend-ready t)
           (message "Viper backend ready!"))))

(lsp-defun lsp-viper--notification-state-change (workspace (&StateChangeParams :new-state))
  (with-lsp-workspace workspace
    (message (format "New state: [ %s ]"
                     (cl-case new-state
                       (0 "stopped")
                       (1 "starting")
                       (2 "verification running")
                       (3 "verification printing help")
                       (4 "verification reporting")
                       (5 "post processing")
                       (6 "ready")
                       (7 "stopping")
                       (8 "stage"))))))

(defun lsp-viper--verification-not-started (_workspace uri)
  "Inform the client that verification not started for file `URI' in `WORKSPACE'."
  (message (format "Verification Not Started for %s" uri)))

(lsp-defun lsp-viper--handle-settings-checked (_workspace (&SettingsCheckedParams :ok :errors :settings))
  "TODO."
  (with-help-window "*settings-checked-results*"
    (progn
      (princ ok)
      (princ errors)
      (princ settings))))

;; --------------------
;; Request handlers

;; TODO include *.sil and actually look for file endings
(defun lsp-viper--notification-get-file-exts (workspace _args)
  "Handle request for current Viper file extensions.
NOTE: only .vpr is currently supported."
  (with-lsp-workspace workspace
    ["*.vpr"]))

(defun lsp-viper--check-settings-versions (workspace _args)
  "TODO."
  (with-lsp-workspace workspace
    nil))

(defun lsp-viper--required-versions (workspace _args)
  "TODO."
  (with-lsp-workspace workspace
    lsp-viper-settings--specified-versions))

;; --------------------
;; Command handlers

;; --------------------
;; Utils

(lsp-defun lsp-viper--logit (type (&LogParams :data :log-level))
  (funcall (cl-case type
             ('error 'lsp--error)
             ('warn 'lsp--warn)
             ('info 'lsp--info))
           "%s" data))

(defun lsp-viper--log (format &rest args)
  "Log LSP Viper request with `FORMAT'ed `ARGS'."
  (when lsp-viper-logging
    (apply #'lsp-log format args)))

;; --------------------
;; Request / Command execution

(defun lsp-viper--execute-command (command &optional args)
  "Send an executeCommand request for `COMMAND' and `ARGS'."
  (lsp--cur-workspace-check)
  (lsp-send-execute-command command (apply #'vector args)))

(defun lsp-viper-verify-buffer ()
  "Verify the current buffer with running Viper server."
  (interactive)
  (-let* ((root (lsp-workspace-root default-directory))
          (uri (buffer-file-name (current-buffer)))
          (params (list :uri uri
                        :manuallyTriggered t
                        :workspace root)))
    (lsp-notify "Verify" params)))

(defun lsp-viper--request-backend-names ()
  "TODO."
  (interactive)
  (-let* ((response (lsp-request "RequestBackendNames" [])))
    (with-help-window "*test*"
      (princ response))))

(defun lsp-viper--stop-all-verifications ()
  "Stop the verification process for all queued files."
  (interactive)
  (let ((response (lsp-request "StopAllVerifications" nil)))
    (unless response
      (lsp-viper--log "ERROR: failed to stop verifications")
      (warn "Unable to stop verifications"))))

(defun lsp-viper--stop-verification (filename)
  "Stop the verification of `FILENAME'."
  (let* ((response (lsp-request "StopVerification"
                                (vector filename))))
    (unless response
      (lsp-viper--log "ERROR: failed to stop %s verification" filename)
      (warn "Unable to stop verification %s" filename))))

(defun lsp-viper-stop-buffer-verification ()
  "Stop the verification of the current buffer."
  (interactive)
  (let ((fn (buffer-file-name (current-buffer))))
    (lsp-viper--stop-verification fn)))

(defun lsp-viper--get-viper-server-url ()
  "TODO."
  (interactive)
  (let ((response (lsp-request "GetViperServerUrl" nil)))
    (with-help-window "*test*"
      (princ response))))

(defun lsp-viper--start-backend ()
  "Start the verification backend server.

NOTE only 'viperserver' supported by LSP client."
  (interactive)
  (lsp-notify "StartBackend"
              ;; FIXME insert other options:
              ;; + carbon
              ;; TODO interactively request from user
              ;; with a dropdown.
              "silicon"))

;; --------------------

(add-to-list 'lsp-language-id-configuration '(viper-mode . "viper"))
(setq lsp-progress-via-spinner t)

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection 'lsp-viper--gen-server-command)
  :major-modes '(viper-mode)
  ;; :activation-fn (lsp-activate-on "viper")
  :priority 1
  :notification-handlers (ht ("Log" 'lsp-viper--notification-log)
                             ("Error" 'lsp-viper--notification-error)
                             ("BackendReady" 'lsp-viper--notification-backend-ready)
                             ("StateChange" 'lsp-viper--notification-state-change)
                             ("FileOpened" 'lsp-viper--file-opened)
                             ("FileClosed" 'lsp-viper--file-closed)
                             ("VerificationNotStarted" 'lsp-viper--verification-not-started)
                             ("SettingsChecked" 'lsp-viper--handle-settings-checked)
                             )
  :request-handlers (ht ("GetViperFileEndings" 'lsp-viper--notification-get-file-exts)
                        ("CheckIfSettingsVersionsSpecified" 'lsp-viper--check-settings-versions) ;; TODO
                        ("RequestRequiredVersion" 'lsp-viper--required-versions) ;; TODO
                        )
  :server-id 'viper-server))

(defun lsp-viper-reload ()
  "Reload the `lsp-viper' package."
  (interactive)
  (unload-feature 'lsp-viper)
  (require 'lsp-viper))

(lsp-consistency-check lsp-viper)

(provide 'lsp-viper)

;;; lsp-viper.el ends here
