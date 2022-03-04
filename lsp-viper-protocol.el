;;; lsp-viper-protocol.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Gavin Gray
;;
;; Author: Gavin Gray <https://github.com/gavingray>
;; Maintainer: Gavin Gray <gavinleroy6@gmail.com>
;; Created: March 03, 2022
;; Modified: March 03, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/gavingray/lsp-viper-protocol
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'lsp-protocol)

(lsp-interface
 (SettingsCheckedParams (:ok :errors :settings) nil)
;;     static RequestRequiredVersion = "RequestRequiredVersion";//void -> requiredVersions: Versions
 (StateChangeParams (:newState) (:progress :success :verificationCompleted :manuallyTriggered
                                 :filename :backendName :time :nofErrors :nofWarnings
                                 :verificationNeeded :uri :stage :error :diagnostics))
 (LogParams (:data :logLevel) nil)
 (ProgressParams (:domain :curr :total) nil)
 (BackendReadyParams (:name :restarted :isViperServer) nil))

;; TODO not sure yet about how to handle these
;;     static Hint = "Hint";//message: string
;;
;;     static FileOpened = "FileOpened";//uri: string
;;     static FileClosed = "FileClosed";//uri: string
;;     static VerificationNotStarted = "VerificationNotStarted";//uri: string
;;     static StopDebugging = "StopDebugging";//void
;;
;;     static StepsAsDecorationOptions = "StepsAsDecorationOptions";//StepsAsDecorationOptionsResult
;;     static HeapGraph = "HeapGraph";//HeapGraph
;;     static UnhandledViperServerMessageType = 'UnhandledViperServerMessageType';

(provide 'lsp-viper-protocol)
;;; lsp-viper-protocol.el ends here
