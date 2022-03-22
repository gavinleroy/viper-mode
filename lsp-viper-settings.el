;;; lsp-viper-settings.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Gavin Gray
;;
;; Author: Gavin Gray <https://github.com/gavingray>
;; Maintainer: Gavin Gray <gavinleroy6@gmail.com>
;; Created: March 11, 2022
;; Modified: March 11, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/gavingray/lsp-viper-settings
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(defcustom lsp-viper-settings--default-config
  '()
  "TODO."
  :group 'lsp-viper-settings
  :type '(repeat string))

;; {
;;                 "viperSettings.viperServerSettings": {
;;                     "type": "object",
;;                     "default": {
;;                         "v": "674a514867b1",
;;                         "serverJars": {
;;                             "windows": [
;;                                 "$viperTools$/backends"
;;                             ],
;;                             "linux": [
;;                                 "$viperTools$/backends"
;;                             ],
;;                             "mac": [
;;                                 "$viperTools$/backends"
;;                             ]
;;                         },
;;                         "customArguments": " $backendSpecificCache$",
;;                         "backendSpecificCache": true,
;;                         "disableCaching": false,
;;                         "timeout": 5000,
;;                         "viperServerPolicy": "create",
;;                         "viperServerAddress": "http://127.0.0.1",
;;                         "viperServerPort": 12345
;;                     },
;;                     "description": "ViperServer-related settings. For more information, see https://github.com/viperproject/viper-ide/wiki/Settings:-ViperServer"
;;                 }

(defcustom lsp-viper-settings--ext-version
  "2.4.0" ; FIXME this is artificial to get a response from the server
  "TODO."
  :group 'lsp-viper-settings
  :type 'string)

(defcustom lsp-viper-settings--specified-versions
  `((viperServerSettingsVersion . "1.0.4")
    (backendSettingsVersion . "1.0.2")
    (pathSettingsVersion . "1.0.1")
    (userPreferencesVersion . "0.6.1")
    (javaSettingsVersion . "0.6.1")
    (advancedFeaturesVersion . "0.6.1")
    (defaultSettings . ,lsp-viper-settings--default-config)
    (extensionVersion . ,lsp-viper-settings--ext-version))
  "Startup settings for Viper."
  :group 'lsp-viper-settings
  ;; :type '(repeat string)
  )

(provide 'lsp-viper-settings)

;;; lsp-viper-settings.el ends here
