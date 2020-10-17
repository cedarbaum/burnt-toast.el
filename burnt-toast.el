;;; burnt-toast.el --- Elisp integration with BurntToast -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2020 Sam Cedarbaum
;;
;; Author: Sam Cedarbaum (scedarbaum@gmail.com)
;; Keywords: alert notifications powershell
;; Homepage: https://github.com/cedarbaum/burnt-toast.el
;; Version: 0.1
;; Package-Requires: ((emacs "25.1"))
;; License: GPL3

;;; Commentary:

;; Elisp integration with BurntToast, a PowerShell module for displaying Windows 10 and Windows Server 2019 Toast Notifications.

;;; Code:

(require 'dash)

(defcustom burnt-toast-powershell-command "powershell"
  "Command to invoke PowerShell."
  :type 'string
  :group 'burnt-toast)

(defun burnt-toast/quote-ps-arg (arg)
  "Surround ARG with double quotes when it is non-nil."
  (when arg
    (concat "\"" arg "\"")))

(defun burnt-toast/string-nil-to-empty (arg)
  "Return ARG when a non-nil string or an empty string otherwise."
  (if (and arg (stringp arg))
      arg
    ""))

(defun burnt-toast/run-powershell-command (command-and-args)
  "Execute a PowerShell command COMMAND-AND-ARGS."
  (let* ((ps-base-command (list burnt-toast-powershell-command nil nil nil))
         (all-args (add-to-list 'ps-base-command command-and-args t)))
    (apply 'call-process all-args)))

(defun burnt-toast/new-ps-object (object args)
  "Create a new PowerShell OBJECT using ARGS."
  (let* ((prefix-string (concat "$(New-" object))
         (non-nil-args (-filter (-lambda ((_ value)) value) args))
         (args-string-list (-map
                            (-lambda ((arg value)) (concat "-" arg " " (burnt-toast/string-nil-to-empty value)))
                            non-nil-args))
         (args-string (seq-reduce (lambda (s1 s2) (concat s1 " " s2)) args-string-list "")))
    (concat prefix-string args-string ")")))

(defun burnt-toast/bt-header-object (id title)
  "Create a new BTHeader with ID and TITLE."
  (burnt-toast/new-ps-object
   "BTHeader"
   `(("Id"    ,id)
     ("Title" ,(burnt-toast/quote-ps-arg title)))))

(cl-defun burnt-toast/new-notification-core (&key text app-logo sound header silent snooze-and-dismiss)
  "Create new notification with subset of arguments.
This function should not be called directly."
  (let* ((combined-text (when (and text (listp text)) (seq-reduce (lambda (s1 s2) (concat s1 ",\"" s2 "\"")) text "\"")))
         (processed-text (or (and combined-text (substring combined-text 2)) (burnt-toast/quote-ps-arg text)))
         (ps-command (burnt-toast/new-ps-object
                      "BurntToastNotification"
                      `(("Text"             ,processed-text)
                        ("AppLogo"          ,app-logo)
                        ("Sound"            ,sound)
                        ("Header"           ,header)
                        ("Silent"           ,silent)
                        ("SnoozeAndDismiss" ,snooze-and-dismiss)))))
    (burnt-toast/run-powershell-command ps-command)))

(cl-defun burnt-toast/new-notification-with-sound (&key text app-logo sound header)
  "Create new notification with TEXT, APP-LOGO, SOUND, and HEADER."
  (burnt-toast/new-notification-core
   :text text
   :app-logo app-logo
   :sound sound
   :header header))

(cl-defun burnt-toast/new-notification-silent (&key text app-logo header)
  "Create new notification with TEXT, APP-LOGO, and HEADER."
  (burnt-toast/new-notification-core
   :text text
   :app-logo app-logo
   :silent t
   :header header))

(cl-defun burnt-toast/new-notification-snooze-and-dismiss-with-sound (&key text app-logo header sound)
  "Create new snooze-and-dismiss notification with TEXT, APP-LOGO, HEADER, and SOUND."
  (burnt-toast/new-notification-core
   :text text
   :app-logo app-logo
   :sound sound
   :snooze-and-dismiss t
   :header header))

(cl-defun burnt-toast/new-notification-snooze-and-dismiss-silent (&key text app-logo header)
  "Create new snooze-and-dismiss notification with TEXT, APP-LOGO, and HEADER."
  (burnt-toast/new-notification-core
   :text text
   :app-logo app-logo
   :silent t
   :snooze-and-dismiss t
   :header header))

(provide 'burnt-toast)
;;; burnt-toast.el ends here
