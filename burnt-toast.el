;;; burnt-toast.el --- Elisp integration with the BurntToast PowerShell module. -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2020 Sam Cedarbaum

;; Author: Sam Cedarbaum (scedarbaum@gmail.com)
;; Keywords: alert notifications powershell comm
;; Homepage: https://github.com/cedarbaum/burnt-toast.el
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (dash "2.10"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Elisp integration with BurntToast, a PowerShell module for displaying Windows 10 and Windows Server 2019 Toast Notifications.

;;; Code:

(require 'dash)
(require 'cl-macs)
(require 'org-macs)

(defcustom burnt-toast-powershell-command "powershell"
  "Command to invoke PowerShell."
  :type 'string
  :group 'burnt-toast)

(defvar burnt-toast--verbose nil "Enable verbose logging.")
(defvar burnt-toast--install-checked nil "Cache if installation has already been checked.")

(defun burnt-toast--check-installation ()
  "Check if PowerShell and BurntToast module are installed and on PATH."
  (when (not burnt-toast--install-checked)
    (when (not (executable-find burnt-toast-powershell-command))
      (error "PowerShell executable not on PATH"))
    (when (eq 1 (burnt-toast--run-powershell-command "Get-Command New-BurntToastNotification" t))
      (error "BurntToast module cannot be found"))
    (setq burnt-toast--install-checked t)))

;; Based on: https://github.com/mplscorwin/erc-burnt-toast-blob/master/erc-burnt-toast.el
(defun burnt-toast--sanitize-powershell-input (string)
  "Return a version of STRING sanitized for use as input to PowerShell.
New-lines are removed, trailing spaces are removed, and single-quotes are doubled."
  (when (stringp string)
    (org-no-properties
     (replace-regexp-in-string
      "\s+$" ""
        (replace-regexp-in-string
         "[\t\n\r]+" ""
         (replace-regexp-in-string
          "\"" "\"\""
          string))))))

(defun burnt-toast--quote-and-sanitize-string (string)
  "Surround STRING with double quotes and sanitize it when it is non-nil."
  (when string
    (concat "\"" (burnt-toast--sanitize-powershell-input string) "\"")))

(defun burnt-toast--nil-string-to-empty (string)
  "Return STRING when a non-nil string or an empty string otherwise."
  (if (stringp string)
      string
    ""))

(defun burnt-toast--run-powershell-command (command-and-args &optional skip-install-check)
  "Execute a PowerShell command COMMAND-AND-ARGS.
Optionally skip BurntToast installation check with SKIP-INSTALL-CHECK."
  (let* ((process-args (list burnt-toast-powershell-command nil nil nil command-and-args)))
    (when burnt-toast--verbose (message command-and-args))
    (or skip-install-check (burnt-toast--check-installation))
    (apply 'call-process process-args)))

(defun burnt-toast--new-ps-object (object args)
  "Create a new PowerShell OBJECT using ARGS."
  (let* ((prefix-string (concat "$(New-" object " "))
         (non-nil-args (-filter (-lambda ((_ value)) value) args))
         (quoted-args (-map
                       (-lambda ((arg value quote)) `(,arg ,(if quote (burnt-toast--quote-and-sanitize-string value) value)))
                       non-nil-args))
         (args-string-list (-map
                            (-lambda ((arg value)) (concat "-" arg " " (burnt-toast--nil-string-to-empty value)))
                            quoted-args))
         (args-string (-reduce (lambda (s1 s2) (concat s1 " " s2)) args-string-list)))
    (concat prefix-string args-string ")")))

(cl-defun burnt-toast--new-notification-core (&key text app-logo sound header silent snooze-and-dismiss)
  "Create new notification with subset of arguments.
Arguments are TEXT, APP-LOGO, SOUND, HEADER, SILENT,and SNOOZE-AND-DISMISS.
This function should not be called directly."
  (let* ((processed-text (if (and text (listp text))
                             (-reduce
                              (lambda (s1 s2) (concat s1 "," s2))
                              (-map #'burnt-toast--quote-and-sanitize-string text))
                           (burnt-toast--quote-and-sanitize-string text)))
         (ps-command (burnt-toast--new-ps-object
                      "BurntToastNotification"
                      `(("Text"             ,processed-text)
                        ("AppLogo"          ,app-logo t)
                        ("Sound"            ,sound t)
                        ("Header"           ,header)
                        ("Silent"           ,silent)
                        ("SnoozeAndDismiss" ,snooze-and-dismiss)))))
    (burnt-toast--run-powershell-command ps-command)))

;;;###autoload
(defun burnt-toast-bt-header-object (id title)
  "Create a new BTHeader with ID and TITLE."
  (burnt-toast--new-ps-object
   "BTHeader"
   `(("Id"    ,id)
     ("Title" ,title t))))

;;;###autoload
(cl-defun burnt-toast-new-notification-with-sound (&key text app-logo sound header)
  "Create new notification with TEXT, APP-LOGO, SOUND, and HEADER."
  (burnt-toast--new-notification-core
   :text text
   :app-logo app-logo
   :sound sound
   :header header))

;;;###autoload
(cl-defun burnt-toast-new-notification-silent (&key text app-logo header)
  "Create new silent notification with TEXT, APP-LOGO, and HEADER."
  (burnt-toast--new-notification-core
   :text text
   :app-logo app-logo
   :silent t
   :header header))

;;;###autoload
(cl-defun burnt-toast-new-notification-snooze-and-dismiss-with-sound (&key text app-logo header sound)
  "Create new snooze-and-dismiss notification with TEXT, APP-LOGO, HEADER, and SOUND."
  (burnt-toast--new-notification-core
   :text text
   :app-logo app-logo
   :sound sound
   :snooze-and-dismiss t
   :header header))

;;;###autoload
(cl-defun burnt-toast-new-notification-snooze-and-dismiss-silent (&key text app-logo header)
  "Create new silent snooze-and-dismiss notification with TEXT, APP-LOGO, and HEADER."
  (burnt-toast--new-notification-core
   :text text
   :app-logo app-logo
   :silent t
   :snooze-and-dismiss t
   :header header))

;;;###autoload
(cl-defun burnt-toast-new-shoulder-tap (image person &key text app-logo header)
  "Create new shoulder tap with IMAGE, PERSON, TEXT, APP-LOGO, and HEADER."
  (let* ((processed-text (if (and text (listp text))
                             (-reduce
                              (lambda (s1 s2) (concat s1 "," s2))
                              (-map #'burnt-toast--quote-and-sanitize-string text))
                           (burnt-toast--quote-and-sanitize-string text)))
         (ps-command (burnt-toast--new-ps-object
                      "BurntToastShoulderTap"
                      `(("Image"   ,image t)
                        ("Person"  ,person t)
                        ("Text"    ,processed-text)
                        ("AppLogo" ,app-logo t)
                        ("Header"  ,header)))))
    (burnt-toast--run-powershell-command ps-command)))

(provide 'burnt-toast)
;;; burnt-toast.el ends here
