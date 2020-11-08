;;; burnt-toast.el --- Elisp integration with the BurntToast PowerShell module -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2020 Sam Cedarbaum

;; Author: Sam Cedarbaum (scedarbaum@gmail.com)
;; Keywords: alert notifications powershell comm
;; Homepage: https://github.com/cedarbaum/burnt-toast.el
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (dash "2.10") (alert "1.2"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Elisp integration with BurntToast, a PowerShell module for displaying Windows 10 and Windows Server 2019 Toast Notifications.

;;; Code:

(require 'dash)
(require 'cl-lib)

(defcustom burnt-toast-powershell-command "powershell"
  "Command to invoke PowerShell."
  :type 'string
  :group 'burnt-toast)

(defvar burnt-toast--verbose nil "Enable verbose logging.")
(defvar burnt-toast-powershell-test-hook nil "Hook to intercept powershell command for testing")
(defvar burnt-toast--install-checked nil "Cache if installation has already been checked.")

(defun burnt-toast--check-installation ()
  "Check if PowerShell and BurntToast module are installed and on PATH."
  (unless burnt-toast--install-checked
    (unless (executable-find burnt-toast-powershell-command)
      (error "PowerShell executable not on PATH"))
    (unless (eq 0 (burnt-toast--run-powershell-command "Get-Command New-BurntToastNotification" t))
      (error "BurntToast module cannot be found"))
    (setq burnt-toast--install-checked t)))

;; Based on: https://github.com/mplscorwin/erc-burnt-toast-blob/master/erc-burnt-toast.el
(defun burnt-toast--sanitize-powershell-input (string)
  "Return a version of STRING sanitized for use as input to PowerShell.
New-lines are removed, trailing spaces are removed, and single-quotes are doubled."
  (when (stringp string)
    (replace-regexp-in-string
     "\s+$" ""
     (replace-regexp-in-string
      "[\t\n\r]+" ""
      (replace-regexp-in-string
       "\"" "\"\""
       string)))))

(defun burnt-toast--quote-and-sanitize-string (string)
  "Surround STRING with double quotes and sanitize it when it is non-nil."
  (when string
    (concat "\"" (burnt-toast--sanitize-powershell-input string) "\"")))

(defun burnt-toast--param-to-string (obj)
  "Return OBJ as string when a non-nil string or an empty string otherwise."
  (cond ((stringp obj) obj)
        ((numberp obj) (number-to-string obj))
        (t "")))

(defun burnt-toast--run-powershell-command (command-and-args &optional skip-install-check)
  "Execute a PowerShell command COMMAND-AND-ARGS.
Optionally skip BurntToast installation check with SKIP-INSTALL-CHECK."
  (when burnt-toast--verbose (message command-and-args))
  (if burnt-toast-powershell-test-hook
      (apply burnt-toast-powershell-test-hook `(,command-and-args))
    (or skip-install-check (burnt-toast--check-installation))
    (call-process burnt-toast-powershell-command nil nil nil
                  "-NoProfile" "-NoExit" "-NonInteractive" "-WindowStyle" "Hidden" command-and-args)))

(defun burnt-toast--create-ps-command (command-prefix args)
  "Create a new PowerShell command with prefix COMMAND-PREFIX using ARGS."
  (let* ((prefix-string (concat "$(" command-prefix " "))
         (non-nil-args (-filter (-lambda ((_ value)) value) args))
         (quoted-args (-map
                       (-lambda ((arg value quote)) `(,arg ,(if quote (burnt-toast--quote-and-sanitize-string value) value)))
                       non-nil-args))
         (args-string-list (-map
                            (-lambda ((arg value)) (concat "-" arg " " (burnt-toast--param-to-string value)))
                            quoted-args))
         (args-string (and args-string-list (-reduce (lambda (s1 s2) (concat s1 " " s2)) args-string-list))))
    (concat prefix-string (or args-string "") ")")))

(defun burnt-toast--new-ps-object (object args)
  "Create a new PowerShell OBJECT with ARGS."
  (let* ((command-prefix (concat "New-" object)))
    (burnt-toast--create-ps-command command-prefix args)))

(defun burnt-toast--new-ps-object-list (objects &optional process)
  "Create a comma separated list of OBJECTS.
Optionally process each object with PROCESS function as list is built."
  (let* ((map-func (or process #'identity)))
    (if (and objects (listp objects))
        (-reduce
         (lambda (s1 s2) (concat s1 "," s2))
         (-map map-func objects))
      (apply map-func `(,objects)))))

(cl-defun burnt-toast--new-notification-core (&key text app-logo sound header silent snooze-and-dismiss
                                                   unique-identifier)
  "Create new notification with subset of arguments.
Arguments are TEXT, APP-LOGO, SOUND, HEADER, SILENT, SNOOZE-AND-DISMISS,
and UNIQUE-IDENTIFIER.
This function should not be called directly."
  (let* ((processed-text (burnt-toast--new-ps-object-list text #'burnt-toast--quote-and-sanitize-string))
         (ps-command (burnt-toast--new-ps-object
                      "BurntToastNotification"
                      `(("Text"             ,processed-text)
                        ("AppLogo"          ,app-logo t)
                        ("Sound"            ,sound t)
                        ("Header"           ,header)
                        ("Silent"           ,silent)
                        ("SnoozeAndDismiss" ,snooze-and-dismiss)
                        ("UniqueIdentifier" ,unique-identifier)))))
    (burnt-toast--run-powershell-command ps-command)))

;;;###autoload
(cl-defun burnt-toast-submit-notification (content &key app-id
                                                   unique-identifier)
  "Submit a new notification.

CONTENT is the notification's content.
Should be created with (burnt-toast-bt-content-object ...).

APP-ID is an the application identifier of Emacs on Windows.

UNIQUE-IDENTIFIER will be assigned to the tag and group of the notification."
  (let* ((ps-command (burnt-toast--create-ps-command
                      "Submit-BTNotification"
                      `(("Content"          ,content)
                        ("AppId"            ,app-id t)
                        ("UniqueIdentifier" ,unique-identifier t)))))
    (burnt-toast--run-powershell-command ps-command)))

;;;###autoload
(cl-defun burnt-toast-bt-header-object (&key id title)
  "Create a new header for a notification.

ID is an identifier for the notification.  It is used to correlate
the notification with others.

TITLE is the display name for the notification."
  (burnt-toast--new-ps-object
   "BTHeader"
   `(("Id"    ,id)
     ("Title" ,title t))))

;;;###autoload
(cl-defun burnt-toast-bt-text-object (&key content max-lines)
  "Create a new text object.

CONTENT is the text content.

MAX-LINES is the maximum number of lines in the text object."
  (burnt-toast--new-ps-object
   "BTText"
   `(("Content"  ,content t)
     ("MaxLines" ,max-lines))))

;;;###autoload
(cl-defun burnt-toast-bt-image-object (&key source app-logo-override)
  "Create a new image object.

SOURCE is where the image is located.

APP-LOGO-OVERRIDE is non-nil if image will be used as application icon, nil otherwise."
  (burnt-toast--new-ps-object
   "BTImage"
   `(("Source"          ,source t)
     ("AppLogoOverride" ,app-logo-override))))

;;;###autoload
(cl-defun burnt-toast-bt-binding-object (&key children app-logo-override)
  "Create a new binding object.

CHILDREN is the elements contained in the binding.

APP-LOGO-OVERRIDE is the image to be used as the app logo."
  (burnt-toast--new-ps-object
   "BTBinding"
   `(("Children"        ,(burnt-toast--new-ps-object-list children))
     ("AppLogoOverride" ,app-logo-override))))

;;;###autoload
(cl-defun burnt-toast-bt-visual-object (binding-generic)
  "Create a new visual object.

BINDING-GENERIC is the binding associated with the visual."
  (burnt-toast--new-ps-object
   "BTVisual"
   `(("BindingGeneric" ,binding-generic))))

;;;###autoload
(cl-defun burnt-toast-bt-content-object (visual &key audio)
  "Create a new content object.

VISUAL is the visual associated with the content.

AUDIO is an optional audio object to play."
  (burnt-toast--new-ps-object
   "BTContent"
   `(("Visual" ,visual)
     ("Audio"  ,audio))))

;;;###autoload
(cl-defun burnt-toast-bt-audio-object (source)
  "Create a new audio object.

SOURCE is the audio's source."
  (burnt-toast--new-ps-object
   "BTAudio"
   `(("Source" ,source))))

;;;###autoload
(cl-defun burnt-toast-new-notification-with-sound (&key text app-logo sound header unique-identifier)
  "Create a new notification.

TEXT is the content of the notification.  This can be a list of strings,
in which case each entry is a new line.

APP-LOGO is a path to an icon to be displayed with the notification.

SOUND is the sound effect to play.

HEADER is the notification's header.
This should be created with (burnt-toast-bt-header-object ID HEADER).

UNIQUE-IDENTIFIER a unique identifier that can be used to remove/edit the notification."
  (burnt-toast--new-notification-core
   :text text
   :app-logo app-logo
   :sound sound
   :header header
   :unique-identifier unique-identifier))

;;;###autoload
(cl-defun burnt-toast-new-notification-silent (&key text app-logo header unique-identifier)
  "Create a new silent notification.

TEXT is the content of the notification.  This can be a list of strings,
in which case each entry is a new line.

APP-LOGO is a path to an icon to be displayed with the notification.

HEADER is the notification's header.
This should be created with (burnt-toast-bt-header-object ID HEADER).

UNIQUE-IDENTIFIER a unique identifier that can be used to remove/edit the notification."
  (burnt-toast--new-notification-core
   :text text
   :app-logo app-logo
   :silent t
   :header header
   :unique-identifier unique-identifier))

;;;###autoload
(cl-defun burnt-toast-new-notification-snooze-and-dismiss-with-sound (&key text app-logo header sound
                                                                           unique-identifier)
  "Create a new snooze-and-dismiss notification.

TEXT is the content of the notification.  This can be a list of strings,
in which case each entry is a new line.

APP-LOGO is a path to an icon to be displayed with the notification.

HEADER is the notification's header.
This should be created with (burnt-toast-bt-header-object ID HEADER).

SOUND is the sound effect to play.

UNIQUE-IDENTIFIER a unique identifier that can be used to remove/edit the notification."
  (burnt-toast--new-notification-core
   :text text
   :app-logo app-logo
   :header header
   :sound sound
   :snooze-and-dismiss t
   :unique-identifier unique-identifier))

;;;###autoload
(cl-defun burnt-toast-new-notification-snooze-and-dismiss-silent (&key text app-logo header
                                                                       unique-identifier)
  "Create a new silent snooze-and-dismiss notification.

TEXT is the content of the notification.  This can be a list of strings,
in which case each entry is a new line.

APP-LOGO is a path to an icon to be displayed with the notification.

HEADER is the notification's header.
This should be created with (burnt-toast-bt-header-object ID HEADER).

UNIQUE-IDENTIFIER a unique identifier that can be used to remove/edit the notification."
  (burnt-toast--new-notification-core
   :text text
   :app-logo app-logo
   :silent t
   :snooze-and-dismiss t
   :header header
   :unique-identifier unique-identifier))

;;;###autoload
(cl-defun burnt-toast-new-shoulder-tap (image person &key text app-logo header)
  "Create a new shoulder tap notification.

IMAGE is the image representing the contact.

PERSON is the name of the contact.

TEXT is the content of the fallback notification.  This can be a list of
strings, in which case each entry is a new line.

APP-LOGO is a path to an icon to be displayed with the fallback notification.

HEADER is the fallback notification's header.
This should be created with (burnt-toast-bt-header-object ID HEADER)."
  (let* ((processed-text (burnt-toast--new-ps-object-list text #'burnt-toast--quote-and-sanitize-string))
         (ps-command (burnt-toast--new-ps-object
                      "BurntToastShoulderTap"
                      `(("Image"   ,image t)
                        ("Person"  ,person t)
                        ("Text"    ,processed-text)
                        ("AppLogo" ,app-logo t)
                        ("Header"  ,header)))))
    (burnt-toast--run-powershell-command ps-command)))

(cl-defun burnt-toast-remove-notification (&key app-id tag group)
  "Remove a notification.

If APP-ID is specified, removes all notifications for that application.

If TAG is specified, removes all notifications with that tag.

If GROUP is specified, removes all notifications in that group."
  (let* ((ps-command (burnt-toast--create-ps-command
                      "Remove-BTNotification"
                      `(("AppId" ,app-id t)
                        ("Tag"   ,tag t)
                        ("Group" ,group)))))
    (burnt-toast--run-powershell-command ps-command)))

(provide 'burnt-toast)
;;; burnt-toast.el ends here
