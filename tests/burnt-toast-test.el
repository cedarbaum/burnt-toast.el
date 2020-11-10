;;; burnt-toast-test.el --- Tests for burnt-toast module -*- lexical-binding: t; coding: utf-8 -*-

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

;;; Code:

(require 'burnt-toast)

(defmacro run-burnt-toast-command (func)
  "Macro for running burnt-toast FUNC and returning what would be sent to PowerShell."
  `(let* ((output-cmd)
          (burnt-toast-powershell-test-hook (lambda (cmd) (setq output-cmd cmd))))
     ,func
     output-cmd))

(ert-deftest empty-notification ()
  "Sends an empty notification."
  (let ((command-output (run-burnt-toast-command (burnt-toast-new-notification-with-sound))))
    ;; Extra space is annoying but not a correctness issue. TODO: trim command text.
    (should (equal "$(New-BurntToastNotification )" command-output))))

(ert-deftest simple-notification ()
  "Sends a 1-line notification."
  (let ((command-output (run-burnt-toast-command (burnt-toast-new-notification-with-sound :text "Hello, world"))))
    (should (equal "$(New-BurntToastNotification -Text \"Hello, world\")" command-output))))

(ert-deftest multiline-notification ()
  "Sends a multi-line notification."
  (let ((command-output (run-burnt-toast-command (burnt-toast-new-notification-with-sound :text '("Hello" "world")))))
    (should (equal "$(New-BurntToastNotification -Text \"Hello\",\"world\")" command-output))))

(ert-deftest submit-custom-notification ()
  "Sends a custom built notification."
  (let* ((title-obj (burnt-toast-bt-text-object :content "title"))
         (message-obj (burnt-toast-bt-text-object :content "message"))
         (image (burnt-toast-bt-image-object :source "path/to/icon" :app-logo-override t))
         (binding (burnt-toast-bt-binding-object :children `(,title-obj ,message-obj) :app-logo-override image))
         (visual (burnt-toast-bt-visual-object binding))
         (audio (burnt-toast-bt-audio-object "ms-winsoundevent:Notification.Default"))
         (content (burnt-toast-bt-content-object visual :audio audio))
         (command-output (run-burnt-toast-command
                          (burnt-toast-submit-notification content :unique-identifier "id" :app-id "app-id"))))
    (should (equal "$(Submit-BTNotification -Content $(New-BTContent -Visual $(New-BTVisual -BindingGeneric $(New-BTBinding -Children $(New-BTText -Content \"title\"),$(New-BTText -Content \"message\") -AppLogoOverride $(New-BTImage -Source \"path/to/icon\" -AppLogoOverride ))) -Audio $(New-BTAudio -Source ms-winsoundevent:Notification.Default)) -AppId \"app-id\" -UniqueIdentifier \"id\")" command-output))))

(ert-deftest new-notification-with-expiration-time ()
  "Sends a notification with an expiration time."
  (let ((command-output (run-burnt-toast-command (burnt-toast-new-notification-with-sound
                                                  :expiration-time (burnt-toast-datetime-seconds-from-now 10)))))
    (should (equal "$(New-BurntToastNotification -ExpirationTime $([DateTime]::Now.AddSeconds(10.000000)))" command-output))))

(ert-deftest new-snooze-and-dismiss-notification-with-expiration-time ()
  "Sends a notification with an expiration time."
  (let ((command-output (run-burnt-toast-command (burnt-toast-new-notification-snooze-and-dismiss-with-sound
                                                  :expiration-time (burnt-toast-datetime-seconds-from-now 10)))))
    (should (equal "$(New-BurntToastNotification -SnoozeAndDismiss  -ExpirationTime $([DateTime]::Now.AddSeconds(10.000000)))" command-output))))

(provide 'burnt-toast-test)
;;; burnt-toast-test ends here
