;;; burnt-toast-alert.el --- BurntToast integration with alert package -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2020 Sam Cedarbaum

;; Author: Sam Cedarbaum (scedarbaum@gmail.com)

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

;; BurntToast integration with alert package.

;;; Code:

(require 'burnt-toast)
(require 'alert)

(defcustom burnt-toast-icon-path (concat (file-name-directory load-file-name) "icons/emacs.png")
  "Path to icon to use for notifications."
  :type 'string
  :group 'burnt-toast)

(defcustom burnt-toast-alert-enable-remover nil
  "Non-nil if alert should remove notifications, nil otherwise."
  :type 'boolean
  :group 'burnt-toast)

(defcustom burnt-toast-emacs-app-id nil
  "The system's AppId for Emacs.  Must be an exact match or notifications will fail."
  :type 'string
  :group 'burnt-toast)

(defcustom burnt-toast-audio-source 'default
  "The audio source to play for non-silent notifications."
  :type 'symbol
  ;; TODO: This doesn't actually do anything. Should be a radio-button selection.
  :options '(default im mail reminder sms alarm alarm2 alarm3 alarm4 alarm5
              alarm6 alarm7 alarm8 alarm9 alarm10 call call2 call3 call4
              call5 call6 call7 call8 call9 call10)
  :group 'burnt-toast)

(defconst burnt-toast--audio-source-map '((default . "ms-winsoundevent:Notification.Default")
                                          (im . "ms-winsoundevent:Notification.IM")
                                          (mail . "ms-winsoundevent:Notification.Mail")
                                          (reminder . "ms-winsoundevent:Notification.Reminder")
                                          (sms . "ms-winsoundevent:Notification.SMS")
                                          (alarm . "ms-winsoundevent:Notification.Looping.Alarm")
                                          (alarm2 . "ms-winsoundevent:Notification.Looping.Alarm2")
                                          (alarm3 . "ms-winsoundevent:Notification.Looping.Alarm3")
                                          (alarm4 . "ms-winsoundevent:Notification.Looping.Alarm4")
                                          (alarm5 . "ms-winsoundevent:Notification.Looping.Alarm5")
                                          (alarm6 . "ms-winsoundevent:Notification.Looping.Alarm6")
                                          (alarm7 . "ms-winsoundevent:Notification.Looping.Alarm7")
                                          (alarm8 . "ms-winsoundevent:Notification.Looping.Alarm8")
                                          (alarm9 . "ms-winsoundevent:Notification.Looping.Alarm9")
                                          (alarm10 . "ms-winsoundevent:Notification.Looping.Alarm10")
                                          (call . "ms-winsoundevent:Notification.Looping.Call")
                                          (call2 . "ms-winsoundevent:Notification.Looping.Call2")
                                          (call3 . "ms-winsoundevent:Notification.Looping.Call3")
                                          (call4 . "ms-winsoundevent:Notification.Looping.Call4")
                                          (call5 . "ms-winsoundevent:Notification.Looping.Call5")
                                          (call6 . "ms-winsoundevent:Notification.Looping.Call6")
                                          (call7 . "ms-winsoundevent:Notification.Looping.Call7")
                                          (call8 . "ms-winsoundevent:Notification.Looping.Call8")
                                          (call9 . "ms-winsoundevent:Notification.Looping.Call9")
                                          (call10 . "ms-winsoundevent:Notification.Looping.Call10"))
  "Mapping from symbols to full audio source names.")

(alert-define-style 'burnt-toast :title "Burnt Toast"
                    :notifier
                    (lambda (info)
                      (let*
                          ;; The message text is :message
                          ((message (plist-get info :message))
                           ;; The :title of the alert
                           (title (plist-get info :title))
                           ;; The :category of the alert
                           ;; (category (plist-get info :category))
                           ;; The major-mode this alert relates to
                           ;; (mode (plist-get info :mode))
                           ;; The buffer the alert relates to
                           ;; (buffer (plist-get info :buffer))
                           ;; Severity of the alert.  It is one of:
                           ;;   `urgent'
                           ;;   `high'
                           ;;   `moderate'
                           ;;   `normal'
                           ;;   `low'
                           ;;   `trivial'
                           ;; (severity (plist-get info :severity))
                           ;; Data which was passed to `alert'.  Can be
                           ;; anything.
                           ;; (data (plist-get info :data))
                           ;; Whether this alert should persist, or fade away
                           ;; (persistent (plist-get info :persistent))
                           (id (plist-get info :id)))
                        (let* ((title-obj (burnt-toast-bt-text-object :content title))
                               (message-obj (burnt-toast-bt-text-object :content message))
                               (image (burnt-toast-bt-image-object :source burnt-toast-icon-path :app-logo-override t))
                               (binding (burnt-toast-bt-binding-object :children `(,title-obj ,message-obj) :app-logo-override image))
                               (visual (burnt-toast-bt-visual-object binding))
                               (audio-source (cdr (assoc burnt-toast-audio-source burnt-toast--audio-source-map)))
                               (audio (burnt-toast-bt-audio-object audio-source))
                               (content (burnt-toast-bt-content-object visual :audio audio)))
                          (burnt-toast-submit-notification content :unique-identifier id :app-id burnt-toast-emacs-app-id))
                        :remover
                        (lambda (info)
                          (when-let ((id (plist-get info :id)))
                            (and burnt-toast-alert-enable-remover (burnt-toast-remove-notification :group id)))))))

(provide 'burnt-toast-alert)
;;; burnt-toast-alert.el ends here
