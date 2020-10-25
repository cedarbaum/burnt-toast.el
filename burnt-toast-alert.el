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

(defcustom default-burnt-toast-header-id "burnt-toast-emacs"
  "Default ID provided to notification headers."
  :type 'string
  :group 'burnt-toast)

(defcustom burnt-toast-icon-path (concat (file-name-directory load-file-name) "icons/emacs.png")
  "Path to icon to use for notifications."
  :type 'string
  :group 'burnt-toast)

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
                           ;; Whether this alert should persist, or fade away
                           (persistent (plist-get info :persistent))
                           ;; Data which was passed to `alert'.  Can be
                           ;; anything.
                           ;; (data (plist-get info :data))
                           (header-id (or (plist-get info :id) default-burnt-toast-header-id)))
                        (if persistent
                            (burnt-toast-new-notification-snooze-and-dismiss-with-sound
                             :app-logo burnt-toast-icon-path
                             :text message
                             :header (burnt-toast-bt-header-object header-id title))
                          (burnt-toast-new-notification-with-sound
                           :app-logo burnt-toast-icon-path
                           :text message
                           :header (burnt-toast-bt-header-object header-id title))))))
                    ;; ;; Removers are optional.  Their job is to remove
                    ;; ;; the visual or auditory effect of the alert.
                    ;; :remover
                    ;; (lambda (info)
                    ;;   ;; It is the same property list that was passed to
                    ;;   ;; the notifier function.
                    ;;
                    ;;   ))

(provide 'burnt-toast-alert)
;;; burnt-toast-alert.el ends here
