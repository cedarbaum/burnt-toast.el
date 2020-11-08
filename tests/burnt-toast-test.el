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

(provide 'burnt-toast-test)
;;; burnt-toast-test ends here
