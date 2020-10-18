# burnt-toast.el
Emacs integration with the [BurntToast](https://github.com/Windos/BurntToast) PowerShell module.

This package contains 2 parts:

1. A library that allows creating notifications via BurntToast.
2. Integration with the [Alert](https://github.com/jwiegley/alert) package.

## Installation

First, ensure the BurntToast PowerShell module is installed:

```powershell
Install-Module -Name BurntToast
```

Then add the project root to your `load-path` and require features as-needed:

```elisp
;; Add project to load-path
(add-to-list 'load-path "~/path/to/burnt-toast.el/")

(require 'burnt-toast)       ;; Load library.
(require 'burnt-toast-alert) ;; Load Alert integration (will load library too).
```

## Usage

### Alert integration

To use Alert style:

```elisp
(require 'burnt-toast-alert)
(setq alert-default-style 'burnt-toast)

;; Test
(alert "This is an alert" :title "I <3 PowerShell")
```

### Library functions

```elisp
;; All notifications can have a header, which has a Title and Id. The Title is
;; displayed at the top of the notification. The Id is used to correlate the
;; notification with others.

;; Create a BT-Header
(burnt-toast/bt-header-object "Title" "Id")

;; Basic notifications
(burnt-toast/new-notification-with-sound
 :text "Some text"                                    ;; Main text
 :app-logo "path/to/image.png"                        ;; Optional icon
 :sound "Alarm10"                                     ;; Optional sound effect
 :header (burnt-toast/bt-header-object "Title" "Id")) ;; Optional header

(burnt-toast/new-notification-with-sound
 :text '("Line 1" "Line 2"))                          ;; Text can be a list of string for multiline messages

(burnt-toast/new-notification-silent
 :text "Some text"
 :app-logo "path/to/image.png"
 :header (burnt-toast/bt-header-object "Title" "Id"))

;; Snooze-and-dismiss notifications
(burnt-toast/new-notification-snooze-and-dismiss-with-sound
 :text "Some text"
 :app-logo "path/to/image.png"
 :sound "Alarm10"
 :header (burnt-toast/bt-header-object "Title" "Id"))

(burnt-toast/new-notification-snooze-and-dismiss-silent
 :text "Some text"
 :app-logo "path/to/image.png"
 :header (burnt-toast/bt-header-object "Title" "Id"))
```

## Attributions

Icon is from Emacs source (commit: fd38c9c0afe2c5bbf04f565eec05daa52a16472b).
