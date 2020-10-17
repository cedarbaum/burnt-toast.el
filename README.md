# burnt-toast.el
Emacs integration with the [BurntToast](https://github.com/Windos/BurntToast) PowerShell module.

## Installation

```elisp
;; Add project to load-path
(add-to-list 'load-path "~/path/to/burnt-toast.el")

(require 'burnt-toast)       ;; Load library.
(require 'burnt-toast-alert) ;; Load alert integration (will load library too).

;; Test
(setq alert-default-style 'burnt-toast)
(alert "This is an alert" :title "I <3 PowerShell")
```

## Attributions

Icon is from Emacs source (commit: fd38c9c0afe2c5bbf04f565eec05daa52a16472b).
