# burnt-toast.el
Emacs integration with the [BurntToast](https://github.com/Windos/BurntToast) PowerShell module.

# Installation

```elisp
;; Add project to load-path
(add-to-list 'load-path "~/path/to/burnt-toast.el")

(require 'burnt-toast)       ;; Load library.
(require 'burnt-toast-alert) ;; Load alert integration (will load library too).

;; Test
(setq alert-default-style 'burnt-toast)
(alert "This is an alert" :title "I <3 PowerShell")
```

# Attributions

Default icon is from https://commons.wikimedia.org/wiki/File:EmacsIcon.svg.
