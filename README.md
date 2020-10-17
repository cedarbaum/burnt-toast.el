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

;; Test
(setq alert-default-style 'burnt-toast)
(alert "This is an alert" :title "I <3 PowerShell")
```

## Attributions

Icon is from Emacs source (commit: fd38c9c0afe2c5bbf04f565eec05daa52a16472b).
