# Torus buffer group manager, branch version 2

The version 2 of Torus is built using the
tperations. It means a cleaner code, easier to maintain and extend,
but also a drastic change in the data structure.

More information [on the project page](https://github.com/chimay/torus/tree/version-2)

# Newcomers

In short, this plugin let you organize your buffers by creating as
many buffer groups as you need, add the buffers you want to it and
quickly navigate between :

- Buffers of the same group
- Buffer groups
- Workspaces, ie sets of buffer groups

Note that :

- A Location is a pair `(filename . position)`
- A buffer group, in fact a location group, is called a Circle
- A list of buffer groups is called a Torus (a Circle of Circles)
- The list of Toruses is called the Wheel

# Important note for version 1 users

Version 1 works well, so if you’re happy with it, it’s fine.

For those who want to switch to version 2, please note that the format
of torus files has changed, so it is recommended to backup your version
1 torus files, just in case something would go wrong with the conversion.

# New features

- Tab bar
	- More customizable
	- Can adapt to window size
- Move or copy circle to torus
- Roll circle, torus or the wheel

# Installation

## With [Straight.el](https://github.com/raxod502/straight.el)

Simply add these lines in your init file :

```
;; To improve your experience
(straight-use-package 'which-key)
(straight-use-package 'helm)
;; Necessary
(straight-use-package '(duo :type git :host github :repo "chimay/duo"))
(straight-use-package '(torus :type git :host github :branch "version-2" :repo "chimay/torus"))
```

# Configuration

## Minimal

```
;; Recommended
(use-package which-key
  :init (which-key-mode)
  :custom ((which-key-idle-delay 1.0)))
(use-package helm
  :config (helm-mode 1))
;; Necessary
(use-package duo
  :config
  (duo-init "duo-common" "duo-referen"))
(use-package torus
  :after (duo)
  :config
  (torus-init)
  (torus-install-default-bindings))
```

## Advanced

If you want the start & quit hooks to load & save your torus file, you
need to add a ~:hook~ section. This declaration gathers main options
and keybindings :

```
(use-package duo
  :config
  (duo-init "duo-common" "duo-referen"))

(use-package torus
  :after (duo)
  :bind-keymap ("s-t" . torus-map)
  :bind (("<s-insert>" . torus-add-here)
         ("s-f" . torus-add-file)
         ("s-b" . torus-add-buffer)
         ("<S-s-insert>" . torus-add-circle)
         ("<s-delete>" . torus-delete-location)
         ("<S-s-delete>" . torus-delete-circle)
         ("<C-prior>" . torus-previous-location)
         ("<C-next>" . torus-next-location)
         ("<C-home>" . torus-previous-circle)
         ("<C-end>" . torus-next-circle)
         ("s-SPC" . torus-switch-location)
         ("s-=" . torus-switch-circle)
         ("s-*" . torus-switch-torus)
         ("s-s" . torus-search-location)
         ("s-/" . torus-search-circle)
         ("<S-prior>" . torus-newer)
         ("<S-next>" . torus-older)
         ("C-^" . torus-alternate)
         ("s-^" . torus-alternate-menu)
         ("<S-home>" . torus-alternate-in-same-torus-other-circle)
         ("<S-end>" . torus-alternate-in-same-circle)
         ("<M-prior>" . torus-move-location-backward)
         ("<M-next>" . torus-move-location-forward)
         ("<M-home>" . torus-rotate-circle-left)
         ("<M-end>" . torus-rotate-circle-right)
         ("s-%" . torus-layout-menu)
         ("s-g" . torus-autogroup-menu)
         :map torus-map
         ("y" . torus-copy-location-to-circle))
         ("Y" . torus-copy-circle-to-torus))
  :hook ((emacs-startup . torus-hello)
         (kill-emacs . torus-bye))
  :custom ((torus-prefix-key "s-t")
           (torus-binding-level 2)
           (torus-verbosity 1)
           (torus-dirname "~/.emacs.d/torus")
           (torus-load-on-startup t)
           (torus-save-on-exit t)
           (torus-autoread-file "auto")
           (torus-autowrite-file "auto")
           (torus-backup-number 5)
           (torus-history-maximum-elements 50)
           (torus-maximum-horizontal-split 3)
           (torus-maximum-vertical-split 4)
           (torus-display-tab-bar t)
       (torus-display-position nil)
       (torus-dashboard-size 2)
           (torus-prefix-separator " : ")
           (torus-join-separator " & "))
  :config
  (torus-init)
  (torus-install-default-bindings))
 ```
