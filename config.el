;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; refresh' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Lambert Green"
      user-mail-address "lambert.green@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 14))
(if (eq system-type 'gnu/linux)
    (setq
     doom-font (font-spec :family "Hack Nerd Font" :size 15)
     ))
(if (eq system-type 'darwin)
    (setq
     doom-font (font-spec :family "Hack Nerd Font" :size 15)
     ))
(if (eq system-type 'windows-nt)
    (setq
     doom-font (font-spec :family "Hack NF" :size 15)
     ))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. These are the defaults.
(setq doom-theme 'doom-one)

;; If you intend to use org, it is recommended you change this!
(setq org-directory "~/dev/my/notes/")

;; If you want to change the style of line numbers, change this to `relative' or
;; `nil' to disable it:
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', where Emacs
;;   looks when you load packages with `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

;; Set find program
(if (eq system-type 'windows-nt)
    (setq find-program "~/scoop/shims/find.exe")
  )

;; Enable Evil motions to treat underscores as word delimeters
;;
;; For python
(add-hook! 'python-mode-hook (modify-syntax-entry ?_ "w"))
;; For ruby
(add-hook! 'enh-ruby-mode-hook (modify-syntax-entry ?_ "w"))
;; For Javascript
(add-hook! 'js2-mode-hook (modify-syntax-entry ?_ "w"))
