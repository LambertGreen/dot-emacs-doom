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
     doom-font (font-spec :family "Hack Nerd Font" :size 13)
     ))
(if (eq system-type 'darwin)
    (setq
     doom-font (font-spec :family "Hack Nerd Font" :size 13)
     ))
(if (eq system-type 'windows-nt)
    (setq
     doom-font (font-spec :family "Hack NF" :size 13)
     ))

;; Set window position and size
;; TODO: This code works when evaluated after Emacs start, but does not result
;; in the desired view on startup.
(if (eq system-type 'darwin)
    (when window-system
      (set-frame-position (selected-frame) 438 261)
      (set-frame-size (selected-frame) 185 47)))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. These are the defaults.
(setq doom-theme 'doom-one)

;; Set org directory
(setq org-directory "~/dev/my/notes/")

;; Set projects directory
(setq projectile-project-search-path '("~/dev/my/" "~/dev/pub/" "~/dev/work/"))

;; If you want to change the style of line numbers, change this to `relative' or
;; `nil' to disable it:
(setq display-line-numbers-type t)

;; Enable mouse support in terminal
(unless window-system
  (require 'mouse)
  (xterm-mouse-mode t)
  (global-set-key [mouse-4] (lambda ()
                              (interactive)
                              (scroll-down 1)))
  (global-set-key [mouse-5] (lambda ()
                              (interactive)
                              (scroll-up 1)))
  (defun track-mouse (_))
  (setq mouse-sel-mode t)
)

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

;; On MacOS use CMD key as Meta key since it is easier to press Meta for the following:
;; 1. M-TAB for OrgMode completion
;; 2. M-RET for OrgMode insert heading
;; 3. M-H/L for OrgMode heading promotion/demotion
;; UPDATE: 6/27/20: Commenting out for the following reasons:
;; 1. It's nice to be able to use the CMD key as the CMD key for OS interactions
;; e.g. CMD-H to hide window, etc.
;; 2. M-TAB for OrgMode completion can also be done using: C-M i
;; 3. M-RET for OrgMode new header can also be done using: C-c RET
;; 4. M-H/L for OrgMode header promote/demote can also be done using: S-<<, S->>
;;
;; (if (eq system-type 'darwin)
;;   (setq mac-command-modifier 'meta)
;;   (setq mac-option-modifier 'meta))

;; Set find program
;; TODO: Check if fd can be used since it so much faster.
(if (eq system-type 'windows-nt)
    (setq find-program (expand-file-name "~/scoop/shims/find.exe"))
  )

;; Show trailing whitespace
;; Well, this unfortunately causes whitespace to be show in all buffers
;; including terminal/shell bufffers -- which we really don't want.
;; So commenting out for now.
;; TODO: Enable ~show-trailing-whitespace~ for code buffers only.
;; (setq-default show-trailing-whitespace t)

;; Enable Evil motions to treat underscores as word delimeters
;;
;; For python
(add-hook! 'python-mode-hook (modify-syntax-entry ?_ "w"))
;; For ruby
(add-hook! 'enh-ruby-mode-hook (modify-syntax-entry ?_ "w"))
;; For Javascript
(add-hook! 'js2-mode-hook (modify-syntax-entry ?_ "w"))

;; Make Jedi to use Pyenv environment for our Python projects
(after! lsp-mode
  (setq lsp-pyls-plugins-jedi-use-pyenv-environment t)
  )

;; Doom removes '.projectile' as a project root file, but we want
;; to use it, so add it back.
;; Note: I never got the below to work, but I have since started using git sub-projects
;; and therefore do not need the below setting any longer.
;;
;; Leaving comment here for historical purposes.
;;
;; (after! projectile
;;   (setq projectile-project-root-files-bottom-up
;;       (append projectile-project-root-files-bottom-up '(".projectile") nil)
;;       )
;;   )

;; Associate file extensions to modes
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.manifest\\'" . json-mode))

;; Org-mode config
;;
(after! org
  ;; Use org-expiry to have timestamps automatically created for tasks
  (use-package! org-expiry
    :config
    (setq org-expiry-inactive-timestamps t))

  ;; Log DONE with timestamp
  (setq org-log-done 'time)

  ;; Update the default Doom "todo" to use TODO instead of [ ]
  (setq org-capture-templates
    '(("t" "todo" entry
      (file+headline +org-capture-todo-file "Inbox")
      "* TODO %?\n%i\n%a" :prepend t)
    ("n" "notes" entry
      (file+headline +org-capture-notes-file "Inbox")
      "* %u %?\n%i\n%a" :prepend t)
    ("j" "Journal" entry
      (file+olp+datetree +org-capture-journal-file)
      "* %U %?\n%i\n%a" :prepend t)
    ("p" "Templates for projects")
    ("pt" "Project-local todo" entry
      (file+headline +org-capture-project-todo-file "Inbox")
      "* TODO %?\n%i\n%a" :prepend t)
    ("pn" "Project-local notes" entry
      (file+headline +org-capture-project-notes-file "Inbox")
      "* %U %?\n%i\n%a" :prepend t)
    ("pc" "Project-local changelog" entry
      (file+headline +org-capture-project-changelog-file "Unreleased")
      "* %U %?\n%i\n%a" :prepend t)
    ("o" "Centralized templates for projects")
    ("ot" "Project todo" entry #'+org-capture-central-project-todo-file "* TODO %?\n %i\n %a" :heading "Tasks" :prepend nil)
    ("on" "Project notes" entry #'+org-capture-central-project-notes-file "* %U %?\n %i\n %a" :heading "Notes" :prepend t)
    ("oc" "Project changelog" entry #'+org-capture-central-project-changelog-file "* %U %?\n %i\n %a" :heading "Changelog" :prepend t)))
  )

;; Whenever a TODO entry is created, we want a timestamp
;;
(defun lgreen/insert-created-timestamp()
  (interactive)
  "Insert a CREATED property using org-expiry.el for TODO entries"
  (org-expiry-insert-created)
  (org-back-to-heading)
  (org-end-of-line)
  )

;; Advice org-insert-todo-heading to insert a created timestamp using org-expiry
(defadvice org-insert-todo-heading (after lgreen/created-timestamp-advice activate)
  "Insert a CREATED property using org-expiry.el for TODO entries"
  (lgreen/insert-created-timestamp))

;; Advice org-capture to insert a created timestamp using org-expiry
(defadvice org-capture (after lgreen/created-timestamp-advice activate)
  "Insert a CREATED property using org-expiry.el for TODO entries"
  ; Test if the captured entry is a TODO, if so insert the created
  ; timestamp property, otherwise ignore
  (when (member (org-get-todo-state) org-todo-keywords-1)
    (lgreen/insert-created-timestamp)))

;; Stop flyspell from stealing ~M-TAB~ from OrgMode
(eval-after-load 'flyspell '(define-key flyspell-mode-map "\M-\t" nil))

;; Transparency
(set-frame-parameter (selected-frame) 'alpha '(95 95))
(add-to-list 'default-frame-alist '(alpha . (95 . 95)))

;; Workaround ripgrep issue on Windows
(if (eq system-type 'windows-nt)
    (setq ripgrep-arguments '("--path-separator /"))
  )

;; Influence Tramp to use a login shell so that ~/.profile is sourced on remote
;; host resulting in $PATH being setup correctly.
;;
(after! tramp
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
)

;; macOS: Change dark/light theme
(if (eq system-type 'darwin)
    (add-hook 'ns-system-appearance-change-functions
        #'(lambda (appearance)
                (mapc #'disable-theme custom-enabled-themes)
                (pcase appearance
                        ('light (load-theme 'doom-one-light t))
                        ('dark (load-theme 'doom-one t))))))

;; Function to add to the Emacs path - swiped from https://gitlab.com/xeijin-dev/doom-config/blob/master/config.org
(defun lgreen/add-to-emacs-path (append-path &rest path)
  "add PATH to both emacs own `exec-path' and to the `PATH' inherited by emacs from the OS (aka `process-environment').
  APPEND-PATH should be non-nil if you want the added path to take priority over existing paths

  this does not modify the actual OS `PATH' just the two emacs internal variables which deal with paths:

  `exec-path' is used when executables are called from emacs directly
  `process-environment' is used when executables are called via the `shell'"

  (dolist (p path)
    (add-to-list 'exec-path p append-path))

  ;; update `process-environment' with whatever is in `exec-path' right now
  (setenv "PATH" (mapconcat #'identity exec-path path-separator))
  (message "exec-path and process-environment synchronised"))

;; LSP Related settings
;;
;; Set cache directory for ccls to be under home directory rather than polutting project directories
(if (eq system-type 'darwin)
    (setq ccls-initialization-options
          `(:cache (:directory "/tmp/ccls-cache"))))

;; Set path to clangd (required when using clangd as cpp lsp)
(if (eq system-type 'darwin)
    (setq lsp-clients-clangd-executable "/usr/local/Cellar/llvm/10.0.1_1/bin/clangd"))

;; macOS: Set locate to use unix locate command instead of `mdfind` because `mdfind` is not indexing
;; all dev files.
;; TODO: check if 'mdfind' can be configured to work better?
;; TODO: check if 'locate.udpatedb' is run periodically by default on macOS.
(after! ivy
  (if (eq system-type 'darwin)
      (setq counsel-locate-cmd 'counsel-locate-cmd-noregex)))

;; Enable gravatars
(setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))
