;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; refresh' after modifying this file!

;; Add Homebrew Emacs site-lisp to load-path
(when (eq system-type 'darwin)
  (let ((default-directory "/opt/homebrew/share/emacs/site-lisp"))
    (normal-top-level-add-subdirs-to-load-path)))

;; Add Homebrew Info to Info path
(when (eq system-type 'darwin)
  (add-to-list `Info-directory-list "/opt/homebrew/share/info/"))

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
     doom-font (font-spec :family "Iosevka Nerd Font" :size 15)
     ))
(if (eq system-type 'darwin)
    (setq
     doom-font (font-spec :family "Iosevka Nerd Font" :size 15)
     doom-unicode-font (font-spec :family "Iosevka Nerd Font")
     ))
(if (eq system-type 'windows-nt)
    (setq
     doom-font (font-spec :family "Iosevka NF" :size 15)
     ))

;;; :ui doom-dashboard
(setq fancy-splash-image (concat doom-private-dir "splash.png"))

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

;; Line numbers are pretty slow all around. The performance boost of disabling
;; them outweighs the utility of always keeping them on.
(setq display-line-numbers-type nil)

;; When I bring up Doom's scratch buffer with SPC x, it's often to play with
;; elisp or note something down (that isn't worth an entry in my notes). I can
;; do both in `lisp-interaction-mode'.
(setq doom-scratch-initial-major-mode 'lisp-interaction-mode)

;;; :editor evil
;; Focus new window after splitting
(setq evil-split-window-below t
      evil-vsplit-window-right t)

;; Set org directory
(setq org-directory "~/org/")
;; Set org agenda files
;; - Only include *.org files (we don't want to include files under the .git directory)
;; - Note: newly added files in an editing session will not be picked up until this line is run again
(setq org-agenda-files (directory-files-recursively org-directory "\\.org$"))
;; Set projects directory
(setq projectile-project-search-path '(("~/org/") ("~/dev/" . 4)))

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
(when (eq system-type 'windows-nt)
  (setq find-program (expand-file-name "~/scoop/shims/find.exe")))

;; Set exec path
(when (eq system-type 'windows-nt)
  (setq exec-path (cons "c:/Users/Lambert/scoop/shims/" exec-path)))

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

;; Associate file extensions to modes
(add-to-list 'auto-mode-alist '("\\.(yaml|yml)\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.manifest\\'" . json-mode))
(add-to-list 'auto-mode-alist '("\\.gitconfig\\'" . gitconfig-mode))
(add-to-list 'auto-mode-alist '("\\.gitignore\\'" . gitignore-mode))

;; Set english dictionary words file for company-ispell
;; Only required on Windows.
(when (eq system-type 'windows-nt)
  (after! ispell
    (setq ispell-alternate-dictionary "~/.ispell/english-words.txt")))

(setq ispell-personal-dictionary "~/.aspell.en.pws")

;; Org-mode config
;;
(after! org
  ;; Use org-expiry to have timestamps automatically created for tasks
  (use-package! org-expiry
    :config
    (setq org-expiry-inactive-timestamps t))

  ;; Use org-contacts for managing contacts and getting birthday's in the agenda
  (use-package! org-contacts)

  ;; Log DONE with timestamp
  (setq org-log-done 'time)

  ;; Doom Emacs default is 'todo.org' but I prefer to use 'inbox.org'
  (setq +org-capture-todo-file "inbox.org")

  ;; Update the default Doom "todo" to use TODO instead of [ ]
  (setq org-capture-templates
    '(("t" "todo" entry
      (file+headline +org-capture-todo-file "Inbox")
      "* TODO %?\n%i\n%a" :prepend t)
    ("n" "notes" entry
      (file+headline +org-capture-notes-file "Inbox")
      "* %u %?\n%i\n%a" :prepend t)
    ("m" "email" entry
      (file+olp +org-capture-todo-file "Inbox")
      "* TODO Mail:%u %?\n%i\n%a" :prepend t)
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

  ;; Disable spell check for org-tables
  (defadvice org-mode-flyspell-verify (after org-mode-flyspell-verify-hack activate)
    (when (and ad-return-value (org-table-p))
      (setq ad-return-value nil)))

  ;; Make org-mode tables pretty
  (add-hook 'org-mode-hook (lambda () (org-pretty-table-mode)))
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
;;
(set-frame-parameter (selected-frame) 'alpha '(97 97))
(add-to-list 'default-frame-alist '(alpha . (97 . 97)))

;; Set transparency of emacs
 (defun transparency (value)
   "Sets the transparency of the frame window. 0=transparent/100=opaque"
   (interactive "nTransparency Value 0 - 100 opaque:")
   (set-frame-parameter (selected-frame) 'alpha value))

;; Workaround ripgrep issue on Windows
(when (eq system-type 'windows-nt)
    (setq ripgrep-arguments '("--path-separator /")))

;; Influence Tramp to use a login shell so that ~/.profile is sourced on remote
;; host resulting in $PATH being setup correctly.
;;
(after! tramp
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
)

;; macOS: Change dark/light theme
(when (eq system-type 'darwin)
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

;; Setup Homebrew related settings
;;
(when (eq system-type 'darwin)
    (defvar homebrew-prefix)
    (if (file-directory-p "/opt/homebrew/")
        (setq homebrew-prefix "/opt/homebrew/")
        (setq homebrew-prefix "/usr/local/")))

;; LSP Related settings
;;
;; Set cache directory for ccls to be under home directory rather than polutting project directories
(when (eq system-type 'darwin)
  (setq ccls-initialization-options
        `(:cache (:directory "/tmp/ccls-cache"))))

;; Set path to clangd (required when using clangd as cpp lsp)
(when (eq system-type 'darwin)
  (setq lsp-clients-clangd-executable (concat homebrew-prefix "opt/llvm/bin/clangd")))

;; macOS: Set locate to use unix locate command instead of `mdfind` because `mdfind` is not indexing
;; all dev files.
;; TODO: check if 'mdfind' can be configured to work better?
;; TODO: check if 'locate.udpatedb' is run periodically by default on macOS.
(when (eq system-type 'darwin)
  (after! ivy
    (setq counsel-locate-cmd 'counsel-locate-cmd-noregex)))

;; Enable gravatars
(setq magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))

;; Microsoft WSL: Enable opening URLs in Windows browser
(when (and (eq system-type 'gnu/linux)
           (string-match
            "Linux.*microsoft.*"
            (shell-command-to-string "uname -a")))
  (setq
   browse-url-generic-program  "/mnt/c/Windows/System32/cmd.exe"
   browse-url-generic-args     '("/c" "start")
   browse-url-browser-function #'browse-url-generic))

;; Magit hide trailing carriage returns
(setq magit-diff-hide-trailing-cr-characters t)

;; TODO: Fix ahk-comment-block-dwim. The below is not yet working
(after! ahk-mode
  (defun ahk-comment-block-dwim (arg)
    "Comment or uncomment current line or region using block notation.
  For details, see `comment-dwim'."
    (interactive "*P")
    (require 'newcomment)
    (ahk-comment-dwim)))

;; Will re-use or startup SSH Agent
(keychain-refresh-environment)

;; Make the frame title include the project name
;; Allows for easy switching to Emacs frame by project name
(setq frame-title-format
      '(""
        "%b"
        (:eval
         (let ((project-name (projectile-project-name)))
           (unless (string= "-" project-name)
             (format " in [%s] - Emacs" project-name))))))

;; eclipse-java-style is the same as the "java" style (copied from
;; cc-styles.el) with the addition of (arglist-cont-nonempty . ++) to
;; c-offsets-alist to make it more like default Eclipse formatting -- function
;; arguments starting on a new line are indented by 8 characters
;; (++ = 2 x normal offset) rather than lined up with the arguments on the
;; previous line
(defconst eclipse-java-style
  '((c-basic-offset . 4)
    (c-comment-only-line-offset . (0 . 0))
    ;; the following preserves Javadoc starter lines
    (c-offsets-alist . ((inline-open . 0)
                        (topmost-intro-cont    . +)
                        (statement-block-intro . +)
                        (knr-argdecl-intro     . 5)
                        (substatement-open     . +)
                        (substatement-label    . +)
                        (label                 . +)
                        (statement-case-open   . +)
                        (statement-cont        . +)
                        (arglist-intro  . c-lineup-arglist-intro-after-paren)
                        (arglist-close  . c-lineup-arglist)
                        (access-label   . 0)
                        (inher-cont     . c-lineup-java-inher)
                        (func-decl-cont . c-lineup-java-throws)
                        (arglist-cont-nonempty . ++)
                        )))
  "Eclipse Java Programming Style")
(c-add-style "ECLIPSE" eclipse-java-style)
(customize-set-variable 'c-default-style (quote ((java-mode . "eclipse") (awk-mode . "awk") (other . "gnu"))))

;; Function to make the background transparent when running in a terminal
(defun lgreen/remove-background-in-terminal (&optional frame)
  "Unsets the background color in terminal mode."
  (interactive)
  (or frame (setq frame (selected-frame)))
  (unless (display-graphic-p frame)
    (set-face-background 'default "unspecified-bg" frame)))

;; No longer setting background in terminal to "transparent" and instead
;; am relying on the terminal transparency to apply the Emacs background color.
;; Note: I am now addressing the flicker issue, by setting the Terminal background color
;; to match the Emacs background color.  I do this for both light and dark mode themes (i.e
;; I am setting the Terminal background accordingly for its dark/light color settings.)
;;
;; TODO consider removing the below if it is not used over a long period of time.
;;
;; (add-hook 'after-make-frame-functions 'lgreen/remove-background-in-terminal)
;; (add-hook 'window-setup-hook 'lgreen/remove-background-in-terminal)

;; We want spaces over tabs
(setq-default indent-tabs-mode nil)

;; Make comments and code keywords italics
;; Not using this anylonger because of the affect it has in Emacs running inside a terminal.
;; The italics is too thin and makes the text hard to read.
;;
;; TODO consider removing the below if it is not used over a long period of time.
;;
;; (custom-set-faces!
;;   '(font-lock-comment-face :slant italic)
;;   '(font-lock-keyword-face :slant italic))

;; In terminal mode make code comment more readable
;; TODO consider removing.
;; Note: Using ~window-system~ like below is not compatible when using Emacs in daemon
;; mode, since the daemon will never have a window-sytem and hence the brighter
;; comments config will end up applying to GUI emacsclient frames.
;; For now we either are ok with the default comments brightness or
;; we need to change it for both terminal and GUI modes.
;; There may be a way to use hooks to change things for the current frame,
;; but I am not sure how we can the frame to 'redraw' with the updated config.
;; (unless window-system
;;   (setq doom-one-brighter-comments t)
;;   )

;; Enable undo in non-file buffers
(global-undo-tree-mode)
(add-hook `evil-local-mode-hook `turn-on-undo-tree-mode)

;; BUG We should not need the below. Docs say one can set JENV_ROOT
;; so lets see if we can get rid of needing the below
(when (eq system-type 'gnu/linux)
  (setq jenv-installation-dir "/home/linuxbrew/.linuxbrew/bin/"))
(when (eq system-type 'darwin)
  (setq jenv-installation-dir "/usr/local/bin/"))

;; TODO See if there is a safer option than this
(setq-default enable-local-variables t)

(defun lgreen/json_prettify ()
  "Switch buffer to json-mode and pretty print it."
  (interactive)
  (json-mode)
  (json-pretty-print-buffer))

;; Setting to ensure mode line VC branch info is updated when
;; switching branches via Magit.
;;
;; NOTE: This setting is not enabled by default due to it not
;; being performant, and so keep an eye out for performance issues e.g.
;; if many buffers are open, then after switching a branch there may
;; be slowness.
;; Don't enable for Windows which is already very slow executing Git
(unless (eq system-type 'windows-nt)
  (setq auto-revert-check-vc-info t))

;; TODO Get TreeSitter working again when they have published aarm64 binaries
(unless (string-match-p (rx string-start "aarch64-") system-configuration)
  (use-package! tree-sitter :config
                (require 'tree-sitter-langs)
                (global-tree-sitter-mode)
                (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)))

;; On macOS use the mdfind command instead of the locate command
;; TODO Move this configuration into Doom's Vertico.el to use `consult--customize` and submit a PR
(when (eq system-type 'darwin)
  (setq consult-locate-args "mdfind -name "))

;; On Windows ignore any f15 keypress since we use Caffeine from time to time
;; and it uses the f15 key to keep the machine from falling asleep
(when (eq system-type 'windows-nt)
  (global-set-key [f15] 'ignore))

(when (eq system-type 'darwin)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)))

;; On Windows: open a cmd.exe shell
(when (eq system-type 'windows-nt)
  (defun command-shell ()
    "opens a shell which can run programs as if run from cmd.exe from Windows"
    (interactive)
    (let ((explicit-shell-file-name "cmdproxy")
          (shell-file-name "cmdproxy") (comint-dynamic-complete t))
      (shell))))

;; Dired-omit-mode is on by default, but hides files too aggresively which has
;; caused me confusion.
(setq dired-omit-extensions nil)

;; Setup mu4e
(after! mu4e
  (setq mu4e-index-cleanup nil
        ;; because gmail uses labels as folders we can use lazy check since
        ;; messages don't really "move"
        mu4e-index-lazy-check t)
  )

;; Set org-roam directory
(setq org-roam-directory "~/roam")
