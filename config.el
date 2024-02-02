;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; refresh' after modifying this file!

;; Setup Homebrew related settings
(when (eq system-type 'darwin)
  (defvar homebrew-prefix)
  (if (file-directory-p "/opt/homebrew/")
      (setq homebrew-prefix "/opt/homebrew/")
    (setq homebrew-prefix "/usr/local/")))

(when (eq system-type 'gnu/linux)
  (defvar homebrew-prefix)
  (setq homebrew-prefix "/home/linuxbrew/.linuxbrew/"))

;; Add Homebrew Emacs site-lisp to load-path
(when (eq system-type 'darwin)
  (let ((default-directory (concat homebrew-prefix "share/emacs/site-lisp")))
    (normal-top-level-add-subdirs-to-load-path)))

;; Add Homebrew Info to Info path
(when (eq system-type 'darwin)
  (add-to-list `Info-directory-list (concat homebrew-prefix "share/info/")))

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
     doom-unicode-font (font-spec :family "Iosevka Nerd Font")
     ))
(if (eq system-type 'darwin)
    (setq
     doom-font (font-spec :family "Iosevka NFM" :size 13)
     doom-unicode-font (font-spec :family "Iosevka NFM")
     doom-variable-pitch-font (font-spec :family "Iosevka NFP" :size 18)
     ))
(if (eq system-type 'windows-nt)
    (setq
     doom-font (font-spec :family "Iosevka NF" :size 15)
     ))

;; :ui doom-dashboard
(setq fancy-splash-image (concat doom-user-dir "splash.png"))

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
(setq org-directory "~/dev/my/org/")

;; Set org agenda files
;; - Only include *.org files (we don't want to include files under the .git directory)
;; - Note: newly added files in an editing session will not be picked up until this line is run again
(setq org-agenda-files (directory-files-recursively org-directory "\\.org$"))

;; Set projects directory
(setq projectile-project-search-path '(("~/dev/" . 10)))
(setq projectile-per-project-compilation-buffer t)

;; If you want to change the style of line numbers, change this to `relative' or
;; `nil' to disable it:
;; TODO Make final decision on whether to use line numbers by default or not
;; (setq display-line-numbers-type 'relative)

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
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

;; Set english dictionary words file for company-ispell
;; Only required on Windows.
(when (eq system-type 'windows-nt)
  (after! ispell
    (setq ispell-alternate-dictionary "~/.ispell/english-words.txt")))

(setq ispell-personal-dictionary "~/.aspell.en.pws")

;; Org-mode config
;;
(after! org
  ;; Use org-contacts for managing contacts and getting birthday's in the agenda
  ;; TODO Running into issues with 'org-contacts' during Doom setup and startup
  ;; (use-package! org-contacts
  ;;   :config (setq org-contacts-files '("~/dev/my/org/contacts.org")))

  ;; Load habits
  (add-to-list 'org-modules 'org-habit)


  ;; Setup org-checklist
  ;; This package enables the auto-reseting of checkbox state for repeating items.
  (use-package! org-checklist)

  ;; Use org-expiry to have timestamps automatically created for tasks
  ;; UPDATE (9/6/22): I used this for timestamp creation, however that is now part of the capture template and
  ;; so I may not need this package any longer.
  ;; TODO Remove if no longer needed.
  ;; (use-package! org-expiry
  ;;   :config
  ;;   (setq org-expiry-inactive-timestamps t))

  (use-package! org-journal
    :config
    (setq org-journal-file-type 'yearly
          org-journal-enable-agenda-integration t))

  (use-package! org-collector)

  (setq org-todo-keywords
        '((sequence
           "TODO(t!)"  ; A task that needs doing & is ready to do
           "PROJ(p!)"  ; A project, which usually contains other tasks
           "LOOP(r!)"  ; A recurring task
           "STRT(s!)"  ; A task that is in progress
           "WAIT(w@/!)"  ; Something external is holding up this task
           "HOLD(h@/!)"  ; This task is paused/on hold because of me
           "IDEA(i)"  ; An unconfirmed and unapproved task or notion
           "|"
           "DONE(d!)"  ; Task successfully completed
           "KILL(k@/!)") ; Task was cancelled, aborted or is no longer applicable
          (sequence
           "[ ](T)"   ; A task that needs doing
           "[-](S)"   ; Task is in progress
           "[?](W)"   ; Task is being held up or paused
           "|"
           "[X](D)")  ; Task was completed
          (sequence
           "|"
           "OKAY(o)"
           "YES(y)"
           "NO(n)")))

  ;; Log DONE with timestamp
  (setq org-log-done 'time)

  ;; Log rescheduling
  ;; BUG: Having trouble using the below config. It breaks the rescheduling from happening
  ;; For now I am trying to use the STARTUP option instead
  ;; (setq org-log-reschedule 'time)

  ;; Log state changes into drawer
  ;; Note: This unfortunately does not apply to scheduling and done timestamp
  (setq org-log-into-drawer t)
  (setq org-agenda-log-mode-items '(closed clock state))
  ;; Set diary file to an org file
  (setq org-agenda-diary-file "~/dev/my/org/diary.org")

  ;; Update the default Doom "todo" to use TODO instead of [ ]
  (setq org-capture-templates
        '(("t" "todo" entry
           (file+headline +org-capture-todo-file "Inbox")
           "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:FROM: %a\n:END:\n" :prepend t)
          ("n" "notes" entry
           (file+headline +org-capture-notes-file "Inbox")
           "* %u %?\n:PROPERTIES:\n:CREATED: %U\n:FROM: %a\n:END:\n" :prepend t)
          ("m" "email" entry
           (file+olp +org-capture-todo-file "Inbox")
           "* TODO Mail:%u %?\n%i\n%a" :prepend t)
          ("j" "Journal" entry
           (file+olp+datetree +org-capture-journal-file)
           "* %U %?\n%i\n%a" :prepend t)
          ("p" "Protocol" entry
           (file+headline +org-capture-notes-file "Inbox")
           "* %? [[%:link][%:description]] \nCaptured On: %U\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
          ("L" "Protocol Link" entry
           (file+headline +org-capture-notes-file "Inbox")
           "* %? [[%:link][%:description]] \nCaptured On: %U")
          ("P" "Templates for projects")
          ("Pt" "Project-local todo" entry
           (file+headline +org-capture-project-todo-file "Inbox")
           "* TODO %?\n%i\n%a" :prepend t)
          ("Pn" "Project-local notes" entry
           (file+headline +org-capture-project-notes-file "Inbox")
           "* %U %?\n%i\n%a" :prepend t)
          ("Pc" "Project-local changelog" entry
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

  ;; Always have a new line at end of Org files
  ;; BUG We are trying to get newlines at the end of source blocks
  ;; and the end of the file to have better looking highlighting
  ;; but it does not seem to be working.
  (add-hook 'org-mode-hook (lambda () (setq require-final-newline t)))

  ;; Make org-mode tables pretty
  (add-hook 'org-mode-hook (lambda () (org-pretty-table-mode)))

  ;; Bigger size font for Org headings
  ;;
  ;; Below was taken from DistroTube.
  ;; I don't quite like the color changes, but I did want to use a variable pitch font and
  ;; modify the heading sizes.
  ;;
  ;; TODO: Do some tweaking yourself
  ;; (dolist
  ;;     (face
  ;;      '((org-level-1 1.7 "#51afef" ultra-bold)
  ;;        (org-level-2 1.6 "#c678dd" extra-bold)
  ;;        (org-level-3 1.5 "#98be65" bold)
  ;;        (org-level-4 1.4 "#da8548" semi-bold)
  ;;        (org-level-5 1.3 "#5699af" normal)
  ;;        (org-level-6 1.2 "#a9a1e1" normal)
  ;;        (org-level-7 1.1 "#46d9ff" normal)
  ;;        (org-level-8 1.0 "#ff6c6b" normal)))
  ;;   (set-face-attribute (nth 0 face) nil :font doom-variable-pitch-font :weight (nth 3 face) :height (nth 1 face) :foreground (nth 2 face)))
  ;; (set-face-attribute 'org-table nil :font doom-font :weight 'normal :height 1.0 :foreground "#bfafdf")
  )

;; Whenever a 'TODO entry is created, we want a timestamp
;; UPDATE (9/5/22): We want to simply the adding of the created property by updating the capture template instead.
;; TODO Remove commented out code after validating that this has been working for a few days.
;; (defun lgreen/insert-created-timestamp()
;;   (interactive)
;;   "Insert a CREATED property using org-expiry.el for TODO entries"
;;   (org-expiry-insert-created)
;;   (org-back-to-heading)
;;   (org-end-of-line)
;;   )

;; Advice org-insert-todo-heading to insert a created timestamp using org-expiry
;; (defadvice org-insert-todo-heading (after lgreen/created-timestamp-advice activate)
;;   "Insert a CREATED property using org-expiry.el for TODO entries"
;;   (lgreen/insert-created-timestamp))

;; Advice org-capture to insert a created timestamp using org-expiry
;; (defadvice org-capture (after lgreen/created-timestamp-advice activate)
;;   "Insert a CREATED property using org-expiry.el for TODO entries"
;;   ; Test if the captured entry is a TODO, if so insert the created
;;   ; timestamp property, otherwise ignore
;;   (when (member (org-get-todo-state) org-todo-keywords-1)
;;     (lgreen/insert-created-timestamp)))

;; Stop flyspell from stealing ~M-TAB~ from OrgMode
(eval-after-load 'flyspell '(define-key flyspell-mode-map "\M-\t" nil))

;; Function to set transparency of emacs
(defun lgreen/transparency (value)
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

;; Function to add to the Emacs path
;; swiped from https://gitlab.com/xeijin-dev/doom-config/blob/master/config.org
(defun lgreen/add-to-emacs-path (append-path &rest path)
  "add PATH to both emacs own `exec-path' and to the `PATH' inherited by emacs
   from the OS (aka `process-environment'). APPEND-PATH should be non-nil if
   you want the added path to take priority over existing paths this does not
   modify the actual OS `PATH' just the two emacs internal variables which deal
   with paths:

  `exec-path' is used when executables are called from emacs directly
  `process-environment' is used when executables are called via the `shell'"

  (dolist (p path)
    (add-to-list 'exec-path p append-path))

  ;; update `process-environment' with whatever is in `exec-path' right now
  (setenv "PATH" (mapconcat #'identity exec-path path-separator))
  (message "exec-path and process-environment synchronised"))

;; LSP Related settings
;;
;; TODO Don't setup LSP unless a project needs it
;; Rather put LSP configuration in functions, and have those functions
;; called only by projects needing it.
;;
;; Set cache directory for ccls to be under home directory rather than polutting project directories
(when (eq system-type 'darwin)
  (setq ccls-initialization-options
        `(:cache (:directory "/tmp/ccls-cache"))))

;; Set path to clangd (required when using clangd as cpp lsp)
(when (eq system-type 'darwin)
  (setq lsp-clangd-binary-path "/Library/Developer/CommandLineTools/usr/bin/clangd" ))
;; (setq lsp-clients-clangd-executable "/Library/Developer/CommandLineTools/usr/bin/clangd "))

(when (eq system-type 'gnu/linux)
  (setq lsp-clangd-binary-path (concat homebrew-prefix "Cellar/llvm@14/14.0.6/bin/clangd")))
;; (setq lsp-clients-clangd-executable lsp-clangd-binary-path))

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
    (ahk-comment-dwim arg)))

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
;; TODO Make finaly decision on what to do using italics for comments and syntax
;; Note: although I have commented out this block, I am still getting italics
;; for comments and syntax!
;;
;; Update (7/24/23): Looks like this is working fine with Wezterm as the terminal
;; Update (21/21/23): Don't do italics for keywords as it will make the pipe (`|`) look
;; like a division character.
;; (custom-set-faces!
;;   '(font-lock-comment-face :slant italic))

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
;; (unless (string-match-p (rx string-start "aarch64-") system-configuration)
;;   (use-package! tree-sitter :config
;;                 (require 'tree-sitter-langs)
;;                 (global-tree-sitter-mode)
;;                 (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)))

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
(setq org-roam-directory "~/dev/my/org/roam")

;;;
;; Capture floating frame
;;
;; taken from: http://www.windley.com/archives/2010/12/capture_mode_and_emacs.shtml
;;;
(defadvice org-capture-finalize
    (after delete-capture-frame activate)
  "Advise capture-finalize to close the frame"
  (if (equal "emacsclient-capture" (frame-parameter nil 'name))
      (delete-frame)))

(defadvice org-capture-destroy
    (after delete-capture-frame activate)
  "Advise capture-destroy to close the frame"
  (if (equal "emacsclient-capture" (frame-parameter nil 'name)
             (delete-frame))))

;; BUG The dashboard window is not getting deleted
;; I have validated the body works as expected in the org-protocol capture frame
(defadvice org-switch-to-buffer-other-window
    (after delete-other-window activate)
  "Advise org-switch-to-buffer-other-window to delete the extra window if we're in a capture frame"
  (if (equal "emacsclient-capture" (frame-parameter nil 'name))
      (delete-other-windows)))

;; TODO Remove this as it is currently unused
;; The emacsclient command-line already provides the frame details in the -F parameter
;; I could not get emacsclient to both eval elisp code and still activate on the org-protocol
;; input parameter
(defun make-capture-frame ()
  "Create a new frame and run org-capture."
  (interactive)
  (make-frame '((name . "emacsclient-capture")
                (width . 120)
                (height . 15)))
  (select-frame-by-name "emacsclient-capture")
  (setq word-wrap 1)
  (setq truncate-lines nil)
  (org-capture))

;; Below code snippet acquired from here:
;; - https://stackoverflow.com/questions/10969617/hiding-markup-elements-in-org-mode
(defun lgreen/org-toggle-emphasis ()
  "Toggle hiding/showing of org emphasize markers."
  (interactive)
  (if org-hide-emphasis-markers
      (set-variable 'org-hide-emphasis-markers nil)
    (set-variable 'org-hide-emphasis-markers t))
  (org-mode-restart))

;; Allow selecting the catppuccin flavor
(defun lgreen/select-catppuccin-flavor ()
  "Select and apply a catppuccin flavor."
  (interactive)
  (let* ((flavors '("mocha" "macchiato" "latte" "frappe"))
         (flavor (completing-read "Choose a catppuccin flavor: " flavors nil t)))
    (setq catppuccin-flavor (intern flavor))
    (catppuccin-reload)
    (message "Switched to %s flavor" flavor)))


;; Just files
;; TODO: Is this config needed? We disable it because it
;; uses up a valuable binding i.e.`e'
;; (use-package! justl)

;; Count lines across folds
(defun lgreen/count-visible-lines (start end)
  "Count the number of visible lines in the region."
  (interactive "r")
  (save-excursion
    (goto-char start)
    (let ((count 0))
      (while (< (point) end)
        (unless (invisible-p (point))
          (setq count (1+ count)))
        (forward-line 1))
      (message "Number of visible lines: %d" count))))

(after! lsp-clangd
  ;; TODO Fix hardcoded path
  (setq lsp-clangd-binary-path "/opt/homebrew/opt/llvm/bin/clangd")
  (setq lsp-clients-clangd-args '("--background-index"
                                  "--clang-tidy"
                                  "--completion-style=detailed"
                                  "--header-insertion=iwyu")))


;; BUG We are trying to fix org-babel source code block highlighting
;; but thus far nothing is working.
;;
;; Fix org-babel source blocks to always have a newline afterwards
;; so that the code block background color does not end up showing
;; on ancestor folded headings
(defun lgreen/add-newline-end-of-babel-blocks ()
  (interactive)
  (when (and (derived-mode-p 'org-mode)
             (save-excursion
               (goto-char (point-min))
               (search-forward "#+end_src" nil t)))
    (save-excursion
      (goto-char (point-max))
      (unless (looking-at "^")
        (insert "\n")))))

(add-hook 'before-save-hook #'lgreen/add-newline-end-of-babel-blocks)

(after! jq-mode
  (add-to-list 'auto-mode-alist '("\\.jq\\'" . jq-mode))
  ;; Add jq for org-babel
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((jq . t))))

;; Below instructions obtained from org-roam-ui github page
;;
(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :after org-roam ;; or :after org
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;; :hook (after-init . org-roam-ui-mode)
  :config (setq org-roam-ui-sync-theme t
                org-roam-ui-follow t
                org-roam-ui-update-on-save t
                org-roam-ui-open-on-start t))
