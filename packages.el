;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)

(package! groovy-mode)
(package! powershell)
(package! ahk-mode)
;; TODO Get SSH working fine on Windows
;; I am running into an issue which I think stems from using OpenSSH from Windows
;; (package! ssh-agency)
(package! dash-at-point)
(package! vimrc-mode)
(package! pdf-tools)
(package! apples-mode)
(package! ob-applescript)
(package! typit)
;; (package! p4 :recipe (:host github :repo "gareth-rees/p4.el"))
(package! jenv :recipe (:host github :repo "shellbj/jenv.el"))
(package! keychain-environment :recipe (:host github :repo "LambertGreen/keychain-environment"))
(package! git-link :recipe (:host github :repo "sshaw/git-link"))
(package! bats-mode)
(package! sword-to-org :recipe (:host github :repo "alphapapa/sword-to-org"))
(package! dtk :recipe (:host github :repo "dtk01/dtk"))
(package! org-mime :recipe (:host github :repo "org-mime/org-mime"))
(package! rfc-mode)
(package! emacs-bazel-mode :recipe (:host github :repo "bazelbuild/emacs-bazel-mode"))
(package! nix-mode)
(package! org-pretty-table :recipe (:host github :repo "Fuco1/org-pretty-table"))
(package! tree-sitter)
(package! tree-sitter-langs)
(package! ov-highlight :recipe (:host github :repo "jkitchin/ov-highlight"))
(package! palette)
(package! mermaid-mode)
(package! gn-mode)
(package! tldr)
(package! pylookup)
(package! dot-mode :recipe (:host github :repo "wyrickre/dot-mode"))
