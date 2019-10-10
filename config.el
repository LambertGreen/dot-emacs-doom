;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here

;; Set font
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
