;; Got information from the below Reddit post:
;; https://www.reddit.com/r/emacs/comments/p82jw2/doom_emcas_nativecomp_gets_stuck_when_compiling/
;;
;; We disable Doom's pre-compilation step, which can take a long time and is error prone.
;; Along with the below change we also enable 'native-comp-deferred-compilation' in 'init.el'.
(advice-add #'native-compile-async :override #'ignore)

