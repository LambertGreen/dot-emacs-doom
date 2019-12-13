;;; c:/Users/lgreen/.doom.d/benchmarking.el -*- lexical-binding: t; -*-

(defun msys-dir ()
    (shell-command "c:/msys64/usr/bin/dir.exe ~ > tmp.txt") )

(defun cyg-dir ()
    (shell-command "c:/cygwin/bin/dir.exe ~ > tmp.txt") )


;(my_fun)
(print (benchmark-run 10 (msys-dir) ))
(print (benchmark-run 10 (cyg-dir) ))
