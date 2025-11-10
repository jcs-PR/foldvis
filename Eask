;; -*- mode: eask; lexical-binding: t -*-

(package "foldvis"
         "0.1.0"
         "Display indicators for various folding systems")

(website-url "https://github.com/emacs-vs/foldvis")
(keywords "convenience" "folding")

(package-file "foldvis.el")
(files "foldvis-*.el")

(script "test" "echo \"Error: no test specified\" && exit 1")

(source 'gnu)
(source 'melpa)
(source 'jcs-elpa)

(depends-on "emacs" "26.1")
(depends-on "elenv")
(depends-on "fringe-helper")

(development
 (depends-on "ts-fold")
 (when (version<= "29.1" emacs-version)
   (depends-on "treesit-fold")))

(setq network-security-level 'low)  ; see https://github.com/jcs090218/setup-emacs-windows/issues/156#issuecomment-932956432
