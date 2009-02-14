
;; possibly controversial as a global default, but shipping a lisp
;; that dies trying to talk to slime is stupid, so:
(set-language-environment "UTF-8")
(setq slime-net-coding-system 'utf-8-unix)

;; load slime:
(setq load-path (cons "/Users/stig/clbuild/source/slime" load-path))
(setq load-path (cons "/Users/stig/clbuild/source/slime/contrib" load-path))
(setq slime-backend "/Users/stig/clbuild/.swank-loader.lisp")
(setq inhibit-splash-screen t)
(load "/Users/stig/clbuild/source/slime/slime")
(setq inferior-lisp-program "/Users/stig/clbuild/clbuild --implementation sbcl lisp")
(setq slime-use-autodoc-mode nil)
(slime-setup '(slime-fancy slime-tramp slime-asdf))
(slime)
