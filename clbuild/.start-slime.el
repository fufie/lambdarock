
;; possibly controversial as a global default, but shipping a lisp
;; that dies trying to talk to slime is stupid, so:
(set-language-environment "UTF-8")
(setq slime-net-coding-system 'utf-8-unix)

;; load slime:
(setq load-path (cons "/Users/stig/lambdarock/clbuild/source/slime" load-path))
(setq load-path (cons "/Users/stig/lambdarock/clbuild/source/slime/contrib" load-path))
(setq slime-backend "/Users/stig/lambdarock/clbuild/.swank-loader.lisp")
(setq inhibit-splash-screen t)
(load "/Users/stig/lambdarock/clbuild/source/slime/slime")
(setq inferior-lisp-program "/Users/stig/lambdarock/clbuild/clbuild --implementation sbcl lisp")
(setq slime-use-autodoc-mode nil)
(slime-setup '(slime-fancy slime-tramp slime-asdf))
(slime)
