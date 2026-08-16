;; -*- mode: Emacs-Lisp; -*-

;; function-args modes (Disabled pending semantic)
;;;;(require 'function-args)
;;(fa-config-default)
;;(setq moo-select-method 'helm)

;; For web mode in tabs, we want to disable whitespace tabs because they conflict with the
;; php-background-coloring.  In space mode we can just use neph-space-cfg, as we want to highlight
;; errant tabs.  BUT - whitespace mode needs to be re-started when screwing with this variable.

;;
;; Package
;;

;; Disabled
;;(require 'package)
;;(add-to-list 'package-archives
;;             '("marmalade" .
;;               "http://marmalade-repo.org/packages/"))
;;(package-initialize)

;;
;; Line numbers
;;

;; OLD: linum
;; (setq linum-format " %d ")
;; (require 'linum)
;; (setq linum-delay t)
;; (setq linum-eager nil)
;;;; Disabled even when linum was active:
;; (global-linum-mode 1)
;; (setq linum-disabled-modes-list '(term-mode))
;; (defun linum-on()
;;   (unless (or (minibufferp) (string-equal mode-name "Helm") (member major-mode linum-disabled-modes-list))
;;     (linum-mode 1)))

(provide 'neph-init)
;;; neph-init.el ends here
