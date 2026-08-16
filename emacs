;; This nonsense has to be set before *any* LSP stuff is touched or loaded by e.g. compile-directory
(setenv "LSP_USE_PLISTS" "true")

;; Don't load outdated .elc files, it's basically never what was intended.
(setq load-prefer-newer t)

(require 'comp)
(setq native-comp-speed 2)
(setq native-comp-always-compile t)

;; Recompile everything on load path. Annoyingly, auto-compile doesn't do this, it's hard-coded to block files that
;; don't have .elc, which defeats much of the point of not needing to manually first-time compile everything...
(defvar neph-compile-stuff '())
(defun neph-add-to-load-path (path dir)
  (add-to-list 'neph-compile-stuff dir)
  (add-to-list path dir))

(neph-add-to-load-path 'load-path "~/.emacs.d/neph")
(neph-add-to-load-path 'load-path "~/.emacs.d/dash") ; dependency of ht
(neph-add-to-load-path 'load-path "~/.emacs.d/emacs-ht")
(neph-add-to-load-path 'load-path "~/.emacs.d/neph-autoloads")
(neph-add-to-load-path 'load-path "~/.emacs.d/bui.el")
(neph-add-to-load-path 'load-path "~/.emacs.d/compat.el")
(neph-add-to-load-path 'load-path "~/.emacs.d/emacs-spinner")
(neph-add-to-load-path 'load-path "~/.emacs.d/s.el")
(neph-add-to-load-path 'load-path "~/.emacs.d/f.el")
(neph-add-to-load-path 'load-path "~/.emacs.d/editorconfig")
(neph-add-to-load-path 'load-path "~/.emacs.d/xterm-color")
(neph-add-to-load-path 'load-path "~/.emacs.d/eterm-256color")
(neph-add-to-load-path 'load-path "~/.emacs.d/markdown-mode")
(neph-add-to-load-path 'load-path "~/.emacs.d/evil")
(neph-add-to-load-path 'load-path "~/.emacs.d/indent-bars")
(neph-add-to-load-path 'load-path "~/.emacs.d/highlight-symbol")
(neph-add-to-load-path 'load-path "~/.emacs.d/rust-mode")
(neph-add-to-load-path 'load-path "~/.emacs.d/rustic")
(neph-add-to-load-path 'load-path "~/.emacs.d/lua-mode")
(neph-add-to-load-path 'load-path "~/.emacs.d/htmlize")
(neph-add-to-load-path 'load-path "~/neph/emacs.d/auto-complete")
(neph-add-to-load-path 'load-path "~/neph/emacs.d/js2-mode")
(neph-add-to-load-path 'load-path "~/neph/emacs.d/polymode")
(neph-add-to-load-path 'load-path "~/neph/emacs.d/skewer-mode")
(neph-add-to-load-path 'load-path "~/neph/emacs.d/emacs-web-server")
(neph-add-to-load-path 'load-path "~/neph/emacs.d/emacs-websocket")
(neph-add-to-load-path 'load-path "~/neph/emacs.d/emacs-ipython-notebook/lisp")
;(neph-add-to-load-path 'load-path "~/.emacs.d/ecb")
(neph-add-to-load-path 'load-path "~/.emacs.d/color-identifiers-mode")
(neph-add-to-load-path 'load-path "~/.emacs.d/consult")
(neph-add-to-load-path 'load-path "~/.emacs.d/vertico")
(neph-add-to-load-path 'load-path "~/.emacs.d/counsel-projectile")
(neph-add-to-load-path 'load-path "~/.emacs.d/emacs-async") ; helm dep
(neph-add-to-load-path 'load-path "~/.emacs.d/helm")
(neph-add-to-load-path 'load-path "~/.emacs.d/fzf")
(neph-add-to-load-path 'load-path "~/.emacs.d/helm-swoop")
(neph-add-to-load-path 'load-path "~/.emacs.d/helm-ag")
(neph-add-to-load-path 'load-path "~/.emacs.d/helm-rg")
(neph-add-to-load-path 'load-path "~/.emacs.d/wgrep") ;; For rg.el
(neph-add-to-load-path 'load-path "~/.emacs.d/rg.el")
(neph-add-to-load-path 'load-path "~/.emacs.d/multiple-cursors")
(neph-add-to-load-path 'load-path "~/.emacs.d/phi-search")
(neph-add-to-load-path 'load-path "~/.emacs.d/swiper")
(neph-add-to-load-path 'load-path "~/.emacs.d/company-mode")
(neph-add-to-load-path 'load-path "~/.emacs.d/company-quickhelp")
(neph-add-to-load-path 'load-path "~/.emacs.d/pos-tip")
(neph-add-to-load-path 'load-path "~/.emacs.d/jsonrpc-1.0.24")
(neph-add-to-load-path 'load-path "~/.emacs.d/copilot")
;;(neph-add-to-load-path 'load-path "~/.emacs.d/emacs-deferred")
;;(neph-add-to-load-path 'load-path "~/.emacs.d/emacs-request")
;;(neph-add-to-load-path 'load-path "~/.emacs.d/emacs-ycmd")
(neph-add-to-load-path 'load-path "~/.emacs.d/yasnippet")
(neph-add-to-load-path 'load-path "~/.emacs.d/flycheck")
(neph-add-to-load-path 'load-path "~/.emacs.d/epl")
(neph-add-to-load-path 'load-path "~/.emacs.d/pkg-info")
(neph-add-to-load-path 'load-path "~/.emacs.d/hydra")
(neph-add-to-load-path 'load-path "~/.emacs.d/ace-window")
(neph-add-to-load-path 'load-path "~/.emacs.d/pfuture")
(neph-add-to-load-path 'load-path "~/.emacs.d/avy")
(neph-add-to-load-path 'load-path "~/.emacs.d/yaml.el")
(neph-add-to-load-path 'load-path "~/.emacs.d/lsp-mode/clients")
(neph-add-to-load-path 'load-path "~/.emacs.d/lsp-mode")
(neph-add-to-load-path 'load-path "~/.emacs.d/lsp-docker")
(neph-add-to-load-path 'load-path "~/.emacs.d/treemacs/src/elisp")
(neph-add-to-load-path 'load-path "~/.emacs.d/treemacs/src/extra")
(neph-add-to-load-path 'load-path "~/.emacs.d/emacs-ccls")
(neph-add-to-load-path 'load-path "~/.emacs.d/dap-mode")
(neph-add-to-load-path 'load-path "~/.emacs.d/jsonrpc")
(neph-add-to-load-path 'load-path "~/.emacs.d/dape")
(neph-add-to-load-path 'load-path "~/.emacs.d/posframe")
(neph-add-to-load-path 'load-path "~/.emacs.d/lsp-ui")
(neph-add-to-load-path 'load-path "~/.emacs.d/lsp-pyright")
(neph-add-to-load-path 'load-path "~/.emacs.d/lsp-treemacs")
(neph-add-to-load-path 'load-path "~/.emacs.d/helm-lsp")
(neph-add-to-load-path 'load-path "~/.emacs.d/irony-mode")
(neph-add-to-load-path 'load-path "~/.emacs.d/company-irony")
(neph-add-to-load-path 'load-path "~/.emacs.d/flycheck-irony")
(neph-add-to-load-path 'load-path "~/.emacs.d/popup-el")
;;(neph-add-to-load-path 'load-path "~/.emacs.d/function-args")
(neph-add-to-load-path 'load-path "~/.emacs.d/smarttabs")
;;(neph-add-to-load-path 'load-path "~/.emacs.d/emacs-gdb")
(neph-add-to-load-path 'load-path "~/.emacs.d/ido-vertical-mode")
(neph-add-to-load-path 'load-path "~/.emacs.d/rainbow-delimiters")
(neph-add-to-load-path 'load-path "~/.emacs.d/minimap")
(neph-add-to-load-path 'load-path "~/.emacs.d/god-mode")
(neph-add-to-load-path 'load-path "~/.emacs.d/fic-mode.git")
(neph-add-to-load-path 'load-path "~/.emacs.d/git-gutter-fringe")
(neph-add-to-load-path 'load-path "~/.emacs.d/git-gutter")
(neph-add-to-load-path 'load-path "~/.emacs.d/fringe-helper")
(neph-add-to-load-path 'load-path "~/.emacs.d/rainbow-mode")
(neph-add-to-load-path 'load-path "~/.emacs.d/p4.el")
(neph-add-to-load-path 'load-path "~/.emacs.d/flyspell-lazy")
(neph-add-to-load-path 'load-path "~/.emacs.d/projectile")
(neph-add-to-load-path 'load-path "~/.emacs.d/helm-projectile")
(neph-add-to-load-path 'load-path "~/.emacs.d/php-mode/lisp")
(neph-add-to-load-path 'load-path "~/.emacs.d/web-mode")
(neph-add-to-load-path 'load-path "~/.emacs.d/yaml-mode")
(neph-add-to-load-path 'load-path "~/.emacs.d/ace-jump-mode")
(neph-add-to-load-path 'load-path "~/.emacs.d/mmm-mode")
(neph-add-to-load-path 'load-path "~/.emacs.d/mmm-jinja2")
(neph-add-to-load-path 'load-path "~/.emacs.d/salt-mode")
(neph-add-to-load-path 'load-path "~/.emacs.d/git-modes")
(neph-add-to-load-path 'load-path "~/.emacs.d/llama") ;; dep of magit
(neph-add-to-load-path 'load-path "~/.emacs.d/cond-let") ;; dep of magit
(neph-add-to-load-path 'load-path "~/.emacs.d/magit-transient/lisp")
(neph-add-to-load-path 'load-path "~/.emacs.d/magit-popup")
(neph-add-to-load-path 'load-path "~/.emacs.d/magit-ghub/lisp")
(neph-add-to-load-path 'load-path "~/.emacs.d/magit/lisp")
(neph-add-to-load-path 'load-path "~/.emacs.d/treepy")
(neph-add-to-load-path 'load-path "~/.emacs.d/with-editor/lisp") ;; Part of magit project, dep
(neph-add-to-load-path 'load-path "~/.emacs.d/ansi-color-overlay-mode")
(neph-add-to-load-path 'load-path "~/.emacs.d/gdb-ansi-color")
;(neph-add-to-load-path 'custom-theme-load-path "~/.emacs.d/sunburst-theme")
(neph-add-to-load-path 'custom-theme-load-path "~/.emacs.d/neph")
(neph-add-to-load-path 'custom-theme-load-path "~/.emacs.d/ample-zen")
;; (neph-add-to-load-path 'custom-theme-load-path "~/.emacs.d/purple-haze-theme")

(neph-add-to-load-path 'load-path "~/.emacs.d/auto-compile")

(dolist (dir neph-compile-stuff)
  (when (and dir (file-directory-p dir))
    (byte-recompile-directory dir 0)))

(setq neph-compile-stuff nil)

;; Turn on autocompile for everything else
(require 'auto-compile)
(setq auto-compile-verbose t)

(auto-compile-on-load-mode)
(auto-compile-on-save-mode)

;; Functions/macros live in neph-lib where they get byte-compiled; this file
;; does not, and keeps to wiring (requires, setqs, hooks, binds).
(require 'neph-lib)

;;
;; ---- Config merged down from neph-init.el (WIP: killing neph-init) ----
;;

;;
;; Flyspell-lazy
(require 'flyspell-lazy)
(setq flyspell-lazy-idle-seconds 1)
(setq flyspell-lazy-window-idle-seconds 1)
(global-set-key (kbd "C-c M-l") 'flyspell-lazy-toggle)

;;
;; Misc
;;

;; Set to block native compilation. Also need to nuke the ~/.emacs.d/eln-cache folder.
;;(add-to-list 'native-comp-bootstrap-deny-list ".*")
;;(add-to-list 'native-comp-deferred-compilation-deny-list ".*")

;; donut
(setq ring-bell-function 'ignore)

(require 'cl) ;; Used so xe and friends can run some crap

(setq redisplay-dont-pause t)
(setq inhibit-eval-during-redisplay nil)
;(setq fast-but-imprecise-scrolling t)
;(setq jit-lock-chunk-size 100)
;(setq jit-lock-defer-time 0)
;(setq jit-lock-stealth-load nil)
;(setq jit-lock-stealth-nice 0.01)
;(setq jit-lock-stealth-time 0.2)


;; Disable silly "type Y-E-S" prompts
(fset 'yes-or-no-p 'y-or-n-p)

(advice-add 'y-or-n-p :around #'neph-y-or-n-p)

; This just makes things slower. Maybe useful on spinning disks?
(setq cache-long-line-scans nil)
(setq cache-long-scans nil)

; Clear suspend-frame binding to use C-z as a prefix
(global-unset-key (kbd "C-z"))

(put 'upcase-region 'disabled nil)

;; Split out so that it can be auto-compiled/native-compiled
(message "loading init")
(require 'neph-init)
