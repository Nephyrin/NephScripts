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

; Fix x clipboard
(setq x-select-enable-primary nil)
(setq x-select-enable-clipboard t)
(setq mouse-drag-copy-region nil)
(when (boundp 'x-cut-buffer-or-selection-value)
  (setq interprogram-paste-function 'x-cut-buffer-or-selection-value))

;(global-set-key (kbd "C-{") 'clipboard-yank)
;(global-set-key (kbd "C-}") 'clipboard-kill-ring-save)
;(global-set-key (kbd "C-M-}") 'clipboard-kill-region)
;(global-set-key "\C-w" 'clipboard-kill-region)
;(global-set-key "\M-w" 'clipboard-kill-ring-save)
;(global-set-key "\C-y" 'clipboard-yank)
(setq yank-pop-change-selection t)
(setq save-interprogram-paste-before-kill t)

(setq inhibit-startup-message t)

(setq-default indent-tabs-mode nil)
(setq js-indent-level 2)
(setq tab-width 2)

(global-auto-revert-mode t)

(setq backup-directory-alist
      `((".*" . , "~/.emacscache/autosave")))
(setq auto-save-file-name-transforms
      `((".*" , "~/.emacscache/autosave" t)))
(setq backup-directory-alist `(("." . "~/.emacscache/backup")))
(setq delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

(setq vc-follow-symlinks t)

(require 'uniquify)
(setq uniquify-buffer-name-style (quote post-forward))

; (global-ede-mode t)

; Hide toolbar, hide menu in console mode
(menu-bar-mode -1)
; OS X builds can lack these, check
(when (functionp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (functionp 'tool-bar-mode)   (tool-bar-mode -1))

(setq split-width-threshold 240)
(setq split-height-threshold 50)
;; TODO customize display-buffer alist so we don't split frames too aggressively for browsing top-level buffers, but do
;; for things like xref popups.  Might require also tweaking split-window-sensibly or overriding the split-window
;; parameters when entering display buffer with a top-level vs widget window.
;; (setq display-buffer-alist '("\\*Async Shell Command\\*" (display-buffer-no-window))

(require 'speedbar)
(speedbar-change-initial-expansion-list "buffers")

(global-set-key  [f8] 'speedbar-get-focus)
(global-set-key (kbd "C-c C-f") 'find-dired)

; Trailing spaces and whitespace
(require 'whitespace)
(global-whitespace-mode)
; Options list of whitespace to mess with, 'face' option uses faces per type
; instead of replacement chars
(setq whitespace-style (quote (face trailing tabs)))

;;
;; Electric mode tweaks
;;

(setq electric-pair-inhibit-predicate 'neph-electric-pair-inhibit-predicate)

;;
;; Mark & Mark Ring
;;

(global-set-key (kbd "C-x p") 'pop-to-mark-command)
(setq set-mark-command-repeat-pop t)

;;
;; Snippets
;;

; Recompile all .elc.  The 0 tells us to compile files that have no .elc
; already. Yes it should be 0, not t. Append t as third arg to force.

; (byte-recompile-directory "~/.emacs.d/" 0)
;   or command line:
; emacs -batch -f batch-byte-compile *.el

;; (progn
;;   (setq kill-ring nil)
;;   (setq buffer-undo-tree nil)
;;   (garbage-collect))

;;
;; Desktop saving
;;

;; Autosave desktop as emacs-server-desktop for the server, otherwise leave
;; disabled unless asked for
(require 'desktop)
(setq desktop-path '("~/.emacs.d/"))
(setq desktop-dirname "~/.emacs.d/")
(setq desktop-base-file-name "emacs-desktop")
(setq desktop-base-lock-name "emacs-desktop.lock")
(setq desktop-restore-eager 0)
(setq desktop-save t)
(add-to-list 'desktop-globals-to-save 'register-alist)
(when (or server-mode (daemonp))
  (setq desktop-base-file-name "emacs-server-desktop")
  (setq desktop-base-lock-name "emacs-server-desktop.lock")
  (desktop-save-mode 1))

;; Global libraries macros in here (and also )
(require 'ht)


;;
;; Xterm color
;;
(require 'xterm-color)


;(require 'eterm-256color) FIXME debug-init

;;(add-hook 'term-mode-hook #'eterm-256color-mode)


;;
;; ansi color mode
;;
(require 'ansi-color-overlay-mode)

;;
;; gdb ansi color
;;
(require 'gdb-ansi-color)
(add-hook 'gud-mode-hook #'gdb-ansi-color-mode)

;;
;; Protobuf mode
;;

;; Shipped with protobuf, so load if present
(if (require 'protobuf-mode nil t)
    (add-to-list 'auto-mode-alist '("\.proto$" . protobuf-mode))
  ;; Basically functions
  (message "NEPH -- No protobuf-mode available, using c-mode for .proto")
  (add-to-list 'auto-mode-alist '("\.proto$" . c-mode)))

;;
;; Markdown mode
;;

(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))


;;
;; Evil
;;

;(require 'neph-evil-autoload)
;(global-set-key (kbd "C-z C-M-SPC") 'evil-mode)


;;
;; Indent bars
;;
(require 'indent-bars)
(require 'indent-bars-ts)
(setq indent-bars-width-frac 0.05)

(setq indent-bars-treesit-support t)
(setq indent-bars-treesit-wrap '((python argument_list parameters
                                         list list_comprehension
                                         dictionary dictionary_comprehension
                                         parenthesized_expression subscript)))
(setq indent-bars-treesit-ignore-blank-lines-types '("module"))

(setq indent-bars-prefer-character nil)
(setq indent-bars-depth-update-delay 0.0)

;; SiGnIfiCaNt WhItEsPaCe
(add-hook 'python-mode-hook 'indent-bars-mode)
(add-hook 'python-ts-mode-hook 'indent-bars-mode)


;;
;; Highlight Symbol
;;
(require 'highlight-symbol)

;; This hack fixes highlight-symbol-mode perf, but breaks the explicit commands
;; See https://github.com/nschum/highlight-symbol.el/issues/26
;(defun highlight-symbol-add-symbol-with-face (symbol face)
;  (save-excursion
;    (goto-char (point-min))
;    (while (re-search-forward symbol nil t)
;      (let ((ov (make-overlay (match-beginning 0)
;                              (match-end 0))))
;        (overlay-put ov 'highlight-symbol t)
;        (overlay-put ov 'face face)))))
;
;(defun highlight-symbol-remove-symbol (_symbol)
;  (dolist (ov (overlays-in (point-min) (point-max)))
;    (when (overlay-get ov 'highlight-symbol)
;      (delete-overlay ov))))

;; TODO Should this merge with highlight-symbol? mostly I want highlight-phrase and highlight-regexp but with
;; highlight-symbol's added functionality, it's odd that highlight-symbol didn't build on the former.

(setq highlight-symbol-idle-delay 0.3)


;; Split out so that it can be auto-compiled/native-compiled
(message "loading init")
(require 'neph-init)
