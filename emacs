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


;;
;; Highlight/unhighlight dwim binds (neph-highlight-dwim / neph-unhighlight-dwim in neph-lib)
;;
(global-set-key (kbd "C-z H") 'neph-highlight-dwim)
(global-set-key (kbd "C-z C-H") 'neph-unhighlight-dwim)


;;
;; Rust mode
;;

(require 'rustic)

(add-to-list 'auto-mode-alist '("\\.rs\\'" . rustic-mode))
(setq rustic-indent-offset 2)
(setq rust-indent-offset 2)


;;
;; Lua mode
;;

(autoload 'lua-mode "lua-mode"
   "Major mode for editing Lua files" t)
(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))

;; Defaults to 3. What in the goddamn.
(setq lua-indent-level 2)


;;
;; Htmlize
(autoload 'htmlize-buffer "htmlize" "htmlize" t)

;(global-set-key (kbd "C-z M-w") 'neph-html-copy)

;;
;; htmlfontify
;;
;; Sometimes htmlize fails on some buffers, sometimes htmlfontify does :-/ :-/

;; :height 96 should be 10pt, ends up at 9pt, increase this a little because idk
(with-eval-after-load "htmlfontify"
  (setq hfy-font-zoom 1.09))

;;
;; Multi-term
;(load-file "~/.emacs.d/multi-term.el")
;(setq multi-term-program "/bin/bash")
;
;(global-set-key (kbd "C-x t") 'multi-term-dedicated-open)

;; Term key overrides
(with-eval-after-load 'term
  (define-key term-raw-map (kbd "C-y") 'term-paste)
  ;; Allow C-z to escape
  (define-key term-raw-map (kbd "C-z") nil)
  ;; But make C-z C-z send a real C-z
  (define-key term-raw-map (kbd "C-z C-z") 'term-send-raw-C-z))

;;
;; ECB
;;

;;(require 'ecb)
;(setq ecb-show-sources-in-directories-buffer 'always)
;(setq ecb-layout-name "left7")
;(setq ecb-tip-of-the-day nil)
;(setq ecb-windows-width 0.1)
;
;;; Quiet startup warning
;(setq ecb-options-version "2.40")
;
;(global-set-key (kbd "C-z q") 'ecb-activate)
;(global-set-key (kbd "C-z Q") 'ecb-deactivate)

;;
;; Color identifiers mode
;;

(require 'color-identifiers-mode)

;;
;; Consult/Vertico
;;

;; WIP
;;(require 'consult)
;;(require 'vertico)
;;(require 'counsel-projectile)

;;
;; Project
;;

;; WIP, replace projectile? Might not have the things we want
;;(require 'project)

;;(global-set-key (kbd "C-z M-f") 'project-find-file)

;;
;; Helm
;;

;;(require 'helm-autoloads)
(require 'helm)
(require 'helm-mode)
(require 'helm-command)
(require 'helm-bookmark)
;; Not sure what I'm configuring wrong but autoloads doesn't always
(require 'helm-for-files)
;;(require 'helm-config)
(require 'helm-for-files)

(helm-mode 1)

;; Since 215005e25718 helm's default score func is just crazy broken
;; and puts really-fuzzy matches above extremely-direct matches
(setq helm-fuzzy-default-score-fn 'helm-fuzzy-helm-style-score)
;;Default: (setq helm-fuzzy-default-score-fn 'helm-fuzzy-flex-style-score)

(global-set-key (kbd "C-z F") 'neph-helm-find-in-directory)
(global-set-key (kbd "M-x") 'helm-M-x)

;; Note: was shadowed by helm-projectile-switch-to-buffer prior to elpacification, commented
;;(global-set-key (kbd "C-z b") 'helm-mini)
(global-set-key (kbd "C-z C-b") 'helm-filtered-bookmarks)
(global-set-key (kbd "C-z C-o") 'helm-occur)
(global-set-key (kbd "C-z C-S-o") 'occur)
(global-set-key (kbd "C-M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-z <C-tab>") 'helm-imenu)

;; Blows up helm on emacs 25 right now
;; (setq helm-follow-mode-persistent nil)

(define-key isearch-mode-map (kbd "C-o") 'helm-occur-from-isearch)
(define-key isearch-mode-map (kbd "C-S-o") 'isearch-occur)

(when (executable-find "ack-grep")
  (setq helm-grep-default-command "ack-grep -Hn --no-group --no-color --smart-case --type-set IGNORED:ext:P,map --noIGNORED %p %f"
        helm-grep-default-recurse-command "ack-grep -H --no-group --no-color --smart-case --type-set IGNORED:ext:P,map --noIGNORED %p %f"))

;; helm-grep ripgrep ;; -color=always --colors 'match:fg:black' --colors 'match:bg:yellow'
(setq helm-grep-ag-command "rg --smart-case --no-heading --line-number %s %s %s")
(setq helm-grep-ag-pipe-cmd-switches '())

(require 'grep)
(setq grep-find-ignored-files (append grep-find-ignored-files
        '( ;; Binaries
          "*.pdb" "*.map" "*.P" "*.dylib" "*.lib" "*.a" "*.dSYM" "*.app" "*.framework" "*.dll"
           "*.so.0" "*.so" "*.o" "*.exe" "*.dbg" "*.sys" "*.h.gch"

           ;; Python compiled thing
           "*.pyd" "*.pyc"

           ;; Archives
           "*.zip" "*.rar" "*.7z" "*.xz" "*.bz2" "*.gz" "*.tar" "*.dmg" "*.deb" "*.rpm" "*.iso"
           "*.msi"

           ;; Source engine cruft
           "*.vtf" "*.vvd" "*.vcd" "*.phy" "*.mdl" "*.dmx" "*.bsp" "*.vpk" "*.vtx"
           "*.fbx" "*.vmt" "*.vmf" "*.dds" "*.smd" "*.nav" "*.vcs" "*.pcf" "*.dem"
           "*.lmp"
           "soundcache/*.manifest"
           "reslists/*.txt"
           "reslists_xbox/*.lst"
           "*.xsiaddon"

           ;; Misc
           "*.ma" "*.mll" ; Maya
           "*.cache"
           "*.svn-base"
           "*.sdf" ; Visual studio database thing
           "*.al" ; Perl cruft
           "*.ppm"
           "*.vcproj" "*.vcxproj"

           ;; PS3 compiled file... thing
           "*.prx" "*.sprx"

           ;; Misc Media
           "*.raw" "*.ani" "*.bik" "*.dat" "*.ttf" "*.pdf" "*.max"

           ;; Images
           "*.tga" "*.jpg" "*.jpeg" "*.png" "*.bmp" "*.psd" "*.cbr" "*.icns" "*.ico" "*.gif"

           ;; Sound
           "*.wav" "*.ogg" "*.mp3"

           ;; Video
           "*.h264" "*.mkv" "*.avi" "*.mp4" "*.mov" "*.webm"

           ;; Oneoffs
           "ip-country-region-city-latitude-longitude-isp.csv"
           "engine_symbols.txt"
           "dedicated_symbols.txt"
           "staging_latest_good.txt")))

(when (functionp 'remove-duplicates)
  (remove-duplicates grep-find-ignored-files :test 'string=))

;; Use ncdu to look at not-ignored files in a directory in this list:
;; (concat "ncdu " (mapconcat (lambda (x) (concat "--exclude '" x "'")) grep-find-ignored-files " "))

;;
;; FZF
;;

;; FIXME Ignore stuff like .ccls-cache by customizing process-environment with defadvice:
;;   (let ((process-environment
;;         (cons (concat "FZF_DEFAULT_COMMAND=git ls-files")
;;               process-environment))

(setenv "FZF_DEFAULT_COMMAND" "rg --files --no-ignore-vcs --hidden")
(setenv "FZF_DEFAULT_OPTS" nil)
(require 'fzf)
(global-set-key (kbd "C-z C-S-f") 'fzf)
(global-set-key (kbd "C-z C-S-M-f") 'fzf-find-file-in-dir)
(setq fzf/args "--no-hscroll --print-query -x --no-unicode")

(setq fzf/window-height 50)

;;
;; Helm Swoop
;;
(require 'helm-swoop)

(global-set-key (kbd "C-z M-s") 'helm-swoop)
(global-set-key (kbd "C-z M-S") 'helm-multi-swoop-all)

;;
;; Helm AG and Helm RG and RG they're all different
;;

(require 'helm-ag)

(setq helm-ag-insert-at-point t)
;; (setq helm-ag-always-set-extra-option t)

(define-key helm-find-files-map (kbd "M-g") 'helm-ff-run-grep-ag)
(add-to-list 'helm-sources-using-default-as-input helm-source-do-ag)
(add-to-list 'helm-sources-using-default-as-input 'helm-ag-source)
;; Helm's auto-affinity thing seems to massively slow it down when the system is
;; under heavy load, even if that load is in low priority compilation cgroups.
;;
;; A common query with all files in cache goes from 20s -> 2s for me with this,
;; similar to running the query on an idle system. It sounds like this affinity
;; thing is trying to work around poor OS-level behavior to begin with, but with
;; it disabled the Right Thing™ seems to happen on my systems.
(setq helm-ag-base-command (concat helm-ag-base-command " --noaffinity"))

(global-set-key (kbd "C-M-z C-M-n") 'neph-helm-ag-next)
(global-set-key (kbd "C-M-z C-M-p") 'neph-helm-ag-prev)
(global-set-key (kbd "C-M-z C-M-g") 'neph-helm-ag-update)


;; RG version (needs helm-projectile-ag fix)
;(setq helm-ag-base-command "rg --vimgrep --no-heading")
;; Older fix:
;;(setq helm-ag-base-command "rg --color=never --with-filename --no-heading")
;;(defun helm-ag--construct-ignore-option (pattern)
;;  (concat "-g !" pattern))

;; Most keybinds in projectile below

;;
;; Helm RG
;;
(require 'helm-rg)

(setq helm-rg-default-extra-args '("--max-columns=120" "--max-columns-preview"))

(add-hook 'neph-rg-bounce-navigation-mode-hook 'neph-rg-bounce-navigation-mode-handler)
(define-key helm-rg--bounce-mode-map (kbd "C-c C-e") #'neph-rg-bounce-navigation-mode)

(define-key neph-rg-bounce-navigation-mode-map (kbd "g") #'helm-rg--bounce-refresh)
(define-key neph-rg-bounce-navigation-mode-map (kbd "r") #'helm-rg--bounce-refresh-current-file)
(define-key neph-rg-bounce-navigation-mode-map (kbd "d") #'helm-rg--bounce-dump)
(define-key neph-rg-bounce-navigation-mode-map (kbd "D") #'helm-rg--bounce-dump-current-file)
(define-key neph-rg-bounce-navigation-mode-map (kbd "RET") #'neph-rg-bounce-visit-current-file)
(define-key neph-rg-bounce-navigation-mode-map (kbd "C-o") #'helm-rg--visit-current-file-for-bounce)
(define-key neph-rg-bounce-navigation-mode-map (kbd "e") #'helm-rg--expand-match-context)
(define-key neph-rg-bounce-navigation-mode-map (kbd "E") #'helm-rg--spread-match-context)
(define-key neph-rg-bounce-navigation-mode-map (kbd "q") #'kill-this-buffer)

;; Defaults on
(add-hook 'helm-rg--bounce-mode-hook 'neph-rg-bounce-navigation-mode)

;;
;; RG
;;
(require 'rg)

;;
;; multiple-cursors
;;

(require 'multiple-cursors)

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-.") 'mc/unmark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-,") 'mc/unmark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)


;;
;; phi-search
(autoload 'phi-search "phi-search" "Phi Search." t)

(global-set-key (kbd "C-S-s") 'phi-search)
(global-set-key (kbd "C-S-r") 'phi-search-backward)

(with-eval-after-load "phisearch"
  (define-key phi-search-default-map (kbd "C-.") 'kill-phisearch-match))


;;
;; Swiper
(autoload 'swiper "swiper" "Swiper popup thing" t)
(global-set-key (kbd "C-z s") 'swiper)

(define-key isearch-mode-map (kbd "C-z s") 'isearch-to-swiper)


;;
;; Company mode
;;
;;(require 'neph-company-autoload)
(require 'company)

;; Turn on in these modes
(add-hook 'c-mode-common-hook   'neph-company-setup)
(add-hook 'python-mode-hook     'neph-company-setup)
(add-hook 'python-ts-mode-hook  'neph-company-setup)
(add-hook 'lisp-mode-hook       'neph-company-setup)
(add-hook 'emacs-lisp-mode-hook 'neph-company-setup)

;; Semantic
; (require 'semantic)
; (require 'semantic/bovine/gcc)
; (global-semantic-decoration-mode t)
; (global-semantic-stickyfunc-mode t)
; (global-semantic-idle-scheduler-mode -1)

;; EDE
;;(global-ede-mode t)

;; Keys for C++ completion and such
;;(global-set-key (kbd "C-z SPC") 'helm-semantic)
;;(global-set-key (kbd "C-z C-SPC") 'moo-jump-local)

;;
;; Copilot
;;
(require 'copilot)

(global-set-key (kbd "C-M-<tab>") 'copilot-panel-complete)
;; This is apparently C-S-<tab>
(global-set-key (kbd "C-<iso-lefttab>") 'copilot-complete)
(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "C-e") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "C-k") 'copilot-clear-overlay)
(define-key copilot-completion-map (kbd "C-M-n") 'copilot-accept-completion-by-line)
(define-key copilot-completion-map (kbd "M-f") 'copilot-accept-completion-by-word)
(define-key copilot-completion-map (kbd "M-n") 'copilot-next-completion)
(define-key copilot-completion-map (kbd "M-p") 'copilot-previous-completion)

;;
;; YouCompleteMe (deprecated for LSP, remove?)
;;

;; Deps

;;(require 'neph-ycmd-autoload)
;;
;;(with-eval-after-load "company-ycmd" (company-ycmd-setup))
;;(with-eval-after-load "ycmd"
;;  (setq ycmd-server-command '("python" "/usr/share/ycmd/ycmd")))
;;
;;(defun neph-ycm-setup ()
;;  (interactive)
;;  (require 'company-ycmd)
;;  (ycmd-mode 1))
;;
;;(add-hook 'python-mode-hook 'neph-ycm-setup)

;;
;; Yasnippet
;;

(require 'yasnippet)

;;
;; Flycheck
;;

(autoload 'flycheck-mode "flycheck" "flycheck-mode" t)

;;
;; C++ Helper mode(s) : Company/lsp and associated helper libraries
;;

(require 'lsp-mode)
(require 'company)
(require 'company-quickhelp)

(advice-add (if (progn (require 'json)
                       (fboundp 'json-parse-buffer))
                'json-parse-buffer
              'json-read)
            :around
            #'lsp-booster--advice-json-parse)
(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)

(setq company-quickhelp-color-background "black")

;; LSP performance recommended
(setq read-process-output-max 1048576)
(setq gc-cons-threshold 100000000)

(setq lsp-lens-enable nil)

;; FIXME?
;;(with-eval-after-load 'lsp-mode
;;  (add-hook 'lsp-after-open-hook (lambda () (lsp-ui-flycheck-enable 1))))

;; ~/.config/clangd/config.yaml:
;; # https://clangd.llvm.org/config
;;   CompileFlags:
;;     Add: [-Wall]
(setq lsp-clients-clangd-args '("--header-insertion-decorators=1" "--query-driver=/usr/bin/**/clang-*,/usr/bin/**/clang++-*,/usr/bin/**/gcc-*,/usr/bin/**/g++-*,/usr/bin/g++,/usr/bin/gcc,/usr/bin/clang,/usr/bin/clang++" "--enable-config"
                                "-j" "50" "--log=info"
                                "--all-scopes-completion" "--background-index" "--rename-file-limit=0"
                                "--background-index-priority=normal" "--limit-references=0" "--limit-results=0"))

;;
;; Fix intelephense
;;

;; The vscode extension allows passing this based on the intelephense.maxMemory setting (which isn't actually an
;; intelephense setting and glues this --max-old-space-size option into some node launching glue somewhere.)
;; FIXME lsp-package-path doesn't work if intelephense isn't installed and i gave up on reading the garbage code
;;(with-eval-after-load "lsp-php"
;;  (setq lsp-intelephense-server-command
;;        (list "env" "NODE_OPTIONS=\"--max-old-space-size=24000\""
;;              ;; Default path lookup the package does -- by putting 'env' first it breaks the register-time looking up
;;              ;; of the path to the nested server, which isn't on PATH if it's auto-installed.
;;              (or (executable-find "intelephense") (lsp-package-path 'intelephense))
;;              "--stdio")))

;; cquery
(setq lsp-pyright-multi-root nil)
(setq lsp-pyright-langserver-command "pyright")

;; Pyright settings are snapshot on library load??
(setq lsp-pyright-multi-root nil)
(require 'lsp-pyright)

(require 'lsp-treemacs)
(lsp-treemacs-sync-mode 1)

(require 'treemacs)
(require 'treemacs-mouse-interface)
(require 'treemacs-hydras)
;;(require 'treemacs-projectile)

(require 'pkg-info)

(require 'lsp-ui)
(require 'lsp-ui-flycheck)
(require 'lsp-headerline)
(require 'lsp-modeline)
(require 'lsp-diagnostics)
(setq lsp-ui-doc-show-with-cursor t)
(setq lsp-ui-peek-always-show t)

(require 'dap-mode)
;;(require 'dap-cpptools)
(require 'dap-ui)
(require 'dap-mouse)
(require 'dap-hydra)

(require 'jsonrpc)

;; Split out so that it can be auto-compiled/native-compiled
(message "loading init")
(require 'neph-init)
