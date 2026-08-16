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

; Clear suspend-frame binding to use C-z as a prefix
(global-unset-key (kbd "C-z"))

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

(require 'dape)

;; Dape config
(add-hook 'dape-display-source-hook 'pulse-momentary-highlight-one-line)
(setq dape-inlay-hints t)
(setq dape-cwd-function 'projectile-project-root)

(require 'helm-lsp)

;; Use helm-lsp-workspace-symbol to replace xref-find-apropos (recommended by helm-lsp readme)
(define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol)

(global-set-key (kbd "C-z M-l")   'neph-ccls-reformat-definition)
;; LSP UI keys, some are not used but reserved from equivalents in rtags configuration
(global-set-key (kbd "C-z C-,")   'lsp-ui-peek-find-references)
(global-set-key (kbd "C-z C-.")   'xref-find-definitions)
(global-set-key (kbd "M-.")       'lsp-ui-peek-find-definitions)
(global-set-key (kbd "C-z C-<")   'ccls-call-hierarchy)
(global-set-key (kbd "C-z ,")     'lsp-find-references)
(global-set-key (kbd "C-z <tab>") 'lsp-ui-imenu)
(global-set-key (kbd "C-z D")     'flycheck-list-errors)
(global-set-key (kbd "C-z C-l")   'neph-lsp-reset)
(global-set-key (kbd "C-z C-S-l") 'neph-toggle-ccls-reload)
(global-set-key (kbd "C-z RET")   'helm-lsp-code-actions)
(global-set-key (kbd "C-z .")     'helm-lsp-workspace-symbol)        ;; Menu to find symbol in project
;; (global-set-key (kbd "C-z >")     'helm-lsp-global-workspace-symbol) ;; Menu to find symbol in open projects
;; (global-set-key (kbd "C-z C-.")           'rtags-find-symbol-at-point)
;; (global-set-key (kbd "C-z M-r")           'rtags-reparse-file)
;; (global-set-key (kbd "C-z C->")           'rtags-find-virtuals-at-point)
;; (global-set-key (kbd "C-z C-/")           'delete-xrefs-window-or-something) ;; Was the rtags bind to dismiss the references
;; (global-set-key (kbd "C-z C-n")           'xref-next-line)
;; (global-set-key (kbd "C-z C-p")           'xref-prev-line)
;; (global-set-key (kbd "C-z i")             'rtags-fixit)
;; (global-set-key (kbd "C-z I")             'rtags-fix-fixit-at-point)
;; (global-set-key (kbd "C-z DEL")           'rtags-location-stack-back)
;; (global-set-key (kbd "C-z <S-backspace>") 'rtags-location-stack-back)
;; (global-set-key (kbd "C-z C-S-R")         'rtags-rename-symbol)

;; Navigate? needs better binds.
(global-set-key (kbd "C-z <C-left>")  'neph-ccls-navigate-up)
(global-set-key (kbd "C-z <C-right>") 'neph-ccls-navigate-down)
(global-set-key (kbd "C-z <C-up>")    'neph-ccls-navigate-left)
(global-set-key (kbd "C-z <C-down>")  'neph-ccls-navigate-right)

;;(require 'lsp-clangd)
(require 'ccls)

;; Block ccls autoregister, register it ourself
;; TODO Example hook from gpt might work
;; (defun my-ccls-setup (workspace)
;;   "Customize CCLS server capabilities."
;;   (let ((caps (lsp--workspace-server-capabilities workspace)))
;;     (lsp:set-server-capabilities-document-symbol-provider? caps nil)
;;     (lsp:set-server-capabilities-completion-provider? caps nil)
;;     (lsp:set-server-capabilities-hover-provider? caps nil)))

;; FIXME I think it's slightly wrong, i want to hook make-lsp-client...
;; (defun my-lsp-register-client-advice (orig-fun &rest args)
;;   "Advice to modify LSP client registration for CCLS."
;;   (let ((client (apply orig-fun args)))
;;     (when (eq (plist-get client :server-id) 'ccls)
;;       (plist-put client :initialized-fn #'my-ccls-setup))
;;    client))

;;(advice-add 'lsp-register-client :around #'my-lsp-register-client-advice)

(with-eval-after-load 'ccls
  (setq ccls-executable "/usr/bin/ccls")
  (ccls-use-default-rainbow-sem-highlight)
  (setq ccls-sem-highlight-method 'font-lock)
;;(setq ccls-sem-highlight-method nil)
;; We'll set these from the theme.  Uncomment for random themes.
  (setq ccls-args
        (list
         (concat "--init=" (json-encode
                            (ht
                             ;; ("index" (ht ("multiVersion" 1))) ;; 80G+ memory usage and doesn't work well
                             ;; Clang args that trip things up, and include /usr/lib/glib-2.0 in compiles
                             ("clang" (ht ("extraArgs" [-ferror-limit=0 -I/usr/lib/glib-2.0/include/])
                                          ("excludeArgs" ["-frounding-math" "-march=pentium4"]))))))
         ;; Extra logging
         "-log-file=/tmp/ccls.log"
         "-v=1")))

;; Default off
(add-to-list 'lsp-disabled-clients 'ccls)

;;
;; Irony-mode (deprecated)
;;   DEPRECATED - going to drop if ccls + lsp keeps working well
;;
;;(require 'neph-irony-autoload)

(add-hook 'irony-mode-hook 'irony-mode-counsel-hook)

;; FIXME irony-mode breaks on headers due to that missing (car found)

;; Disabled by default - flycheck-irony is incredibly laggy for some reason, rtags provides better diagnostics
;;(with-eval-after-load "flycheck" (neph-flycheck-irony-setup))
;;(with-eval-after-load "irony" (neph-flycheck-irony-setup))

;; popup.el for rtags tooltips (needed anymore?)
(autoload 'popup "popup" "Popup tooltip thing." t)

;;
;; Smart Tabs
;;

(require 'smart-tabs-mode)
(smart-tabs-insinuate 'c 'javascript 'c++)


;;
;; emacs-gdb -- weirdNox's replacement for gdb-mi. kinda bad.
;;

;; (fmakunbound 'gdb)
;; (fmakunbound 'gdb-enable-debug)
;;(load-library "gdb-mi")

;;(require 'neph-weirdnox-gdb-autoload)
;; FIXME automatically replace gdb-mi


;;
;; ido
;;

(require 'ido)
(require 'ido-vertical-mode)
;(autoload 'ido "ido" "Ido thing." t)
;(autoload 'ido-vertical-mode "ido-vertical-mode" "ido-vertical-mode" t)
(ido-vertical-mode 1)

(global-set-key (kbd "C-x C-f") 'neph-ido-find-file)


;;
;; Rainbow Delimiters
;;

(autoload 'rainbow-delimiters-mode "rainbow-delimiters" "rainbow-delimiters" t)


;;
;; Minimap
;;

(autoload 'minimap-mode "minimap" "minimap" t)

(with-eval-after-load "minimap"
              (set-face-attribute 'minimap-font-face nil :family "Droid Sans Mono" :height 10 :weight 'ultrabold)
              (setq minimap-window-location (quote right))
              (setq minimap-width-fraction 0.01))


;;
;; God mode
(autoload 'god-mode "god-mode" "god-mode" t)
(global-set-key (kbd "C-z C-z") 'god-local-mode)

;;
;; Scrolling
; For scrolling when moving the cursor offscreen
(setq scroll-margin 1
      scroll-conservatively 0
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01)
(setq-default scroll-up-aggressively 0.01
              scroll-down-aggressively 0.01)

(setq mouse-wheel-scroll-amount '(10 ((shift) . 10)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)
(setq scroll-step 1)
(setq scroll-conservatively 10000)

;;
;; re-builder
(autoload 're-builder "re-builder" "re-builder" t)
(setq reb-re-syntax 'string)

;;
;; Misc modes
(autoload 'fic-mode "fic-mode" "fic-mode" t)
(with-eval-after-load "fic-mode"
  (add-to-list 'fic-highlighted-words "XXX"))

(require 'fringe-helper)
(require 'git-gutter)
(require 'git-gutter-fringe)
(autoload 'rainbow-mode "rainbow-mode" "Rainbow Mode." t)

;;
;; P4
;;

;; p4.el
(autoload 'p4 "p4" "p4" t)

;; Note: was shadowed by p4-edit-current prior to elpacification, commented
;;(global-set-key (kbd "C-z C-e") 'neph-p4-edit-current)
(global-set-key (kbd "C-z P r") 'neph-p4-revert-current)
(global-set-key (kbd "C-z P t") 'neph-p4vc-tlv)
(global-set-key (kbd "C-z P c") 'neph-p4vc-revgraph)
(global-set-key (kbd "C-z P h") 'neph-p4vc-history)

;;
;; Ediff
(add-hook 'ediff-prepare-buffer-hook 'neph-ediff-mode)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-merge-split-window-function 'split-window-horizontally)

;;
;; Projectile
;;


;; Must be set before loading helm-projectile according to help text. Makes it not super slow.
(setq helm-projectile-fuzzy-match nil)

;; In server mode, let's just load it synchronously
(require 'projectile)

(setq projectile-switch-project-action 'projectile-find-file)

(with-eval-after-load "projectile"
  (setq projectile-project-root-files
        (remove "?*.sln" projectile-project-root-files))
  (setq projectile-completion-system 'helm)
  (setq projectile-generic-command "fd . -E '/.*cache' --hidden -0")
  (setq projectile-indexing-method 'alien)
  (setq projectile-project-name-function 'neph-projectile-project-name)
  (setq projectile-enable-caching 'persistent)
  ;; caching big projects still very slow even with fd
  ;(setq projectile-files-cache-expire 3600)
  (projectile-global-mode t)
  (with-eval-after-load "helm"
    ;; This just wraps some stuff with 'helpers' like helm-projectile-find-file which is hella slow because it tries to
    ;; pull in dired too and such.  Should bind/turn on those things one and a time if they're handy, otherwise projectile
    ;; commands already use the helm completion backend.
    ;;(helm-projectile-on)
    ))


;;(let ((neph-ignored-patterns '("*.dwo" "*.o" "*.P" "*.dSYM" "*.vtx" "*.vtf" "*.wav" "*.mdl" "*.vvd"
;;                               "*.mp3" "*.png" "*.phy" "*.jpg" "*.pyc" "*.lib" "*.psd" "*.tga"
;;                               "*.dll" "*.vcs" "*.bsp" "*.zip" "*.exe")))
;;  (setq projectile-generic-command (concat "find . -type f "
;;                                           (mapconcat (lambda (x) (concat "-not -iname '" x "'"))
;;                                                      neph-ignored-patterns " -and ")
;;                                           " -print0")))

(require 'helm-projectile)


;; Additional autoloads for helm-projectile
(autoload 'helm-projectile-ag "~/.emacs.d/helm-projectile/helm-projectile")
(autoload 'helm-projectile-switch-to-buffer "~/.emacs.d/helm-projectile/helm-projectile")
(autoload 'helm-projectile-switch-project "~/.emacs.d/helm-projectile/helm-projectile")

;; WIP migrating to project.el as possible
(global-set-key (kbd "C-z M-f") 'projectile-find-file)
(global-set-key (kbd "C-c p a") 'projectile-find-other-file) ;; did projectile drop this bind or did I break loading its map, who knows
(global-set-key (kbd "C-z M-F") 'projectile-find-file-in-known-projects)
(global-set-key (kbd "C-z M-g") 'helm-projectile-rg-cpp)
(global-set-key (kbd "C-z M-G") 'helm-projectile-ag-cpp-this-word)
(global-set-key (kbd "C-z C-M-G") 'helm-do-ag-buffers)
(global-set-key (kbd "C-z g") 'helm-projectile-rg)
(global-set-key (kbd "C-z G") 'helm-projectile-ag-this-word)
;; Non-incremental, but can be faster and supports prefix arg for filename globbing
(global-set-key (kbd "C-z C-G") 'projectile-grep)
(global-set-key (kbd "C-z b") 'helm-projectile-switch-to-buffer)
(global-set-key (kbd "C-z B") 'helm-buffers-list)
(global-set-key (kbd "C-z p") 'projectile-switch-project)
;; No, this is used as a prefix elsewhere
;;(global-set-key (kbd "C-z C-p") 'helm-projectile)

(global-set-key (kbd "C-z C-p g") 'neph-projectile-switch-and-rg)
(global-set-key (kbd "C-z C-p M-g") 'neph-projectile-switch-and-rg-cpp)

(with-eval-after-load "helm-projectile"
  (define-key helm-projectile-find-file-map (kbd "M-g") (lambda ()
                                                          (interactive)
                                                          (with-helm-alive-p
                                                            ;; For some reason we need to have a lambda swallow the options string or helm-ag breaks
                                                            (helm-exit-and-execute-action (lambda (&optional options)
                                                                                            (interactive)
                                                                                            (helm-projectile-ag)))))))
;; Default. Setting this to helm-projectile-find-file seems to make it laggy?
;; (setq projectile-switch-project-action 'projectile-find-file)

;;
;; php-mode
;;
(require 'php-mode)

(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))
(add-hook 'php-mode-hook 'neph-tab-cfg)
(add-hook 'php-mode-hook 'neph-lsp-if-projectile)

;;
;; Web-mode
;;

(require 'web-mode)
(setq web-mode-indent-style 1)
(setq web-mode-script-padding 2)
(setq web-mode-style-padding 2)
(setq web-mode-enable-css-colorization t)
(setq web-mode-enable-comment-keywords t)
(setq web-mode-enable-block-face t)
(setq web-mode-enable-part-face t)
(setq web-mode-enable-current-element-highlight t)
(setq web-mode-enable-auto-pairing t)
(add-to-list 'auto-mode-alist '(".html?$" . web-mode))

;;
;; Neph mode. Aka enable defaults in programming modes
;;

;; Default modes

(add-to-list 'auto-mode-alist '("/yaourtrc\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("/bash-fc.[^/]+\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.ma?k\\'" . makefile-mode))
(add-to-list 'auto-mode-alist '("\\.service\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.service.d/.+\\.conf\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.sch\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.svelte\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("/PKGBUILD\\'" . neph-bash-mode))
(add-to-list 'auto-mode-alist '("/\\.?bash\\(rc\\|_profile\\)\\'" . sh-mode))
;; Default .j2 files to conf-mode, though these are jinja files that could be anything
(add-to-list 'auto-mode-alist '("\\.j2\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-ts-mode))
;; Use js-mode for vpc/vgc/res files for now, using tab-cfg
(add-to-list 'auto-mode-alist '("\.\\(v[pg]c\\|res\\)$" . js-mode))
(add-hook 'js-mode-hook 'neph-js-mode-hook)
(add-hook 'typescript-ts-mode-hook 'neph-tab-cfg)
(add-hook 'tsx-ts-mode-hook 'neph-tab-cfg)
(add-hook 'sh-mode-hook 'neph-space-cfg)
(add-hook 'conf-space-mode-hook 'neph-space-cfg)
(add-hook 'sql-mode-hook 'neph-space-cfg)
(add-hook 'python-mode-hook 'neph-space-cfg)
(add-hook 'python-ts-mode-hook 'neph-space-cfg)
(add-hook 'java-mode-hook 'neph-space-cfg)
(add-hook 'lisp-mode-hook 'neph-space-cfg)
(add-hook 'emacs-lisp-mode-hook 'neph-space-cfg)
(add-hook 'rustic-mode-hook 'neph-space-cfg)
(add-hook 'conf-mode-hook 'neph-space-cfg)
(add-hook 'typescript-ts-mode-hook 'neph-space-cfg)
(add-hook 'go-ts-mode-hook 'neph-space-cfg)
(add-hook 'c-mode-common-hook 'neph-tab-cfg) ; Default to tabs mode for now,
                                             ; should have path detection or
                                             ; something

;; Modes to try to auto-start lsp in, if they're part of a project
(add-hook 'c-mode-hook 'neph-lsp-if-projectile)
(add-hook 'c++-mode-hook 'neph-lsp-if-projectile)
(add-hook 'sh-mode-hook 'neph-lsp-if-projectile)
(add-hook 'python-mode-hook 'neph-lsp-if-projectile)
(add-hook 'python-ts-mode-hook 'neph-lsp-if-projectile)
(add-hook 'typescript-ts-mode-hook 'neph-lsp-if-projectile)
(add-hook 'tsx-ts-mode-hook 'neph-lsp-if-projectile)
(add-hook 'go-ts-mode-hook 'neph-lsp-if-projectile)

(add-hook 'lsp-after-open-hook 'neph-lsp-mode)

;;
;; Web-mode indent config
;;

(add-hook 'web-mode-hook 'neph-web-tab-cfg)

;;
;; IswitchBuffers
;;

;; Disabled in favor of ido-mode
;(iswitchb-mode 1)
;(setq iswitchb-buffer-ignore '("^ " "^\*"))

;(defun iswitchb-local-keys ()
;  (mapc (lambda (K)
;	  (let* ((key (car K)) (fun (cdr K)))
;	    (define-key iswitchb-mode-map (edmacro-parse-keys key) fun)))
;	'(("<right>" . iswitchb-next-match)
;	  ("<left>"  . iswitchb-prev-match)
;	  ("<up>"    . ignore             )
;	  ("<down>"  . ignore             ))))
;
;(add-hook 'iswitchb-define-mode-map-hook 'iswitchb-local-keys)


;;
;; Tramp
(require 'tramp)
(setq tramp-default-method "sshx")

;; suck less?
;;(setq remote-file-name-inhibit-locks t)
(setq tramp-use-scp-direct-remote-copying t)
;;(setq remote-file-name-inhibit-auto-save-visited t)
;; Use direct-async-process
(connection-local-set-profile-variables
 'remote-direct-async-process
 '((tramp-direct-async-process . t)))
(connection-local-set-profiles
 '(:application tramp :protocol "scp")
 'remote-direct-async-process)
(connection-local-set-profiles
 '(:application tramp :protocol "rsync")
 'remote-direct-async-process)

(global-set-key (kbd "C-z C-u") 'sudoize-buffer)
(global-set-key (kbd "C-z C-M-u") 'drop-sudo)

;;
;; Magit
;;

;; Fix magit in that mode
;; https://github.com/magit/magit/issues/5220
(setq magit-tramp-pipe-stty-settings 'pty)

(require 'with-editor)
(require 'magit)
(require 'magit-blame)
(global-set-key (kbd "C-z C-<return>") 'magit-status)
(global-set-key (kbd "C-z L") 'magit-blame-mode)
(global-set-key (kbd "C-z x") 'magit)
(global-set-key (kbd "C-z X") 'magit-ediff-stage)
(global-set-key (kbd "C-z C") 'magit-commit)

;;
;; Artist mode
;;
(global-set-key (kbd "C-z C-M-a") 'artist-mode) ;; C-c C-c exits artist mode


;;
;; Term mode
;; Global hl-line-mode block
(add-hook 'eshell-mode-hook 'neph-disable-global-hl-line)
(add-hook 'term-mode-hook 'neph-disable-global-hl-line)

;;
;; isearch tweaks
(add-hook 'isearch-mode-end-hook 'isearch-exit-at-start-hook)
(define-key isearch-mode-map (kbd "C-.") 'kill-isearch-match)

;;
;; Yaml mode
;;
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.sls\\'" . yaml-mode)) ;; Salt
(with-eval-after-load "yaml-mode"
  (add-hook 'yaml-mode-hook 'neph-space-cfg))


;;
;; Custom binds
;;

;; Bound to shift + the window nav keys below
(global-set-key (kbd "C-z C-S-S") 'neph-transpose-windows-backward)
;; Note: was shadowed by diff-buffer-with-file prior to elpacification, commented
;;(global-set-key (kbd "C-z C-S-D") 'transpose-windows)

;; Revert without prompting
(global-set-key (kbd "C-z R") 'neph-revert-buffer-noconfirm)

; Quick eval-defun
(global-set-key (kbd "C-z e") 'eval-region)
(global-set-key (kbd "C-z E") 'eval-defun)

(global-set-key (kbd "C-z C-S-G") 'gdb)
(global-set-key (kbd "C-z M") 'gdb-many-windows)

;; Delete trailing whitespace
;; Note: was shadowed by ediff-current-file prior to elpacification, commented
;;(global-set-key (kbd "C-z C-M-S-D") 'delete-trailing-whitespace)

; helm shortcuts
(global-set-key (kbd "C-z C-f") 'helm-find-files)
(global-set-key (kbd "C-z h") 'helm-resume)

;; Back one window
(global-set-key (kbd "C-x O") 'neph-other-window-backward)

; Scroll window
(global-set-key (kbd "s-n") 'neph-scroll-up-one)
(global-set-key (kbd "s-p") 'neph-scroll-down-one)
(global-set-key (kbd "s-l") 'neph-move-to-window-center-line)

; Fast window nav
(global-set-key (kbd "C-z C-s") 'neph-other-window-backward)
(global-set-key (kbd "C-z C-d") 'neph-other-window-forward)

;; Diff current changes
(global-set-key (kbd "C-z C-S-D") 'diff-buffer-with-file)
(global-set-key (kbd "C-z C-M-S-D") 'ediff-current-file)

;; Keybind for enabling debug stuff quickly when I'm mad at something hanging.  Which is always.
(global-set-key (kbd "C-z C-M-S-Q") 'neph-toggle-debug)

;; Bonus align keys

(global-set-key (kbd "C-z C-M-S-M") 'neph-run-makepkg-g-on-region)
(global-set-key (kbd "C-z C-M-s") 'neph-align-smss-table)
(global-set-key (kbd "C-z C-M-S-S") 'neph-markdownify-smss-table-yank)
(global-set-key (kbd "C-z C-M-p") 'neph-align-protobuf-message)
(global-set-key (kbd "C-z C-a") 'align-regexp)
(global-set-key (kbd "C-z a") 'neph-align-regexp-u)

(global-set-key (kbd "M-u") 'toggle-case)
(global-set-key (kbd "C-M-k") 'merge-next-line)
(global-set-key (kbd "C-S-Y") 'yank-and-indent)
(global-set-key (kbd "M-Y") 'smart-yank-before-line)

(global-set-key (kbd "C-z C-S-B") 'bookmark-current-line)

(global-set-key [(control shift up)] 'move-line-up)
;; Prefer to org-mode's default bind
(eval-after-load 'org '(define-key org-mode-map [(control shift up)] nil))

(global-set-key [(control shift down)] 'move-line-down)
;; Prefer to org-mode's default bind
(eval-after-load 'org '(define-key org-mode-map [(control shift down)] nil))

(global-set-key (kbd "M-P") 'smart-move-current-region-up)
(global-set-key (kbd "M-N") 'smart-move-current-region-down)

(global-set-key (kbd "C-S-o") 'open-next-line)

; F3 inserts current filename into minibuffer
(define-key minibuffer-local-map [f3] 'neph-insert-selected-window-buffer-name)

(global-set-key (kbd "C-z C-e") 'p4-edit-current)

;; Take slash away from electric indent ('electric-slash)
(eval-after-load 'cc-mode
  '(define-key c-mode-base-map "/" 'self-insert-command))
;; (global-set-key (kbd "/") 'self-insert-command)

;; Custom binds for existing commands
(global-set-key (kbd "C-z C-k") 'copy-to-register)
(global-set-key (kbd "C-z k") 'insert-register)
(global-set-key (kbd "C-z C-j") 'point-to-register)
(global-set-key (kbd "C-z j") 'jump-to-register)
(global-set-key (kbd "C-z C-w") 'window-configuration-to-register)

(global-set-key (kbd "C-c C-j") 'term-line-mode)
(global-set-key (kbd "C-c C-k") 'term-char-mode)
(global-set-key (kbd "C-M-a") 'back-to-indentation)
(global-set-key (kbd "C-S-k") 'kill-whole-line)
; Make ret auto-indent, but S-RET bypass
;(define-key global-map (kbd "RET") 'newline)
(global-set-key (kbd "<C-return>") 'electric-indent-just-newline)
;; Merge with previous line
(global-set-key (kbd "C-M-S-k") 'delete-indentation)

(global-set-key (kbd "C-S-M-j") 'copy-line)
(global-set-key (kbd "C-S-j") 'duplicate-line)

;; Replaces backwards/forwards sexp.
(global-set-key (kbd "C-M-f") 'jump-to-char)
(global-set-key (kbd "C-M-b") 'backward-jump-to-char)
(global-set-key (kbd "M-G") 'goto-line)

(global-set-key (kbd "C-S-U") 'neph-backward-kill-line)
(global-set-key (kbd "C-M-S-Z") 'current-word-to-kill-ring)
(global-set-key (kbd "M-@") 'neph-mark-current-word)
(global-set-key (kbd "M-B") 'backward-to-word)
(global-set-key (kbd "M-F") 'forward-to-word)
(global-set-key (kbd "M-D") 'neph-kill-to-word)
(global-set-key (kbd "<M-S-delete>") 'neph-backward-kill-to-word)

(with-eval-after-load "sql"
  (define-key sql-mode-map (kbd "C-c C-a") 'sql-send-secondary))

;; Quick register movement.
;; Default to register 7 since it's awkward to hit, leaving other registers available for explicit.
(global-set-key (kbd "C-z SPC") 'neph-point-to-register-quick)
(global-set-key (kbd "C-z C-SPC") 'neph-jump-to-register-quick)

(global-set-key (kbd "C-M-S-A") 'mark-current-line)

(global-set-key (kbd "C-x 2") 'vsplit-last-buffer)
(global-set-key (kbd "C-x 3") 'hsplit-last-buffer)

(global-set-key (kbd "C-z T") 'touch-current-file)

(global-set-key (kbd "C-z C-S-n") 'neph-buffer-name-to-kill-ring)

(global-set-key (kbd "C-z C-!") 'neph-xdg-open-this-file)

(global-set-key (kbd "C-z C-S-c") 'neph-show-file-coding)

;; Non-hooked version is C-x C-k b
(global-set-key (kbd "C-x C-k C-b") 'neph-kmacro-bind-to-key-amalgamate)

;; Disabled (requires semantic)
;;(defun jump-to-container ()
;;  (interactive)
;;  (let* ((tag (and (functionp 'semantic-current-tag) (semantic-current-tag)))
;;         (overlay (and tag (last (semantic-current-tag))))
;;         (char (and overlay (overlay-start (car overlay)))))
;;    (when char
;;      (goto-char char))))
;;
;;(global-set-key (kbd "C-z C") 'jump-to-container)

;;
;; Line-highlight

;;

;; highlight the current line; set a custom face, so we can
;; recognize from the normal marking (selection)
(defface hl-line '((t (:background "Gray")))
  "Face to use for `hl-line-face'." :group 'hl-line)
(setq hl-line-face 'hl-line)
;(global-hl-line-mode t)

;;
;; PlantUML
;;

;; Default install path from package
(setq org-plantuml-jar-path
      (expand-file-name "/usr/share/java/plantuml/plantuml.jar"))

;;
;; zap-to-char
(global-set-key (kbd "M-Z") 'backwards-zap-to-char)

;;
;; ace-jump-mode
;;

(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)

(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))

;; TODO Drop ace-jump?
(require 'avy)
(define-key global-map (kbd "C-z C-c") 'ace-jump-mode-pop-mark)
(define-key global-map (kbd "C-z C-x") 'avy-goto-word-1)

;;
;; mmm/jinja/salt mode
;;
(require 'salt-mode)

;;
;; Theme
;;

;(load-theme 'sunburst t)

;; See also neph-ample-zen-theme.el

;;
;; Load theme selected by env
;;
(setq default-neph-theme (let ((envtheme (getenv "NEPH_EMACS_THEME")))
                           (if envtheme envtheme
                             "ample-zen")))

(load-neph-theme default-neph-theme)

(global-set-key (kbd "C-z C-S-W") 'neph-whiteboard-mode)

;; Default font
(set-face-attribute 'default nil :family "DejaVu Sans Mono")
(set-face-attribute 'default nil :height 100)
(when (eq system-type 'darwin)
  (set-face-attribute 'default nil :family "Monaco")
  (set-face-attribute 'default nil :height 120))
(put 'downcase-region 'disabled nil)

;;
;; purple-haze (needs to be made into a neph-purple-haze-theme.el)
;;

;; (set-face-attribute 'cursor nil :background "#D96E26")
;; (load-theme 'purple-haze t)

;; (set-face-attribute 'mode-line nil :height 82)
;; (set-face-background 'hl-line "#19151D")
;; (set-face-attribute 'vertical-border nil :foreground "#222")
;; (set-face-attribute 'web-mode-block-face nil :background "#0E0B10")
;; ; These are way too strong by default
;; (set-face-attribute 'rainbow-delimiters-depth-1-face nil   :foreground "#fff")
;; (set-face-attribute 'rainbow-delimiters-depth-2-face nil   :foreground "#dcf")
;; (set-face-attribute 'rainbow-delimiters-depth-3-face nil   :foreground "#cbf")
;; (set-face-attribute 'rainbow-delimiters-depth-4-face nil   :foreground "#baf")
;; (set-face-attribute 'rainbow-delimiters-depth-5-face nil   :foreground "#a9e")
;; (set-face-attribute 'rainbow-delimiters-depth-6-face nil   :foreground "#98e")
;; (set-face-attribute 'rainbow-delimiters-depth-7-face nil   :foreground "#87d")
;; (set-face-attribute 'rainbow-delimiters-depth-8-face nil   :foreground "#76d")
;; (set-face-attribute 'rainbow-delimiters-depth-9-face nil   :foreground "#65c")
;; (set-face-attribute 'rainbow-delimiters-unmatched-face nil :foreground "#A00")
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
;; GDB - upstream gdb-mi, not to be confused with the weirdnox version below
;;

(setq gdb-debuginfod-enable-setting t) ;; This is completely fucking broken otherwise in emacs 29

; (setq gdb-non-stop-setting nil)
; (gdb-many-windows t)

; Replace this to not be dumb
;; (defadvice gud-display-line (around do-it-better activate) ... )
;;(defadvice gud-display-line (around do-it-better activate)
;;  (let* ((last-nonmenu-event t)	 ; Prevent use of dialog box for questions.
;;	 (buffer
;;	  (with-current-buffer gud-comint-buffer
;;	    (gud-find-file true-file)))
;;	 (window (and buffer
;;		      (or (get-buffer-window buffer)
;;                          (and gdb-source-window (set-window-buffer gdb-source-window buffer))
;;			  (display-buffer buffer))))
;;	 (pos))
;;    (when buffer
;;      (with-current-buffer buffer
;;	(unless (or (verify-visited-file-modtime buffer) gud-keep-buffer)
;;	  (if (yes-or-no-p
;;	       (format "File %s changed on disk.  Reread from disk? "
;;		       (buffer-name)))
;;	      (revert-buffer t t)
;;	    (setq gud-keep-buffer t)))
;;	(save-restriction
;;	  (widen)
;;	  (goto-char (point-min))
;;	  (forward-line (1- line))
;;	  (setq pos (point))
;;	  (or gud-overlay-arrow-position
;;	      (setq gud-overlay-arrow-position (make-marker)))
;;	  (set-marker gud-overlay-arrow-position (point) (current-buffer))
;;	  ;; If they turned on hl-line, move the hl-line highlight to
;;	  ;; the arrow's line.
;;	  (when (featurep 'hl-line)
;;	    (cond
;;	     (global-hl-line-mode
;;	      (global-hl-line-highlight))
;;	     ((and hl-line-mode hl-line-sticky-flag)
;;	      (hl-line-highlight)))))
;;	(cond ((or (< pos (point-min)) (> pos (point-max)))
;;	       (widen)
;;	       (goto-char pos))))
;;      (when window
;;	(set-window-point window gud-overlay-arrow-position)
;;	(if (eq gud-minor-mode 'gdbmi)
;;	    (setq gdb-source-window window))))))

;; Stop GDB from force-displaying I/O buffer (what the actual hell)
;;(defadvice gdb-inferior-filter
;;    (around gdb-inferior-filter-without-stealing)
;;  (with-current-buffer (gdb-get-buffer-create 'gdb-inferior-io)
;;    (comint-output-filter proc string)))
;;(ad-activate 'gdb-inferior-filter)
;;
;;(global-set-key (kbd "C-z C-M-i") 'gdb-io-interrupt)
;;(global-set-key (kbd "C-z C-M-c") 'gud-cont)


;;
;; remember-notes
;;

;; New in 24.4
(if (fboundp 'remember-notes)
    (progn
      (setq initial-buffer-choice 'remember-notes)
      (setq remember-notes-buffer-name "#Notes")))


;;
;; Mode line
;;

(require 'neph-modeline-util)
(add-hook 'find-file-hook 'neph-cache-projectile-info)

(setq neph-modeline-path
      '(:eval (let* ((rawname (buffer-file-name))
                     (bufname (if rawname (propertize rawname 'face 'neph-modeline-path) nil))
                     ;; Paths to replace. Of the form ((search replace) ...)
                     (replacements (list (list (getenv "HOME") "~"))))
                ;; Also replace projectile root with project name when available
                (when (and (featurep 'projectile) (bound-and-true-p neph-cached-projectile-project-root))
                  (cl-pushnew (list neph-cached-projectile-project-root
                                    (concat neph-cached-projectile-project-name "/"))
                              replacements))
                (if bufname
                    (progn
                      ;; Trim filename from path
                      (setq bufname (replace-regexp-in-string "/[^/]*$" "/" bufname))
                      ;; Apply neph-modeline-shortpaths replacements
                      (while replacements
                        (let* ((search (car (car replacements)))
                               (replace (car (cdr (car replacements))))
                               (splitname (split-string bufname (concat "^" (regexp-quote search))))
                               (remainder (car (cdr splitname))))
                          (if remainder
                              (setq bufname
                                    (concat
                                     ;; This blows away the default propertize, they're not additive.
                                     (propertize replace 'face 'neph-modeline-path-replacement)
                                     (propertize remainder 'face 'neph-modeline-path)))))
                        (setq replacements (cdr replacements)))
                      ;; Return
                      bufname)
                  ""))))

(setq neph-modeline-bufstat
      '(:eval (cond (buffer-read-only
                     (propertize " RO " 'face 'neph-modeline-stat-readonly))
                    ((buffer-modified-p)
                     (propertize " ** " 'face 'neph-modeline-stat-modified))
                    (t (propertize " -- " 'face 'neph-modeline-stat-clean)))))

(setq-default mode-line-format
              '(:eval
                (list
                 ;; Bonus hacky alignment, such that if the buffer is too narrow
                 ;; to show the modeline below hud, the height of the modeline stays the same
                 (propertize "\u200d" 'display '(list (raise -0.30) (height 1.5)))
                 neph-modeline-bufstat
                 ;; Position
                 "%[%l:%c"
                 ;; End brace for position
                 "%] "
                 ;; path
                 neph-modeline-path
                 ;; buffer name
                 `(:propertize "%b" face ,(if (neph-modeline-active)
                                              'neph-modeline-id
                                            'neph-modeline-id-inactive))
                 ; Mode
                 " :: "
                 '(:propertize mode-name face neph-modeline-mode)
                 ;;""
                 ; misc
                 '(:propertize mode-line-process face neph-modeline-misc)
                 '(global-mode-string (" " (:propertize global-mode-string face neph-modeline-misc)))
                 '(:propertize minor-mode-alist face neph-modeline-misc)
                 (when vc-mode (propertize (concat " /" vc-mode)
                                         'face 'neph-modeline-misc))
                 ;; righthand side

                 ;; For disabled rtags
                 ;; (let ((rtags-status (if (featurep 'rtags)
                 ;;                    (propertize (rtags-modeline) 'face 'neph-modeline-which-func)
                 ;;                  "")))
                 (list
                  ;; Pad to right side
                  (neph-fill-to 9) ;; 9 if enabling hud

                  ;; Disabled rtags
                  ;; (neph-fill-to (+ 9 (string-width rtags-status))) ;; Instead of fill-to above
                  ;; rtags-status

                  ;; Percentage and modeline-hud
                  "%p "
                  (neph-modeline-hud 1.5 10)
                  ))
                ))

;; Force modeline updates when rtags status changes
;;(when (featurep 'rtags)
;;  (add-hook 'rtags-diagnostics-hook (lambda ()
;;                                      (force-mode-line-update)
;;                                      (message "RTAGS DIAGNOSTICS"))))

(setq rtags-current-container-hook 'neph-rtags-current-container-hook)

(setq-default header-line-format
              '(:eval (let ((which-func (and which-function-mode (fboundp 'which-function) (which-function)))
                            (valid-neph-sticky-header (and neph-sticky-header-valid-range
                                                           (>= (point) (car neph-sticky-header-valid-range))
                                                           (<= (point) (cdr neph-sticky-header-valid-range)))))
                        (list
                         "  "
                         (when (and which-func
                                    (not (string= "" which-func))
                                    (or (not valid-neph-sticky-header)
                                        (not (string-match-p (regexp-quote which-func) neph-sticky-header))))
                           (propertize (concat which-func " ") 'face 'neph-modeline-which-func))
                         (when valid-neph-sticky-header neph-sticky-header)))))

;;
;; ; #120F14
;; (set-face-attribute 'whitespace-tab nil :background "#100D20")

;; (set-face-attribute 'mode-line nil
;;                     :background "#111"
;;                     :foreground "#666"
;;                     :box '(:line-width 1 :color "#221" :style nil))
;; (set-face-attribute 'mode-line-inactive nil
;;                     :background "#333"
;;                     :foreground "#666"
;;                     :box '(:line-width 1 :color "#333" :style nil))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(company-backends
   '(company-capf company-irony company-bbdb company-eclim company-semantic company-clang company-xcode company-cmake company-capf company-files
                  (company-dabbrev-code company-gtags company-etags company-keywords)
                  company-oddmuse company-dabbrev))
 '(company-quickhelp-color-background "black")
 '(compilation-skip-threshold 2)
 '(copilot-max-char 1000000)
 '(dap-auto-configure-features
   '(sessions locals breakpoints expressions repl controls tooltip))
 '(display-line-numbers-grow-only t)
 '(display-line-numbers-width 6)
 '(ediff-split-window-function 'split-window-horizontally)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(ein:completion-backend 'ein:use-company-backend)
 '(flycheck-checker-error-threshold nil)
 '(helm-candidate-number-limit 1000)
 '(helm-move-to-line-cycle-in-source nil)
 '(helm-rg-input-min-search-chars 1)
 '(ido-vertical-define-keys 'C-n-and-C-p-only)
 '(irony-completion-availability-filter '(available deprecated notaccessible notavailable))
 '(lsp-enable-file-watchers nil)
 '(lsp-enable-on-type-formatting nil)
 '(lsp-intelephense-files-associations ["*.php" "*.phtml" "*.js" "*.css"])
 '(lsp-intelephense-paths-include ["../common"])
 '(lsp-intelephense-php-version "7.4.3")
 '(lsp-pyright-multi-root nil)
 '(lsp-rust-analyzer-server-display-inlay-hints t)
 '(lsp-semantic-tokens-enable t)
 '(lsp-ui-doc-alignment 'window)
 '(lsp-ui-doc-header t)
 '(lsp-ui-doc-include-signature t)
 '(lsp-ui-doc-position 'top)
 '(lsp-ui-peek-always-show nil)
 '(lsp-ui-peek-list-width 70)
 '(lsp-ui-sideline-show-code-actions t)
 '(markdown-command "marked")
 '(org-agenda-files '("~/.emacs.d/notes-holo.org"))
 '(org-babel-load-languages '((plantuml . t) (python . t) (shell . t) (emacs-lisp . t)))
 '(phi-search-limit 5000)
 '(reb-auto-match-limit 2000)
 '(rtags-follow-symbol-try-harder nil)
 '(rtags-imenu-syntax-highlighting nil)
 '(safe-local-variable-values
   '((eval local-set-key
           (kbd "C-z M-g")
           'helm-projectile-rg-php)
     (local-set-key
      (kbd "C-z M-G")
      (lambda nil
        (interactive)
        (require 'helm-projectile)
        (let
            ((helm-rg-default-extra-args
              (append helm-rg-default-extra-args
                      (split-string-and-unquote "-g **/support/** -t php"))))
          (call-interactively 'helm-projectile-rg))))
     (eval progn
           (local-set-key
            (kbd "C-z M-G")
            (lambda nil
              (interactive)
              (require 'helm-projectile)
              (let
                  ((helm-rg-default-extra-args
                    (append helm-rg-default-extra-args
                            (split-string-and-unquote "-g **/support/** -t php"))))
                (call-interactively 'helm-projectile-rg))))
           (local-set-key
            (kbd "C-z M-g")
            'helm-projectile-rg-php))
     (eval progn
           (local-set-key
            (kbd "C-z M-G")
            (lambda nil
              (interactive)
              (require 'helm-projectile)
              (let
                  ((helm-rg-default-extra-args
                    (split-string-and-unquote "-g **/support/** -t php")))
                (call-interactively 'helm-projectile-rg))))
           (local-set-key
            (kbd "C-z M-g")
            'helm-projectile-rg-php))
     (eval progn
           (local-set-key
            (kbd "C-z M-S-G")
            (lambda nil
              (interactive)
              (require 'helm-projectile)
              (let
                  ((helm-rg-default-extra-args
                    (split-string-and-unquote "-g **/support/** -t php")))
                (call-interactively 'helm-projectile-rg))))
           (local-set-key
            (kbd "C-z M-g")
            'helm-projectile-rg-php))
     (eval progn
           (local-set-key
            (kbd "C-z M-S-g")
            (lambda nil
              (interactive)
              (require 'helm-projectile)
              (let
                  ((helm-rg-default-extra-args
                    (split-string-and-unquote "-g **/support/** -t php")))
                (call-interactively 'helm-projectile-rg))))
           (local-set-key
            (kbd "C-z M-g")
            'helm-projectile-rg-php))
     (eval progn
           (local-set-key
            (kbd "C-z M-S-g")
            (lambda nil
              (interactive)
              (require 'helm-projectile)
              (let
                  ((helm-rg-default-extra-args
                    (split-string-and-unquote "-g **/support/** -t php")))
                (call-interactively 'helm-projectile-rg))))
           (local-set-key
            (kbd "C-Z M-g")
            'helm-projectile-rg-php))
     (highlight-80+-columns . 100)
     (eval c-set-offset 'arglist-cont-nonempty
           '(c-lineup-gcc-asm-reg c-lineup-arglist))
     (eval c-set-offset 'arglist-close 0)
     (eval c-set-offset 'arglist-intro '++)
     (eval c-set-offset 'case-label 0)
     (eval c-set-offset 'statement-case-open 0)
     (eval c-set-offset 'substatement-open 0)))
 '(set-mark-command-repeat-pop t)
 '(show-paren-mode t)
 '(warning-suppress-types '((comp))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ccls-code-lens-face ((t (:inherit shadow :height 0.7))))
 '(ccls-code-lens-mouse-face ((t (:underline t)))))
