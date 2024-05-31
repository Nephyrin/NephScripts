;; -*- mode: Emacs-Lisp; -*-

;;
;; Misc
;;

;; TODO Make ~/.emacs a stub that sets up auto-compile and native comp and then loads ~/.emacs.d/init.el
;; That'll make sure the functions defined here are compiled and so on.

;; Don't load outdated .elc files, it's basically never what was intended.
(setq load-prefer-newer t)
(require 'comp)
(setq native-comp-speed 3)
(setq native-comp-always-compile t)
(add-to-list 'load-path "~/.emacs.d/auto-compile")
(require 'auto-compile)
(auto-compile-on-load-mode)
(auto-compile-on-save-mode)
;; Set to block native compilation. Also need to nuke the ~/.emacs.d/eln-cache folder.
;;(add-to-list 'native-comp-bootstrap-deny-list ".*")
;;(add-to-list 'native-comp-deferred-compilation-deny-list ".*")

;; Global libraries macros in here (and also )
(add-to-list 'load-path "~/.emacs.d/dash") ; dependency of ht
(add-to-list 'load-path "~/.emacs.d/emacs-ht")
(require 'ht)

(require 'cl) ;; Used so xe and friends can run some crap

(setq redisplay-dont-pause t)
(setq inhibit-eval-during-redisplay nil)
(setq fast-but-imprecise-scrolling t)
(setq jit-lock-chunk-size 100)
(setq jit-lock-defer-time 0)
(setq jit-lock-stealth-load nil)
(setq jit-lock-stealth-nice 0.01)
(setq jit-lock-stealth-time 0.2)

(add-to-list 'load-path "~/.emacs.d/neph-autoloads")

;; Disable silly "type Y-E-S" prompts
(fset 'yes-or-no-p 'y-or-n-p)

;; Always answer yes to: File %s is %s on disk.  Make buffer %s, too?
;; (There's no variable to control this)
(defun neph-y-or-n-p (orig-func prompt &rest args)
  (if (string-match "^File .* is .* on disk.  Make buffer .*, too\\? $"
                    prompt)
      t
    (apply orig-func prompt args)))
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

(setq split-width-threshold 170)
(setq split-height-threshold 1000)

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
;; Libraries.  These are all dumped on the path before loading things
;;

(add-to-list 'load-path "~/.emacs.d/bui.el")
(add-to-list 'load-path "~/.emacs.d/compat.el")
(add-to-list 'load-path "~/.emacs.d/emacs-spinner")
(add-to-list 'load-path "~/.emacs.d/s.el")
(add-to-list 'load-path "~/.emacs.d/f.el")
(add-to-list 'load-path "~/.emacs.d/editorconfig")

;;
;; Electric mode tweaks
;;

;; Custom inhibits on top of the normal behavior, since some choices are pretty bad by default.
(setq electric-pair-inhibit-predicate
      (lambda (c)
        (let ((whitespace-forward (or (looking-at "[ \n\t\"]") (looking-at "$")))
              (whitespace-backward (or (eq (point) 2) (looking-back "[ \n\t]." 2)))
              (is-quote (char-equal c ?\")))
          ;; Inhibit quotes unless there is whitespace on either side of the point.
          ;;
          ;; Inhibit non-quotes unless there is whitespace ahead fo the point (because `foo(` should work, but `foo"' is
          ;; less sensical for auto-pairing)
          (if (or (not whitespace-forward)
                  (and (not whitespace-backward) is-quote))
              ;; Inhibit
              t
            ;; Otherwise chain to normal inhibit behavior
            (electric-pair-conservative-inhibit c)))))

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

;;
;; Xterm color
;;
(add-to-list 'load-path "~/.emacs.d/xterm-color")
(require 'xterm-color)
(add-to-list 'load-path "~/.emacs.d/eterm-256color")
;(require 'eterm-256color) FIXME debug-init

;(add-hook 'term-mode-hook #'eterm-256color-mode)

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

(add-to-list 'load-path "~/.emacs.d/markdown-mode")
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;
;; Evil
;;

(add-to-list 'load-path "~/.emacs.d/evil")
(require 'neph-evil-autoload)
(global-set-key (kbd "C-z C-M-SPC") 'evil-mode)

;;
;; Indent bars
;;
(add-to-list 'load-path "~/.emacs.d/indent-bars")
(require 'indent-bars)
(setq indent-bars-width-frac 0.1)

;; SiGnIfiCaNt WhItEsPaCe
(add-hook 'python-mode-hook 'indent-bars-mode)
(add-hook 'python-ts-mode-hook 'indent-bars-mode)

;;
;; Highlight Symbol
;;
(add-to-list 'load-path "~/.emacs.d/highlight-symbol")
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

(global-set-key (kbd "C-z H") 'highlight-symbol)
(global-set-key (kbd "C-z C-H") 'highlight-symbol-remove-all)
(setq highlight-symbol-idle-delay 0.3)

;;
;; Rust mode
;;

(add-to-list 'load-path "~/.emacs.d/rust-mode")
(add-to-list 'load-path "~/.emacs.d/rustic")
(require 'rustic)

;;
;; Lua mode
;;

(add-to-list 'load-path "~/.emacs.d/lua-mode")
(autoload 'lua-mode "lua-mode"
   "Major mode for editing Lua files" t)
(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))

;; Defaults to 3. What in the goddamn.
(setq lua-indent-level 2)

;;
;; Command helpers
;;

(defun neph-buffer-command (cmd &optional name callback buffer-init-func)
  "Runs a command into a new buffer, noting when it finishes, with a callback"
  (interactive "MCommand: \n")
  (let* ((envcmd (concat "env -u NEPH_COLOR_TERM " cmd))
         (name (if name name "neph-buffer-command"))
         (buf (generate-new-buffer (concat name ": " cmd))))
    (switch-to-buffer buf nil t)
    (when buffer-init-func (with-current-buffer buf
                             (apply buffer-init-func nil)))
    (insert (concat "<command: " cmd ">"))
    (newline)
    (let ((proc (start-process-shell-command
                 (concat name "-proc")
                 buf envcmd)))
      (set-process-sentinel
       proc
       `(lambda (process signal)
          (when (eq (process-status process) 'exit)
            (message (concat ,name " finished"))
            (with-current-buffer (process-buffer process)
              (rename-buffer (concat (buffer-name) " <command finished>"))
              (newline)
              (insert "<command finished>")
              (when ,callback
                (apply ,callback (list process)))
              (goto-char (point-min)))))))))

(defun neph-p4inter (args)
  "Runs the p4inter command with args"
  (interactive "Mp4inter: \n")
  (neph-buffer-command
   (concat "p4inter " args) "neph-p4inter"
   (lambda (process)
     (delete-trailing-whitespace)
     (highlight-regexp "^Change" 'git-commit-note))))

;; FIXME We force-wrap env around it in neph-buffer-command
(defun neph-evmk (args)
  "Runs the p4inter command with args"
  (interactive "Mevmk: \n")
  (neph-buffer-command
   (concat "evmk " args)
   "neph-evmk"
   ;; callback
   nil
   ;; buffer-init-func
   (lambda ()
     (font-lock-mode t)
     (compilation-minor-mode t))))

;;
;; Htmlize
;;

(add-to-list 'load-path "~/.emacs.d/htmlize")
(autoload 'htmlize-buffer "htmlize" "htmlize" t)

;; Hacky thing to htmlize a region and send it straight to browser
;;(defun neph-html-region ()
;;  (interactive)
;;  (let* ((regionp (region-active-p))
;;         (beg (and regionp (region-beginning)))
;;         (end (and regionp (region-end)))
;;         (buf (current-buffer))
;;         ;; poor man's with-temp-killring (requires let*)
;;         (kill-ring (list "temp kill ring"))
;;         (kill-ring-yank-pointer kill-ring))
;;    (with-temp-buffer
;;      ;;(switch-to-buffer (current-buffer) nil t)
;;      (rename-buffer "*Neph HTMLIZE Temp Buffer*" t)
;;      (font-lock-mode -1) ;; We want to keep the face properties from the source buffer always
;;      (insert-buffer-substring-as-yank buf beg end)
;;      (with-current-buffer (htmlize-buffer)
;;        (write-file "~/.emacs.d/htmlize-temp.htm"))))
;;        ;;(kill-buffer)))
;;    ;; This is the way the help actually suggests you prevent it from opening this buffer.
;;    (let ((display-buffer-alist (cons '("\\*Async Shell Command\\*" (display-buffer-no-window))
;;                                      display-buffer-alist)))
;;      (async-shell-command "chromium ~/.emacs.d/htmlize-temp.htm")))

(defun neph-html-region ()
  (interactive)
  (require 'htmlize)
  (with-current-buffer
      (htmlize-region (point) (mark))
    (write-file "~/.emacs.d/htmlize-temp.htm"))
  ;;(kill-buffer)))
  (start-process-shell-command "neph-html-region" nil "chromium ~/.emacs.d/htmlize-temp.htm"))

(defun neph-html-copy ()
  "Copy the selected region to the clipboard as html.  Requires awk and xclip be available."
  (interactive)
  (require 'htmlize)
  ;; Detect some modes that clash with htmlize, set mode to inline-css for maximal CnP compatibility
  (let ((ghl (and (boundp 'global-hl-line-mode) global-hl-line-mode))
        (htmlize-output-type 'inline-css)
        (htmlize-pre-style 't))
    ;; Disable incompatible modes, run htmlize, re-enable
    (when ghl (global-hl-line-mode -1))
    (with-current-buffer
        (htmlize-region (region-beginning) (region-end))
      (write-file "~/.emacs.d/htmlize-temp.htm"))
    (when ghl (global-hl-line-mode 1)))
  ;; Awful awk script to skip all the doctype/html/body/head document tags and just select the 'pre'
  ;; tag, then stuff it onto the clipboard
  (start-process-shell-command "neph-html-copy" nil
                               (concat "awk -i inplace '/^ *<pre/ { inpre=1; };"
                                       "  /^ *<\\/pre/ { inpre=0; print };"
                                       "  inpre { print };'"
                                       "  ~/.emacs.d/htmlize-temp.htm && "
                                       "xclip -quiet -i -selection clipboard -target text/html"
                                       "  ~/.emacs.d/htmlize-temp.htm")))

;(global-set-key (kbd "C-z M-w") 'neph-html-copy)

;;
;; htmlfontify
;;
;; Sometimes htmlize fails on some buffers, sometimes htmlfontify does :-/ :-/

;; :height 96 should be 10pt, ends up at 9pt, increase this a little because idk
(with-eval-after-load "htmlfontify"
  (setq hfy-font-zoom 1.09))

;; Like neph-html-region, uses htmlfontify to fontify things, pops up in a browser
;; WIP STILL DOESNT WORK WITH ansi-term
(defun WIP-neph-hfy-html-region ()
  (interactive)
  ;; hfy breaks on buffers that have non-font-lock propertization on text in emacs 25, just capture current text into a
  ;; non-font-lock buffer.
  (let* ((regionp (region-active-p))
         (beg (and regionp (region-beginning)))
         (end (and regionp (region-end)))
         (buf (current-buffer))
         ;; poor man's with-temp-killring (requires let*)
         (kill-ring (list "temp kill ring"))
         (kill-ring-yank-pointer kill-ring))
         ;;(hfy-optimizations (list 'skip-refontification)))
;;    (flet ((hfy-force-fontification () (message "Prevented hfy-force-fontification")) ;; See above comment
;;           (hfy-fontified-p () (message "Lying about fontification") t))
    (with-temp-buffer
      ;;(switch-to-buffer (current-buffer) nil t)
      ;;(font-lock-mode t) ;; We want to keep the face properties from the source buffer always
      (let ((tempbuf (current-buffer)))
        (flet ((hfy-buffer () (message "Intercepted hfy-buffer") tempbuf))
;;               (copy-to-buffer (buffer start end)
;;                               (message "Intercepted copy-to-buffer")
;;                               (let ((thisbuf (current-buffer)))
;;                                 (with-current-buffer (get-buffer buffer)
;;                                   (insert-buffer-substring thisbuf start end))
;;                                 (with-current-buffer (get-buffer "tmp.tmp")
;;                                   (insert-buffer-substring thisbuf start end)))))
          (switch-to-buffer buf)
          ;;(rename-buffer "*Neph HTMLIZE Temp Buffer*" t)
          ;;(insert-buffer-substring-as-yank buf beg end)
          (hfy-fontify-buffer)
          (switch-to-buffer tempbuf)
          (write-file "~/.emacs.d/htmlize-temp.htm")))))
  ;; This is the way the help actually suggests you prevent it from opening this buffer.
  (let ((display-buffer-alist (cons '("\\*Async Shell Command\\*" (display-buffer-no-window))
                                    display-buffer-alist)))
    (async-shell-command "chromium ~/.emacs.d/htmlize-temp.htm")))

;;
;; Multi-term
;;

;(load-file "~/.emacs.d/multi-term.el")
;(setq multi-term-program "/bin/bash")
;
;(global-set-key (kbd "C-x t") 'multi-term-dedicated-open)

;; Term key overrides
(defun term-send-raw-C-z ()
  "Send a raw Control-z value to term."
  (interactive)
  (term-send-raw-string (kbd "C-z")))

(with-eval-after-load 'term
  (define-key term-raw-map (kbd "C-y") 'term-paste)
  ;; Allow C-z to escape
  (define-key term-raw-map (kbd "C-z") nil)
  ;; But make C-z C-z send a real C-z
  (define-key term-raw-map (kbd "C-z C-z") 'term-send-raw-C-z))

;;
;; Emacs Interactive Notebook (jupyter)
;;

;; FIXME This should autoload, but the janky-AF http proxy disabling needs to be looked at
;; (something inherits it during the load process and I can't stop it)
(add-to-list 'load-path "~/neph/emacs.d/auto-complete")
(add-to-list 'load-path "~/neph/emacs.d/js2-mode")
(add-to-list 'load-path "~/neph/emacs.d/polymode")
(add-to-list 'load-path "~/neph/emacs.d/skewer-mode")
(add-to-list 'load-path "~/neph/emacs.d/emacs-web-server")
(add-to-list 'load-path "~/neph/emacs.d/emacs-websocket")
(add-to-list 'load-path "~/neph/emacs.d/emacs-ipython-notebook/lisp")
(defun neph-load-ein ()
  "Janky function to load ein late."
  (interactive)
  (setenv "HTTP_PROXY" nil)
  (setenv "HTTPS_PROXY" nil)
  (setenv "http_proxy" nil)
  (setenv "https_proxy" nil)
  (require 'ein-notebook)
  (define-key ein:notebook-mode-map (kbd "C-c <C-return>") 'ein:worksheet-execute-autoexec-cells)
  (define-key ein:notebook-mode-map (kbd "C-c <C-S-return>") 'neph-ein-restart-and-autoexec))

(defun neph-ein-restart-and-autoexec ()
  "Restart the current notebook's kernel and then execute all autoexec cells."
  (interactive)
  ;; This inlines ein:kernel-restart-session since it doesn't take a callback.
  (ein:aif ein:%notebook%
    (let ((kernel (ein:$notebook-kernel it)))
      (ein:kernel-delete-session
       kernel
       (lambda (kernel)
         (ein:events-trigger (ein:$kernel-events kernel) 'status_restarting.Kernel)
         (ein:kernel-retrieve-session
          kernel 0
          (lambda (kernel)
            (ein:events-trigger (ein:$kernel-events kernel)
                                'status_restarted.Kernel)
            (ein:notebook-execute-autoexec-cells ein:%notebook%))))))
    (message "Not in notebook buffer")))


;;
;; ECB
;;

;(add-to-list 'load-path "~/.emacs.d/ecb")
;(require 'ecb)
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

(add-to-list 'load-path "~/.emacs.d/color-identifiers-mode")
(require 'color-identifiers-mode)

;;
;; Helm
;;

(add-to-list 'load-path "~/.emacs.d/emacs-async") ; helm dep
(add-to-list 'load-path "~/.emacs.d/helm")
(require 'helm-autoloads)
;;(require 'helm)
;;(require 'helm-config)
;;(require 'helm-files)

(helm-mode 1)

;; Since 215005e25718 helm's default score func is just crazy broken
;; and puts really-fuzzy matches above extremely-direct matches
(setq helm-fuzzy-default-score-fn 'helm-fuzzy-helm-style-score)
;;Default: (setq helm-fuzzy-default-score-fn 'helm-fuzzy-flex-style-score)

(global-set-key (kbd "C-z F") (lambda () (interactive) (helm-find-1 (read-directory-name "Run find in directory: " nil "" t))))
(global-set-key (kbd "M-x") 'helm-M-x)

(global-set-key (kbd "C-z b") 'helm-mini)
(global-set-key (kbd "C-z C-b") 'helm-bookmarks)
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

(add-to-list 'load-path "~/.emacs.d/fzf")
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
(add-to-list 'load-path "~/.emacs.d/helm-swoop")
(require 'helm-swoop)

(global-set-key (kbd "C-z M-s") 'helm-swoop)
(global-set-key (kbd "C-z M-S") 'helm-multi-swoop-all)

;;
;; Helm AG and Helm RG and RG they're all different
;;

(add-to-list 'load-path "~/.emacs.d/helm-ag")
(add-to-list 'load-path "~/.emacs.d/helm-rg")
(add-to-list 'load-path "~/.emacs.d/wgrep") ;; For rg.el
(add-to-list 'load-path "~/.emacs.d/rg.el")
(require 'helm-ag)
(require 'helm-rg)
(require 'rg)

;; Define a minor mode to lock rg bounce buffers into read-only and provide some quick access keys
;;
;; Pressing the default bind (C-c C-e) will turn off this mode and unlock helm-rg--bounce's editing mode, which is
;; useful, but not by default when I just want a persistent buffer to visit search results.
(defun neph-rg-bounce-navigation-mode-handler ()
  "Default hook for neph-rg-bounce-navigation-mode."
  (if neph-rg-bounce-navigation-mode
      (progn
        (message "Neph: Visit Mode")
        (read-only-mode 1))
    (message "Neph: Edit Mode")
    (read-only-mode -1)))
(define-minor-mode neph-rg-bounce-navigation-mode
  "Mode that puts helm-rg bounce buffers into read-only navigation rather than editing."
  :keymap '())
(add-hook 'neph-rg-bounce-navigation-mode-hook 'neph-rg-bounce-navigation-mode-handler)
(define-key helm-rg--bounce-mode-map (kbd "C-c C-e") #'neph-rg-bounce-navigation-mode)

(define-key neph-rg-bounce-navigation-mode-map (kbd "g") #'helm-rg--bounce-refresh)
(define-key neph-rg-bounce-navigation-mode-map (kbd "r") #'helm-rg--bounce-refresh-current-file)
(define-key neph-rg-bounce-navigation-mode-map (kbd "d") #'helm-rg--bounce-dump)
(define-key neph-rg-bounce-navigation-mode-map (kbd "D") #'helm-rg--bounce-dump-current-file)
(define-key neph-rg-bounce-navigation-mode-map (kbd "RET")
  ;; This function always calls 'alternate-method', so let bind that to normal method for the "normal visit" keybind.
  (lambda () (interactive)
    (let ((helm-rg-display-buffer-alternate-method
           helm-rg-display-buffer-normal-method))
      (helm-rg--visit-current-file-for-bounce))))
(define-key neph-rg-bounce-navigation-mode-map (kbd "C-o") #'helm-rg--visit-current-file-for-bounce)
(define-key neph-rg-bounce-navigation-mode-map (kbd "e") #'helm-rg--expand-match-context)
(define-key neph-rg-bounce-navigation-mode-map (kbd "E") #'helm-rg--spread-match-context)
(define-key neph-rg-bounce-navigation-mode-map (kbd "q") #'kill-this-buffer)

;; Defaults on
(add-hook 'helm-rg--bounce-mode-hook 'neph-rg-bounce-navigation-mode)

(setq helm-ag-insert-at-point t)
;; (setq helm-ag-always-set-extra-option t)

(defun helm-ff-helm-do-ag ()
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action '(lambda (basedir)
                                     (let ((parent (file-name-directory (directory-file-name basedir)))
                                           (default-directory nil))
                                       (helm-do-ag nil (list parent)))))))
;; FIXME not needed?
;; (put 'helm-ff-helm-do-ag 'helm-only nil)

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

;; Keys to walk a visible helm-ag buffer
(defun neph-helm-ag-next (arg)
  (interactive "P")
  (let* ((direction (if arg -1 1))
         (agbuf (or (get-buffer "*helm ag results*") (get-buffer "*hgrep*")))
         (agwin (get-buffer-window agbuf)))
    (flet ((notdone () (if (and (looking-at "$") (looking-back "^"))
                           (progn (message "End of results") nil)
                         t))
           (move () (next-logical-line direction) (beginning-of-line)))
      (when agbuf
        (if agwin
            (progn (select-window agwin)
                   (move)
                   (when (notdone)
                     (helm-ag-mode-jump-other-window)))
          (switch-to-buffer agbuf)
          (move)
          (when (notdone)
            (helm-ag-mode-jump)))))))
(defun neph-helm-ag-prev (arg)
  (interactive "P")
  (neph-helm-ag-next (if arg nil 1)))
(defun neph-helm-ag-update ()
  (interactive)
  (let ((agbuf (get-buffer "*helm ag results*")))
    (when agbuf
      (with-current-buffer agbuf
        (helm-ag--update-save-results)))))
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
;; multiple-cursors
;;

(add-to-list 'load-path "~/.emacs.d/multiple-cursors")
(require 'multiple-cursors)

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-.") 'mc/unmark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-,") 'mc/unmark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;;
;; phi-search
;;

(add-to-list 'load-path "~/.emacs.d/phi-search")
(autoload 'phi-search "phi-search" "Phi Search." t)

(global-set-key (kbd "C-S-s") 'phi-search)
(global-set-key (kbd "C-S-r") 'phi-search-backward)

;; See also
;;phi-search-additional-keybinds
;;phi-replace-additional-keybinds
;; -- NOT keymaps tho, see doc
(defun kill-phisearch-match ()
    "Kill the current isearch match string and continue searching."
    (interactive)
    (when phi-search--selection
      ;; In phisearch, we're in the minibuffer by default, and there are N
      ;; search-overlays of which we are centered on index
      ;; phi-search--selection, if any.
      (phi-search--with-target-buffer
       (let ((ov (nth phi-search--selection phi-search--overlays)))
         (kill-region (overlay-end ov) (overlay-start ov)))))
    (phi-search-complete))

(with-eval-after-load "phisearch"
  (define-key phi-search-default-map (kbd "C-.") 'kill-phisearch-match))

;;
;; Swiper
;;

(defun neph-swiper-current-word ()
  "Start swiper with the current word"
  (interactive)
  (let ((current-word (save-excursion
                         (neph-mark-current-word)
                         (buffer-substring (mark) (point)))))
    (swiper current-word)))

(add-to-list 'load-path "~/.emacs.d/swiper")
(autoload 'swiper "swiper" "Swiper popup thing" t)
(global-set-key (kbd "C-z s") 'swiper)

(defun isearch-to-swiper ()
    "Drop into swiper with current isearch"
    (interactive)
    (isearch-exit)
    (swiper isearch-string))

(define-key isearch-mode-map (kbd "C-z s") 'isearch-to-swiper)

;;
;; Company mode
;;
(add-to-list 'load-path "~/.emacs.d/company-mode")
(add-to-list 'load-path "~/.emacs.d/company-quickhelp")
(add-to-list 'load-path "~/.emacs.d/pos-tip")
(require 'neph-company-autoload)

(defun neph-company-setup ()
  (interactive)
  (company-mode t)
  (company-quickhelp-mode t)
  ;;(semantic-mode t)
  (local-set-key (kbd "<C-tab>") 'company-complete))

;; Turn on in these modes
(add-hook 'c-mode-common-hook   'neph-company-setup)
(add-hook 'python-mode-hook     'neph-company-setup)
(add-hook 'python-ts-mode-hook  'neph-company-setup)
(add-hook 'lisp-mode-hook       'neph-company-setup)
(add-hook 'emacs-lisp-mode-hook 'neph-company-setup)

;;
;; Copilot
;;
(add-to-list 'load-path "~/.emacs.d/jsonrpc-1.0.24")
(add-to-list 'load-path "~/.emacs.d/copilot")
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
;;(add-to-list 'load-path "~/.emacs.d/emacs-deferred")
;;(add-to-list 'load-path "~/.emacs.d/emacs-request")

;;(add-to-list 'load-path "~/.emacs.d/emacs-ycmd")
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

(add-to-list 'load-path "~/.emacs.d/yasnippet")
(require 'yasnippet)

;;
;; Flycheck
;;

(add-to-list 'load-path "~/.emacs.d/flycheck")
(autoload 'flycheck-mode "flycheck" "flycheck-mode" t)

;;
;; C++ Helper mode(s) : Company/lsp and associated helper libraries
;;

;; cquery
(add-to-list 'load-path "~/.emacs.d/epl")
(add-to-list 'load-path "~/.emacs.d/pkg-info")
(add-to-list 'load-path "~/.emacs.d/hydra")
(add-to-list 'load-path "~/.emacs.d/ace-window")
(add-to-list 'load-path "~/.emacs.d/pfuture")
(add-to-list 'load-path "~/.emacs.d/avy")
(add-to-list 'load-path "~/.emacs.d/yaml.el")
(add-to-list 'load-path "~/.emacs.d/lsp-mode")
(add-to-list 'load-path "~/.emacs.d/lsp-docker")
(add-to-list 'load-path "~/.emacs.d/lsp-mode/clients")
(add-to-list 'load-path "~/.emacs.d/treemacs/src/elisp")
(add-to-list 'load-path "~/.emacs.d/treemacs/src/extra")
(add-to-list 'load-path "~/.emacs.d/emacs-ccls")
(add-to-list 'load-path "~/.emacs.d/dap-mode")
(add-to-list 'load-path "~/.emacs.d/posframe")
(add-to-list 'load-path "~/.emacs.d/lsp-ui")
(add-to-list 'load-path "~/.emacs.d/lsp-pyright")
(add-to-list 'load-path "~/.emacs.d/lsp-treemacs")
(add-to-list 'load-path "~/.emacs.d/helm-lsp")
(setq lsp-pyright-multi-root nil)
(require 'lsp-treemacs)
(require 'treemacs)
(require 'treemacs-mouse-interface)
(require 'treemacs-hydras)
(require 'pkg-info)
(require 'lsp)
(require 'company)
(require 'company-quickhelp)
;;(require 'treemacs-projectile)
(require 'lsp-ui)
(require 'lsp-ui-flycheck)
(require 'lsp-headerline)
(require 'lsp-modeline)
(require 'lsp-diagnostics)

;; Pyright settings are snapshot on library load??
(setq lsp-pyright-multi-root nil)
(require 'lsp-pyright)
(require 'dap-mode)
(require 'dap-cpptools)
(require 'dap-ui)
(require 'dap-mouse)
(require 'dap-hydra)
(require 'ccls)
(require 'helm-lsp)
(setq company-quickhelp-color-background "black")
(setq ccls-executable "/usr/bin/ccls")

;; Use helm-lsp-workspace-symbol to replace xref-find-apropos (recommended by helm-lsp readme)
(define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol)

(lsp-treemacs-sync-mode 1)
(setq lsp-ui-doc-show-with-cursor t)
(setq lsp-lens-enable nil)


;; FIXME?
;;(with-eval-after-load 'lsp-mode
;;  (add-hook 'lsp-after-open-hook (lambda () (lsp-ui-flycheck-enable 1))))

(setq lsp-ui-peek-always-show t)

(setq ccls-sem-highlight-method 'font-lock)
(ccls-use-default-rainbow-sem-highlight)
;;(setq ccls-sem-highlight-method nil)
;; We'll set these from the theme.  Uncomment for random themes.
(setq ccls-args
      (list
       (concat "--init=" (json-encode
                          (ht
                           ;; ("index" (ht ("multiVersion" 1))) ;; 60G memory usage lol
                           ;; Clang args that trip things up, and include /usr/lib/glib-2.0 in compiles
                           ("clang" (ht ("extraArgs" [-ferror-limit=0 -I/usr/lib/glib-2.0/include/])
                                        ("excludeArgs" ["-frounding-math" "-march=pentium4"]))))))
       ;; Extra logging
       "-log-file=/tmp/ccls.log"
       "-v=1"))

(defun neph-clear-text-properties ()
  "Reset all text properties in the buffer."
  (interactive)
  (with-silent-modifications (set-text-properties (buffer-end 0) (buffer-end 1) nil)))

(defun neph-lsp-reset ()
  "Reconnects to LSP, fixing annoying CCLS highlighting bug."
  (interactive)
  (lsp-disconnect)
  (neph-clear-text-properties)
  (lsp))

(defun neph-lsp-reformat-definition ()
  "Reformat the definition under the cursor according to how LSP parsed it."
  (interactive)
  ;; Request the "hover" of the thing under cursor, which is the expanded definition of it.
  ;; FIXME this is only true in ccls
  (let* ((hoverContents (-some->> (lsp--text-document-position-params)
                      (lsp--make-request "textDocument/hover")
                      (lsp--send-request)
                      (gethash "contents")))
         (hoverText (-some->> (seq-subseq hoverContents -1)
                      (lsp-seq-first)
                      (gethash "value")))
         ;; Request the location/textual-range of the definition for the thing under cursor.
         (defRange (-some->> (lsp--text-document-position-params)
                     (lsp--make-request "textDocument/definition")
                     (lsp--send-request)
                     (car)
                     (gethash "targetRange")
                     (lsp--range-to-region)))
         (defStart (car defRange))
         (defEnd (cdr defRange)))
    (if (and hoverText (<= defStart (point) defEnd))
        ;;(message "Range: %s point: %s hover: %s" defRange point hoverText)
        (save-excursion
          (goto-char defStart)
          (delete-region defStart defEnd)
          (insert hoverText))
      (message "No definition recognized under cursor. (save file, and make sure you're on the type name)"))))

;; LSP UI keys, some are not used but reserved from equivalents in rtags configuration
(global-set-key (kbd "C-z C-,")   'lsp-ui-peek-find-references)
(global-set-key (kbd "C-z C-<")   'ccls-call-hierarchy)
(global-set-key (kbd "C-z ,")     'lsp-find-references)
(global-set-key (kbd "C-z <tab>") 'lsp-ui-imenu)
(global-set-key (kbd "C-z D")     'flycheck-list-errors)
(global-set-key (kbd "C-z C-l")   'neph-lsp-reformat-definition)
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
(global-set-key (kbd "C-z <C-left>")  (lambda () (interactive) (ccls-navigate "U")))
(global-set-key (kbd "C-z <C-right>") (lambda () (interactive) (ccls-navigate "D")))
(global-set-key (kbd "C-z <C-up>")    (lambda () (interactive) (ccls-navigate "L")))
(global-set-key (kbd "C-z <C-down>")  (lambda () (interactive) (ccls-navigate "R")))

;;
;; Irony-mode (deprecated)
;;   DEPRECATED - going to drop if ccls + lsp keeps working well
;;
(add-to-list 'load-path "~/.emacs.d/irony-mode")
(add-to-list 'load-path "~/.emacs.d/company-irony")
(add-to-list 'load-path "~/.emacs.d/flycheck-irony")
(require 'neph-irony-autoload)

;; Bonus key to use counsel-irony if available
(defun irony-mode-counsel-hook ()
  (when (require 'counsel nil t)
    (define-key irony-mode-map
      ;;[remap completion-at-point] 'counsel-irony)
      ;;[remap complete-symbol] 'counsel-irony)
      (kbd "<C-M-tab>") 'counsel-irony)))
(add-hook 'irony-mode-hook 'irony-mode-counsel-hook)

;; FIXME irony-mode breaks on headers due to that missing (car found)

;; Load flycheck-irony if both flycheck and irony get enabled
(defun neph-flycheck-irony-setup ()
  "Load flycheck-irony if both irony and flycheck are loaded."
  (when (and (featurep 'flycheck)
             (featurep 'irony)
             (not (featurep 'flycheck-irony)))
    (require 'flycheck-irony)
    (add-hook 'flycheck-mode-hook #'flycheck-irony-setup)))

;; Disabled by default - flycheck-irony is incredibly laggy for some reason, rtags provides better diagnostics
;;(with-eval-after-load "flycheck" (neph-flycheck-irony-setup))
;;(with-eval-after-load "irony" (neph-flycheck-irony-setup))

;;
;; Rtags
;;   DEPRECATED - going to drop if ccls + lsp keeps working well
;;

;; popup.el for rtags tooltips (needed anymore?)
(add-to-list 'load-path "~/.emacs.d/popup-el")
(autoload 'popup "popup" "Popup tooltip thing." t)

;; Rtags is installed separate from NephScripts, don't assume it is available
;; Don't load it in non-interactive mode, we don't want to issue calls to rc/etc.
(if (and (not noninteractive) (require 'rtags-disabled nil t))
    (progn
      (require 'company)
      (require 'company-quickhelp)
      (require 'company-rtags)
      ;; If we wanted to use rtags instead of irony-mode above
      ;;(with-eval-after-load "flycheck" (require flycheck-rtags))

      (cl-defun popup-tip (string
                           &key
                           point
                           (around t)
                           width
                           (height 15)
                           min-height
                           max-width
                           truncate
                           margin
                           margin-left
                           margin-right
                           scroll-bar
                           parent
                           parent-offset
                           nowait
                           nostrip
                           prompt
                           &aux tip lines)
        (tooltip-show string))

      (add-to-list 'company-backends 'company-rtags)

      (setq company-idle-delay nil)

      (setq company-async-timeout 10000)
      (setq company-rtags-max-wait 10000)
      (setq rtags-completions-enabled t) ; Needed?
      (setq rtags-track-container t)
      (setq company-rtags-use-async nil)

      (setq rtags-use-helm nil)
      (setq rtags-max-bookmark-count 10)

      (setq rtags-autostart-diagnostics t)
      (setq rtags-find-file-case-insensitive t)
      ;; FIXME rtags bug, it tries to do this but ends up not? Commented out, I think turned out unnecessary
      ;;(add-hook 'window-configuration-change-hook 'rtags-update-buffer-list)

      ;; FIXME: Messy, kinda works. Remaining problem is the results-buffer-other-window behavior --
      ;; we ideally want to wrap rtags-switch-to-buffer *within* handle-results-buffer, and do more
      ;; logic on where to open the results window it is trying to other-window-open
      ;;
      ;; I think the logic we want is split-current-pane-if-sensible-always

      (setq rtags-show-containing-function t)
      (defun neph-rtags-split-window ()
        ;;(message "neph-rtags-split-window!")
        ;;(message "Trying default split with %d" split-height-threshold)
        (let ((window (split-window-sensibly)))
          ;;(message "Called!")
          (if window window
            ;;(message "Trying lessened-height split")
            (let ((split-height-threshold 80))
              (split-window-sensibly)))))
      (defun neph-rtags-other-window ()
        ;;(message "neph-rtags-other-window!")
        (if (boundp 'neph-rtags-original-command-window)
            (if (eq neph-rtags-original-command-window (get-buffer-window rtags-buffer-name))
                (progn
                  ;;(message "other-window: Falling back to split")
                  (neph-rtags-split-window))
              ;;(message "other-window: Using original")
              neph-rtags-original-command-window)
          ;;(message "other-window: using other-window 1")
          (other-window 1)))

      (setq rtags-popup-results-buffer t)
      (setq rtags-results-buffer-other-window t)
      (setq rtags-split-window-function 'neph-rtags-split-window)
      (setq rtags-other-window-function 'neph-rtags-other-window)

      (defadvice rtags-find-references-at-point (around neph-rtags-find-references-at-point activate)
        ;;(message "find-references-at-point advice!")
        ;;(let ((neph-rtags-original-command-window (selected-window)))
          ad-do-it)
      (defadvice rtags-handle-results-buffer (around neph-rtags-handle-results-buffer activate)
        ;;(message "ADVICE rtags-handle-results-buffer")
        (let ((split-height-threshold 80))
          ad-do-it))

      (defadvice rtags-select (around neph-rtags-select activate)
        ;;(message "ADVICE rtags-select")
        ad-do-it)
      (defadvice rtags-switch-to-buffer (around neph-rtags-switch-to-buffer activate)
        ;;(message "ADVICE rtags-switch-to-buffer")
        ad-do-it)
      (defadvice rtags-select-other-window (around neph-rtags-select-other-window activate)
        ;;(message "ADVICE rtags-select-other-window")
        ad-do-it)
      (defadvice rtags-jump-to-first-match (around neph-rtags-jump-to-first-match activate)
        ;;(message "ADVICE rtags-jump-to-first-match")
        ad-do-it)
      (defadvice rtags-goto-location (around neph-rtags-goto-location activate)
        ;;(message "ADVICE rtags-goto-location")
        ad-do-it)
      (defadvice rtags-rtags-show-target-in-other-window (around neph-rtags-rtags-show-target-in-other-window activate)
        ;;(message "ADVICE rtags-rtags-show-target-in-other-window")
        ad-do-it)


      (setq rtags-enable-unsaved-reparsing nil)
      (rtags-set-periodic-reparse-timeout nil)

      (setq rtags-tooltips-enabled nil)
      (setq rtags-display-current-error-as-tooltip nil)
      (setq rtags-display-summary-as-tooltip nil)

      ;; When using rtags provide a backend to irony
      (defun irony-cdb-rtags-neph (command &rest args)
        (cl-case command
          (get-compile-options (irony-cdb-rtags-neph--get-compile-options))))

      (defun irony-cdb-rtags-neph--get-compile-options ()
        (if (rtags-is-running)
          (let ((path (rtags-buffer-file-name)))
            (when path
              (with-temp-buffer
                (rtags-call-rc :path path "--sources" path "--compilation-flags-only" "--compilation-flags-pwd" "--compilation-flags-split-line")
                (let* ((str (buffer-substring-no-properties (point-min) (point-max)))
                       (result (split-string str "\n" t))
                       (pwdraw (car-safe result))
                       (pwd (when (and pwdraw (string= (substring pwdraw 0 5) "pwd: ")) (substring pwdraw 5))))
                  (when pwd
                    (list (cons
                           (append '("-Wextra" "-ferror-limit=0")
                                   (delete "-fpch-preprocess"
                                           ;; Stripping first two (c++ -c) and last 3 (-o output
                                           ;; input) args for just the file specific compilation
                                           ;; flags
                                           (butlast
                                            (nthcdr
                                             2
                                             ;; Strip leading pwd: and take everything up to
                                             ;; the next pwd:
                                             ;;
                                             ;; (multi-compile mode -- pwd: means start of
                                             ;; next mode for file)
                                             ;;
                                             ;; TODO: Ideally we'd somehow combine the
                                             ;; multiple entries
                                             (seq-take-while
                                              (lambda (e)
                                                (not (string-prefix-p "pwd: " e)))
                                              (nthcdr 1 result)))
                                            ;; (v-- end of butlast)
                                            3)))
                           pwd)))))))
          ;; Else, warn and nill
          (message "irony-cdb-rtags-neph: No RDM, cannot pull flags for this file")
          nil)))
  ;; Else - No rtags
  ;; Provide the irony backend but make it always return nuh
  (defun irony-cdb-rtags-neph (command &rest args) nil))

;; Semantic
; (require 'semantic)
; (require 'semantic/bovine/gcc)
; (global-semantic-decoration-mode t)
; (global-semantic-stickyfunc-mode t)
; (global-semantic-idle-scheduler-mode -1)

;; EDE
(global-ede-mode t)

;; function-args modes (Disabled pending semantic)
;;(add-to-list 'load-path "~/.emacs.d/function-args")
;;(require 'function-args)
;;(fa-config-default)
;;(setq moo-select-method 'helm)

(defun company-mode-moz ()
  (setq company-clang-arguments (split-string
                                 (shell-command-to-string
                                  (concat "~/.emacs.d/moz_objdir.sh "
                                          (buffer-file-name)))))
  (company-mode t)
  (local-set-key (kbd "<C-tab>") 'company-complete))

;; Keys for C++ completion and such
;;(global-set-key (kbd "C-z SPC") 'helm-semantic)
;;(global-set-key (kbd "C-z C-SPC") 'moo-jump-local)

(when (featurep 'rtags)
  ;; FIXME need to also wrap rtags-references-tree, then rtags-goto-location needs to deactivate it so single-item matches don't asplode.
  ;;(defadvice rtags-references-tree (around neph-rtags-references-tree activate)
  ;;  (let* ((neph-in-references-tree t)
  ;;         (neph-original-split-height-threshold split-height-threshold)
  ;;         (split-height-threshold 70))
  ;;    ;;(message (concat "rtags-references-tree with height " (number-to-string split-height-threshold)))
  ;;    ad-do-it))
  ;;(defadvice rtags-goto-location (around neph-rtags-goto-location activate)
  ;;  (let ((split-height-threshold (if (boundp 'neph-original-split-height-threshold)
  ;;                                    neph-original-split-height-threshold
  ;;                                  split-height-threshold)))
  ;;    ;;(message (concat "rtags-goto-location with height " (number-to-string split-height-threshold)))
  ;;    (if (boundp 'neph-in-references-tree)
  ;;        (rtags-select-and-remove-rtags-buffer))
  ;;    ad-do-it))

  (global-set-key (kbd "C-z C-.") 'rtags-find-symbol-at-point)
  (global-set-key (kbd "C-z M-r") 'rtags-reparse-file)
  (global-set-key (kbd "C-z C-,") 'rtags-find-references-at-point)
  (global-set-key (kbd "C-z C-<") 'rtags-references-tree)
  (global-set-key (kbd "C-z C->") 'rtags-find-virtuals-at-point)
  (global-set-key (kbd "C-z .") 'rtags-find-symbol)
  (global-set-key (kbd "C-z ,") 'rtags-find-references)
  (global-set-key (kbd "C-z C-/") (lambda () (interactive) (delete-windows-on rtags-buffer-name t)))
  (global-set-key (kbd "C-z C-n") 'rtags-next-match)
  (global-set-key (kbd "C-z C-p") 'rtags-previous-match)
  (global-set-key (kbd "C-z <tab>") 'rtags-imenu)
  (global-set-key (kbd "C-z D") 'rtags-diagnostics)
  (global-set-key (kbd "C-z i") 'rtags-fixit)
  (global-set-key (kbd "C-z I") 'rtags-fix-fixit-at-point)
  (global-set-key (kbd "C-z DEL") 'rtags-location-stack-back)
  (global-set-key (kbd "C-z <S-backspace>") 'rtags-location-stack-back)
  (global-set-key (kbd "C-z C-S-R") 'rtags-rename-symbol)
  (global-set-key (kbd "C-z C-l") 'neph-rtags-expand-auto)

  ;; Rtags janky replace-auto-with-symbol.  Needs work -- only works if you're in the symbol name
  ;; itself, and the declaraction is of the style (auto ... pFoo) and not something fancier like a
  ;; function declaration (needs more support from rtags)
  (defun neph-rtags-expand-auto ()
    "Expands current auto symbol with its definition"
    (interactive)
      (save-excursion
        (let ((symb (rtags-current-symbol))
              (tok (rtags-current-token))
              (word (current-word)))
          ;; If we have a symbol, and it's not the same as the token, and we see [auto ...] before
          ;; us and [... =] after.  This is because we only support the pretty basic case.
          ;;
          ;; Checking tok!=symb is because sometimes rtags will tell us the current symbol is just
          ;; the token name when it hasn't parsed enough to have all the type information.
          (if (and symb (not (string= symb "")) (not (string= symb tok))
                   (looking-back "auto [^=]*") (looking-at ".*="))
              (progn
                (re-search-backward "[\t\s]auto[\t\s]")
                (forward-char 1)
                (set-mark (point))
                (re-search-forward word)
                (delete-region (mark) (point))
                (insert symb))
            ;; else
            (message "Couldn't find auto symbol at point")))))

  (defun rtags-test-menu ()
    "Test help text"
    (rtags-location-stack-push)
    (let* ((helm-source-grep
            (helm-build-async-source
                (capitalize (helm-grep-command t))
              :header-name (lambda (name)
                             "Rtags global menu thing")
              :candidates-process (lambda ()
                                    (with-temp-buffer
                                      (rtags-call-rc ;; "--imenu"
                                       "--list-symbols"
                                       init
                                       "-Y" "--imenu"
                                       (if rtags-wildcard-symbol-names "--wildcard-symbol-names"))
                                      (eval (read (buffer-string)))) )
              :filter-one-by-one 'helm-grep-filter-one-by-one
              :candidate-number-limit 9999
              :nohighlight t
              :mode-line helm-grep-mode-line-string
              ;; We need to specify keymap here and as :keymap arg [1]
              ;; to make it available in further resuming.
              :keymap helm-grep-map
              :history 'helm-grep-history
              :action (helm-make-actions
                       "Find file" 'helm-grep-action
                       "Find file other frame" 'helm-grep-other-frame
                       (lambda () (and (locate-library "elscreen")
                                       "Find file in Elscreen"))
                       'helm-grep-jump-elscreen
                       "Save results in grep buffer" 'helm-grep-save-results
                       "Find file other window" 'helm-grep-other-window)
              :persistent-action 'helm-grep-persistent-action
              :persistent-help "Jump to line (`C-u' Record in mark ring)"
              :requires-pattern 2)))
      (helm
       :sources 'helm-source-grep
       :input (if (region-active-p)
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (thing-at-point 'symbol))
       :buffer (format "*helm %s*" (if use-ack-p
                                       "ack"
                                     "grep"))
       :default-directory (projectile-project-root)
       :keymap helm-grep-map
       :history 'helm-grep-history
       :truncate-lines t)))

  (defun rtags-global-imenu ()
    (interactive)
    (rtags-location-stack-push)
    (let* ((fn (buffer-file-name))
           (init (read-string "Initial search: "))
           (alternatives (with-temp-buffer
                           (message (concat "Using: " init))
                           (rtags-call-rc :path fn "--imenu"
                                          "--list-symbols" init
                                          "-Y"
                                          (when rtags-wildcard-symbol-names "--wildcard-symbol-names"))
                           (eval (read (buffer-string)))))
           (match (car alternatives)))
      (if (> (length alternatives) 1)
          (setq match (completing-read "Symbol: " alternatives nil t)))
      (if match
          (rtags-goto-location (with-temp-buffer (rtags-call-rc :path fn "-F" match) (buffer-string)))
        (message "RTags: No symbols"))))

  (global-set-key (kbd "C-z <C-M-tab>") 'rtags-global-imenu))

;;
;; Smart Tabs
;;

(add-to-list 'load-path "~/.emacs.d/smarttabs")
(require 'smart-tabs-mode)
(smart-tabs-insinuate 'c 'javascript 'c++)

;;
;; GDB - upstream gdb-mi, not to be confused with the weirdnox version below
;;

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
;; emacs-gdb -- weirdNox's replacement for gdb-mi
;;

(add-to-list 'load-path "~/.emacs.d/emacs-gdb")
(fmakunbound 'gdb)
(fmakunbound 'gdb-enable-debug)
(load-library "gdb-mi")

;;(require 'neph-weirdnox-gdb-autoload)
;; FIXME automatically replace gdb-mi

;;
;; ido
;;

(add-to-list 'load-path "~/.emacs.d/ido-vertical-mode")
(autoload 'ido "ido" "Ido thing." t)
(autoload 'ido-vertical-mode "ido-vertical-mode" "ido-vertical-mode" t)
(ido-mode t)
(ido-vertical-mode 1)

;; Something keeps disabling this silently.
;; Intercept it and print a backtrace.
(defun neph-ido-mode (&optional arg)
  "Advice to intercept `'ido-mode`' (with ARG) turning itself off and print a backtrace."
  (when (and (not (called-interactively-p))
             (not ido-mode))
    (debug nil "NEPH: Something disabled ido-mode")))
(advice-add 'ido-mode :after #'neph-ido-mode)

;;
;; Rainbow Delimiters
;;

(add-to-list 'load-path "~/.emacs.d/rainbow-delimiters")
(autoload 'rainbow-delimiters-mode "rainbow-delimiters" "rainbow-delimiters" t)

;;
;; Minimap
;;

(add-to-list 'load-path "~/.emacs.d/minimap")
(autoload 'minimap-mode "minimap" "minimap" t)

(with-eval-after-load "minimap"
              (set-face-attribute 'minimap-font-face nil :family "Droid Sans Mono" :height 10 :weight 'ultrabold)
              (setq minimap-window-location (quote right))
              (setq minimap-width-fraction 0.01))

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

(load-file "~/.emacs.d/neph-modeline-util.el")
(defun neph-fill-to (reserve)
  `(:eval (propertize " " 'display '(space :align-to (- right-margin
                                                        ,reserve)))))
(defun neph-modeline-hud (height width)
  (propertize " " 'display (neph-hud "#0C0C0C" "#222222" height width)
              'face 'neph-modeline-hud))

;; TODO set this only when buffer path changes, rather than per frame
(setq neph-modeline-path
      '(:eval (let* ((rawname (buffer-file-name))
                     (bufname (if rawname (propertize rawname 'face 'neph-modeline-path) nil))
                     ;; Paths to replace. Of the form ((search replace) ...)
                     (replacements `((,(getenv "HOME") "~"))))
                ;; Also replace projectile root with project name when available
                (when (and (featurep 'projectile) (projectile-project-p))
                  (add-to-list 'replacements
                               (list
                                (projectile-project-root)
                                (concat (projectile-project-name) "/"))))
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

(defface neph-modeline-hud
  '((t (:inherit mode-line-face)))
  "Neph modeline hud face")
(defface neph-modeline-id
  '((t (:inherit mode-line-face
        :foreground "#DD5"
        :weight bold)))
  "Neph modeline buffer id face")
(defface neph-modeline-mode
  '((t (:inherit mode-line-face
        :foreground "#464")))
  "Neph modeline mode face")
(defface neph-modeline-misc
  '((t (:inherit mode-line-face
        :height 75
        :foreground "#444"
        :width condensed)))
  "Neph modeline minor info face")
(defface neph-modeline-path
  '((t (:inherit mode-line-face
        :foreground "#DFDDDD")))
  "Neph modeline path face")
(defface neph-modeline-path-replacement
  '((t (:inherit neph-modeline-path
        :foreground "#7F7777")))
  "Neph modeline path face for replacements made by neph-modeline-shortpaths")
(defface neph-modeline-id-inactive
  '((t (:inherit neph-modeline-id
        :foreground "#CC9")))
  "Neph modeline buffer id inactive face")
(defface neph-modeline-stat-readonly
  '((t (:inherit mode-line-face
        :foreground "#6666EE"
        :box (:line-width 2))))
  "Neph modeline readonly status face")
(defface neph-modeline-stat-modified
  '((t (:inherit mode-line-face
        :foreground "#FF5555"
        :weight bold)))
  "Neph modeline modified status face")
(defface neph-modeline-stat-clean
  '((t (:inherit mode-line-face
        :foreground "#555")))
  "Neph modeline clean status face")
(defface neph-modeline-which-func
  '((t (:inherit mode-line-face
        :foreground "#666"
        :height 90)))
  "Neph modeline which-func-mode face")
(setq-default mode-line-format
              '(:eval
                (list
                 ;; Bonus hacky alignment, such that if the buffer is too narrow
                 ;; to show the modeline below hud, the height of the modeline stays the same
                 (propertize "\u200d" 'display '(list (raise -0.30) (height 1.25)))
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

(defcustom neph-sticky-header nil "Event-updated portion of the header line")
(defcustom neph-sticky-header-valid-range nil "Range of characters for which the current sticky header is valid")
(setq rtags-current-container-hook (lambda (containerName)
                                     (let* ((container       (rtags-current-container))
                                            (startLineCell   (when container (assoc 'startLine container)))
                                            (endLineCell     (when container (assoc 'endLine container)))
                                            (startColumnCell (when container (assoc 'startColumn container)))
                                            (endColumnCell   (when container (assoc 'endColumn container)))
                                            (needed          (and container startLineCell endLineCell
                                                                  startColumnCell endColumnCell))
                                            (startLine       (when needed (cdr startLineCell)))
                                            (endLine         (when needed (cdr endLineCell)))
                                            (startColumn     (when needed (cdr startColumnCell)))
                                            (endColumn       (when needed (cdr endColumnCell)))
                                            (curLine         (when needed (line-number-at-pos)))
                                            (lineOffset      (when needed (- startLine curLine)))
                                            (endLineOffset   (when needed (- endLine curLine)))
                                            (charStart       (when needed (line-beginning-position (+ lineOffset 1))))
                                            (charEnd         (when needed (line-end-position (+ lineOffset 1))))
                                            (regionStart     (when needed (+ (line-beginning-position (+ lineOffset 1))
                                                                             startColumn)))
                                            (regionEnd       (when needed (+ (line-end-position (+ endLineOffset 1))
                                                                             endColumn)))
                                            (lineStr         (when (and needed charStart charEnd)
                                                               (replace-regexp-in-string "^[ \t\n]+" ""
                                                                                         (buffer-substring charStart charEnd)))))
                                       (setq neph-sticky-header
                                              (if lineStr lineStr (propertize "- unknown -" 'face 'neph-modeline-misc)))

                                       (setq neph-sticky-header-valid-range
                                             (if (and regionStart regionEnd)
                                                 (cons regionStart regionEnd)
                                               nil))
                                       (force-mode-line-update))))

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
;; God mode
;;

(add-to-list 'load-path "~/.emacs.d/god-mode")
(autoload 'god-mode "god-mode" "god-mode" t)
(global-set-key (kbd "C-z C-z") 'god-local-mode)

;;
;; Scrolling
;;

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
;;

(autoload 're-builder "re-builder" "re-builder" t)
(setq reb-re-syntax 'string)

;;
;; Misc modes
;;

(add-to-list 'load-path "~/.emacs.d/fic-mode.git")
(autoload 'fic-mode "fic-mode" "fic-mode" t)
(with-eval-after-load "fic-mode"
  (add-to-list 'fic-highlighted-words "XXX"))

(add-to-list 'load-path "~/.emacs.d/git-gutter-fringe")
(add-to-list 'load-path "~/.emacs.d/git-gutter")
(add-to-list 'load-path "~/.emacs.d/fringe-helper")
(require 'fringe-helper)
(require 'git-gutter)
(require 'git-gutter-fringe)
(add-to-list 'load-path "~/.emacs.d/rainbow-mode")
(autoload 'rainbow-mode "rainbow-mode" "Rainbow Mode." t)

;;
;; P4
;;

;; p4.el
(add-to-list 'load-path "~/.emacs.d/p4.el")
(autoload 'p4 "p4" "p4" t)

;; p4vc commands
(defun neph-p4vc (command)
  (if (buffer-file-name)
      (if (executable-find "p4vc")
          (let ((cmd (concat
                      "cd $(dirname " (shell-quote-argument (buffer-file-name)) ") && "
                      "env XDG_CURRENT_DESKTOP=gnome p4vc "
                      (shell-quote-argument command)
                      " "
                      (shell-quote-argument (buffer-file-name))
                      "| tee /dev/null" ;; This works around some p4v bug where it fails when it has no stdout
                      )))
            ;; This is the way the help actually suggests you prevent it from opening this buffer.
            (let ((display-buffer-alist (cons '("\\*Async Shell Command\\*" (display-buffer-no-window))
                                              display-buffer-alist)))
              (message (concat "Running: " cmd))
              (async-shell-command cmd)))
        (message "!! p4vc not installed/available"))
    (message "!! This buffer has no file name")))

(global-set-key (kbd "C-z P t") (lambda () (interactive) (neph-p4vc "tlv")))
(global-set-key (kbd "C-z P c") (lambda () (interactive) (neph-p4vc "revgraph")))

;;
;; Ediff
;;

(defun neph-ediff-mode ()
  (git-gutter-mode -1))
(add-hook 'ediff-prepare-buffer-hook 'neph-ediff-mode)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-merge-split-window-function 'split-window-horizontally)

;;
;; AStyle
;;

(defun astyle-beautify-region()
  (interactive)
  (if (executable-find "astyle")
      (let ((cmd "astyle --style=allman --pad-paren-in --pad-oper --pad-header --unpad-paren --max-code-length=100 --break-blocks"))
        (shell-command-on-region (region-beginning) (region-end) cmd (current-buffer) t))
    (message "!! astyle command not installed/available")))

;;
;; js-beautify
;;

(defun js-beautify-region()
  (interactive)
  (if (executable-find "js-beautify")
      (let ((cmd "js-beautify"))
        (shell-command-on-region (region-beginning) (region-end) cmd (current-buffer) t))
    (message "!! js-beautify command not installed/available")))

;;
;; Flyspell-lazy
(add-to-list 'load-path "~/.emacs.d/flyspell-lazy")
(require 'neph-flyspell-lazy-autoload)
(setq flyspell-lazy-idle-seconds 1)
(setq flyspell-lazy-window-idle-seconds 1)

;; With the lazy mode window timer set
(defun flyspell-lazy-toggle (arg)
  "Toggle flyspell lazy mode"
  (interactive "p")
  (if (and (boundp 'flyspell-mode) flyspell-mode)
      (progn
        (flyspell-mode 0)
        (flyspell-lazy-mode 0))
    (flyspell-lazy-mode t)
    (flyspell-mode t)
    (flyspell-lazy-check-visible)))
(global-set-key (kbd "C-c M-l") 'flyspell-lazy-toggle)

;;
;; Projectile
;;

(add-to-list 'load-path "~/.emacs.d/projectile")
(add-to-list 'load-path "~/.emacs.d/helm-projectile")
(require 'neph-projectile-autoload)
;; Must be set before loading helm-projectile according to help text. Makes it not super slow.
;;(setq helm-projectile-fuzzy-match nil)
(require 'neph-helm-projectile-autoload)

;; Additional autoloads for helm-projectile
(autoload 'helm-projectile-ag "~/.emacs.d/helm-projectile/helm-projectile")
(autoload 'helm-projectile-switch-to-buffer "~/.emacs.d/helm-projectile/helm-projectile")
(autoload 'helm-projectile-switch-project "~/.emacs.d/helm-projectile/helm-projectile")

(with-eval-after-load "projectile"
  (setq projectile-completion-system 'helm)
  (setq projectile-enable-caching t)
  (projectile-global-mode t)
  (with-eval-after-load "helm"
    (helm-projectile-on)))

;; If -alt appears in the path preceeding the final component, append -alt to the name
;; e.g. ~/git-alt/project shows up differently from ~/git/project
;; (Incredibly specific to the author's workflow)
;; A more robust version would be to feed known projects into uniquify
(defun neph-projectile-project-name (root)
  "Neph hook for projectile-project-name"
  (let ((default-name
          (if (string-match "/main/$" root)
              (concat
               (projectile-default-project-name
                (replace-regexp-in-string "/main/$" "" root))
               "-main")
            (projectile-default-project-name root))))
    (if (string-match "-alt.*/" root)
        (concat default-name "-alt")
        default-name)))

(setq projectile-project-name-function 'neph-projectile-project-name)

(let ((neph-ignored-patterns '("*.dwo" "*.o" "*.P" "*.dSYM" "*.vtx" "*.vtf" "*.wav" "*.mdl" "*.vvd"
                               "*.mp3" "*.png" "*.phy" "*.jpg" "*.pyc" "*.lib" "*.psd" "*.tga"
                               "*.dll" "*.vcs" "*.bsp" "*.zip" "*.exe")))
  (setq projectile-generic-command (concat "find . -type f "
                                           (mapconcat (lambda (x) (concat "-not -iname '" x "'"))
                                                      neph-ignored-patterns " -and ")
                                           " -print0")))

;; Default
;; (setq projectile-indexing-method 'alien)

;; helm projectile-ag/rg with default args
(defun helm-projectile-ag-cpp()
  (interactive)
  (require 'helm-projectile)
  (let ((helm-ag-base-command (concat helm-ag-base-command " --cpp --cc")))
    (helm-projectile-ag)))
(defun helm-projectile-rg-cpp()
  (interactive)
  (require 'helm-projectile)
  (let ((helm-rg-default-extra-args (split-string-and-unquote "-t cpp -t c")))
    (call-interactively 'helm-projectile-rg)))
(defun helm-projectile-ag-cpp-this-word()
  (interactive)
  (require 'helm-projectile)
  (save-excursion
    (neph-mark-current-word)
    (let ((helm-ag-base-command (concat helm-ag-base-command " --cpp --cc")))
      (helm-projectile-ag))))
(defun helm-projectile-ag-this-word()
  (require 'helm-projectile)
  (interactive)
  (save-excursion
    (neph-mark-current-word)
    (helm-projectile-ag)))
(defadvice helm-projectile-find-file (around helm-projectile-find-file-no-case activate)
  (let ((helm-case-fold-search nil))
    ad-do-it))
(defadvice projectile-find-file (around projectile-find-file-no-case activate)
  (let ((helm-case-fold-search nil))
    ad-do-it))
(defadvice projectile-find-file-in-known-projects (around projectile-find-file-in-known-projects-no-case activate)
  (let ((helm-case-fold-search nil))
    ad-do-it))
(defadvice helm-projectile-find-file-in-known-projects (around helm-projectile-find-file-in-known-projects-no-case activate)
  (let ((helm-case-fold-search nil))
    ad-do-it))

(global-set-key (kbd "C-z M-f") 'projectile-find-file)
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
(global-set-key (kbd "C-z p") 'helm-projectile-switch-project)
;(global-set-key (kbd "C-z C-p") 'helm-projectile)

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
;; Web-mode
;;

(add-to-list 'load-path "~/.emacs.d/web-mode")
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

(defface neph-highlight-whitespace-tab '((t (:background "#442222")))
  "Neph face to be used when whitespace-tab should be highlighted")

(defun neph-set-whitespace-tab-override (override-face)
  ;; Reset current overwrrite
  (when (and (boundp 'neph-remapped-whitespace-tab-cookie) neph-remapped-whitespace-tab-cookie)
    (face-remap-remove-relative neph-remapped-whitespace-tab-cookie))
  ;; If passed, set new override
  (when override-face
    (setq neph-remapped-whitespace-tab-cookie
          (face-remap-add-relative 'whitespace-tab override-face))))

(defun neph-base-cfg ()
  "Set minor modes and buffer-local settings for a coding-mode buffer."
  (display-line-numbers-mode t)
  (yas-minor-mode t)
  (smart-tabs-mode 0)
  (c-set-offset 'cpp-macro 0 nil) ;; Indent preprocessor macros with code instead of
                                  ;; beggining-of-line
  (c-set-offset 'case-label '+) ;; Indent case statements in switches
  (setq c-basic-offset 2)
  (setq python-indent-offset 2)
  (setq c-default-style "linux")
  ;; Indent one-liners but not others
  ;;
  ;; Unless we just created a brace pair, assume {} is about to become multi-line and let
  ;; electric-brace move it back over.
  (c-set-offset 'substatement-open (lambda (foo)
                                     (when (not (and (looking-back "{") (looking-at "}")))
                                       (c-indent-one-line-block foo))))
  ;; Don't indent inline definitions in e.g. classes, except for one liners
  (c-set-offset 'inline-open 'c-indent-one-line-block)
  (setq sh-basic-offset 2)
  (setq sh-indentation 2)
  (setq indent-tabs-mode nil)
  (setq tab-width 2)
  (setq js-indent-level 2)
  (setq css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-markup-indent-offset 2)
  (git-gutter-mode t)
  (rainbow-mode t)
  (display-fill-column-indicator-mode)
  ;; Breaks in noninteractive mode
  (when (not noninteractive) (flycheck-mode t))
  (electric-pair-mode t)
  ;;(highlight-symbol-mode t) ;; Forces fontify maybe?
  (neph-set-whitespace-tab-override nil)
  (when (featurep 'rtags) (rtags-enable-standard-keybindings))
  (setq fill-column 120)
  ;; This is awful, still needed? Something was forcing fontify on the whole buffer instantly,
  ;; making new files janky
  (run-with-idle-timer 0.5 nil (lambda ()
                                 (rainbow-delimiters-mode t) ;; FIXME forces fontification always maybe?
                                 (when (not (and (boundp 'lsp-mode) lsp-mode))
                                   (color-identifiers-mode t))
                                 (fic-mode t))))

;; Currently just the base config
(defun neph-space-cfg ()
  "Set minor modes and config for coding-mode buffer using default space indentation."
  (interactive)
  (neph-base-cfg)
  ;; Remap whitespace-tab to the highlighted tab face
  (neph-set-whitespace-tab-override 'neph-highlight-whitespace-tab))

(defadvice align-regexp (around align-regexp-with-spaces activate)
  (let ((indent-tabs-mode nil))
    ad-do-it))

(defadvice align (around align-with-spaces activate)
  (let ((indent-tabs-mode nil))
    ad-do-it))

;; Tabs, 4 wide with 4 indent to match e.g. default VS style. Smart-tabs.
(defun neph-tab-cfg ()
  "Set minor modes and config for a coding-mode buffer using VS-compatible tab indentation."
  (interactive)
  (neph-base-cfg)
  (smart-tabs-mode t)
  (setq indent-tabs-mode 'tabs)
  (setq c-basic-offset 4)
  (setq sh-basic-offset 4)
  (setq sh-indentation 4)
  (setq tab-width 4)
  (setq lua-indent-level 4)
  (setq js-indent-level 4)
  (setq python-indent-offset 4)
  (setq web-mode-code-indent-offset 4)
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-css-indent-offset 4))

(defun neph-lsp-if-projectile ()
  "Invoke lsp if this buffer is a projectile project."
  (interactive)
  (let ((projectile-dir (when (and (featurep 'projectile) (projectile-project-p)) (projectile-project-root))))
    (when (and projectile-dir (length projectile-dir))
      (lsp-deferred))))

(defun neph-lsp-mode ()
  "Set minor modes and config for buffers using LSP."
  (interactive)
  ;; LSP provides variable coloring, so turn this off there
  ;; (thus keeping it on for non-LSP languages)
  ;; FIXME actually only ccls does and it's off
  (color-identifiers-mode 0)
  )

;; Default modes

(add-to-list 'auto-mode-alist '("/yaourtrc\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("/bash-fc.[^/]+\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.ma?k\\'" . makefile-mode))
(add-to-list 'auto-mode-alist '("\\.sch\\'" . c-mode))
(add-to-list 'auto-mode-alist '("/PKGBUILD\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("/\\.?bash\\(rc\\|_profile\\)\\'" . sh-mode))
;; Default .j2 files to conf-mode, though these are jinja files that could be anything
(add-to-list 'auto-mode-alist '("\\.j2\\'" . conf-mode))
;; Use js-mode for vpc/vgc/res files for now, using tab-cfg
(defun neph-js-mode-hook ()
  "Set minor modes and buffer-local configuration for js language buffers."
  (if (and (stringp buffer-file-name)
           (string-match "\\.\\(v[pg]c\\|res\\)\\'" buffer-file-name))
      (neph-tab-cfg)
    (neph-space-cfg)))
(add-to-list 'auto-mode-alist '("\.\\(v[pg]c\\|res\\)$" . js-mode))
(add-hook 'js-mode-hook 'neph-js-mode-hook)
(add-hook 'sh-mode-hook 'neph-space-cfg)
(add-hook 'conf-space-mode-hook 'neph-space-cfg)
(add-hook 'sql-mode-hook 'neph-space-cfg)
(add-hook 'python-mode-hook 'neph-space-cfg)
(add-hook 'java-mode-hook 'neph-space-cfg)
(add-hook 'lisp-mode-hook 'neph-space-cfg)
(add-hook 'emacs-lisp-mode-hook 'neph-space-cfg)
(add-hook 'rustic-mode-hook 'neph-space-cfg)
(add-hook 'c-mode-common-hook 'neph-tab-cfg) ; Default to tabs mode for now,
                                             ; should have path detection or
                                             ; something

;; Modes to try to auto-start lsp in, if they're part of a project
(add-hook 'c-mode-hook 'neph-lsp-if-projectile)
(add-hook 'c++-mode-hook 'neph-lsp-if-projectile)
(add-hook 'sh-mode-hook 'neph-lsp-if-projectile)
(add-hook 'python-mode-hook 'neph-lsp-if-projectile)

(add-hook 'lsp-after-open-hook 'neph-lsp-mode)

;; For web mode in tabs, we want to disable whitespace tabs because they conflict with the
;; php-background-coloring.  In space mode we can just use neph-space-cfg, as we want to highlight
;; errant tabs.  BUT - whitespace mode needs to be re-started when screwing with this variable.
(defun neph-web-tab-cfg ()
  (interactive)
  (let ((filtered-whitespace-style (remove 'tabs whitespace-style)))
    (setq-local whitespace-style filtered-whitespace-style))
  (whitespace-mode nil)
  (whitespace-mode t)
  (neph-tab-cfg))
(defun neph-web-space-cfg ()
  (interactive)
  (kill-local-variable 'whitespace-style)
  (whitespace-mode nil)
  (whitespace-mode t)
  (neph-space-cfg))

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
;;

(require 'tramp)
(setq tramp-default-method "ssh")
; No auto-save
(defun tramp-set-auto-save ()
  (auto-save-mode -1))

(defun sudoize-buffer ()
  "Reopens the current file with sudo."
  (interactive)
  ;; By default changing the visited file name counts as a modification, but this should be the same file.
  (with-silent-modifications
    (set-visited-file-name
     (concat "/sudo::/" (buffer-file-name))))
  (toggle-read-only 0))

(defun drop-sudo ()
  "Drops tramp sudo sessions."
  (interactive)
  (dolist (buffer (buffer-list))
    (let ((name (buffer-name buffer)))
      (when (and name (string-match "^*tramp/sudo " name))
        (kill-buffer buffer))))
  (message "Dropped sudo buffers"))

(global-set-key (kbd "C-z C-S-u") 'sudoize-buffer)
(global-set-key (kbd "C-z C-M-S-u") 'drop-sudo)

;;
;; Artist mode
;;
(global-set-key (kbd "C-z C-M-a") 'artist-mode) ;; C-c C-c exits artist mode


;;
;; Yaml mode
;;
(add-to-list 'load-path "~/.emacs.d/yaml-mode")
(require 'neph-yaml-mode-autoload)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.sls\\'" . yaml-mode)) ;; Salt
(with-eval-after-load "yaml-mode"
  (add-hook 'yaml-mode-hook 'neph-space-cfg))

;;
;; Term mode
;;

;; Global hl-line-mode block
(add-hook 'eshell-mode-hook (lambda ()
                                    (setq-local global-hl-line-mode
                                                nil)))
(add-hook 'term-mode-hook (lambda ()
                                    (setq-local global-hl-line-mode
                                                nil)))

;;
;; isearch tweaks
;;

; Always exit isearch at the beginning of the match
(defun isearch-exit-at-start-hook ()
  (when (and isearch-forward isearch-other-end (not isearch-mode-end-hook-quit))
    (goto-char isearch-other-end)))

(add-hook 'isearch-mode-end-hook 'isearch-exit-at-start-hook)
(defadvice isearch-exit (after isearch-exit-at-start-hook)
  "Go to beginning of match."
  (when (and isearch-forward isearch-other-end)
    (goto-char isearch-other-end)))

;; Exit isearch killing the current match
(defun kill-isearch-match ()
    "Kill the current isearch match string and continue searching."
    (interactive)
    (kill-region isearch-other-end (point))
    (isearch-exit))

(define-key isearch-mode-map (kbd "C-.") 'kill-isearch-match)

;;
;; Custom binds
;;

;; Transpose windows
(defun transpose-windows (arg)
  "Transpose the buffers shown in two windows."
  (interactive "p")
  (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
    (while (/= arg 0)
      (let ((this-win (window-buffer))
            (next-win (window-buffer (funcall selector))))
        (set-window-buffer (selected-window) next-win)
        (set-window-buffer (funcall selector) this-win)
        (select-window (funcall selector)))
      (setq arg (if (plusp arg) (1- arg) (1+ arg))))))

;; Bound to shift + the window nav keys below
(global-set-key (kbd "C-z C-S-S") (lambda () (interactive) (transpose-windows -1)))
(global-set-key (kbd "C-z C-S-D") 'transpose-windows)

;; Revert without prompting
(global-set-key (kbd "C-z R") (lambda () (interactive) (revert-buffer t t)))

; Quick eval-defun
(global-set-key (kbd "C-z e") 'eval-region)
(global-set-key (kbd "C-z E") 'eval-defun)

(global-set-key (kbd "C-z C-S-G") 'gdb)
(global-set-key (kbd "C-z M") 'gdb-many-windows)

;; Delete trailing whitespace
(global-set-key (kbd "C-z C-M-S-D") 'delete-trailing-whitespace)

; helm shortcuts
(global-set-key (kbd "C-z C-f") 'helm-find-files)
(global-set-key (kbd "C-z h") 'helm-resume)

;; Back one window
(global-set-key (kbd "C-x O") (lambda ()
                                (interactive)
                                (other-window -1)))

; Scroll window
(global-set-key (kbd "s-n") (lambda ()
                              (interactive)
                              (scroll-up 1)))
(global-set-key (kbd "s-p") (lambda ()
                              (interactive)
                              (scroll-down 1)))
(global-set-key (kbd "s-l") (lambda ()
                              (interactive)
                              (move-to-window-line nil)))

; Fast window nav
(global-set-key (kbd "C-z C-s") (lambda ()
                                  (interactive)
                                  (other-window -1)))
(global-set-key (kbd "C-z C-d") (lambda ()
                                  (interactive)
                                  (other-window 1)))

;; Diff current changes
(global-set-key (kbd "C-z C-S-D") 'diff-buffer-with-file)
(global-set-key (kbd "C-z C-M-S-D") 'ediff-current-file)

;; Debug mode
(defun neph-toggle-debug ()
  "Helper to toggle 'debug-on-error' and 'debug-on-quit' modes."
  (interactive)
  ;; If in mismatched state, default to disabling the enabled one
  (if (or debug-on-error debug-on-quit)
      (progn
        (setq debug-on-error nil)
        (setq debug-on-quit nil)
        (message "Disabled debug-on-error and debug-on-quit"))
    (setq debug-on-error t)
    (setq debug-on-quit t)
    (message "Enabled debug-on-error and debug-on-quit")))

;; Keybind for enabling debug stuff quickly when I'm mad at something hanging.  Which is always.
(global-set-key (kbd "C-z C-M-S-Q") 'neph-toggle-debug)

;; Bonus align keys

;; align-regexp but defaults to complex mode interactively
(defun align-regexp-complex (&rest rest)
  "Invoke align-regexp in complex mode"
  (interactive)
  (let ((current-prefix-arg 1))
    (if (called-interactively-p 'any)
        (call-interactively 'align-regexp rest)
      (apply 'align-regexp rest))))

(defun neph-run-python (python-code)
  "Run PYTHON-CODE as python and return the stdout."
  (interactive "sPython: ")
  (with-temp-buffer
    (set-mark (point))
    (insert python-code)
    (shell-command-on-region (point) (mark) "python -" (current-buffer) t)
    (buffer-substring (point) (mark))))

(defun neph-align-protobuf-message ()
  "Helper to align a protobuf message"
  (interactive)
  (indent-region (region-beginning) (region-end))
  ;; Prefix regexp that matches a field line of a protobuf message, quoted or not
  (let ((protoline "^\\s-*\\(//\\)?\\s-*\\(optional\\|repeated\\)")
        ;; Version that requires it be quoted
        (protoline-quoted "^\\s-*\\(//\\)\\s-*\\(optional\\|repeated\\)")
        ;; How many groups does the match-a-protoline prefix have
        (protoline-groups 2)
        ;; Which replace string refers to the field type
        (protoline-type-group 2))
    ;; Fix any commented out lines to have the comment as the first few characters with indentation
    ;; after -- Protobuf messages may have many commented out fields, and this leaves them aligned
    ;; with the live fields nicely.
    (let ((start (region-beginning))
          (end (region-end)))
      (save-excursion
        (goto-char start)
        (while (re-search-forward protoline-quoted end t)
          (replace-match (concat "//	" (format "\\%d" protoline-type-group)))))

    ;; Align the field name after optional/repeated
    (align-regexp (region-beginning) (region-end)
                  (concat protoline "\\s-+[^[:space:]]+\\(\\s-+\\)")
                  (+ protoline-groups 1) 1 nil)
    ;; Align the first =
    (align-regexp (region-beginning) (region-end)
                  (concat protoline ".*?\\(\\s-*\\)=")
                  (+ protoline-groups 1) 1 nil)
    ;; Align the start of the trailing comment
    (align-regexp (region-beginning) (region-end)
                  (concat protoline ".*?\\(\\s-*\\)=[^/]+;\\(\\s-*\\)//")
                  (+ protoline-groups 2) 1 nil)
    ;; Align the interior of the comment in case we have old code where the contents were aligned
    ;; after the //
    (align-regexp (region-beginning) (region-end)
                  (concat protoline ".*?\\(\\s-*\\)=[^/]+;\\(\\s-*\\)//\\(\\s-*\\)")
                  (+ protoline-groups 3) 1 nil))))

(defun neph-align-smss-table ()
  "Helper to align a copied table from SMSS."
  (interactive)
  (let ((tab-width 1)
        (start (region-beginning)))
    (align-regexp (region-beginning) (region-end)
                  (concat "\\(" (kbd "TAB") "+\\)") 1 1 t)
    (save-excursion
      (set-mark (region-end))
      (goto-char start)
      (while (re-search-forward (kbd "TAB") (region-end) t)
        (replace-match " ")))))

(defun neph-markdownify-smss-table-yank ()
  "Helper to transform a copied table from SMSS to markdown (from-killring version)."
  (interactive)
  (let ((start (point))
        (deactivate-mark))
    (yank)
    (neph-markdownify-smss-table start (point))
    (push-mark start)))

(defun neph-markdownify-smss-table (start end)
  "Helper to transform a copied table from SMSS to markdown.  Region is used unless START/END are passed."
  (interactive "r")
  (if (or (region-active-p) (not (called-interactively-p))) ;; Don't operate on inactive region
      (save-excursion
        ;; Ensure mark is at the end
        (set-mark end)
        (goto-char start)
        ;; Skip whitespace at start
        (while (and (not (= (point) (point-max))) (looking-at "[[:space:]]*$"))
          (beginning-of-line 2))
        (setq start (point))
        ;; TAB -> " | "
        (while (re-search-forward (kbd "TAB") (mark) t) (replace-match " | "))
        (goto-char start)
        ;; Wrap lines in | .. |
        (while (re-search-forward "^\\(.\\)" (mark) t) (replace-match "| \\1"))
        (goto-char start)
        (while (and (not (= (point) (point-max))) ;; Make sure we're not on a non-terminated line at end of file
                    (re-search-forward "\\(.\\)$" (mark) t))
          (replace-match "\\1 |")
          (when (not (= (point) (point-max))) (forward-char 1)))

        ;; Align table
        (align-regexp start (mark) "\\(\\ +\\)|" 1 1 t)

        ;; Select first line
        (setq end (region-end))
        (goto-char start)
        (set-mark (point))
        (re-search-forward "$" end t)

        ;; Duplicate first line for header divider
        (when (and (< (point) end) (not (= (point) (mark))))
          (let ((line (buffer-substring (region-beginning) (region-end)))
                (end (region-end))
                (tstart 0))
            (newline)
            (insert line)
            (set-mark (point))
            (beginning-of-line)

            ;; Keep finding | Foo | columns and replace with an equal number of dashes
            (setq tstart (+ 2 (point)))
            (while (and (< (+ 2 (point)) (mark))
                        (re-search-forward " \\([^|]+\\) |" (mark) t))
              (let ((text (match-substitute-replacement "\\1")))
                (backward-char 2)
                (delete-region tstart (point))
                (insert (replace-regexp-in-string "." "-" text))
                (forward-char 2)
                (setq tstart (+ 1 (point))))))))
    ;; else - inactive region
    (message "No region selected")))

(defun neph-run-makepkg-g-on-region (start end)
    "Run `makepkg -g 2>/dev/null` on region specified as START and END (defaults to marked region)."
  (interactive (list (region-beginning) (region-end)))
  (shell-command-on-region start end "makepkg -g 2>/dev/null" 1 1))

(global-set-key (kbd "C-z C-M-S-M") 'neph-run-makepkg-g-on-region)
(global-set-key (kbd "C-z C-M-s") 'neph-align-smss-table)
(global-set-key (kbd "C-z C-M-S-S") 'neph-markdownify-smss-table-yank)
(global-set-key (kbd "C-z C-M-p") 'neph-align-protobuf-message)
(global-set-key (kbd "C-z C-a") 'align-regexp)
(global-set-key (kbd "C-z a") 'neph-align-regexp-u)

; Toggle case of the next letter
(defun toggle-case ()
  "Toggle the casing of the character under point"
  (interactive)
  (let* ((curchar   (char-after))
         (curcapped (if curchar (upcase (char-after)))))
    (if curchar
        (save-excursion
          (delete-char 1)
          (insert (if (eq curchar curcapped)
                      (downcase curchar)
                    curcapped))))))
(global-set-key (kbd "M-u") 'toggle-case)

;; merge-next-line
(defun merge-next-line (arg)
  "Merge line with next"
  (interactive "p")
  (next-line 1)
  (delete-indentation))
(global-set-key (kbd "C-M-k") 'merge-next-line)

;; yank-and-indent
(defun yank-and-indent ()
  "Yank and then indent the newly formed region according to mode."
  (interactive)
  (yank)
  (call-interactively 'indent-region))

(defun smart-yank-before-line ()
  "Yank starting on a new line previous to this, indent, and end at the beginning of said line"
  (interactive)
  (beginning-of-line)
  ;; If this isn't a blank line, open a new line before
  (if (not (looking-at "\\s-*$"))
      (open-line 1)
    ;; Otherwise just clear said
    (delete-horizontal-space))

  ;; Do yank, but return to here
  (save-excursion
    (yank)
    (call-interactively 'indent-region)
    ;; Was the last line of this yank whitespace? Nuke it.
    (when (save-excursion
            (beginning-of-line)
            (looking-at "\\s-*$"))
      (kill-whole-line)))

  ;; Go to indent
  (back-to-indentation))

(global-set-key (kbd "C-S-Y") 'yank-and-indent)
(global-set-key (kbd "M-Y") 'smart-yank-before-line)

(defun bookmark-current-line ()
  "Bookmark the current line, using itself as the bookmark name"
  (interactive)
  (let ((line (thing-at-point 'line t)))
    (when (string-match "[ \t\n]*$" line)
      (setq line (replace-match "" nil nil line)))
    (bookmark-set line)
    (message (concat "Created bookmark: " line))))

(global-set-key (kbd "C-z C-S-B") 'bookmark-current-line)

(defun move-line-up ()
  "Move the current line up."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))
(global-set-key [(control shift up)] 'move-line-up)
;; Prefer to org-mode's default bind
(eval-after-load 'org '(define-key org-mode-map [(control shift up)] nil))

(defun move-line-down ()
  "Move the current line down."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))
(global-set-key [(control shift down)] 'move-line-down)
;; Prefer to org-mode's default bind
(eval-after-load 'org '(define-key org-mode-map [(control shift down)] nil))

(defun smart-expand-region-to-lines ()
  "Expand the current region to line breaks if and only if it
     already contains all non-whitespace in that region"
  (interactive)
    ;; Elasticly expand the region to move entire discrete lines if the beginning and end of the
    ;; region contain those whole lines except for white-space
    (let* ((o-point (point))
           (o-mark (if mark-active (mark) o-point))
           (point-first (< o-point o-mark)) ; Is the point at the begining or end of the region
           (region-start (if point-first o-point o-mark))
           (region-end (if point-first o-mark o-point))
           (end-is-newline (save-excursion
                             (goto-char region-end)
                             (or (looking-at "\\s-*$") (looking-back "^"))))
           (start-is-newline (save-excursion
                               (goto-char region-start)
                               (looking-back "^\\s-*"))))
      (when (and start-is-newline end-is-newline)
        ;; Both start and end contain the entire region's lines but for whitespace, expand region

        ;; Start
        (goto-char region-start)
        (beginning-of-line)
        (setq region-start (point))
        ;; End
        (goto-char region-end)
        (when (not (looking-back "^")) ; Already a new line, don't be two
          (end-of-line)
          ;; Open a newline if we're at the end of the buffer, otherwise forward one
          (if (= (point-max) (point))
              (newline)
            (forward-char 1))
          (setq region-end (point)))
        ;; Adjust point/mark
        (if point-first
            (progn (goto-char region-start)
                   (set-mark region-end))
          (set-mark region-start)))))

(defun move-region (start end n)
  "Move the current region up or down by N lines."
  (interactive "r\np")
  (if mark-active
      (progn
        (let ((line-text (delete-and-extract-region start end)))
          (next-line n)
          (let ((start (point)))
            (insert line-text)
            (setq deactivate-mark nil)
            (set-mark start))))
    ;; Mark not active, just move without touching
    (next-line n)))

(defun smart-move-current-region-up (n)
  "Move the current region up by N lines, smart expanding to line
     breaks with smart-expand-region-to-lines first."
  (interactive "p")
  (smart-expand-region-to-lines)
  (move-region (point) (mark) (if (null n) -1 (- n))))

(defun smart-move-current-region-down (n)
  "Move the current region up by N lines, smart expanding to line
     breaks with smart-expand-region-to-lines first."
  (interactive "p")
  (smart-expand-region-to-lines)
  (move-region (point) (mark) (if (null n) 1 n)))

(defun move-region-up (start end n)
  "Move the current line up by N lines."
  (interactive "r\np")
  (move-region start end (if (null n) -1 (- n))))

(defun move-region-down (start end n)
  "Move the current line down by N lines."
  (interactive "r\np")
  (move-region start end (if (null n) 1 n)))

(global-set-key (kbd "M-P") 'smart-move-current-region-up)
(global-set-key (kbd "M-N") 'smart-move-current-region-down)

(defun open-next-line (arg)
  "Move to the next line and then opens a line.
    See also `newline-and-indent'."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (next-line 1)
  (indent-according-to-mode))
(global-set-key (kbd "C-S-o") 'open-next-line)

; F3 inserts current filename into minibuffer
(define-key minibuffer-local-map [f3]
  (lambda () (interactive)
     (insert (buffer-name (window-buffer (minibuffer-selected-window))))))

(defun p4-edit-current ()
  "Checks out the current buffer and mark editable"
  (interactive)
  (message "Attempting p4 edit %s" (buffer-file-name))
  (let ((default-directory (file-name-directory (buffer-file-name)))
        (process-environment (copy-sequence process-environment)))
    (setenv "P4CONFIG" "P4CONFIG")
    (if (= 0 (call-process "p4" nil nil nil "edit" (buffer-file-name)))
        (progn (read-only-mode 0)
               (message "p4 opened into default changeset"))
      (message "p4 edit failed"))))
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
(define-key global-map (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "<C-return>") 'newline)
;; Merge with previous line
(global-set-key (kbd "C-M-S-k") 'delete-indentation)

(defun copy-line (&optional arg)
  "Copy lines (as many as prefix argument) in the kill ring"
  (interactive "p")
  (kill-ring-save (line-beginning-position)
                  (line-beginning-position (+ 1 arg)))
  (message "%d line%s copied" arg (if (= 1 arg) "" "s")))

(global-set-key (kbd "C-S-M-j") 'copy-line)

(defun duplicate-line (arg)
  "Duplicate current line, leaving point in lower line."
  (interactive "*p")

  ;; save the point for undo
  (setq buffer-undo-list (cons (point) buffer-undo-list))

  ;; local variables for start and end of line
  (let ((bol (save-excursion (beginning-of-line) (point)))
        eol)
    (save-excursion

      ;; don't use forward-line for this, because you would have
      ;; to check whether you are at the end of the buffer
      (end-of-line)
      (setq eol (point))

      ;; store the line and disable the recording of undo information
      (let ((line (buffer-substring bol eol))
            (buffer-undo-list t)
            (count arg))
        ;; insert the line arg times
        (while (> count 0)
          (newline)         ;; because there is no newline in 'line'
          (insert line)
          (setq count (1- count))))

      ;; create the undo information
      (setq buffer-undo-list (cons (cons eol (point)) buffer-undo-list))))

  ;; put the point in the lowest line and return
  (next-line arg))

(global-set-key (kbd "C-S-j") 'duplicate-line)

(defun jump-to-char (arg char)
  "Jump forward to ARGth occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     (read-char "Jump to char: " t)))
  ;; Avoid "obsolete" warnings for translation-table-for-input.
  (with-no-warnings
    (if (char-table-p translation-table-for-input)
        (setq char (or (aref translation-table-for-input char) char))))
  (if (and (search-forward (char-to-string char) nil nil arg)
           (> arg 0))
      (backward-char 1)))

(defun backward-jump-to-char (arg char)
  "Jump backward to ARGth occurrence of CHAR.
Case is ignored if `case-fold-search' is non-nil in the current buffer.
Goes backward if ARG is negative; error if CHAR not found."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     (read-char "Backward jump to char: " t)))
  (jump-to-char (* -1 arg) char))

;; Replaces backwards/forwards sexp.
(global-set-key (kbd "C-M-f") 'jump-to-char)
(global-set-key (kbd "C-M-b") 'backward-jump-to-char)
(global-set-key (kbd "M-G") 'goto-line)

(defun backward-to-word (&optional arg)
  "Move backward until encountering the *end* of a word.
With argument ARG, do this that many times.
If ARG is omitted or nil, move point backward one word.

This is roughly (backward-word arg) followed by (forward-ward 1),
with a special case for when you are in a word"
  (interactive "^p")
  (forward-to-word (- (or arg 1))))

(defun forward-to-word (&optional arg)
    "Move backwards to the *beginning* of the next recognized
word. This is a combination of (forward-word) (backward-word)
with a special case for when you are within a word"
  (interactive "^p")
  (let ((original-point (point))
        (n (or arg 1))
        (inc (if (< (or arg 1) 0) -1 1)))
    (forward-word inc)
    (forward-word (- inc))
    (if (or (and (> n 0) (<= (point) original-point))
            (and (< n 0) (>= (point) original-point)))
        (forward-word (* 2 inc))
      (forward-word inc))
    (if (or (> n 1) (< n -1))
        (forward-word (* inc (- arg 1))))
    (forward-word (- inc))))

(defun backward-whitespace (&optional arg)
  "'forward-whitespace' but with ARG inverted."
  (interactive "^p")
  (forward-whitespace (* -1 (or arg 1))))

(defun neph-kill-to-word (&optional arg)
  "Like kill word, but behaes like forward-to-word rather than
forward-word to find the boundry"
  (interactive "^p")
  (save-excursion
    (set-mark (point))
    (forward-to-word arg)
    (kill-region (mark) (point))))

(defun neph-backward-kill-to-word (&optional arg)
  "Like 'kill-word', but behaves like 'forward-to-word'.
\(Rather than 'forward-word', to find the boundry.)
ARG has the same meaning as 'kill-word' otherwise."
  (interactive "p")
  (neph-kill-to-word (- (or arg 1))))

(defun neph-backward-kill-line (&optional arg)
  "Like `kill-line' but inverts meaning of ARG.

This includes the special ARG value of zero (vs nil) to reverse direction on the
same line (see `kill-line')."
  (interactive "P")
  (kill-line (and (not (= (or arg 1) 0)) (- (or arg 0)))))

(defun neph-mark-current-word (&optional arg)
    "Determines if you are over a word, and moves the mark to the
beginning of it and the point to the end of it if so"
  (interactive "^p")
  (let ((original-point (point)))
    (forward-word -1)
    (let ((startword (point)))
      (forward-word 1)
      (if (> (point) original-point)
          (set-mark startword)
        (message "No word at point")
        (goto-char original-point)))))

(defun current-word-to-kill-ring (&optional arg)
  "Puts the current word in the kill ring"
  (interactive)
  (kill-new (current-word)))

(global-set-key (kbd "C-S-U") 'neph-backward-kill-line)
(global-set-key (kbd "C-M-S-Z") 'current-word-to-kill-ring)
(global-set-key (kbd "M-@") 'neph-mark-current-word)
(global-set-key (kbd "M-B") 'backward-to-word)
(global-set-key (kbd "M-F") 'forward-to-word)
(global-set-key (kbd "M-D") 'neph-kill-to-word)
(global-set-key (kbd "<M-S-delete>") 'neph-backward-kill-to-word)

(defun neph-pop-to-secondary ()
  "Pop to secondary selection"
  (interactive)
  (let ((buf (overlay-buffer mouse-secondary-overlay)))
    (when buf
      (pop-to-buffer buf)
      (goto-char (overlay-start mouse-secondary-overlay)))))

(defun sql-send-secondary ()
  "Send the secondary selection to SQL buffer."
  (interactive)
  (let ((buf (overlay-buffer mouse-secondary-overlay)))
    (when (eq buf (current-buffer))
      (sql-send-region (overlay-start mouse-secondary-overlay)
                       (overlay-end mouse-secondary-overlay)))))

(with-eval-after-load "sql"
  (define-key sql-mode-map (kbd "C-c C-a") 'sql-send-secondary))

;; Quick register movement.
;; Default to register 7 since it's awkward to hit, leaving other registers available for explicit.
(global-set-key (kbd "C-z SPC") (lambda (&optional arg) (interactive "P")
                                  (let ((reg (if (eq arg nil) 7 arg)))
                                    (point-to-register reg)
                                    (message "Set register %d" reg))))
(global-set-key (kbd "C-z C-SPC") (lambda (&optional arg) (interactive "P")
                                    (let ((reg (if (eq arg nil) 7 arg)))
                                      (jump-to-register reg)
                                      (message "Jump to register %d" reg))))

(defun mark-current-line (&optional arg)
  "Mark the current line without moving the cursor"
  (interactive)
  (end-of-line)
  (set-mark (line-beginning-position)))

(global-set-key (kbd "C-M-S-A") 'mark-current-line)

(defun vsplit-last-buffer ()
  (interactive)
  (split-window-vertically)
  (other-window 1 nil)
  (switch-to-next-buffer))

(defun hsplit-last-buffer ()
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil)
  (switch-to-next-buffer))

(global-set-key (kbd "C-x 2") 'vsplit-last-buffer)
(global-set-key (kbd "C-x 3") 'hsplit-last-buffer)

(defun touch-current-file ()
     "updates mtime on the file for the current buffer"
     (interactive)
     (if (buffer-file-name)
         (progn
           (shell-command (concat "touch " (shell-quote-argument (buffer-file-name))))
           (clear-visited-file-modtime)
           (message (concat "Ran touch on " (buffer-file-name))))
       (message "No filename for current file")))

(global-set-key (kbd "C-z T") 'touch-current-file)

(defun neph-ia-bigfont ()
  "Shorthand for changing font size for hdpi"
  (interactive)
  (set-default-font "DejaVu Sans Mono-16"))

(defun neph-ia-server ()
  "Prompt for a server name, set server-name to that, start the server"
  (interactive)
    (setq server-name (read-string "(Re)start server with name: "))
    (server-start)
    (message (concat "Server started as '" server-name "'")))

;; Copy file name to kill ring
(defun neph-buffer-name-to-kill-ring ()
  (interactive)
  (kill-new (buffer-file-name))
  (message "Copied buffer name to kill ring"))
(global-set-key (kbd "C-z C-S-n") 'neph-buffer-name-to-kill-ring)

(defun neph-xdg-open-this-file ()
  "Pass the current file to xdg-open whynot."
  (interactive)
  (if (buffer-file-name)
      (shell-command (concat "xdg-open " (shell-quote-argument (buffer-file-name))))
    (message "!! This buffer has no associated file")))

(global-set-key (kbd "C-z C-!") 'neph-xdg-open-this-file)

(defun neph-show-file-coding ()
  (interactive)
  (message (symbol-name buffer-file-coding-system)))

(defun neph-increment ()
  (interactive)
  (message (number-to-string (string-to-number (buffer-substring (mark) (point)))))
  (let (num (string-to-number (buffer-substring (mark) (point))))
    (save-excursion
      (kill-region (mark) (point))
      (insert (number-to-string (+ num 1))))))

(global-set-key (kbd "C-z C-S-c") 'neph-show-file-coding)

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
(global-hl-line-mode t)

;;
;; ace-jump-mode
;;

(add-to-list 'load-path "~/.emacs.d/ace-jump-mode")
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
(define-key global-map (kbd "C-z C-c") 'ace-jump-mode-pop-mark)
(define-key global-map (kbd "C-z C-x") 'avy-goto-word-1)

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
;; PlantUML
;;

;; Default install path from package
(setq org-plantuml-jar-path
      (expand-file-name "/usr/share/java/plantuml/plantuml.jar"))

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

;;
;; mmm/jinja/salt mode
;;
(add-to-list 'load-path "~/.emacs.d/mmm-mode")
(add-to-list 'load-path "~/.emacs.d/mmm-jinja2")
(add-to-list 'load-path "~/.emacs.d/salt-mode")
(require 'salt-mode)

;;
;; zap-to-char
;;

; Make zap-to-char zap-up-to-char
(defadvice zap-to-char (after my-zap-to-char-advice (arg char) activate)
  "Kill up to the ARG'th occurence of CHAR, and leave CHAR. If
  you are deleting forward, the CHAR is replaced and the point is
  put before CHAR"
  (insert char)
  (if (< 0 arg) (forward-char -1)))

; Just inverts the argument to zap-to-char
(defun backwards-zap-to-char (arg char)
  "zap-to-char with an inverted argument"
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     (read-char "Zap backwards to char: ")))
  (zap-to-char (* -1 arg) char))
(global-set-key (kbd "M-Z") 'backwards-zap-to-char)

;;
;; Magit
;;

(add-to-list 'load-path "~/.emacs.d/git-modes")
(add-to-list 'load-path "~/.emacs.d/magit-transient/lisp")
(add-to-list 'load-path "~/.emacs.d/magit-popup")
(add-to-list 'load-path "~/.emacs.d/magit-ghub/lisp")
(add-to-list 'load-path "~/.emacs.d/magit/lisp")
(add-to-list 'load-path "~/.emacs.d/treepy")
(add-to-list 'load-path "~/.emacs.d/with-editor/lisp") ;; Part of magit project, dep
(require 'with-editor)
(require 'magit)
(require 'magit-blame)
;(global-set-key (kbd "C-z g") 'magit-status)
(global-set-key (kbd "C-z L") 'magit-blame-mode)
(global-set-key (kbd "C-z X") 'magit-ediff-stage)
(global-set-key (kbd "C-z C") 'magit-commit)

;;
;; Theme
;;

;(add-to-list 'custom-theme-load-path "~/.emacs.d/sunburst-theme")
;(load-theme 'sunburst t)

;; See also neph-ample-zen-theme.el
(add-to-list 'custom-theme-load-path "~/.emacs.d/ample-zen")

;;
;; Load theme selected by env
;;
(setq default-neph-theme (let ((envtheme (getenv "NEPH_EMACS_THEME")))
                           (if envtheme envtheme
                             "ample-zen")))

(defun load-neph-theme (neph-theme)
  "Load the given theme, possibly with neph wrapper"
  (interactive (list (read-string "Theme: ")))
  ;; Disable all existing
  (dolist (elem custom-enabled-themes)
    (disable-theme elem))
  ;; Custom handlers
  (if (string= neph-theme "ample-zen")
      (progn
        (load-theme 'ample-zen t)
        (load-theme 'neph-ample-zen t))
    ;; Safe handlers
    (if (string= neph-theme "tango")
        (load-theme 'tango t)
      ;; Else just forward to load-theme
      (load-theme (intern neph-theme))))
  (when (and (boundp 'color-identifiers-mode) color-identifiers-mode)
    (color-identifiers:refresh))
  (when (and (boundp 'display-line-numbers-mode) display-line-numbers-mode)
    (display-line-numbers-mode nil)
    (display-line-numbers-mode t))
  (redisplay))
(load-neph-theme default-neph-theme)

(defun neph-whiteboard-mode ()
  "Enter or exit whiteboard mode"
  (interactive)
  (if (member 'ample-zen custom-enabled-themes)
      (progn (load-neph-theme "whiteboard")
             (global-whitespace-mode -1))
    (load-neph-theme "ample-zen")
    (global-whitespace-mode t)))
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
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/purple-haze-theme")
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
 '(dap-auto-configure-features
   '(sessions locals breakpoints expressions repl controls tooltip))
 '(display-line-numbers-grow-only t)
 '(display-line-numbers-width 6)
 '(ediff-split-window-function 'split-window-horizontally)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(ein:completion-backend 'ein:use-company-backend)
 '(flycheck-checker-error-threshold nil)
 '(helm-candidate-number-limit 1000)
 '(helm-exit-idle-delay 0)
 '(helm-input-idle-delay 0.0)
 '(helm-rg-input-min-search-chars 1)
 '(ido-vertical-define-keys 'C-n-and-C-p-only)
 '(irony-completion-availability-filter '(available deprecated notaccessible notavailable))
 '(lsp-enable-file-watchers nil)
 '(lsp-rust-analyzer-server-display-inlay-hints t)
 '(lsp-semantic-tokens-enable t)
 '(lsp-ui-doc-alignment 'window)
 '(lsp-ui-doc-header t)
 '(lsp-ui-doc-include-signature t)
 '(lsp-ui-doc-position 'top)
 '(lsp-ui-peek-always-show t)
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
   '((highlight-80+-columns . 100)
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
