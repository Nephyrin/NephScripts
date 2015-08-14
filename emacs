; -*- mode: Emacs-Lisp; -*-

;;
;; Misc
;;

;; Load this before something tries to load built-in CEDET libraries
(load-file "~/.emacs.d/cedet-git/cedet-devel-load.el")

; This just makes things slower. Maybe useful on spinning disks?
(setq cache-long-line-scans nil)
(setq cache-long-scans nil)

; Clear suspend-frame binding to use C-z as a prefix
(global-unset-key (kbd "C-z"))

;; Copy file name to kill ring
(defun neph-buffer-name-to-kill-ring ()
  (interactive)
  (kill-new (buffer-file-name))
  (message "Copied buffer name to kill ring"))

(global-set-key (kbd "C-z C-S-n") 'neph-buffer-name-to-kill-ring)

(put 'upcase-region 'disabled nil)

; Fix x clipboard
(setq x-select-enable-primary nil)
(setq x-select-enable-clipboard t)
(setq mouse-drag-copy-region nil)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)
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

(add-to-list 'auto-mode-alist '("/yaourtrc$" . sh-mode))
(add-to-list 'auto-mode-alist '("/PKGBUILD$" . sh-mode))
(add-to-list 'auto-mode-alist '("/bash(rc|_profile)$" . sh-mode))
;; Use js-mode for vpc/vgc files for now
(add-to-list 'auto-mode-alist '("\.v[pg]c$" . js-mode))

(setq vc-follow-symlinks t)

(require 'uniquify)
(setq uniquify-buffer-name-style (quote post-forward))

; (global-ede-mode t)

; Hide toolbar, hide menu in console mode
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)


(setq split-width-threshold 170)
(setq split-height-threshold 50)

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

(setq desktop-path '("~/.emacs.d/"))
; Autosave desktop as emacs-server-desktop for the server, otherwise leave
; disabled unless asked for
;(if (or server-mode (daemonp))
;    (progn
;      (setq desktop-base-file-name "emacs-server-desktop")
;      (setq desktop-save t)
;      (desktop-save-mode 1))
;  (setq desktop-base-file-name "emacs-desktop"))

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
;; Lua mode
;;

(add-to-list 'load-path "~/.emacs.d/lua-mode")
(autoload 'lua-mode "lua-mode"
   "Major mode for editing Lua files" t)
(add-to-list 'auto-mode-alist '("\\.lua\\'" . lua-mode))

;;
;; Htmlize
;;

(load-file "~/.emacs.d/htmlize.el")

;;
;; Multi-term
;;

(load-file "~/.emacs.d/multi-term.el")
(setq multi-term-program "/bin/bash")

(global-set-key (kbd "C-x t") 'multi-term-dedicated-open)

; Paste not yank
(add-hook 'term-mode-hook (lambda ()
                            (hl-line-mode nil)
                            (define-key term-raw-map (kbd "C-y") 'term-paste)))

;;
;; ECB
;;

(add-to-list 'load-path "~/.emacs.d/ecb")
(require 'ecb)
(setq ecb-show-sources-in-directories-buffer 'always)
(setq ecb-layout-name "left7")
(setq ecb-tip-of-the-day nil)
(setq ecb-windows-width 0.1)

;; Quiet startup warning
(setq ecb-options-version "2.40")

(global-set-key (kbd "C-z q") 'ecb-activate)
(global-set-key (kbd "C-z Q") 'ecb-deactivate)

;;
;; Color identifiers mode
;;

(add-to-list 'load-path "~/.emacs.d/color-identifiers-mode")
(add-to-list 'load-path "~/.emacs.d/dash") ; dependency
(require 'color-identifiers-mode)

;;
;; Helm
;;

(add-to-list 'load-path "~/.emacs.d/helm")
(require 'helm)
(require 'helm-config)
(require 'helm-files)

(global-set-key (kbd "C-z F") (lambda () (interactive) (helm-find-1 (read-directory-name "Run find in directory: " nil "" t))))
(global-set-key (kbd "M-x") 'helm-M-x)

(global-set-key (kbd "C-z b") 'helm-mini)
(global-set-key (kbd "C-z C-o") 'helm-occur)

(define-key isearch-mode-map (kbd "C-o") 'helm-occur-from-isearch)

(when (executable-find "ack-grep")
  (setq helm-grep-default-command "ack-grep -Hn --no-group --no-color --smart-case --type-set IGNORED:ext:P,map --noIGNORED %p %f"
        helm-grep-default-recurse-command "ack-grep -H --no-group --no-color --smart-case --type-set IGNORED:ext:P,map --noIGNORED %p %f"))

(require 'grep)
(add-to-list 'grep-find-ignored-files "*.pdb")
(add-to-list 'grep-find-ignored-files "*.map")
(add-to-list 'grep-find-ignored-files "*.P")
(add-to-list 'grep-find-ignored-files "*.dylib")
(add-to-list 'grep-find-ignored-files "*.lib")
(add-to-list 'grep-find-ignored-files "*.a")
(add-to-list 'grep-find-ignored-files "*.dSYM")
(add-to-list 'grep-find-ignored-files "*.app")
(add-to-list 'grep-find-ignored-files "*.framework")
(add-to-list 'grep-find-ignored-files "*.dll")
(add-to-list 'grep-find-ignored-files "*.so.0")
(add-to-list 'grep-find-ignored-files "*.so")
(add-to-list 'grep-find-ignored-files "*.o")
(add-to-list 'grep-find-ignored-files "*.exe")
(add-to-list 'grep-find-ignored-files "*.dbg")

;; Python compiled thing
(add-to-list 'grep-find-ignored-files "*.pyd")
(add-to-list 'grep-find-ignored-files "*.pyc")

(add-to-list 'grep-find-ignored-files "*.zip")
(add-to-list 'grep-find-ignored-files "*.rar")
(add-to-list 'grep-find-ignored-files "*.7z")
(add-to-list 'grep-find-ignored-files "*.xz")
(add-to-list 'grep-find-ignored-files "*.bz2")
(add-to-list 'grep-find-ignored-files "*.gz")
(add-to-list 'grep-find-ignored-files "*.tar")
(add-to-list 'grep-find-ignored-files "*.dmg")
(add-to-list 'grep-find-ignored-files "*.deb")
(add-to-list 'grep-find-ignored-files "*.rpm")
(add-to-list 'grep-find-ignored-files "*.iso")

(add-to-list 'grep-find-ignored-files "*.vtf")
(add-to-list 'grep-find-ignored-files "*.vvd")
(add-to-list 'grep-find-ignored-files "*.vcd")
(add-to-list 'grep-find-ignored-files "*.phy")
(add-to-list 'grep-find-ignored-files "*.mdl")
(add-to-list 'grep-find-ignored-files "*.dmx")
(add-to-list 'grep-find-ignored-files "*.bsp")
(add-to-list 'grep-find-ignored-files "*.vpk")
(add-to-list 'grep-find-ignored-files "*.vtx")
(add-to-list 'grep-find-ignored-files "*.fbx")
(add-to-list 'grep-find-ignored-files "*.vmt")
(add-to-list 'grep-find-ignored-files "*.dds")

(add-to-list 'grep-find-ignored-files "*.cache")
(add-to-list 'grep-find-ignored-files "*.svn-base")
(add-to-list 'grep-find-ignored-files "*.sdf") ; Visual studio database thing

;; PS3 compiled file... thing
(add-to-list 'grep-find-ignored-files "*.prx")
(add-to-list 'grep-find-ignored-files "*.sprx")

(add-to-list 'grep-find-ignored-files "*.raw")
(add-to-list 'grep-find-ignored-files "*.ani")
(add-to-list 'grep-find-ignored-files "*.bik")
(add-to-list 'grep-find-ignored-files "*.dat")
(add-to-list 'grep-find-ignored-files "*.ttf")
(add-to-list 'grep-find-ignored-files "*.pdf")

(add-to-list 'grep-find-ignored-files "*.tga")
(add-to-list 'grep-find-ignored-files "*.jpg")
(add-to-list 'grep-find-ignored-files "*.jpeg")
(add-to-list 'grep-find-ignored-files "*.png")
(add-to-list 'grep-find-ignored-files "*.bmp")
(add-to-list 'grep-find-ignored-files "*.psd")
(add-to-list 'grep-find-ignored-files "*.cbr")
(add-to-list 'grep-find-ignored-files "*.icns")
(add-to-list 'grep-find-ignored-files "*.ico")

(add-to-list 'grep-find-ignored-files "*.wav")
(add-to-list 'grep-find-ignored-files "*.ogg")
(add-to-list 'grep-find-ignored-files "*.mp3")

(add-to-list 'grep-find-ignored-files "*.h264")
(add-to-list 'grep-find-ignored-files "*.mkv")
(add-to-list 'grep-find-ignored-files "*.avi")
(add-to-list 'grep-find-ignored-files "*.mp4")

; Oneoffs
(add-to-list 'grep-find-ignored-files "ip-country-region-city-latitude-longitude-isp.csv")
(add-to-list 'grep-find-ignored-files "engine_symbols.txt")
(add-to-list 'grep-find-ignored-files "dedicated_symbols.txt")

;;
;; Swiper
;;

(add-to-list 'load-path "~/.emacs.d/swiper")
(require 'swiper)
(global-set-key (kbd "C-z s") 'swiper)

;;
;; C++ Helper mode(s) : Company/rtags/semantic
;;

;; popup.el for rtags tooltips
(add-to-list 'load-path "~/.emacs.d/popup-el")
(require 'popup)

(add-to-list 'load-path "~/.emacs.d/company-mode")
(require 'company)
(require 'rtags)
(require 'company-rtags)

(setq rtags-completions-enabled t) ; Needed?

(setq rtags-autostart-diagnostics t)
(setq rtags-find-file-case-insensitive t)
(setq rtags-show-containing-function nil)
;; (setq rtags-tooltips-enabled nil)

;; Buggy
(setq rtags-completions-timer-interval 0)

(setq rtags-tooltips-enabled t)
(setq rtags-display-current-error-as-tooltip t)
(setq rtags-display-summary-as-tooltip t)

;; Semantic
(require 'semantic)
(require 'semantic/bovine/gcc)
(global-semantic-decoration-mode t)
(global-semantic-stickyfunc-mode t)

(add-to-list 'company-backends (list 'company-semantic 'company-rtags))

;; function-args modes
(add-to-list 'load-path "~/.emacs.d/function-args")
(require 'function-args)
(fa-config-default)

(defun company-mode-moz ()
  (setq company-clang-arguments (split-string
                                 (shell-command-to-string
                                  (concat "~/.emacs.d/moz_objdir.sh "
                                          (buffer-file-name)))))
  (company-mode t)
  (local-set-key (kbd "<C-tab>") 'company-complete))
(defun company-mode-neph ()
  (interactive)
  (company-mode t)
  (semantic-mode t)
  (local-set-key (kbd "<C-tab>") 'company-complete))
(add-hook 'c-mode-common-hook 'company-mode-neph)

;; Keys for C++ completion and such
(global-set-key (kbd "C-z C-.") 'rtags-find-symbol-at-point)
(global-set-key (kbd "C-z M-r") 'rtags-reparse-file)
(global-set-key (kbd "C-z C-,") 'rtags-find-references-at-point)
(global-set-key (kbd "C-z C->") 'rtags-find-virtuals-at-point)
(global-set-key (kbd "C-z .") 'rtags-find-symbol)
(global-set-key (kbd "C-z ,") 'rtags-find-references)
(global-set-key (kbd "C-z C-/") (lambda () (interactive) (delete-windows-on rtags-buffer-name t)))
(global-set-key (kbd "C-z C-n") 'rtags-next-match)
(global-set-key (kbd "C-z C-p") 'rtags-previous-match)
(global-set-key (kbd "C-z C-i") 'rtags-imenu)
(global-set-key (kbd "C-z SPC") 'helm-semantic)
(global-set-key (kbd "C-z C-SPC") 'moo-jump-local)
(global-set-key (kbd "C-z D") 'rtags-diagnostics)
(global-set-key (kbd "C-z i") 'rtags-fixit)
(global-set-key (kbd "C-z I") 'rtags-fix-fixit-at-point)
(global-set-key (kbd "C-z DEL") 'rtags-location-stack-back)
(global-set-key (kbd "C-z <S-backspace>") 'rtags-location-stack-back)

;;
;; GDB
;;

; (setq gdb-non-stop-setting nil)
; (gdb-many-windows t)

; Replace this to not be dumb
;; (defadvice gud-display-line (around do-it-better activate) ... )
(defadvice gud-display-line (around do-it-better activate)
  (let* ((last-nonmenu-event t)	 ; Prevent use of dialog box for questions.
	 (buffer
	  (with-current-buffer gud-comint-buffer
	    (gud-find-file true-file)))
	 (window (and buffer
		      (or (get-buffer-window buffer)
                          (and gdb-source-window (set-window-buffer gdb-source-window buffer))
			  (display-buffer buffer))))
	 (pos))
    (when buffer
      (with-current-buffer buffer
	(unless (or (verify-visited-file-modtime buffer) gud-keep-buffer)
	  (if (yes-or-no-p
	       (format "File %s changed on disk.  Reread from disk? "
		       (buffer-name)))
	      (revert-buffer t t)
	    (setq gud-keep-buffer t)))
	(save-restriction
	  (widen)
	  (goto-char (point-min))
	  (forward-line (1- line))
	  (setq pos (point))
	  (or gud-overlay-arrow-position
	      (setq gud-overlay-arrow-position (make-marker)))
	  (set-marker gud-overlay-arrow-position (point) (current-buffer))
	  ;; If they turned on hl-line, move the hl-line highlight to
	  ;; the arrow's line.
	  (when (featurep 'hl-line)
	    (cond
	     (global-hl-line-mode
	      (global-hl-line-highlight))
	     ((and hl-line-mode hl-line-sticky-flag)
	      (hl-line-highlight)))))
	(cond ((or (< pos (point-min)) (> pos (point-max)))
	       (widen)
	       (goto-char pos))))
      (when window
	(set-window-point window gud-overlay-arrow-position)
	(if (eq gud-minor-mode 'gdbmi)
	    (setq gdb-source-window window))))))

;; Stop GDB from force-displaying I/O buffer (what the actual hell)
(defadvice gdb-inferior-filter
    (around gdb-inferior-filter-without-stealing)
  (with-current-buffer (gdb-get-buffer-create 'gdb-inferior-io)
    (comint-output-filter proc string)))
(ad-activate 'gdb-inferior-filter)

;;
;; ido
;;

(add-to-list 'load-path "~/.emacs.d/ido-vertical-mode")
(require 'ido)
(require 'ido-vertical-mode)
(ido-mode t)
(ido-vertical-mode 1)

;;
;; Rainbow Delimiters
;;

(add-to-list 'load-path "~/.emacs.d/rainbow-delimiters")
(require 'rainbow-delimiters)

;;
;; Minimap
;;

(add-to-list 'load-path "~/.emacs.d/minimap")
(require 'minimap)

(set-face-attribute 'minimap-font-face nil :family "Droid Sans Mono" :height 20)
(setq minimap-window-location (quote right))
(setq minimap-width-fraction 0.01)

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

(setq neph-modeline-path
      '(:eval (let ((bufname (propertize (buffer-file-name) 'face 'neph-modeline-path))
                    ;; Paths to replace. Of the form ((search replace) ...)
                    (replacements `((,(getenv "HOME") "~"))))
                ;; Also replace projectile root with project name when available
                (when (featurep 'projectile)
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
        :foreground "#866"
        :height 80)))
  "Neph modeline which-func-mode face")
(setq-default mode-line-format
              '(:eval
                (list
                 neph-modeline-bufstat
                 ;; Position
                 "%[%l:%c"
                 ;; which-function
                 (let ((which-func (and which-function-mode (fboundp 'which-function) (which-function))))
                   (when (and which-func (not (string= "" which-func)))
                      (propertize (concat " " which-func) 'face 'neph-modeline-which-func)))
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
                 (propertize mode-name 'face 'neph-modeline-mode)
                 ""
                 ; misc
                 '(:propertize mode-line-process face neph-modeline-misc)
                 '(:propertize global-mode-string face neph-modeline-misc)
                 '(:propertize minor-mode-alist face neph-modeline-misc)
                 (when vc-mode (propertize (concat " /" vc-mode)
                                           'face 'neph-modeline-misc))
                 ; Padding to righthand side
                 (neph-fill-to 13)
                 ; Percentage and modeline-hud
                 "%p "
                 ; Bonus hacky alignment, such that if the buffer is too narrow
                 ; to show the hud, the height of the modeline stays the same
                 (propertize " " 'display '(list (raise -0.30) (height 1.25)))
                 (neph-modeline-hud 1.5 10)
                 )))

;;
;; God mode
;;

(add-to-list 'load-path "~/.emacs.d/god-mode")
(require 'god-mode)
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
;; fci-mode
;;

(add-to-list 'load-path "~/.emacs.d/fill-column-indicator")
(require 'fill-column-indicator)
(setq fci-rule-color "#444")

;;
;; re-builder
;;

(require 're-builder)
(setq reb-re-syntax 'string)

;;
;; Misc modes with no config
;;

(add-to-list 'load-path "~/.emacs.d/fic-mode.git")
(require 'fic-mode)

(add-to-list 'load-path "~/.emacs.d/git-gutter-fringe")
(add-to-list 'load-path "~/.emacs.d/git-gutter")
(add-to-list 'load-path "~/.emacs.d/fringe-helper")
(require 'fringe-helper)
(require 'git-gutter-fringe)
(load-file "~/.emacs.d/rainbow-mode.el")

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

(global-set-key (kbd "C-z C-S-B") 'astyle-beautify-region)

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
;; Projectile
;;

(add-to-list 'load-path "~/.emacs.d/projectile")
(require 'projectile)
(require 'helm-projectile)
(projectile-global-mode t)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(setq projectile-enable-caching t)

(global-set-key (kbd "C-z M-f") 'projectile-find-file)
(global-set-key (kbd "C-z M-F") 'projectile-find-file-in-known-projects)
(global-set-key (kbd "C-z M-g") 'helm-projectile-grep)
;; Non-incremental, but can be faster and supports prefix arg for filename globbing
(global-set-key (kbd "C-z M-G") 'projectile-grep)
(global-set-key (kbd "C-z b") 'helm-projectile-switch-to-buffer)
(global-set-key (kbd "C-z B") 'helm-projectile-switch-to-buffer-other-window)
(global-set-key (kbd "C-z p") 'projectile-switch-project)
(global-set-key (kbd "C-z C-p") 'helm-projectile)

(setq projectile-switch-project-action 'helm-projectile)

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

(defun neph-cfg ()
;  (fci-mode t)
  (setq c-basic-offset 2)
  (setq c-default-style "linux")
  (setq sh-basic-offset 2)
  (setq sh-indentation 2)
  (git-gutter-mode t)
  (rainbow-mode t)
  (setq fill-column 80)
  (rainbow-delimiters-mode t)
  (run-with-idle-timer 1 nil (lambda ()
                               (color-identifiers-mode t)
                               (fic-mode t))))

(defadvice align-regexp (around align-regexp-with-spaces activate)
  (let ((indent-tabs-mode nil))
    ad-do-it))

(defadvice align (around align-with-spaces activate)
  (let ((indent-tabs-mode nil))
    ad-do-it))

; Tabs, 4 wide with 4 indent to match e.g. default VS style
(defun neph-valve-cfg ()
  (interactive)
  (setq c-default-style "linux")
  (setq indent-tabs-mode 'tabs)
  (setq c-basic-offset 4)
  (setq sh-basic-offset 4)
  (setq sh-indentation 4)
  (setq tab-width 4)
  (setq js-indent-level 4)
  (git-gutter-mode t)
  (rainbow-mode t)
  (setq fill-column 120)
  (rainbow-delimiters-mode t)
  (run-with-idle-timer 1 nil (lambda ()
                               (color-identifiers-mode t)
                               (fic-mode t))))
(add-hook 'sh-mode-hook 'neph-cfg)
(add-hook 'js-mode-hook 'neph-cfg)
(add-hook 'python-mode-hook 'neph-cfg)
(add-hook 'java-mode-hook 'neph-cfg)
(add-hook 'lisp-mode-hook 'neph-cfg)
(add-hook 'emacs-lisp-mode-hook 'neph-cfg)
(add-hook 'c-mode-common-hook 'neph-valve-cfg) ; Default to valve mode for now,
                                               ; should have path detection or
                                               ; something

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
  "Reopens the current file with sudo"
  (interactive)
  (set-visited-file-name
   (concat "/sudo::/" (buffer-file-name)))
  (toggle-read-only 0))

(defun drop-sudo ()
  "Drops tramp sudo sessions"
  (interactive)
  (dolist (buffer (buffer-list))
    (let ((name (buffer-name buffer)))
      (when (and name (string-match "^*tramp/sudo " name))
        (kill-buffer buffer))))
  (message "Dropped sudo buffers"))

(global-set-key (kbd "C-z S") 'sudoize-buffer)
(global-set-key (kbd "C-z C-S") 'drop-sudo)

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
(global-set-key (kbd "C-z C-S") (lambda () (interactive) (transpose-windows -1)))
(global-set-key (kbd "C-z C-D") 'transpose-windows)

;; Revert without prompting
(global-set-key (kbd "C-z R") (lambda () (interactive) (revert-buffer t t)))

; Quick eval-defun
(global-set-key (kbd "C-z E") 'eval-defun)
(global-set-key (kbd "C-z G") 'gdb)
(global-set-key (kbd "C-z M") 'gdb-many-windows)

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

;; Debug mode
(global-set-key (kbd "C-z C-M-D") (lambda ()
                                (interactive)
                                ;; If in mismatched state, default to enabling the disabled one
                                (if (and debug-on-error debug-on-quit)
                                    (progn
                                      (setq debug-on-error nil)
                                      (setq debug-on-quit nil)
                                      (message "Disabled debug-on-error and debug-on-quit"))
                                  (setq debug-on-error t)
                                  (setq debug-on-quit t)
                                  (message "Enabled debug-on-error and debug-on-quit"))))

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
(global-set-key (kbd "C-z C-c") 'toggle-case)

(defun flyspell-toggle (arg)
  "Toggle flyspell mode, and check the entire buffer when enabling"
  (interactive "p")
  (if (and (boundp 'flyspell-mode) flyspell-mode)
      (flyspell-mode 0)
    (flyspell-mode)
    (flyspell-buffer)))
(global-set-key (kbd "C-c M-l") 'flyspell-toggle)

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

(global-set-key (kbd "C-S-y") 'yank-and-indent)

(defun move-line-up ()
  "Move the current line up."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))
(global-set-key [(control shift up)] 'move-line-up)

(defun move-line-down ()
  "Move the current line down."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))
(global-set-key [(control shift down)] 'move-line-down)

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
  (if (call-process "p4" nil nil nil "edit" (buffer-file-name))
      (progn (read-only-mode 0)
             (message "p4 opened into default changeset"))
    (message "p4 edit failed")))
(global-set-key (kbd "C-z C-e") 'p4-edit-current)

;; Custom binds for existing commands
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

(define-key global-map (kbd "C-z C-c") 'ace-jump-mode-pop-mark)
(define-key global-map (kbd "C-z C-x") 'ace-jump-mode)

;;
;; Package
;;

(require 'package)
(add-to-list 'package-archives
             '("marmalade" .
               "http://marmalade-repo.org/packages/"))
(package-initialize)

;;
;; Line numbers
;;

(setq linum-format " %d ")
(global-linum-mode 1)
(setq linum-disabled-modes-list '(term-mode))
(defun linum-on()
  (unless (or (minibufferp) (member major-mode linum-disabled-modes-list))
    (linum-mode 1)))


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
(add-to-list 'load-path "~/.emacs.d/magit")
(require 'magit)
(require 'magit-blame)
(add-hook 'magit-blame-file-on (lambda() (fci-mode -1)))
(add-hook 'magit-blame-file-off (lambda() (fci-mode 1)))
(global-set-key (kbd "C-z g") 'magit-status)
(global-set-key (kbd "C-z L") 'magit-blame-mode)

;;
;; Theme
;;

;(add-to-list 'custom-theme-load-path "~/.emacs.d/sunburst-theme")
;(load-theme 'sunburst t)

;;
;; ample-zen
;;

(add-to-list 'custom-theme-load-path "~/.emacs.d/ample-zen")
(load-theme 'ample-zen t)

(set-face-attribute 'mode-line nil
                    :background "#111"
                    :foreground "#999"
                    :box '(:line-width 1 :color "#000" :style nil))
(set-face-attribute 'header-line nil
                    :background "#111"
                    :foreground "#999"
                    :box '(:line-width 1 :color "#000" :style nil))
(set-face-attribute 'mode-line-inactive nil
                    :background "#555"
                    :foreground "#999"
                    :box '(:line-width 1 :color "#333" :style nil))
(set-face-attribute 'hl-line nil
                    :background "#232020"
                    :box '(:line-width -1 :color "#444" :style nil))
(set-face-attribute 'linum nil
                    :foreground "#555"
                    :background "#222"
                    :box nil)
(setq linum-format " %5d  ")
(set-face-attribute 'whitespace-tab nil :background "#242424")

(set-face-attribute 'region nil :background "#392121")

;; rtags
(set-face-attribute 'rtags-skippedline nil :background "#323030")
(set-face-attribute 'rtags-errline nil :background "#511411")
(set-face-attribute 'rtags-fixitline nil :background "#513121")

;; ediff
(require 'ediff)
(set-face-attribute 'ediff-current-diff-A nil :background "#412421")
(set-face-attribute 'ediff-current-diff-B nil :background "#244121")
(set-face-attribute 'ediff-fine-diff-A nil :background "#811411")
(set-face-attribute 'ediff-fine-diff-B nil :background "#148111")

;; Semantic
(set-face-attribute 'semantic-tag-boundary-face nil :overline "#544")

;; Company
(set-face-attribute 'company-tooltip nil :background "#333" :foreground "white")
(set-face-attribute 'company-tooltip-selection nil :background "#555")
(set-face-attribute 'company-tooltip-common-selection nil :foreground "#955")
(set-face-attribute 'company-tooltip-common nil :foreground "#944")
(set-face-attribute 'company-tooltip-annotation nil :foreground "#A99")
(set-face-attribute 'company-scrollbar-fg nil :background "#222")
(set-face-attribute 'company-scrollbar-bg nil :background "#555")
(setq company-tooltip-margin 2)

;;
;; purple-haze
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

; Default font
(set-face-attribute 'default nil :family "DejaVu Sans Mono")
(set-face-attribute 'default nil :height 96)
