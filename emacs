; -*- mode: Emacs-Lisp; -*-

;;
;; Misc
;;

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
(setq load-path (cons "~/.emacs.d" load-path))

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
;; Color identifiers mode
;;

(add-to-list 'load-path "~/.emacs.d/color-identifiers-mode")
(add-to-list 'load-path "~/.emacs.d/dash") ; dependency
(require 'color-identifiers-mode)

;;
;; Company mode
;;

(add-to-list 'load-path "~/.emacs.d/company-mode")
(require 'company)
(require 'rtags)
(require 'company-rtags)
(add-to-list 'company-backends 'company-rtags)
(setq rtags-completions-enabled t) ; Needed?

(defun company-mode-moz ()
  (setq company-clang-arguments (split-string
                                 (shell-command-to-string
                                  (concat "~/.emacs.d/moz_objdir.sh "
                                          (buffer-file-name)))))
  (company-mode t)
  (local-set-key (kbd "<C-tab>") 'company-complete))
(defun company-mode-neph ()
  (rtags-diagnostics)
  (company-mode t)
  (local-set-key (kbd "<C-tab>") 'company-complete))
(add-hook 'c-mode-common-hook 'company-mode-neph)

(global-set-key (kbd "C-z C-.") 'rtags-find-symbol-at-point)
(global-set-key (kbd "C-z C-,") 'rtags-find-references-at-point)
(global-set-key (kbd "C-z .") 'rtags-find-symbol)
(global-set-key (kbd "C-z ,") 'rtags-find-references)
(global-set-key (kbd "C-z C-/") (lambda () (interactive) (delete-windows-on rtags-buffer-name t)))
(global-set-key (kbd "C-z C-n") 'rtags-next-match)
(global-set-key (kbd "C-z C-p") 'rtags-previous-match)

;;
;; GDB
;;

(setq gdb-non-stop-setting nil)
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
(defun neph-fill-to (reserve)
  `(:eval (propertize " " 'display '(space :align-to (- right-margin
                                                        ,reserve)))))
(defun neph-modeline-hud (height width)
  (propertize " " 'display (neph-hud "#0C0C0C" "#222222" height width)
                  'face 'neph-modeline-hud))
(setq neph-modeline-path
      '(:eval (propertize (let ((bufname (buffer-file-name)))
                            (if bufname
                                (replace-regexp-in-string
                                 "/[^/]*$" "/"
                                 (replace-regexp-in-string
                                  (regexp-quote (getenv "HOME")) "~"
                                  bufname)) ""))
                          'face 'neph-modeline-path)))
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
        :foreground "#866")))
  "Neph modeline which-func-mode face")
(setq-default mode-line-format
              '(:eval
                (list
                 neph-modeline-bufstat
                 " [%l:%2c] "
                 ;; which-function
                 (let ((which-func (and (boundp 'which-function) (which-function))))
                   (when (and which-func (not (string= "" which-func)))
                     (concat
                      (propertize which-func 'face 'neph-modeline-which-func)
                      " :: ")))
                 ; path
                 neph-modeline-path
                 ; buffer name
                 `(:propertize "%b" face ,(if (neph-modeline-active)
                                              'neph-modeline-id
                                            'neph-modeline-id-inactive))
                 ; Mode
                 "  %["
                 (propertize mode-name 'face 'neph-modeline-mode)
                 "%]  "
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

(require 'fill-column-indicator)
(setq fci-rule-color "#444")

;;
;; Misc modes with no config
;;

(add-to-list 'load-path "~/.emacs.d/fic-mode.git")
(require 'fic-mode)

(add-to-list 'load-path "~/.emacs.d/git-gutter-fringe")
(add-to-list 'load-path "~/.emacs.d/git-gutter")
(require 'fringe-helper)
(require 'git-gutter-fringe)
(require 'rainbow-mode)

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
;; Helm
;;
(add-to-list 'load-path "~/.emacs.d/helm")
(require 'helm-config)
(require 'helm-files)
; Way too broken
; (global-set-key (kbd "C-x C-f") 'helm-find-files)
(defun helm-find-moz ()
  "Find files in moz dir"
  (interactive)
  (helm-find-1 "~/moz/moz-git"))
(global-set-key (kbd "C-x M-f") 'helm-find-moz)

(when (executable-find "ack-grep")
  (setq helm-grep-default-command "ack-grep -Hn --no-group --no-color --smart-case --type-set IGNORED:ext:map --noIGNORED %p %f"
        helm-grep-default-recurse-command "ack-grep -H --no-group --no-color --smart-case --type-set IGNORED:ext:map --noIGNORED %p %f"))

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
; Tabs, 4 wide with 4 indent to match e.g. default VS style
(defun neph-valve-cfg ()
  (setq c-default-style "linux")
  (setq indent-tabs-mode 'tabs)
  (setq c-basic-offset 4)
  (setq sh-basic-offset 4)
  (setq sh-indentation 4)
  (setq tab-width 4)
  (git-gutter-mode t)
  (rainbow-mode t)
  (setq fill-column 100)
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

;;
;; Custom binds
;;

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
(global-set-key (kbd "C-z C-d") (lambda ()
                                  (interactive)
                                  (other-window -1)))
(global-set-key (kbd "C-z C-f") (lambda ()
                                  (interactive)
                                  (other-window 1)))

; New frame
(global-set-key (kbd "C-z C-n") (lambda ()
                                  (interactive)
                                  (make-frame)))

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

(defun sudoize-buffer ()
  "Reopens the current file with sudo"
  (interactive)
  (set-visited-file-name
   (concat "/sudo:root@localhost:/" (buffer-file-name)))
  (toggle-read-only 0))

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

(require 'linum)
(setq linum-format " %d ")
(global-linum-mode 1)
(setq linum-disabled-modes-list '(term-mode))
(defun linum-on()
  (unless (or (minibufferp) (member major-mode linum-disabled-modes-list))
    (linum-mode 1)))

;(setq linum-delay t)
;(setq linum-eager nil)

;;
;; Multi-term
;;

(require 'multi-term)
(setq multi-term-program "/bin/bash")

(global-set-key (kbd "C-x t") 'multi-term-dedicated-open)

(make-variable-buffer-local 'global-hl-line-mode)
; Paste not yank
(add-hook 'term-mode-hook (lambda ()
                            (setq global-hl-line-mode nil)
                            (define-key term-raw-map (kbd "C-y") 'term-paste)
                            ))

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
(global-set-key (kbd "C-z b") 'magit-blame-mode)

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
(set-face-attribute 'mode-line-inactive nil
                    :background "#555"
                    :foreground "#999"
                    :box '(:line-width 1 :color "#333" :style nil))

(set-face-attribute 'linum nil
                    :foreground "#555"
                    :background "#222")
(setq linum-format " %5d  ")
(set-face-attribute 'whitespace-tab nil :background "#242424")

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
;; (set-face-attribute 'minimap-font-face nil :family "Droid Sans Mono" :height 22)
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
