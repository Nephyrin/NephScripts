; -*- mode: Lisp; -*-
;;
;; Misc
;;

; Fix x clipboard
(setq x-select-enable-clipboard t)
(setq mouse-drag-copy-region nil)
(setq interprogram-paste-function 'x-cut-buffer-or-selection-value)

(setq inhibit-startup-message t)
(setq load-path (cons "~/.emacs.d" load-path))

(setq-default indent-tabs-mode nil)
(setq js-indent-level 2)

(global-auto-revert-mode t)
(setq make-backup-files nil)

(setq vc-follow-symlinks t)

(require 'ido)

; (global-ede-mode t)

; In gui mode, start server
(if window-system (server-start))

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

(setq desktop-path '("~/.emacs.d/"))
(setq desktop-dirname "~/.emacs.d/")
(setq desktop-base-file-name "emacs-desktop")

(if window-system (desktop-save-mode 1))

; fci
(require 'fill-column-indicator)
(setq-default fill-column 80)
(setq fci-rule-color "#444")
(setq fci-rule-column 80)
(defun enable-fci-mode ()
  (fci-mode t)
  (setq fci-rule-column 80))
(add-hook 'js-mode-hook 'enable-fci-mode)
(add-hook 'c-mode-common-hook 'enable-fci-mode)
(add-hook 'python-mode-hook 'enable-fci-mode)
(add-hook 'lisp-mode-hook 'enable-fci-mode)

;;
;; IswitchBuffers
;;

(iswitchb-mode 1)
(setq iswitchb-buffer-ignore '("^ " "^\*"))

(defun iswitchb-local-keys ()
  (mapc (lambda (K)
	  (let* ((key (car K)) (fun (cdr K)))
	    (define-key iswitchb-mode-map (edmacro-parse-keys key) fun)))
	'(("<right>" . iswitchb-next-match)
	  ("<left>"  . iswitchb-prev-match)
	  ("<up>"    . ignore             )
	  ("<down>"  . ignore             ))))

(add-hook 'iswitchb-define-mode-map-hook 'iswitchb-local-keys)

;;
;; Tramp
;;

(require 'tramp)
(setq tramp-default-method "ssh")

;;
;; Custom binds
;;

(global-set-key (kbd "C-S-k") 'kill-whole-line)
; Make ret auto-indent, but S-RET bypass
(define-key global-map (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "<C-return>") 'newline)

(defun yank-and-indent ()
  "Yank and then indent the newly formed region according to mode."
  (interactive)
  (yank)
  (call-interactively 'indent-region))

(global-set-key (kbd "C-S-y") 'yank-and-indent)

;; Behave like vi's o command
(defun open-next-line (arg)
  "Move to the next line and then opens a line.
    See also `newline-and-indent'."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (next-line 1)
  (indent-according-to-mode))

(global-set-key (kbd "C-S-o") 'open-next-line)
(global-set-key (kbd "C-c C-j") 'term-line-mode)
(global-set-key (kbd "C-c C-k") 'term-char-mode)

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
;; Theme
;;

(require 'color-theme)
(load-library "color-theme-sunburst")
(color-theme-tm)
; set a line highlight for this theme
(set-face-background 'hl-line "#282828")
; And a better font
(set-default-font "Monospace-10")

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
;; Auto-complete-clang
;;

(add-to-list 'load-path (concat "~/.emacs.d/" "auto-complete"))

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories (concat "~/.emacs.d/" "auto-complete/ac-dict"))

(require 'auto-complete-clang)

(setq ac-auto-start nil)
(setq ac-quick-help-delay 0.5)
;; (ac-set-trigger-key "TAB")
;; (define-key ac-mode-map  [(control tab)] 'auto-complete)
(define-key ac-mode-map  [(control tab)] 'auto-complete)
(defun my-ac-config ()
  (setq-default ac-sources '(ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))
  (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
  (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
  (add-hook 'ruby-mode-hook 'ac-ruby-mode-setup)
  (add-hook 'css-mode-hook 'ac-css-mode-setup):
  (add-hook 'auto-complete-mode-hook 'ac-common-setup)
  (global-auto-complete-mode t))
(defun my-ac-cc-mode-setup ()
  (setq ac-sources (append '(ac-source-clang ac-source-yasnippet) ac-sources)))
(add-hook 'c-mode-common-hook 'my-ac-cc-mode-setup)
;; ac-source-gtags
(my-ac-config)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ede-project-directories (quote ("/home/johns/moz/moz-git")))
 '(safe-local-variable-values (quote ((js-indent-level . 2)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
