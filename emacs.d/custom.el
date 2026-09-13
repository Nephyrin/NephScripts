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

;; Global hl-line-mode block

;;
;; isearch tweaks

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
;; Custom binds
;;

;; Bound to shift + the window nav keys below
;; Note: was shadowed by diff-buffer-with-file prior to elpacification, commented
;;(global-set-key (kbd "C-z C-S-D") 'transpose-windows)

;; Revert without prompting

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

; Scroll window

; Fast window nav

;; Diff current changes
(global-set-key (kbd "C-z C-S-D") 'diff-buffer-with-file)
(global-set-key (kbd "C-z C-M-S-D") 'ediff-current-file)

;; Keybind for enabling debug stuff quickly when I'm mad at something hanging.  Which is always.

;; Bonus align keys

(global-set-key (kbd "C-z C-a") 'align-regexp)
(global-set-key (kbd "C-z a") 'neph-align-regexp-u)

;; Prefer to org-mode's default bind
(eval-after-load 'org '(define-key org-mode-map [(control shift up)] nil))

;; Prefer to org-mode's default bind
(eval-after-load 'org '(define-key org-mode-map [(control shift down)] nil))

; F3 inserts current filename into minibuffer

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


;; Replaces backwards/forwards sexp.
(global-set-key (kbd "M-G") 'goto-line)


(with-eval-after-load "sql"
  (define-key sql-mode-map (kbd "C-c C-a") 'sql-send-secondary))

;; Quick register movement.
;; Default to register 7 since it's awkward to hit, leaving other registers available for explicit.



;; Non-hooked version is C-x C-k b

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
;; Highlight/unhighlight dwim binds (neph-highlight-dwim / neph-unhighlight-dwim in neph-lib)
;;


;;
;; Artist mode
;;
(global-set-key (kbd "C-z C-M-a") 'artist-mode) ;; C-c C-c exits artist mode


;;
;; zap-to-char
