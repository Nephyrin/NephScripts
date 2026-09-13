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
