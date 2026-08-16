;; -*- mode: Emacs-Lisp; -*-

;; function-args modes (Disabled pending semantic)
;;;;(require 'function-args)
;;(fa-config-default)
;;(setq moo-select-method 'helm)

;; For web mode in tabs, we want to disable whitespace tabs because they conflict with the
;; php-background-coloring.  In space mode we can just use neph-space-cfg, as we want to highlight
;; errant tabs.  BUT - whitespace mode needs to be re-started when screwing with this variable.

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
                                  (point-to-register (or arg 7))
                                  (message "Set register %d" (or arg 7))))
(global-set-key (kbd "C-z C-SPC") (lambda (&optional arg) (interactive "P")
                                    (jump-to-register (or arg 7))
                                    (message "Jump to register %d" (or arg 7))))

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

;; kmacro-bind-to-key but wraps it in with-undo-amalgamate so it binds it as one atomic do/undo action
(defun neph-kmacro-bind-to-key-amalgamate ()
  (interactive)
  ;; Hook kmacro-ring-head that kmacro-bind-to-key uses to get the last macro, return a lambda instead that calls it
  ;; with undo-amalgamate. The macro `, fuckery means we call (kmacro-ring-head) at binding time and embed it in the
  ;; returned lambda
  (cl-letf* (((symbol-function 'kmacro-ring-head)
              `(lambda ()
                 (lambda () (interactive)
                   (with-undo-amalgamate (funcall ,(kmacro-ring-head)))))))
    (call-interactively 'kmacro-bind-to-key)))

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
(require 'salt-mode)

;;
;; zap-to-char
;;

; Make zap-to-char zap-up-to-char
(defadvice zap-to-char (after my-zap-to-char-advice (arg char &optional interactive) activate)
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

(provide 'neph-init)
;;; neph-init.el ends here
