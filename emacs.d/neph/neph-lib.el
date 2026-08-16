;; -*- lexical-binding: nil; -*-
;;
;; neph-lib: functions and macros for the emacs config.
;;
;; ~/.emacs itself is not byte-compiled, so anything that runs more than
;; once per session (defuns, hook lambdas, etc) lives here where it gets
;; compiled, and ~/.emacs stays wiring-only (requires, setqs, binds).

;;
;; Flyspell-lazy
;;

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

;;
;; Misc
;;

(defun neph-foo ()
  ""
  (interactive))

;; Always answer yes to: File %s is %s on disk.  Make buffer %s, too?
;; (There's no variable to control this)
(defun neph-y-or-n-p (orig-func prompt &rest args)
  (if (string-match "^File .* is .* on disk.  Make buffer .*, too\\? $"
                    prompt)
      t
    (apply orig-func prompt args)))

;;
;; Electric mode tweaks
;;

;; Custom inhibits on top of the normal behavior, since some choices are pretty bad by default.
(defun neph-electric-pair-inhibit-predicate (c)
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
      (electric-pair-conservative-inhibit c))))

;;
;; Misc yank/text/font handling helpers
;;

(defun neph-yank-with-properties ()
  "Yank text without stripping properties."
  (interactive)
  (let ((yank-excluded-properties nil))
    (yank)))

(defun neph-copy-face-to-font-lock-face (start end)
  "Copy all 'face' properties with 'font-lock-face' in the region START to END."
  (interactive "r")
  (save-excursion
    (let ((pos start))
      (while (< pos end)
        (let ((next (next-single-property-change pos 'face nil end))
              (current-face (get-text-property pos 'face)))
          (when current-face
            ;; Add the new property
            (put-text-property pos next 'font-lock-face current-face)
            ;; Remove the old property
            )
            ;;(remove-list-of-text-properties pos next '(face)))
          (setq pos next))))))

;;
;; Terminal color helpers
;;

;; Just wraps ansi-color-apply which works better than xterm-color it seems, handles truecolor
(defun neph-term-color-region (start end)
  "Turn terminal color codes into text properties in START to END (defaults to region interactively)."
  (interactive "r")
  (ansi-color-apply-on-region start end))

;; Interactive wrap on ansi-color but whole buffer
(defun neph-term-color-buffer ()
  "Turn terminal color codes into text properties in START to END (defaults to region interactively)."
  (interactive)
  (neph-term-color-region 0 (point-max)))


;; TODO: Pick sequential colors.  This lets us look up a list of colors in use:
;; (mapcar (lambda (pattern) (hi-lock-keyword->face pattern)) hi-lock-interactive-patterns)

(defvar neph-highlight-dwim-history nil)
(defun neph-highlight-dwim (text &optional as-regexp)
  "Highlight the given TEXT.
Called interactively, picks the regexp mode and text contextually.

If AS-REGEXP is set, highlight via `highlight-regexp', otherwise
via `highlight-phrase'

Interactively, TEXT picks the current region if active and
non-empty, else the word at point, else prompts the user.

Interactively, AS-REGEXP is true if the user was prompted for
explicit input."
  (interactive
   ;; text - region if active, otherwise word at point, otherwise input
   ;;        with prefix, always use input
   (let ((word-at-point (word-at-point))
         (region (and mark-active (buffer-substring (region-beginning) (region-end)))))
     (cond
      ((and (not current-prefix-arg) region (length region))
       (list region nil))
      ((and (not current-prefix-arg) word-at-point (length word-at-point))
       (list word-at-point nil))
      (t
       (list (read-string "Highlight: " nil 'neph-highlight-dwim-history) t)))))
  (if (and text (length text))
      (progn
        (if as-regexp (highlight-regexp text) (highlight-phrase text))
        (message (concat "Highlighting: " text)))
    (message "No selection or word at point to highlight")))

(defun neph-unhighlight-dwim ()
  (interactive)
  (if current-prefix-arg
      (unhighlight-regexp t)
    (call-interactively 'unhighlight-regexp)))


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
   (concat "/home/johns/neph/valve/bin/p4inter " args) "neph-p4inter"
   (lambda (process)
     (delete-trailing-whitespace)
     (highlight-regexp "^Change" 'git-commit-note))))

(defun neph-test (args)
  "Runs the p4inter command with args"
  (interactive "MTest: \n")
  (neph-buffer-command
   args "neph-test"
   (lambda (process)
     (font-lock-mode t)
     (compilation-minor-mode t))))

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
  ;; FIXME font from (face-attribute 'default :family)
  ;; FIXME charset utf-8?
  (start-process-shell-command "neph-html-region" nil "xdg-open ~/.emacs.d/htmlize-temp.htm"))

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

;;
;; htmlfontify
;;

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

;; Term key overrides
(defun term-send-raw-C-z ()
  "Send a raw Control-z value to term."
  (interactive)
  (term-send-raw-string (kbd "C-z")))

;;
;; Emacs Interactive Notebook (jupyter)
;;

;; FIXME This should autoload, but the janky-AF http proxy disabling needs to be looked at
;; (something inherits it during the load process and I can't stop it)
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
;; Helm
;;

;; Was an inline lambda on the C-z F global-set-key
(defun neph-helm-find-in-directory ()
  "Run helm-find under a prompted-for directory."
  (interactive)
  (helm-find-1 (read-directory-name "Run find in directory: " nil "" t)))

;;
;; Helm AG
;;

;; NOTE: neph-lib is byte-compiled before helm/cl are loaded, so make the macros
;; these functions use (with-helm-alive-p, flet) visible to the compiler; they were
;; in scope in neph-init.el, which requires both at the top of the file.
(eval-when-compile
  (require 'cl)
  (require 'helm))

(defun helm-ff-helm-do-ag ()
  (interactive)
  (with-helm-alive-p
    (helm-exit-and-execute-action '(lambda (basedir)
                                     (let ((parent (file-name-directory (directory-file-name basedir)))
                                           (default-directory nil))
                                       (helm-do-ag nil (list parent)))))))
;; FIXME not needed?
;; (put 'helm-ff-helm-do-ag 'helm-only nil)

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

;;
;; Helm RG
;;

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

;; This function always calls 'alternate-method', so let bind that to normal method for the "normal visit" keybind.
(defun neph-rg-bounce-visit-current-file ()
  "Visit the helm-rg bounce result at point using the normal display method."
  (interactive)
  (let ((helm-rg-display-buffer-alternate-method
         helm-rg-display-buffer-normal-method))
    (helm-rg--visit-current-file-for-bounce)))

;;
;; phi-search
;;

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

(defun isearch-to-swiper ()
    "Drop into swiper with current isearch"
    (interactive)
    (isearch-exit)
    (swiper isearch-string))


;;
;; Company mode
;;

(defun neph-company-setup ()
  (interactive)
  (company-mode t)
  (company-quickhelp-mode t)
  ;;(semantic-mode t)
  (local-set-key (kbd "<C-tab>") 'company-complete))

(defun company-mode-moz ()
  (setq company-clang-arguments (split-string
                                 (shell-command-to-string
                                  (concat "~/.emacs.d/moz_objdir.sh "
                                          (buffer-file-name)))))
  (company-mode t)
  (local-set-key (kbd "<C-tab>") 'company-complete))

;;
;; C++ Helper mode(s) : Company/lsp and associated helper libraries
;;

(defun neph-hook-client-init (client func)
  "Add a post-call hook FUNC to the given lsp CLIENT."
  (message "HOOK")
  (let ((original-init-fn (lsp--client-initialized-fn client)))
    (setf (lsp--client-initialized-fn client)
          `(lambda (workspace)
             (when ,original-init-fn
               (funcall ,original-init-fn workspace))
             (funcall ,func workspace)))))

;; Strip semantic tokens from lsp-clangd, set ccls to be an addon with only semantic tokens
;; Lets us use both with ccls just providing superior semantic highlighting
;;(let ((clangd-client (gethash 'clangd lsp-clients))
;;      (ccls-client (gethash 'ccls lsp-clients)))
;;  ;; clangd: remove semantic tokens
;;  (when clangd-client
;;    (neph-hook-client-init
;;     clangd-client
;;     (lambda (workspace)
;;       (-> workspace
;;           (lsp--workspace-server-capabilities)
;;           (lsp:set-server-capabilities-semantic-tokens-provider? nil))
;;       (message "lsp-clangd capabilities stripped of semanticTokensProvider")))
;;    (message "lsp-clangd client configuration updated"))
;;  ;; ccls: set to addon mode, hook init to replace all caps with just semantic tokens
;;  (when ccls-client
;;    (setf (lsp--client-priority ccls-client) -3)
;;    (setf (lsp--client-add-on? ccls-client) t)
;;    (neph-hook-client-init
;;     ccls-client
;;     (lambda (workspace)
;;       (let* ((caps (lsp--workspace-server-capabilities workspace))
;;              (semantic-tokens (plist-get caps :semanticTokensProvider)))
;;         (setq caps nil)
;;         (when semantic-tokens
;;           (setq caps (plist-put caps :semanticTokensProvider semantic-tokens)))
;;         (setf (lsp--workspace-server-capabilities workspace) caps)
;;         (message "CCLS capabilities limited to semanticTokensProvider"))))
;;    (message "lsp-clangd client configuration updated")))

;; Lsp booster (chunk of lisp from their setup steps)
;;
(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)                             ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             lsp-use-plists
             (not (functionp 'json-rpc-connection))  ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
            (setcar orig-result command-from-exec-path))
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      (message "NOT using lsp-booster")
      orig-result)))

(defun neph-clear-text-properties ()
  "Reset all text properties in the buffer."
  (interactive)
  (with-silent-modifications
    (delete-all-overlays)
    (set-text-properties (buffer-end 0) (buffer-end 1) nil)))

(defun neph-lsp-reset ()
  "Reconnects to LSP, fixing annoying CCLS highlighting bug."
  (interactive)
  (lsp-disconnect)
  (neph-clear-text-properties)
  (lsp))

;;
;; ccls navigation (were inline lambdas on the C-z <C-arrow> binds)
;;

(defun neph-ccls-navigate-up ()
  "Navigate to the ccls \"U\" (up) node."
  (interactive)
  (ccls-navigate "U"))

(defun neph-ccls-navigate-down ()
  "Navigate to the ccls \"D\" (down) node."
  (interactive)
  (ccls-navigate "D"))

(defun neph-ccls-navigate-left ()
  "Navigate to the ccls \"L\" (left) node."
  (interactive)
  (ccls-navigate "L"))

(defun neph-ccls-navigate-right ()
  "Navigate to the ccls \"R\" (right) node."
  (interactive)
  (ccls-navigate "R"))

;;
;; ccls
;;

(defun neph-toggle-ccls-client (&optional force)
  "Toggle the ccls LSP client.
If FORCE is 'nil, enable ccls (remove from disabled list).
If FORCE is 't, disable ccls (add to disabled list).
If FORCE is not specified, toggle the current state."
  (interactive)
  (let* ((is-disabled (memq 'ccls lsp-disabled-clients))
         (should-disable (if (null force)
                            (not is-disabled)
                          force)))
    (if should-disable
        (progn
          (setq lsp-semantic-tokens-enable t)
          (add-to-list 'lsp-disabled-clients 'ccls))
      (setq lsp-semantic-tokens-enable nil)
      (setq lsp-disabled-clients (remove 'ccls lsp-disabled-clients)))
    (message "ccls LSP client %s" (if should-disable "disabled" "enabled"))))

(defun neph-toggle-ccls-reload ()
  "Toggle the enabled state of the ccls client, and then reload the current lsp workspace."
  (interactive)
  (neph-toggle-ccls-client)
  (neph-clear-text-properties)
  (call-interactively 'lsp-workspace-restart))

;; `-some->>' below is a dash macro, and neph-lib is byte-compiled before dash
;; is loaded; without this it compiles to a plain function call and breaks.
(eval-when-compile (require 'dash))

(defun neph-ccls-reformat-definition ()
  "Reformat the definition under the cursor according to how LSP parsed it."
  (interactive)
  (let* ((hover-response (-some->> (lsp--text-document-position-params)
                                   (lsp--make-request "textDocument/hover")
                                   (lsp--send-request)))
         (hover-contents (plist-get hover-response :contents))
         (hover-text (if (vectorp hover-contents)
                         (plist-get (aref hover-contents 0) :value)
                       (plist-get hover-contents :value)))
         ;; Request the location/textual-range of the definition for the thing under cursor.
         (def-response (-some->> (lsp--text-document-position-params)
                                 (lsp--make-request "textDocument/definition")
                                 (lsp--send-request)
                                 (car)))
         (target-range (plist-get def-response :targetRange))
         (start-pos (plist-get target-range :start))
         (end-pos (plist-get target-range :end)))

    ;; Debug output
    (message "Debug: hover-contents: %s" hover-contents)
    (message "Debug: hover-text: %s" hover-text)
    (message "Debug: def-response: %S" def-response)
    (message "Debug: target-range: %S" target-range)
    (message "Debug: start-pos: %S, end-pos: %S" start-pos end-pos)

    (if (and start-pos end-pos)
        (let ((def-start (lsp--position-to-point start-pos))
              (def-end (lsp--position-to-point end-pos)))
          (message "Debug: def-start: %s, def-end: %s, current-point: %s" def-start def-end (point))
          (if (and hover-text def-start def-end (<= def-start (point) def-end))
              (save-excursion
                (goto-char def-start)
                (delete-region def-start def-end)
                (insert hover-text))
            (message "No valid definition range or hover text found.")))
      (message "No definition range recognized. (save file, and make sure you're on the type name)"))))

;;
;; Irony-mode (deprecated)
;;

;; Bonus key to use counsel-irony if available
(defun irony-mode-counsel-hook ()
  (when (require 'counsel nil t)
    (define-key irony-mode-map
      ;;[remap completion-at-point] 'counsel-irony)
      ;;[remap complete-symbol] 'counsel-irony)
      (kbd "<C-M-tab>") 'counsel-irony)))

;; Load flycheck-irony if both flycheck and irony get enabled
(defun neph-flycheck-irony-setup ()
  "Load flycheck-irony if both irony and flycheck are loaded."
  (when (and (featurep 'flycheck)
             (featurep 'irony)
             (not (featurep 'flycheck-irony)))
    (require 'flycheck-irony)
    (add-hook 'flycheck-mode-hook #'flycheck-irony-setup)))

;;
;; Rtags
;;   DEPRECATED - going to drop if ccls + lsp keeps working well
;;

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
;; ido
;;

;; global ido-mode is incompatible with helm mode, but we just want it for find file.  Which, annoyingly, gets
;; intercepted by helm...
(defun neph-ido-find-file ()
  "Call 'ido-find-file' with ido enabled, then return to previous state."
  (interactive)
  (let ((was-ido-mode ido-mode))
    (ido-mode t)
    (unwind-protect (call-interactively 'ido-find-file)
      (when (not was-ido-mode) (ido-mode -1)))))


;;
;; P4
;;

;; p4vc commands. Throw in some systemd unit to sidestep async-process garbage in emacs
(defun neph-p4v-cmd (file command &rest args)
  (if (executable-find "p4vc")
      (let* ((cmd (append (list "systemd-run" "--user" "--property=ExitType=cgroup"
                                (concat "--working-directory=" (file-name-directory file))
                                "--setenv=P4CONFIG=P4CONFIG"
                                "--" "p4vc" command)
                          args
                          (list file))))
        (message (concat "Running: " (mapconcat #'identity cmd " ")))
        (apply #'call-process (car cmd) nil nil nil (cdr cmd)))
    (message "!! p4vc not installed/available")))

(defun neph-p4-cmd (file &rest args)
  (let ((default-directory (file-name-directory file))
        (process-environment (copy-sequence process-environment)))
    (setenv "P4CONFIG" "P4CONFIG")
    (apply #'call-process "p4" nil nil nil (append args (list file)))))

(defun neph-p4-cmd-current (&rest args)
  (if (buffer-file-name)
      (apply #'neph-p4-cmd (buffer-file-name) args)
    (message "!! Current buffer has no associated file")
    -1))

(defun neph-p4v-cmd-current (&rest args)
  (if (buffer-file-name)
      (apply #'neph-p4v-cmd (buffer-file-name) args)
    (message "!! Current buffer has no associated file")
    -1))

(defun neph-p4-edit-current ()
  "p4 edit the current buffer"
  (interactive)
  (if (= 0 (neph-p4-cmd-current "edit"))
      (progn (setq buffer-read-only nil)
             (message "p4 opened into default changeset"))
    (message "p4 edit failed")
    -1))

(defun neph-p4-revert-current ()
  "p4 revert the current buffer"
  (interactive)
  (if (= 0 (neph-p4-cmd-current "revert"))
      (progn (setq buffer-read-only t)
             (message "p4 reverted"))
    (message "!! p4 revert failed")))

(defun neph-p4vc-tlv      () (interactive) (neph-p4v-cmd-current "tlv"))
(defun neph-p4vc-revgraph () (interactive) (neph-p4v-cmd-current "revgraph"))
(defun neph-p4vc-history  () (interactive) (neph-p4v-cmd-current "history"))

(provide 'neph-lib)
