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

;;
;; Ediff
;;

(defun neph-ediff-mode ()
  (git-gutter-mode -1))

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
;; Projectile
;;

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

;;
;; helm-projectile
;;

;; helm projectile-ag/rg with default args
(defun helm-projectile-ag-cpp()
  (interactive)
  (require 'helm-projectile)
  (let ((helm-ag-base-command (concat helm-ag-base-command " --cpp --cc")))
    (helm-projectile-ag)))
(defun helm-projectile-rg-cpp()
  (interactive)
  (require 'helm-projectile)
  (let ((helm-rg-default-extra-args (append helm-rg-default-extra-args (split-string-and-unquote "-t cpp -t c"))))
    (call-interactively 'helm-projectile-rg)))
(defun helm-projectile-rg-php()
  (interactive)
  (require 'helm-projectile)
  (let ((helm-rg-default-extra-args (append helm-rg-default-extra-args (split-string-and-unquote "-t php"))))
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

;; Switch project action
(defun neph-projectile-switch-and-rg ()
  (interactive)
  (let ((projectile-switch-project-action 'helm-projectile-rg))
    (call-interactively 'helm-projectile-switch-project)))
(defun neph-projectile-switch-and-rg-cpp ()
  (interactive)
  (let ((projectile-switch-project-action 'helm-projectile-rg-cpp))
    (call-interactively 'helm-projectile-switch-project)))

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
  (setq truncate-lines t) ;; Lines go off screen
  ;; Alternative, wrap at whitespace.
  ;; With neither, hard-wraps at whatever char
  ;; (visual-line-mode)
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
  (electric-indent-mode t)
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

(defun neph-bash-mode ()
  "Invokes 'sh-mode' but defaulting to bash."
  (sh-mode)
  (sh-set-shell "bash"))

(defun neph-js-mode-hook ()
  "Set minor modes and buffer-local configuration for js language buffers."
  (if (and (stringp buffer-file-name)
           (string-match "\\.\\(v[pg]c\\|res\\)\\'" buffer-file-name))
      (neph-tab-cfg)
    (neph-space-cfg)))

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

;;
;; Tramp
;;

; No auto-save
(defun tramp-set-auto-save ()
  (auto-save-mode -1))

(defun sudoize-buffer ()
  "Reopens the current file with sudo."
  (interactive)
  ;; By default changing the visited file name counts as a modification, but this should be the same file.
  (with-silent-modifications
    (set-visited-file-name
     (neph-prepend-tramp-hop (buffer-file-name) "sudo" "root" "")))
  ;; Re-run this since we're probably in read-only mode and hooks didn't initialize expecting tramp etc..
  ;; Read-only is auto-enabled but not auto-disabled, so start with it off before re-running normal mode.
  (read-only-mode 0)
  (normal-mode))

(defun drop-sudo ()
  "Drops tramp sudo sessions."
  (interactive)
  (dolist (buffer (buffer-list))
    (let ((name (buffer-name buffer)))
      (when (and name (string-match "^*tramp/sudo " name))
        (kill-buffer buffer))))
  (message "Dropped sudo buffers"))

(defun neph-prepend-tramp-hop (filename method user host)
  "Given a FILENAME, prepend a hop to the tramp chain with METHOD USER and HOST.
If this is a local file, turn it into a tramp file file with said information."
  (let ((is-tramp-file (tramp-tramp-file-p filename))
        (localname filename)
        (hop nil))
    (if is-tramp-file
        ;; Already tramp, parse the struct and stuff its data into the sub-hop
        (with-parsed-tramp-file-name filename vec
          ;; This is the only part of the structure not part of the "hop" string, so we can just make a new
          ;; structure and turn the old one into a hop string within it.
          (setq localname vec-localname)
          ;; tramp-make-tramp-hop-name will consider nested hops, so we're just pushing the whole struct down one
          ;; nesting level.
          (setq hop (tramp-make-tramp-hop-name vec))))
    ;; Now make the new file string
    (tramp-make-tramp-file-name
     (make-tramp-file-name
      :method method
      :user user
      :host host
      :localname localname
      :hop hop))))

;;
;; Term mode
;;

(defun neph-disable-global-hl-line ()
  (setq-local global-hl-line-mode nil))

;;
;; isearch tweaks
;;

; Always exit isearch at the beginning of the match
(defun isearch-exit-at-start-hook ()
  (when (and isearch-forward isearch-other-end (not isearch-mode-end-hook-quit))
    (goto-char isearch-other-end)))

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

;; Was an inline lambda on the C-z C-S-S global-set-key
(defun neph-transpose-windows-backward ()
  "Transpose the buffers shown in this window and the previous one."
  (interactive)
  (transpose-windows -1))

;; Was an inline lambda on the C-z R global-set-key
(defun neph-revert-buffer-noconfirm ()
  "Revert the current buffer, ignoring auto-save and without prompting."
  (interactive)
  (revert-buffer t t))

;; Was an inline lambda on the C-x O and C-z C-s global-set-keys
(defun neph-other-window-backward ()
  "Select the previous window."
  (interactive)
  (other-window -1))

;; Was an inline lambda on the C-z C-d global-set-key
(defun neph-other-window-forward ()
  "Select the next window."
  (interactive)
  (other-window 1))

;; Was an inline lambda on the s-n global-set-key
(defun neph-scroll-up-one ()
  "Scroll this window up one line."
  (interactive)
  (scroll-up 1))

;; Was an inline lambda on the s-p global-set-key
(defun neph-scroll-down-one ()
  "Scroll this window down one line."
  (interactive)
  (scroll-down 1))

;; Was an inline lambda on the s-l global-set-key
(defun neph-move-to-window-center-line ()
  "Move point to the center line of this window."
  (interactive)
  (move-to-window-line nil))

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

;; merge-next-line
(defun merge-next-line (arg)
  "Merge line with next"
  (interactive "p")
  (next-line 1)
  (delete-indentation))

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

(defun bookmark-current-line ()
  "Bookmark the current line, using itself as the bookmark name"
  (interactive)
  (let ((line (thing-at-point 'line t)))
    (when (string-match "[ \t\n]*$" line)
      (setq line (replace-match "" nil nil line)))
    (bookmark-set line)
    (message (concat "Created bookmark: " line))))

(defun move-line-up ()
  "Move the current line up."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move the current line down."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

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

(defun open-next-line (arg)
  "Move to the next line and then opens a line.
    See also `newline-and-indent'."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (next-line 1)
  (indent-according-to-mode))

;; Was an inline lambda on the minibuffer-local-map [f3] define-key
(defun neph-insert-selected-window-buffer-name ()
  "Insert the name of the buffer the minibuffer was entered from."
  (interactive)
  (insert (buffer-name (window-buffer (minibuffer-selected-window)))))

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

(defun copy-line (&optional arg)
  "Copy lines (as many as prefix argument) in the kill ring"
  (interactive "p")
  (kill-ring-save (line-beginning-position)
                  (line-beginning-position (+ 1 arg)))
  (message "%d line%s copied" arg (if (= 1 arg) "" "s")))

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

;; Was an inline lambda on the C-z SPC global-set-key
(defun neph-point-to-register-quick (&optional arg)
  "Store point in register ARG, defaulting to register 7."
  (interactive "P")
  (point-to-register (or arg 7))
  (message "Set register %d" (or arg 7)))

;; Was an inline lambda on the C-z C-SPC global-set-key
(defun neph-jump-to-register-quick (&optional arg)
  "Jump to the point stored in register ARG, defaulting to register 7."
  (interactive "P")
  (jump-to-register (or arg 7))
  (message "Jump to register %d" (or arg 7)))

(defun mark-current-line (&optional arg)
  "Mark the current line without moving the cursor"
  (interactive)
  (end-of-line)
  (set-mark (line-beginning-position)))

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

(defun touch-current-file ()
     "updates mtime on the file for the current buffer"
     (interactive)
     (if (buffer-file-name)
         (progn
           (shell-command (concat "touch " (shell-quote-argument (buffer-file-name))))
           (clear-visited-file-modtime)
           (message (concat "Ran touch on " (buffer-file-name))))
       (message "No filename for current file")))

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

(defun neph-xdg-open-this-file ()
  "Pass the current file to xdg-open whynot."
  (interactive)
  (if (buffer-file-name)
      (shell-command (concat "xdg-open " (shell-quote-argument (buffer-file-name))))
    (message "!! This buffer has no associated file")))

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

(provide 'neph-lib)
