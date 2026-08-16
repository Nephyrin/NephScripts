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

(provide 'neph-lib)
