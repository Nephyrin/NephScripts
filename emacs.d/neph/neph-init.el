;; -*- mode: Emacs-Lisp; -*-

;; function-args modes (Disabled pending semantic)
;;;;(require 'function-args)
;;(fa-config-default)
;;(setq moo-select-method 'helm)

;; For web mode in tabs, we want to disable whitespace tabs because they conflict with the
;; php-background-coloring.  In space mode we can just use neph-space-cfg, as we want to highlight
;; errant tabs.  BUT - whitespace mode needs to be re-started when screwing with this variable.
;;
;; Tramp
;;

(require 'tramp)
(setq tramp-default-method "sshx")

;; suck less?
;;(setq remote-file-name-inhibit-locks t)
(setq tramp-use-scp-direct-remote-copying t)
;;(setq remote-file-name-inhibit-auto-save-visited t)
;; Use direct-async-process
(connection-local-set-profile-variables
 'remote-direct-async-process
 '((tramp-direct-async-process . t)))
(connection-local-set-profiles
 '(:application tramp :protocol "scp")
 'remote-direct-async-process)
(connection-local-set-profiles
 '(:application tramp :protocol "rsync")
 'remote-direct-async-process)
;; Fix magit in that mode
;; https://github.com/magit/magit/issues/5220
(setq magit-tramp-pipe-stty-settings 'pty)

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

(global-set-key (kbd "C-z C-u") 'sudoize-buffer)
(global-set-key (kbd "C-z C-M-u") 'drop-sudo)

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
;; Artist mode
;;
(global-set-key (kbd "C-z C-M-a") 'artist-mode) ;; C-c C-c exits artist mode


;;
;; Yaml mode
;;
(require 'yaml-mode)
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
;(define-key global-map (kbd "RET") 'newline)
(global-set-key (kbd "<C-return>") 'electric-indent-just-newline)
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
;; Magit
;;

(require 'with-editor)
(require 'magit)
(require 'magit-blame)
(global-set-key (kbd "C-z C-<return>") 'magit-status)
(global-set-key (kbd "C-z L") 'magit-blame-mode)
(global-set-key (kbd "C-z x") 'magit)
(global-set-key (kbd "C-z X") 'magit-ediff-stage)
(global-set-key (kbd "C-z C") 'magit-commit)

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
