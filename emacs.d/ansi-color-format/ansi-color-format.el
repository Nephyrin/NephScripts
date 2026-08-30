;;; ansi-color-format.el --- Edit files containing ANSI escape sequences  -*- lexical-binding: t; -*-

;; Keywords: faces, terminals, files
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; `ansi-color-format-mode' shows a file containing ANSI escape sequences as the colored text a terminal would display,
;; while you edit the plain text underneath and the escape sequences are written back out on save.
;;
;; It is built on Emacs's file-format layer (`format-alist', the machinery behind `enriched-mode').  Turning the mode on
;; removes every escape sequence from the buffer text and stashes its bytes in an `ansi-color-escapes' text property on
;; the text that follows it.  Saving hands those bytes back to `write-region' as annotations, so the buffer itself is
;; never touched on save and an unedited buffer writes back byte for byte.  Because the bytes ride along with the text
;; they precede, typed text lands inside the same escapes as its neighbors, and killing colored text takes its escapes
;; with it.  Turning the mode off puts the bytes back into the text.
;;
;; Colors are a view derived from the stashed bytes by a jit-lock function, the way font-lock derives faces from
;; keywords, so text that arrives with stashes attached (yank, undo, `insert-buffer') is recolored on display without
;; special handling.  Everything else sees plain text: `goto-address-mode', isearch, occur and friends just work.
;;
;; Usage:
;;
;;   M-x ansi-color-format-mode
;;
;; Raw escape sequences pasted into the buffer stay visible until you run `ansi-color-format-decode-region' on them.

;;; Code:

(require 'ansi-color)

(defgroup ansi-color-format nil
  "Edit files containing ANSI escape sequences."
  :group 'ansi-color)

(defcustom ansi-color-format-state-lookback 200
  "How many stashes to look back through for the state in effect somewhere.
Terminal output resets its attributes constantly, so the search
normally stops after a handful; this only bounds pathological files."
  :type 'natnum)

(defconst ansi-color-format--sequence-regexp
  (concat
   ;; CSI sequences, e.g. SGR "\e[1;31m" and "\e[K".
   ansi-color-control-seq-regexp
   ;; String sequences (OSC, DCS, PM, APC), ended by BEL or ESC \.
   "\\|\e[]P^_][^\a\e]*\\(?:\a\\|\e\\\\\\)"
   ;; Any other ESC sequence, e.g. "\e(B" or "\e7".
   "\\|\e[\x20-\x2F]*[\x30-\x7E]")
  "Regexp matching one escape sequence.")

(defconst ansi-color-format--sgr-regexp "\e\\[[\x30-\x3F]*[\x20-\x2F]*m"
  "Regexp matching one SGR (color and attribute) sequence.")

(defconst ansi-color-format--sgr-reset-regexp "\e\\[0?[;m]"
  "Regexp matching an SGR sequence that begins by resetting all attributes.")

(defvar-local ansi-color-format--trailing nil
  "Escape sequences that ended the buffer, as (MARKER . BYTES).
With no text after them to ride on, they are kept here and written
back at MARKER, which stays where they were.")
(put 'ansi-color-format--trailing 'permanent-local t)

;;;; Stashing escape sequences

(defun ansi-color-format--run-start-p (pos)
  "Non-nil if POS begins a run of text sharing one stash."
  (or (= pos (point-min))
      (not (eq (get-text-property pos 'ansi-color-escapes)
               (get-text-property (1- pos) 'ansi-color-escapes)))))

(defun ansi-color-format--stash (pos bytes)
  "Record that the escape sequences BYTES came right before POS."
  (let ((old (get-text-property pos 'ansi-color-escapes))
        (end (next-single-property-change pos 'ansi-color-escapes nil
                                          (point-max))))
    ;; A stash already starting at POS is emitted at POS too, so BYTES
    ;; goes in ahead of it.  One starting earlier stays with its run.
    (put-text-property pos end 'ansi-color-escapes
                       (if (ansi-color-format--run-start-p pos)
                           (concat bytes old)
                         bytes))))

(defun ansi-color-format--clear-trailing ()
  "Forget the trailing escape sequences."
  (when ansi-color-format--trailing
    (set-marker (car ansi-color-format--trailing) nil)
    (setq ansi-color-format--trailing nil)))

(defun ansi-color-format--flush-trailing ()
  "Stash the trailing sequences onto text that has since appeared after them."
  (pcase ansi-color-format--trailing
    (`(,marker . ,bytes)
     (when (< marker (point-max))
       (ansi-color-format--stash (marker-position marker) bytes)
       (ansi-color-format--clear-trailing)))))

(defun ansi-color-format--decode-region (beg end)
  "Move the escape sequences in BEG..END out of the text and into stashes.
Return the new end of the region."
  (let ((end (copy-marker end))
        (inhibit-read-only t)
        (inhibit-modification-hooks t))
    (save-excursion
      (save-match-data
        (ansi-color-format--flush-trailing)
        (goto-char beg)
        (while (re-search-forward ansi-color-format--sequence-regexp end t)
          (let ((start (match-beginning 0)))
            ;; Adjacent sequences form one stash.
            (while (looking-at ansi-color-format--sequence-regexp)
              (goto-char (match-end 0)))
            (let ((bytes (delete-and-extract-region start (point))))
              (if (< start (point-max))
                  (ansi-color-format--stash start bytes)
                (setq ansi-color-format--trailing
                      (cons (or (car ansi-color-format--trailing)
                                (copy-marker start))
                            (concat (cdr ansi-color-format--trailing)
                                    bytes)))))))))
    (prog1 (marker-position end)
      (set-marker end nil))))

(defun ansi-color-format--decode-buffer ()
  "Decode every escape sequence in the buffer, keeping the modified flag."
  (let ((modified (buffer-modified-p)))
    (save-excursion
      (save-restriction
        (widen)
        (ansi-color-format--flush-trailing)
        (goto-char (point-min))
        ;; Nothing to do before the first ESC, and finding it is cheap
        ;; even in a huge log.
        (when (search-forward "\e" nil t)
          (let ((beg (line-beginning-position)))
            (jit-lock-refontify
             beg (ansi-color-format--decode-region beg (point-max)))))))
    (restore-buffer-modified-p modified)))

(defun ansi-color-format--encode-buffer ()
  "Put the stashed escape sequences back into the buffer text."
  (let ((modified (buffer-modified-p))
        (inhibit-read-only t)
        (inhibit-modification-hooks t))
    (save-excursion
      (save-restriction
        (widen)
        (format-insert-annotations
         (ansi-color-format-encode (point-min) (point-max) (current-buffer)))
        (remove-list-of-text-properties
         (point-min) (point-max) '(ansi-color-escapes font-lock-face))
        (ansi-color-format--clear-trailing)))
    (restore-buffer-modified-p modified)))

;;;; The file format

(defun ansi-color-format-decode (from to)
  "Decode the escape sequences in FROM..TO; return the new end.
The decoding half of the `ansi-color' entry in `format-alist'."
  (ansi-color-format--decode-region from to))

(defun ansi-color-format-encode (from to _orig-buf)
  "Return `write-region' annotations restoring the escape sequences in FROM..TO.
FROM and TO are nil when the whole buffer is being written.  The
encoding half of the `ansi-color' entry in `format-alist'."
  (let ((from (or from (point-min)))
        (to (or to (point-max)))
        (annotations nil))
    (let ((pos from))
      (while (< pos to)
        (let ((bytes (get-text-property pos 'ansi-color-escapes)))
          (when bytes
            (push (cons pos bytes) annotations)))
        (setq pos (next-single-property-change pos 'ansi-color-escapes
                                               nil to))))
    (pcase ansi-color-format--trailing
      (`(,marker . ,bytes)
       (when (<= from marker to)
         (push (cons (marker-position marker) bytes) annotations))))
    (sort annotations #'car-less-than-car)))

(add-to-list 'format-alist
             '(ansi-color "Text colored with ANSI escape sequences."
                          nil ansi-color-format-decode ansi-color-format-encode
                          nil ansi-color-format-mode nil))

;;;; Deriving faces from the stashes

(defun ansi-color-format--sgr-sequences (bytes)
  "Return the SGR sequences among the escape sequences BYTES, concatenated."
  (let ((start 0) (parts nil))
    (while (and bytes (string-match ansi-color-format--sgr-regexp bytes start))
      (push (match-string 0 bytes) parts)
      (setq start (match-end 0)))
    (apply #'concat (nreverse parts))))

(defun ansi-color-format--state-before (pos)
  "Return, as one string, the SGR sequences establishing the state before POS.
Walks back over the stashes to the last full reset, or at most
`ansi-color-format-state-lookback' of them."
  (let ((sequences nil) (n 0) (start pos))
    (while (and (> start (point-min))
                (< n ansi-color-format-state-lookback)
                (not (and sequences
                          (string-match-p ansi-color-format--sgr-reset-regexp
                                          (car sequences)))))
      (setq start (previous-single-property-change
                   start 'ansi-color-escapes nil (point-min))
            n (1+ n))
      (let ((sgr (ansi-color-format--sgr-sequences
                  (get-text-property start 'ansi-color-escapes))))
        (unless (string-empty-p sgr)
          (push sgr sequences))))
    (apply #'concat sequences)))

(defun ansi-color-format--fontify (beg end)
  "Give BEG..END the `font-lock-face' its stashed escape sequences call for.
Registered with jit-lock."
  (save-match-data
    ;; Rebuild what a terminal saw: the state in effect at BEG, then
    ;; each run's SGR sequences followed by its text.  ESC in the text
    ;; itself (an undecoded paste) is neutralized so the rendered string
    ;; keeps the region's length.
    (let ((pieces (list (ansi-color-format--state-before beg)))
          (pos beg))
      (while (< pos end)
        (let ((next (next-single-property-change pos 'ansi-color-escapes
                                                 nil end)))
          (when (ansi-color-format--run-start-p pos)
            (push (ansi-color-format--sgr-sequences
                   (get-text-property pos 'ansi-color-escapes))
                  pieces))
          (push (subst-char-in-string
                 ?\e ?\C-a (buffer-substring-no-properties pos next))
                pieces)
          (setq pos next)))
      (let ((rendered (let ((ansi-color-context nil))
                        (ansi-color-apply (apply #'concat (nreverse pieces)))))
            (i 0))
        (with-silent-modifications
          (remove-list-of-text-properties beg end '(font-lock-face))
          (while (< i (length rendered))
            (let ((face (get-text-property i 'font-lock-face rendered))
                  (next (next-single-property-change
                         i 'font-lock-face rendered (length rendered))))
              (when face
                (put-text-property (+ beg i) (+ beg next)
                                   'font-lock-face face))
              (setq i next))))))))

;;;; The mode

(defvar ansi-color-format-mode)

(defun ansi-color-format--after-change-major-mode ()
  "Reinstall the mode's machinery after a major mode change."
  (when ansi-color-format-mode
    (ansi-color-format--setup)))
(put 'ansi-color-format--after-change-major-mode 'permanent-local-hook t)

(defun ansi-color-format--after-revert ()
  "Decode escape sequences brought in by a revert or by a tail append."
  (when ansi-color-format-mode
    (add-to-list 'buffer-file-format 'ansi-color)
    ;; A full revert replaced the text the trailing marker sat in.
    (when revert-buffer-in-progress-p
      (ansi-color-format--clear-trailing))
    (ansi-color-format--decode-buffer)))
(put 'ansi-color-format--after-revert 'permanent-local-hook t)

(defun ansi-color-format--setup ()
  "Install the view machinery and hooks in the current buffer."
  ;; `font-lock-face' displays only through font-lock's alias for it.
  (unless font-lock-mode
    (setq-local char-property-alias-alist '((face font-lock-face))))
  (jit-lock-register #'ansi-color-format--fontify)
  (add-hook 'after-revert-hook #'ansi-color-format--after-revert nil t)
  (add-hook 'after-change-major-mode-hook
            #'ansi-color-format--after-change-major-mode nil t))

(defun ansi-color-format--teardown ()
  "Remove the view machinery and hooks from the current buffer."
  (jit-lock-unregister #'ansi-color-format--fontify)
  (remove-hook 'after-revert-hook #'ansi-color-format--after-revert t)
  (remove-hook 'after-change-major-mode-hook
               #'ansi-color-format--after-change-major-mode t))

;;;###autoload
(define-minor-mode ansi-color-format-mode
  "Edit a file containing ANSI escape sequences as the text they color.

When enabled, the escape sequences are taken out of the buffer text and
remembered alongside the text they precede; the text is colored
accordingly, and the sequences are written back out when the buffer
is saved.  Disabling the mode puts them back into the text.

Toggling the mode changes buffer positions, so it resets undo history."
  :lighter " ANSI"
  (if ansi-color-format-mode
      (progn
        (ansi-color-format--decode-buffer)
        (add-to-list 'buffer-file-format 'ansi-color)
        (ansi-color-format--setup))
    (ansi-color-format--teardown)
    (setq buffer-file-format (delq 'ansi-color buffer-file-format))
    (ansi-color-format--encode-buffer))
  (unless (eq buffer-undo-list t)
    (setq buffer-undo-list nil))
  (jit-lock-refontify))
(put 'ansi-color-format-mode 'permanent-local t)

(defun ansi-color-format-decode-region (beg end)
  "Decode raw escape sequences in BEG..END, e.g. pasted terminal output."
  (interactive "r")
  (unless ansi-color-format-mode
    (user-error "Enable `ansi-color-format-mode' first"))
  (jit-lock-refontify beg (ansi-color-format--decode-region beg end)))

(provide 'ansi-color-format)

;;; ansi-color-format.el ends here
