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
;; Colors and OSC 8 hyperlinks are a view derived from the stashed bytes by a jit-lock function, the way font-lock
;; derives faces from keywords, so text that arrives with stashes attached (yank, undo, `insert-buffer') is recolored on
;; display without special handling.  Everything else sees plain text: `goto-address-mode', isearch, occur and friends
;; just work.
;;
;; Changing the colors means rewriting the stashed bytes: `ansi-color-format-apply-sgr' applies SGR parameters on top of
;; a region's look (`ansi-color-format-set-foreground' and friends choose them for you), `ansi-color-format-copy-sgr'
;; picks up the look at point to apply elsewhere, and `ansi-color-format-set-link' makes a region a hyperlink.
;;
;; Usage:
;;
;;   M-x ansi-color-format-mode
;;
;; Raw escape sequences pasted into the buffer stay visible until you run `ansi-color-format-decode-region' on them.

;;; Code:

(require 'ansi-color)
(require 'seq)

(defgroup ansi-color-format nil
  "Edit files containing ANSI escape sequences."
  :group 'ansi-color)

(defcustom ansi-color-format-state-lookback 200
  "How many stashes to look back through for the state in effect somewhere.
Terminal output resets its attributes constantly, so the search
normally stops after a handful; this only bounds pathological files."
  :type 'natnum)

(defface ansi-color-format-link '((t :underline t))
  "Face added to OSC 8 hyperlinks, on top of their colors.")

(defconst ansi-color-format--sequence-regexp
  (concat
   ;; CSI sequences, e.g. SGR "\e[1;31m" and "\e[K".
   ansi-color-control-seq-regexp
   ;; String sequences (OSC, DCS, PM, APC), ended by BEL or ESC \.
   "\\|\e[]P^_][^\a\e]*\\(?:\a\\|\e\\\\\\)"
   ;; Any other ESC sequence, e.g. "\e(B" or "\e7".
   "\\|\e[\x20-\x2F]*[\x30-\x7E]")
  "Regexp matching one escape sequence.")

(defconst ansi-color-format--sgr-regexp "\e\\[\\([\x30-\x3F]*\\)[\x20-\x2F]*m"
  "Regexp matching one SGR (color and attribute) sequence.
Group 1 is its parameters.")

(defconst ansi-color-format--sgr-reset-regexp "\e\\[0?[;m]"
  "Regexp matching an SGR sequence that begins by resetting all attributes.")

(defconst ansi-color-format--osc8-regexp
  "\e\\]8;\\([^;\a\e]*\\);\\([^\a\e]*\\)\\(?:\a\\|\e\\\\\\)"
  "Regexp matching an OSC 8 hyperlink sequence.
Group 2 is the URL; an empty one ends the link.")

(defconst ansi-color-format--derived-properties
  '(font-lock-face ansi-color-url mouse-face help-echo follow-link keymap)
  "Text properties the mode derives from the stashed escape sequences.")

(defvar ansi-color-format-mode)

(defvar-local ansi-color-format--trailing nil
  "Escape sequences that ended the buffer, as (MARKER . BYTES).
With no text after them to ride on, they are kept here and written
back at MARKER, which stays where they were.")
(put 'ansi-color-format--trailing 'permanent-local t)

(defun ansi-color-format--check ()
  "Signal an error unless the mode is on."
  (unless ansi-color-format-mode
    (user-error "Enable `ansi-color-format-mode' first")))

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

(defun ansi-color-format--stash-or-trail (pos bytes)
  "Stash BYTES before POS, or as trailing sequences if no text follows POS."
  (if (< pos (point-max))
      (ansi-color-format--stash pos bytes)
    (setq ansi-color-format--trailing
          (cons (or (car ansi-color-format--trailing) (copy-marker pos))
                (concat (cdr ansi-color-format--trailing) bytes)))))

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
            (ansi-color-format--stash-or-trail
             start (delete-and-extract-region start (point)))))))
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
         (point-min) (point-max)
         (cons 'ansi-color-escapes ansi-color-format--derived-properties))
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

;;;; Reading the stashes

(defun ansi-color-format--sgr-sequences (bytes)
  "Return the SGR sequences among the escape sequences BYTES, concatenated."
  (let ((start 0) (parts nil))
    (while (and bytes (string-match ansi-color-format--sgr-regexp bytes start))
      (push (match-string 0 bytes) parts)
      (setq start (match-end 0)))
    (apply #'concat (nreverse parts))))

(defun ansi-color-format--other-sequences (bytes)
  "Return the escape sequences in BYTES other than SGR ones, concatenated."
  (let ((start 0) (parts nil))
    (while (and bytes
                (string-match ansi-color-format--sequence-regexp bytes start))
      (unless (string-match-p ansi-color-format--sgr-regexp
                              (match-string 0 bytes))
        (push (match-string 0 bytes) parts))
      (setq start (match-end 0)))
    (apply #'concat (nreverse parts))))

(defun ansi-color-format--link-in (bytes)
  "Return (t . URL) for the last OSC 8 sequence in BYTES, or nil if none.
URL is nil when the sequence ends a link."
  (let ((start 0) (link nil))
    (while (and bytes (string-match ansi-color-format--osc8-regexp bytes start))
      (let ((url (match-string 2 bytes)))
        (setq link (cons t (and (not (string-empty-p url)) url))
              start (match-end 0))))
    link))

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

(defun ansi-color-format--link-before (pos)
  "Return the URL of the OSC 8 hyperlink open just before POS, if any."
  (let ((link nil) (n 0) (start pos))
    (while (and (> start (point-min))
                (< n ansi-color-format-state-lookback)
                (not link))
      (setq start (previous-single-property-change
                   start 'ansi-color-escapes nil (point-min))
            n (1+ n)
            link (ansi-color-format--link-in
                  (get-text-property start 'ansi-color-escapes))))
    (cdr link)))

;;;; Deriving faces and links from the stashes

(defvar ansi-color-format-link-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'ansi-color-format-follow-link)
    (define-key map [mouse-2] #'ansi-color-format-follow-link)
    map)
  "Keymap on OSC 8 hyperlinks.")

(defun ansi-color-format-follow-link (&optional event)
  "Browse the OSC 8 hyperlink at point, or at EVENT's position."
  (interactive (list last-nonmenu-event))
  (let* ((pos (if (mouse-event-p event) (posn-point (event-end event)) (point)))
         (url (get-text-property pos 'ansi-color-url)))
    (unless url
      (user-error "No hyperlink here"))
    (browse-url url)))

(defun ansi-color-format--with-link-face (face)
  "Return FACE with `ansi-color-format-link' added to it."
  (cond ((null face) 'ansi-color-format-link)
        ((or (symbolp face) (keywordp (car face)))
         (list 'ansi-color-format-link face))
        (t (cons 'ansi-color-format-link face))))

(defun ansi-color-format--linkify (beg end url)
  "Make BEG..END follow the hyperlink URL, keeping its colors."
  (add-text-properties beg end
                       (list 'ansi-color-url url
                             'mouse-face 'highlight
                             'help-echo url
                             'follow-link t
                             'keymap ansi-color-format-link-map))
  (let ((pos beg))
    (while (< pos end)
      (let ((next (next-single-property-change pos 'font-lock-face nil end)))
        (put-text-property pos next 'font-lock-face
                           (ansi-color-format--with-link-face
                            (get-text-property pos 'font-lock-face)))
        (setq pos next)))))

(defun ansi-color-format--fontify (beg end)
  "Derive the faces and hyperlinks of BEG..END from its stashed escape sequences.
Registered with jit-lock."
  (save-match-data
    (let ((pieces (list (ansi-color-format--state-before beg)))
          (url (ansi-color-format--link-before beg))
          (links nil)
          (pos beg))
      ;; Rebuild what a terminal saw: the state in effect at BEG, then
      ;; each run's SGR sequences followed by its text.  ESC in the text
      ;; itself (an undecoded paste) is neutralized so the rendered
      ;; string keeps the region's length.  Links are tracked alongside.
      (while (< pos end)
        (let ((next (next-single-property-change pos 'ansi-color-escapes
                                                 nil end)))
          (when (ansi-color-format--run-start-p pos)
            (let ((bytes (get-text-property pos 'ansi-color-escapes)))
              (push (ansi-color-format--sgr-sequences bytes) pieces)
              (pcase (ansi-color-format--link-in bytes)
                (`(t . ,new) (setq url new)))))
          (when url
            (push (list pos next url) links))
          (push (subst-char-in-string
                 ?\e ?\C-a (buffer-substring-no-properties pos next))
                pieces)
          (setq pos next)))
      (let ((rendered (let ((ansi-color-context nil))
                        (ansi-color-apply (apply #'concat (nreverse pieces)))))
            (i 0))
        (with-silent-modifications
          (remove-list-of-text-properties
           beg end ansi-color-format--derived-properties)
          (while (< i (length rendered))
            (let ((face (get-text-property i 'font-lock-face rendered))
                  (next (next-single-property-change
                         i 'font-lock-face rendered (length rendered))))
              (when face
                (put-text-property (+ beg i) (+ beg next)
                                   'font-lock-face face))
              (setq i next)))
          (pcase-dolist (`(,from ,to ,url) links)
            (ansi-color-format--linkify from to url)))))))

;;;; SGR parameters as data

;; A state is a plist of the attribute keys below plus :fg and :bg,
;; whose values are the parameters selecting the color, e.g. "31" or
;; "38;2;255;128;0".

(defconst ansi-color-format--attributes
  '((:bold . 1) (:faint . 2) (:italic . 3) (:underline . 4) (:blink . 5)
    (:fast-blink . 6) (:inverse . 7) (:conceal . 8) (:strike . 9))
  "SGR attributes and the codes that switch them on.")

(defconst ansi-color-format--attribute-off-codes
  '((22 :bold :faint) (23 :italic) (24 :underline) (25 :blink :fast-blink)
    (27 :inverse) (28 :conceal) (29 :strike))
  "SGR codes that switch attributes off, with the attributes they clear.")

(defun ansi-color-format--sgr-apply (params state)
  "Return the state that STATE becomes after the SGR parameter string PARAMS."
  (let ((codes (mapcar #'string-to-number (split-string params ";")))
        (state (copy-sequence state)))
    (while codes
      (let ((code (pop codes)))
        (cond
         ((= code 0) (setq state nil))
         ((<= 1 code 9)
          (setq state (plist-put state (car (rassq code ansi-color-format--attributes)) t)))
         ((assq code ansi-color-format--attribute-off-codes)
          (dolist (key (cdr (assq code ansi-color-format--attribute-off-codes)))
            (setq state (plist-put state key nil))))
         ((or (<= 30 code 37) (<= 90 code 97))
          (setq state (plist-put state :fg (number-to-string code))))
         ((= code 39) (setq state (plist-put state :fg nil)))
         ((or (<= 40 code 47) (<= 100 code 107))
          (setq state (plist-put state :bg (number-to-string code))))
         ((= code 49) (setq state (plist-put state :bg nil)))
         ((memq code '(38 48))
          ;; 38;5;N selects one of 256 colors, 38;2;R;G;B a truecolor;
          ;; likewise 48 for the background.
          (let* ((count (pcase (car codes) (5 2) (2 4) (_ 0)))
                 (args (seq-take codes count)))
            (setq codes (nthcdr count codes))
            (when (and (> count 0) (= (length args) count))
              (setq state (plist-put state (if (= code 38) :fg :bg)
                                     (mapconcat #'number-to-string
                                                (cons code args) ";")))))))))
    state))

(defun ansi-color-format--sgr-render (state)
  "Return the SGR parameters producing STATE from scratch, e.g. \"0;1;31\"."
  (mapconcat #'identity
             (append (list "0")
                     (delq nil (mapcar (lambda (attr)
                                         (and (plist-get state (car attr))
                                              (number-to-string (cdr attr))))
                                       ansi-color-format--attributes))
                     (delq nil (list (plist-get state :fg)
                                     (plist-get state :bg))))
             ";"))

(defun ansi-color-format--sgr-fold (sequences &optional state)
  "Return the state after applying the SGR SEQUENCES (a string) to STATE."
  (let ((start 0))
    (while (string-match ansi-color-format--sgr-regexp sequences start)
      (setq state (ansi-color-format--sgr-apply (match-string 1 sequences) state)
            start (match-end 0))))
  state)

(defun ansi-color-format--state-at (pos)
  "Return the SGR state of the character at POS."
  (ansi-color-format--sgr-fold
   (ansi-color-format--state-before (min (1+ pos) (point-max)))))

;;;; Changing the look of a region

;; These rewrite the stashes in a region.  A region may start or end in
;; the middle of a run; the part of the run before it keeps its stash,
;; which still applies at the run's start, while the part after it
;; needs a stash of its own once the run's sequences have been
;; rewritten, since it can no longer share theirs.

(defvar ansi-color-format--last-sgr nil
  "SGR parameters last copied by `ansi-color-format-copy-sgr'.")

(defvar ansi-color-format-sgr-history nil
  "History of SGR parameters read by `ansi-color-format-apply-sgr'.")

(defun ansi-color-format--tail (end)
  "Describe the text at END before a region ending there is rewritten.
Return (RUN-START-P STASH TAIL-END), or nil at the end of the buffer."
  (and (< end (point-max))
       (list (ansi-color-format--run-start-p end)
             (get-text-property end 'ansi-color-escapes)
             (next-single-property-change end 'ansi-color-escapes nil
                                          (point-max)))))

(defun ansi-color-format--refontify-after (beg end)
  "Refontify BEG..END and the run that END falls in."
  (jit-lock-refontify beg (next-single-property-change end 'ansi-color-escapes
                                                       nil (point-max))))

(defun ansi-color-format-apply-sgr (beg end params)
  "Apply the SGR parameters PARAMS on top of the look of BEG..END.
Each run of text keeps its own look with PARAMS applied over it: \"1\"
makes the region bold without touching its colors, \"0;31\" makes it
plain red, \"39\" drops its foreground color.  The text after END
keeps its look.  Interactively, PARAMS is pre-filled with what
`ansi-color-format-copy-sgr' last copied."
  (interactive
   (list (region-beginning) (region-end)
         (read-string "SGR parameters: " ansi-color-format--last-sgr
                      'ansi-color-format-sgr-history)))
  (ansi-color-format--check)
  (save-match-data
    (let ((state (ansi-color-format--sgr-fold
                  (ansi-color-format--state-before beg)))
          (tail (ansi-color-format--tail end))
          (last nil)
          (pos beg))
      ;; STATE follows the original look run by run; each run becomes
      ;; an absolute sequence for that look plus PARAMS.
      (while (< pos end)
        (let ((next (next-single-property-change pos 'ansi-color-escapes
                                                 nil end))
              (old (get-text-property pos 'ansi-color-escapes)))
          (when (ansi-color-format--run-start-p pos)
            (setq state (ansi-color-format--sgr-fold
                         (ansi-color-format--sgr-sequences old) state)))
          (let ((new (concat "\e[" (ansi-color-format--sgr-render
                                    (ansi-color-format--sgr-apply params state))
                             "m" (ansi-color-format--other-sequences old))))
            ;; Equal neighbors share one string, so they merge into one
            ;; sequence in the file.
            (unless (equal new last)
              (setq last new))
            (put-text-property pos next 'ansi-color-escapes last))
          (setq pos next)))
      ;; Give the text after END its original look back.
      (pcase tail
        (`(,run-start-p ,old ,tail-end)
         (let* ((restore (ansi-color-format--sgr-render state))
                (needed (not (equal restore
                                    (ansi-color-format--sgr-render
                                     (ansi-color-format--sgr-apply params state)))))
                (bytes (concat "\e[" restore "m")))
           (if run-start-p
               ;; Not if the next run starts by resetting anyway.
               (when (and needed
                          (not (string-match-p
                                (concat "\\`" ansi-color-format--sgr-reset-regexp)
                                (ansi-color-format--sgr-sequences old))))
                 (ansi-color-format--stash end bytes))
             (put-text-property end tail-end 'ansi-color-escapes
                                (if needed bytes last))))))))
  (ansi-color-format--refontify-after beg end))

(defun ansi-color-format-copy-sgr ()
  "Copy the SGR parameters for the look of the text at point.
They pre-fill the next `ansi-color-format-apply-sgr'."
  (interactive)
  (ansi-color-format--check)
  (let ((params (ansi-color-format--sgr-render
                 (ansi-color-format--state-at (point)))))
    (setq ansi-color-format--last-sgr params)
    (message "Copied SGR parameters: %s"
             (propertize params 'face
                         (get-text-property (point) 'font-lock-face)))))

(defun ansi-color-format--osc8 (url)
  "Return the OSC 8 sequence starting a link to URL, or ending one if URL is nil."
  (concat "\e]8;;" url "\e\\"))

(defun ansi-color-format-set-link (beg end url)
  "Make BEG..END an OSC 8 hyperlink to URL.
With an empty URL, remove hyperlinks from the region instead.
Interactively, URL defaults to the link at point."
  (interactive
   (list (region-beginning) (region-end)
         (read-string "Link URL (empty to remove): "
                      (get-text-property (point) 'ansi-color-url))))
  (ansi-color-format--check)
  (save-match-data
    (let* ((url (and (not (string-empty-p url)) url))
           (before (ansi-color-format--link-before beg))
           (state before)
           (tail (ansi-color-format--tail end))
           (pos beg))
      ;; Drop the region's own link sequences, following the original
      ;; link state; start the new link (or end the old one) at BEG.
      (while (< pos end)
        (let* ((next (next-single-property-change pos 'ansi-color-escapes
                                                  nil end))
               (old (get-text-property pos 'ansi-color-escapes))
               (new (and (ansi-color-format--run-start-p pos) old)))
          (when new
            (pcase (ansi-color-format--link-in new)
              (`(t . ,link) (setq state link)))
            (setq new (replace-regexp-in-string
                       ansi-color-format--osc8-regexp "" new t t)))
          (when (and (= pos beg) (or url before))
            (setq new (concat (ansi-color-format--osc8 url) new)))
          (put-text-property pos next 'ansi-color-escapes
                             (and new (not (string-empty-p new)) new))
          (setq pos next)))
      ;; Give the text after END its original link state back.
      (pcase tail
        (`(,run-start-p ,old ,tail-end)
         (if run-start-p
             ;; Not if the next run sets its own.
             (unless (or (equal state url) (ansi-color-format--link-in old))
               (ansi-color-format--stash end (ansi-color-format--osc8 state)))
           (put-text-property end tail-end 'ansi-color-escapes
                              (if (equal state url)
                                  (get-text-property (1- end) 'ansi-color-escapes)
                                (ansi-color-format--osc8 state))))))))
  (ansi-color-format--refontify-after beg end))

;;;; Choosing colors

(defconst ansi-color-format--color-names
  '("black" "red" "green" "yellow" "blue" "magenta" "cyan" "white"
    "bright-black" "bright-red" "bright-green" "bright-yellow"
    "bright-blue" "bright-magenta" "bright-cyan" "bright-white")
  "The 16 ANSI colors in SGR order; each has an `ansi-color-NAME' face.")

(defun ansi-color-format--color-code (name background)
  "Return the SGR code selecting the ANSI color NAME.
For the foreground, or the background if BACKGROUND is non-nil."
  (let ((i (seq-position ansi-color-format--color-names name)))
    (+ (if (< i 8) (+ 30 i) (+ 82 i))
       (if background 10 0))))

(defun ansi-color-format--rgb-params (color background)
  "Return the SGR parameters selecting COLOR (a name or #RRGGBB) as truecolor."
  ;; Not `color-values': that gives the display's approximation of the
  ;; color, and the file should get the color that was asked for.
  (let ((rgb (or (tty-color-standard-values color)
                 (user-error "Unknown color: %s" color))))
    (format "%d;2;%d;%d;%d" (if background 48 38)
            (round (/ (nth 0 rgb) 257.0))
            (round (/ (nth 1 rgb) 257.0))
            (round (/ (nth 2 rgb) 257.0)))))

(defun ansi-color-format-read-color (prompt &optional background)
  "Read a color with PROMPT and return the SGR parameters selecting it.
Offers the 16 ANSI colors, shown in the theme's `ansi-color-*' faces
so that picking one means \"red as this terminal shows red\"; `default'
for the terminal's default color; and `custom', which reads any color
name or #RRGGBB with `read-color' and selects it as truecolor.
BACKGROUND non-nil chooses a background color."
  (let* ((swatch
          (lambda (name)
            (let ((face (intern (concat "ansi-color-" name))))
              (if background
                  (list :background (face-background face nil t))
                (list :foreground (face-foreground face nil t))))))
         (candidates
          (append (mapcar (lambda (name)
                            (propertize name 'face (funcall swatch name)))
                          ansi-color-format--color-names)
                  '("default" "custom")))
         (completion-extra-properties
          (list :affixation-function
                (lambda (names)
                  (mapcar (lambda (name)
                            (let ((face (get-text-property 0 'face name)))
                              (list name
                                    (if face
                                        (concat (propertize "██" 'face face) " ")
                                      "   ")
                                    (if face
                                        (format "  %d" (ansi-color-format--color-code
                                                        name background))
                                      ""))))
                          names))))
         (choice (completing-read prompt candidates nil t)))
    (pcase choice
      ("default" (if background "49" "39"))
      ("custom" (ansi-color-format--rgb-params
                 (read-color "Color (name or #RRGGBB): ") background))
      (_ (number-to-string (ansi-color-format--color-code choice background))))))

(defun ansi-color-format-set-foreground (beg end params)
  "Give BEG..END the foreground color selected by the SGR parameters PARAMS.
Interactively, choose it with `ansi-color-format-read-color'."
  (interactive (list (region-beginning) (region-end)
                     (ansi-color-format-read-color "Foreground: ")))
  (ansi-color-format-apply-sgr beg end params))

(defun ansi-color-format-set-background (beg end params)
  "Give BEG..END the background color selected by the SGR parameters PARAMS.
Interactively, choose it with `ansi-color-format-read-color'."
  (interactive (list (region-beginning) (region-end)
                     (ansi-color-format-read-color "Background: " t)))
  (ansi-color-format-apply-sgr beg end params))

(defun ansi-color-format-set-attributes (beg end params)
  "Switch on the attributes PARAMS (bold, italic, ...) in BEG..END.
Interactively, choose them by name; `none' switches them all off."
  (interactive
   (list (region-beginning) (region-end)
         (let ((names (mapcar (lambda (attr) (substring (symbol-name (car attr)) 1))
                              ansi-color-format--attributes)))
           (mapconcat
            (lambda (choice)
              (if (equal choice "none")
                  (mapconcat (lambda (off) (number-to-string (car off)))
                             ansi-color-format--attribute-off-codes ";")
                (number-to-string
                 (cdr (assq (intern (concat ":" choice))
                            ansi-color-format--attributes)))))
            (completing-read-multiple "Attributes: " (append names '("none"))
                                      nil t)
            ";"))))
  (ansi-color-format-apply-sgr beg end params))

;;;; The mode

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
  (ansi-color-format--check)
  (jit-lock-refontify beg (ansi-color-format--decode-region beg end)))

(provide 'ansi-color-format)

;;; ansi-color-format.el ends here
