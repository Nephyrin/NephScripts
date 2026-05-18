;;; ansi-color-overlay-mode.el --- Render ANSI escapes as colors via overlays  -*- lexical-binding: t; -*-

;; Author: you
;; Keywords: faces, terminals
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; A buffer-local minor mode that visualizes ANSI color escape
;; sequences using overlays, leaving the underlying buffer text (and
;; therefore the file on disk) completely untouched.
;;
;; Under the hood the buffer text is copied into a temporary buffer
;; where `ansi-color-apply-on-region' is run normally; the resulting
;; face information is then projected back onto the original buffer
;; as `face' overlays, while each ANSI control sequence in the
;; original buffer is hidden by an `invisible' overlay.
;;
;; In addition, the bytes that make up each ANSI control sequence are
;; given a punctuation `syntax-table' text property.  This way
;; word-boundary regexps such as the one used by `goto-address-mode'
;; match correctly across the hidden escape sequences, and word
;; movement, `isearch', etc. don't treat e.g. the trailing `m' of
;; `\e[34;1m' as part of the URL that follows it.
;;
;; Usage:
;;
;;   M-x ansi-color-overlay-mode

;;; Code:

(require 'ansi-color)

(defgroup ansi-color-overlay nil
  "Visualize ANSI escape sequences via overlays."
  :group 'ansi-color)

(defvar-local ansi-color-overlay--overlays nil
  "List of overlays currently managed by `ansi-color-overlay-mode'.")

(defun ansi-color-overlay--make-overlay (beg end &rest props)
  "Create a tagged overlay from BEG to END with PROPS and record it."
  (let ((ov (make-overlay beg end nil t nil)))
    (overlay-put ov 'ansi-color-overlay t)
    (while props
      (overlay-put ov (pop props) (pop props)))
    (push ov ansi-color-overlay--overlays)
    ov))

(defun ansi-color-overlay--clear (beg end)
  "Remove overlays created by this mode within BEG..END."
  (dolist (ov (overlays-in beg end))
    (when (overlay-get ov 'ansi-color-overlay)
      (delete-overlay ov)
      (setq ansi-color-overlay--overlays
            (delq ov ansi-color-overlay--overlays)))))

(defun ansi-color-overlay--clear-all ()
  "Remove every overlay and syntax override created by this mode."
  (mapc #'delete-overlay ansi-color-overlay--overlays)
  (setq ansi-color-overlay--overlays nil)
  (ansi-color-overlay--unpropertize-region (point-min) (point-max)))

(defun ansi-color-overlay--unpropertize-region (beg end)
  "Remove `syntax-table' overrides set by this mode within BEG..END.
Only positions previously marked with the
`ansi-color-overlay-syntax' property are touched, so syntax
properties installed by the major mode are left alone."
  (with-silent-modifications
    (let ((pos beg))
      (while (and pos (< pos end))
        (setq pos (text-property-not-all
                   pos end 'ansi-color-overlay-syntax nil))
        (when pos
          (let ((next (or (next-single-property-change
                           pos 'ansi-color-overlay-syntax nil end)
                          end)))
            (remove-list-of-text-properties
             pos next '(syntax-table ansi-color-overlay-syntax))
            (setq pos next)))))))

(defun ansi-color-overlay--propertize-region (beg end)
  "Give ANSI escape sequences in BEG..END punctuation syntax.
The bytes of each control sequence get the `syntax-table' text
property set to punctuation, plus an `ansi-color-overlay-syntax'
marker so we can find and remove our own overrides later."
  (save-match-data
    (with-silent-modifications
      ;; Drop any previous marks in this region first so that stale
      ;; properties (e.g. from an edit that broke up an escape
      ;; sequence) are not left behind.
      (ansi-color-overlay--unpropertize-region beg end)
      (save-excursion
        (goto-char beg)
        (while (re-search-forward ansi-color-control-seq-regexp end t)
          (add-text-properties
           (match-beginning 0) (match-end 0)
           `(syntax-table ,(string-to-syntax ".")
             ansi-color-overlay-syntax t)))))))

(defun ansi-color-overlay--fontify-region (beg end)
  "Project ANSI colors from BEG..END onto overlays.
The buffer text itself is left untouched: ANSI control sequences
are hidden by `invisible' overlays, and the surrounding text is
colored with `face' overlays whose values are computed by
running `ansi-color-apply-on-region' in a scratch buffer.  The
escape sequences are also given punctuation syntax so that
word-boundary regexps work across them."
  (save-match-data
    (ansi-color-overlay--clear beg end)
    (ansi-color-overlay--propertize-region beg end)
    (let* ((raw (buffer-substring-no-properties beg end))
           (orig (current-buffer)))
      (with-temp-buffer
        (insert raw)
        (let ((ansi-color-apply-face-function
               (lambda (b e face)
                 (when face
                   (put-text-property b e 'face face)))))
          (ansi-color-apply-on-region (point-min) (point-max)))
        (let ((tmp (current-buffer))
              (clean-pos (point-min)))
          (with-current-buffer orig
            (save-excursion
              (goto-char beg)
              (while (< (point) end)
                (cond
                 ;; An ANSI control sequence: hide it.
                 ((looking-at ansi-color-control-seq-regexp)
                  (ansi-color-overlay--make-overlay
                   (match-beginning 0) (match-end 0)
                   'invisible t)
                  (goto-char (match-end 0)))
                 ;; Plain text: paint a run sharing one face.
                 (t
                  (let* ((face (with-current-buffer tmp
                                 (get-text-property clean-pos 'face)))
                         (next-change
                          (with-current-buffer tmp
                            (next-single-property-change
                             clean-pos 'face nil (point-max))))
                         (run-length (- next-change clean-pos))
                         (run-start (point))
                         (taken 0))
                    (while (and (< taken run-length) (< (point) end))
                      (cond
                       ((looking-at ansi-color-control-seq-regexp)
                        (ansi-color-overlay--make-overlay
                         (match-beginning 0) (match-end 0)
                         'invisible t)
                        (goto-char (match-end 0)))
                       (t
                        (forward-char 1)
                        (setq taken (1+ taken)))))
                    (when face
                      (ansi-color-overlay--make-overlay
                       run-start (point) 'face face))
                    (setq clean-pos next-change))))))))))))

(defun ansi-color-overlay-refresh ()
  "Recompute ANSI color overlays for the entire buffer."
  (interactive)
  (ansi-color-overlay--clear-all)
  (ansi-color-overlay--fontify-region (point-min) (point-max)))

(defun ansi-color-overlay--after-change (beg end _len)
  "Re-fontify the lines spanning BEG..END after a change."
  (save-match-data
    (let ((rb (save-excursion (goto-char beg) (line-beginning-position)))
          (re (save-excursion (goto-char end)   (line-end-position))))
      (ansi-color-overlay--fontify-region rb re))))

;;;###autoload
(define-minor-mode ansi-color-overlay-mode
  "Toggle display of ANSI color escapes via overlays.

When enabled, ANSI control sequences in the buffer are hidden
with invisible overlays, and the visible text is colored
according to the escape sequences in effect, computed by
`ansi-color-apply-on-region' in a temporary buffer.  The escape
sequences are also given punctuation syntax via the
`syntax-table' text property, so word-boundary regexps (like the
one used by `goto-address-mode' for URL detection) match
correctly across them.

The buffer contents (and the underlying file) are never
modified.  Use \\[ansi-color-overlay-refresh] to recompute the
overlays for the entire buffer."
  :lighter " AnsiOv"
  (if ansi-color-overlay-mode
      (progn
        (ansi-color-overlay--fontify-region (point-min) (point-max))
        (add-hook 'after-change-functions
                  #'ansi-color-overlay--after-change nil t))
    (remove-hook 'after-change-functions
                 #'ansi-color-overlay--after-change t)
    (ansi-color-overlay--clear-all)))

(provide 'ansi-color-overlay-mode)

;;; ansi-color-overlay-mode.el ends here
