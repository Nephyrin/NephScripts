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


(provide 'neph-lib)
