;;; gdb-ansi-color.el --- Fontify ANSI codes mangled by gdb-mi -*- lexical-binding: t; -*-

(require 'ansi-color)
(require 'cl-lib)

(defvar-local gdb-ansi-color--inhibit nil
  "Re-entrancy guard for the after-change hook.")

(defvar gdb-ansi-color--active-buffers nil
  "Live buffers where `gdb-ansi-color-mode' is enabled.
Used to scope the `display-warning' advice so it's only installed
while the mode is in use somewhere.")

(defconst gdb-ansi-color--regexp
  "e\\(\\[[0-9;]*m\\)"
  "Printable remainder of an ANSI SGR sequence whose ESC was eaten by gdb-mi.
The leading `e' stands in for the missing ESC.")

(defun gdb-ansi-color--after-change (beg end _len)
  "Restore ESC bytes in the changed region, then run `ansi-color' on it."
  (unless gdb-ansi-color--inhibit
    (let ((gdb-ansi-color--inhibit t)
          (inhibit-modification-hooks t)
          (inhibit-read-only t))
      (save-excursion
        (let ((rbeg (max (point-min)
                         (save-excursion (goto-char beg)
                                         (line-beginning-position 0))))
              (rend (min (point-max)
                         (save-excursion (goto-char end)
                                         (line-end-position 2)))))
          ;; Strip readline non-printing markers (SOH/STX) that gef and
          ;; similar wrap around their color codes.
          (goto-char rbeg)
          (while (re-search-forward "[\001\002]" rend t)
            (replace-match "" t t)
            (setq rend (1- rend)))
          ;; Restore ESC bytes eaten by gdb-mi.
          (goto-char rbeg)
          (while (re-search-forward gdb-ansi-color--regexp rend t)
            (replace-match (concat (string ?\e) (match-string 1)) t t))
          (ansi-color-apply-on-region rbeg rend))))))

(defun gdb-ansi-color--any-buffer-active-p ()
  "Non-nil if any live buffer has `gdb-ansi-color-mode' enabled.
Prunes dead buffers from the registry as a side effect."
  (setq gdb-ansi-color--active-buffers
        (cl-remove-if-not #'buffer-live-p gdb-ansi-color--active-buffers))
  (consp gdb-ansi-color--active-buffers))

(defun gdb-ansi-color--display-warning-advice (orig type message &rest args)
  "Silence gdb-mi's complaint about the `\\e' escape it doesn't grok.
Only suppresses while at least one buffer has `gdb-ansi-color-mode' on."
  (unless (and (eq type 'emacs)
               (stringp message)
               (string-match-p "\\`Unrecognized escape char: e\\'" message)
               (gdb-ansi-color--any-buffer-active-p))
    (apply orig type message args)))

(define-minor-mode gdb-ansi-color-mode
  "Fontify ANSI color codes mangled by Emacs' gdb-mi.

gdb-mi parses console-stream output as a C string but doesn't
recognize `\\e' as ESC, so it drops the backslash and leaves
`e[31m' etc. visible. This mode watches buffer changes, restores
the missing ESC, hands the result to `ansi-color', strips
readline's SOH/STX prompt-width markers, and silences gdb-mi's
accompanying `Unrecognized escape char' warning."
  :lighter " ansi"
  (cond
   (gdb-ansi-color-mode
    (add-hook 'after-change-functions
              #'gdb-ansi-color--after-change nil t)
    (cl-pushnew (current-buffer) gdb-ansi-color--active-buffers)
    ;; `advice-add' is idempotent, so unconditional add is fine.
    (advice-add 'display-warning :around
                #'gdb-ansi-color--display-warning-advice))
   (t
    (remove-hook 'after-change-functions
                 #'gdb-ansi-color--after-change t)
    (setq gdb-ansi-color--active-buffers
          (delq (current-buffer) gdb-ansi-color--active-buffers))
    (unless (gdb-ansi-color--any-buffer-active-p)
      (advice-remove 'display-warning
                     #'gdb-ansi-color--display-warning-advice)))))

(provide 'gdb-ansi-color)
