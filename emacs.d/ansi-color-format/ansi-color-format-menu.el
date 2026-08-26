;;; ansi-color-format-menu.el --- Transient menu for ansi-color-format-mode  -*- lexical-binding: t; no-byte-compile: t; -*-

;; Package-Requires: ((emacs "28.1") (transient "0.4"))

;;; Commentary:

;; A `transient' menu (like magit's) for `ansi-color-format-mode': the
;; 16 ANSI colors on single keys, drawn in the theme's faces, switches
;; for the attributes to apply along with a color, and the mode's
;; other commands.
;;
;; The menu works on the region that was active when it was opened.
;; Pick attributes with the switches, then a color to apply both, or
;; `a' to apply the attributes alone.
;;
;; This file is loaded on the first use of `ansi-color-format-menu' (it
;; is autoloaded from ansi-color-format.el) and deliberately not
;; byte-compiled, so that whichever `transient' is on the load-path by
;; then is the one used.

;;; Code:

(require 'transient)
(require 'ansi-color-format)

(defun ansi-color-format-menu--region ()
  "Return the region the menu was opened on, as (BEG . END)."
  (or (transient-scope)
      (user-error "Select a region first")))

(defun ansi-color-format-menu--background-p ()
  "Non-nil if the background switch is set."
  (member "--background" (transient-args 'ansi-color-format-menu)))

(defun ansi-color-format-menu--params (args &optional color)
  "Return SGR parameters for the attribute switches in ARGS, plus COLOR's."
  (mapconcat #'identity
             (append (delq nil (mapcar (lambda (attr)
                                         (and (member (concat "--" (substring (symbol-name (car attr)) 1))
                                                      args)
                                              (number-to-string (cdr attr))))
                                       ansi-color-format--attributes))
                     (and color (list color)))
             ";"))

(defun ansi-color-format-menu--apply (color)
  "Apply the chosen attributes and COLOR (SGR parameters, or nil) to the region."
  (let ((region (ansi-color-format-menu--region))
        (params (ansi-color-format-menu--params
                 (transient-args 'ansi-color-format-menu) color)))
    (when (string-empty-p params)
      (user-error "No attributes selected"))
    (ansi-color-format-apply-sgr (car region) (cdr region) params)))

(defun ansi-color-format-menu--label (name)
  "Return the ANSI color NAME drawn in the theme's face for it."
  (propertize name 'face
              (list :foreground
                    (face-foreground (intern (concat "ansi-color-" name)) nil t))))

(defmacro ansi-color-format-menu--define-color (name)
  "Define a suffix command applying the ANSI color NAME."
  `(transient-define-suffix ,(intern (concat "ansi-color-format-menu--" name)) ()
     :description (lambda () (ansi-color-format-menu--label ,name))
     (interactive)
     (ansi-color-format-menu--apply
      (number-to-string
       (ansi-color-format--color-code
        ,name (ansi-color-format-menu--background-p))))))

(defmacro ansi-color-format-menu--define-colors ()
  "Define a suffix command for each ANSI color."
  `(progn ,@(mapcar (lambda (name)
                      `(ansi-color-format-menu--define-color ,name))
                    ansi-color-format--color-names)))

(ansi-color-format-menu--define-colors)

(transient-define-suffix ansi-color-format-menu--custom ()
  :description "custom (name or #RRGGBB)"
  (interactive)
  (let ((background (ansi-color-format-menu--background-p)))
    (ansi-color-format-menu--apply
     (ansi-color-format--rgb-params
      (read-color (if background "Background: " "Foreground: "))
      background))))

(transient-define-suffix ansi-color-format-menu--default ()
  :description "default"
  (interactive)
  (ansi-color-format-menu--apply
   (if (ansi-color-format-menu--background-p) "49" "39")))

(transient-define-suffix ansi-color-format-menu--attributes ()
  :description "apply attributes alone"
  (interactive)
  (ansi-color-format-menu--apply nil))

(transient-define-suffix ansi-color-format-menu--plain ()
  :description "clear attributes"
  (interactive)
  (let ((region (ansi-color-format-menu--region)))
    (ansi-color-format-apply-sgr
     (car region) (cdr region)
     (mapconcat (lambda (off) (number-to-string (car off)))
                ansi-color-format--attribute-off-codes ";"))))

(transient-define-suffix ansi-color-format-menu--apply-sgr ()
  :description "apply SGR parameters"
  (interactive)
  (let ((region (ansi-color-format-menu--region)))
    (ansi-color-format-apply-sgr
     (car region) (cdr region)
     (read-string "SGR parameters: " ansi-color-format--last-sgr
                  'ansi-color-format-sgr-history))))

(transient-define-suffix ansi-color-format-menu--set-link ()
  :description "link region"
  (interactive)
  (let ((region (ansi-color-format-menu--region)))
    (ansi-color-format-set-link
     (car region) (cdr region)
     (read-string "Link URL (empty to remove): "
                  (get-text-property (car region) 'ansi-color-url)))))

(transient-define-suffix ansi-color-format-menu--add-links ()
  :description "link URLs in region"
  (interactive)
  (let ((region (ansi-color-format-menu--region)))
    (ansi-color-format-add-links (car region) (cdr region))))

(transient-define-suffix ansi-color-format-menu--decode ()
  :description "decode raw escapes in region"
  (interactive)
  (let ((region (ansi-color-format-menu--region)))
    (ansi-color-format-decode-region (car region) (cdr region))))

;;;###autoload (autoload 'ansi-color-format-menu "ansi-color-format-menu" nil t)
(transient-define-prefix ansi-color-format-menu ()
  "Change the look of the region with ANSI escape sequences."
  ["Attributes, applied along with a color or alone with a"
   ("-b" "bold" "--bold")
   ("-f" "faint" "--faint")
   ("-i" "italic" "--italic")
   ("-u" "underline" "--underline")
   ("-k" "blink" "--blink")
   ("-v" "inverse" "--inverse")
   ("-c" "conceal" "--conceal")
   ("-s" "strike" "--strike")
   ("-g" "background instead of foreground" "--background")]
  [["Color"
    ("k" ansi-color-format-menu--black)
    ("r" ansi-color-format-menu--red)
    ("g" ansi-color-format-menu--green)
    ("y" ansi-color-format-menu--yellow)
    ("b" ansi-color-format-menu--blue)
    ("m" ansi-color-format-menu--magenta)
    ("c" ansi-color-format-menu--cyan)
    ("w" ansi-color-format-menu--white)]
   ["Bright"
    ("K" ansi-color-format-menu--bright-black)
    ("R" ansi-color-format-menu--bright-red)
    ("G" ansi-color-format-menu--bright-green)
    ("Y" ansi-color-format-menu--bright-yellow)
    ("B" ansi-color-format-menu--bright-blue)
    ("M" ansi-color-format-menu--bright-magenta)
    ("C" ansi-color-format-menu--bright-cyan)
    ("W" ansi-color-format-menu--bright-white)]
   ["Other"
    ("#" ansi-color-format-menu--custom)
    ("d" ansi-color-format-menu--default)
    ("a" ansi-color-format-menu--attributes)
    ("n" ansi-color-format-menu--plain)
    ("p" ansi-color-format-menu--apply-sgr)
    ("P" "copy look at point" ansi-color-format-copy-sgr)]]
  ["Links and text"
   ("l" "insert link" ansi-color-format-insert-link)
   ("L" ansi-color-format-menu--set-link)
   ("A" ansi-color-format-menu--add-links)
   ("e" ansi-color-format-menu--decode)
   ("q" "quit" transient-quit-one)]
  (interactive)
  (ansi-color-format--check)
  (transient-setup 'ansi-color-format-menu nil nil
                   :scope (and (use-region-p)
                               (cons (region-beginning) (region-end)))))

(provide 'ansi-color-format-menu)

;;; ansi-color-format-menu.el ends here
