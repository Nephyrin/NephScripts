; Based off of / stolen from powerline-hud from powerline
(defun neph/make-xpm (name color1 color2 data)
  "Return an XPM image with NAME using COLOR1 for enabled and COLOR2 for disabled bits specified in DATA."
  (when window-system
    (create-image
     (concat
      (format "/* XPM */
static char * %s[] = {
\"%i %i 2 1\",
\". c %s\",
\"  c %s\",
"
              (downcase (replace-regexp-in-string " " "_" name))
              (length (car data))
              (length data)
              color1
              color2)
      (let ((len  (length data))
            (idx  0))
        (apply 'concat
               (mapcar #'(lambda (dl)
                           (setq idx (+ idx 1))
                           (concat
                            "\""
                            (concat
                             (mapcar #'(lambda (d)
                                         (if (eq d 0)
                                             (string-to-char " ")
                                           (string-to-char ".")))
                                     dl))
                            (if (eq idx len)
                                "\"};"
                              "\",\n")))
                       data))))
     'xpm t :ascent 'center)))

(defun neph/percent-xpm
  (height pmax pmin winend winstart width color1 color2)
  "Generate percentage xpm of HEIGHT for PMAX to PMIN given WINEND and WINSTART with WIDTH and COLOR1 and COLOR2."
  (let* ((height- (1- height))
         (fillstart (round (* height- (/ (float winstart) (float pmax)))))
         (fillend (round (* height- (/ (float winend) (float pmax)))))
         (data nil)
         (i 0))
    (while (< i height)
      (setq data (cons
                  (if (and (<= fillstart i)
                           (<= i fillend))
                      (append (make-list width 1))
                    (append (make-list width 0)))
                  data))
      (setq i (+ i 1)))
    (neph/make-xpm "percent" color1 color2 (reverse data))))

(defun neph-hud (color1 color2 height width)
  "Return an XPM of relative buffer location using COLOR1 and COLOR2 of optional WIDTH."
  (let ((height (* (frame-char-height) height))
        pmax
        pmin
        (ws (window-start))
        (we (window-end)))
    (save-restriction
      (widen)
      (setq pmax (point-max))
      (setq pmin (point-min)))
    (neph/percent-xpm height pmax pmin we ws (* (frame-char-width) width) color1 color2)))

(defvar neph/minibuffer-selected-window-list '())

(defun neph/minibuffer-selected-window ()
  "Return the selected window when entereing the minibuffer."
  (when neph/minibuffer-selected-window-list
    (car neph/minibuffer-selected-window-list)))

(defun neph/minibuffer-setup ()
  "Save the `minibuffer-selected-window' to `neph/minibuffer-selected-window'."
  (push (minibuffer-selected-window) neph/minibuffer-selected-window-list))

(add-hook 'minibuffer-setup-hook 'neph/minibuffer-setup)

(defun neph/minibuffer-exit ()
  "Set `neph/minibuffer-selected-window' to nil."
  (pop neph/minibuffer-selected-window-list))

(add-hook 'minibuffer-exit-hook 'neph/minibuffer-exit)

(defun neph-modeline-active ()
  "Return whether the current window is active."
  (or (eq (frame-selected-window)
          (selected-window))
      (and (minibuffer-window-active-p
            (frame-selected-window))
           (eq (neph/minibuffer-selected-window)
               (selected-window)))))

;;
;; Mode line
;;

(defun neph-fill-to (reserve)
  `(:eval (propertize " " 'display '(space :align-to (- right-margin
                                                        ,reserve)))))
(defun neph-modeline-hud (height width)
  (propertize " " 'display (neph-hud "#0C0C0C" "#222222" height width)
              'face 'neph-modeline-hud))

;; TODO set this only when buffer path changes, rather than per frame
(defun neph-cache-projectile-info ()
  "Cache projectile-project-root and project-name once for spammy non-critical things like modeline"
  (when (and (featurep 'projectile) (projectile-project-name))
    (setq-local neph-cached-projectile-project-root (projectile-project-root))
    (setq-local neph-cached-projectile-project-name (projectile-project-name))))

(defface neph-modeline-hud
  '((t (:inherit mode-line-face)))
  "Neph modeline hud face")
(defface neph-modeline-id
  '((t (:inherit mode-line-face
        :foreground "#DD5"
        :weight bold)))
  "Neph modeline buffer id face")
(defface neph-modeline-mode
  '((t (:inherit mode-line-face
        :foreground "#464")))
  "Neph modeline mode face")
(defface neph-modeline-misc
  '((t (:inherit mode-line-face
        :height 75
        :foreground "#444"
        :width condensed)))
  "Neph modeline minor info face")
(defface neph-modeline-path
  '((t (:inherit mode-line-face
        :foreground "#DFDDDD")))
  "Neph modeline path face")
(defface neph-modeline-path-replacement
  '((t (:inherit neph-modeline-path
        :foreground "#7F7777")))
  "Neph modeline path face for replacements made by neph-modeline-shortpaths")
(defface neph-modeline-id-inactive
  '((t (:inherit neph-modeline-id
        :foreground "#CC9")))
  "Neph modeline buffer id inactive face")
(defface neph-modeline-stat-readonly
  '((t (:inherit mode-line-face
        :foreground "#6666EE"
        :box (:line-width 2))))
  "Neph modeline readonly status face")
(defface neph-modeline-stat-modified
  '((t (:inherit mode-line-face
        :foreground "#FF5555"
        :weight bold)))
  "Neph modeline modified status face")
(defface neph-modeline-stat-clean
  '((t (:inherit mode-line-face
        :foreground "#555")))
  "Neph modeline clean status face")
(defface neph-modeline-which-func
  '((t (:inherit mode-line-face
        :foreground "#666"
        :height 90)))
  "Neph modeline which-func-mode face")

(defcustom neph-sticky-header nil "Event-updated portion of the header line")
(defcustom neph-sticky-header-valid-range nil "Range of characters for which the current sticky header is valid")
(defun neph-rtags-current-container-hook (containerName)
  (let* ((container       (rtags-current-container))
         (startLineCell   (when container (assoc 'startLine container)))
         (endLineCell     (when container (assoc 'endLine container)))
         (startColumnCell (when container (assoc 'startColumn container)))
         (endColumnCell   (when container (assoc 'endColumn container)))
         (needed          (and container startLineCell endLineCell
                               startColumnCell endColumnCell))
         (startLine       (when needed (cdr startLineCell)))
         (endLine         (when needed (cdr endLineCell)))
         (startColumn     (when needed (cdr startColumnCell)))
         (endColumn       (when needed (cdr endColumnCell)))
         (curLine         (when needed (line-number-at-pos)))
         (lineOffset      (when needed (- startLine curLine)))
         (endLineOffset   (when needed (- endLine curLine)))
         (charStart       (when needed (line-beginning-position (+ lineOffset 1))))
         (charEnd         (when needed (line-end-position (+ lineOffset 1))))
         (regionStart     (when needed (+ (line-beginning-position (+ lineOffset 1))
                                          startColumn)))
         (regionEnd       (when needed (+ (line-end-position (+ endLineOffset 1))
                                          endColumn)))
         (lineStr         (when (and needed charStart charEnd)
                            (replace-regexp-in-string "^[ \t\n]+" ""
                                                      (buffer-substring charStart charEnd)))))
    (setq neph-sticky-header
           (if lineStr lineStr (propertize "- unknown -" 'face 'neph-modeline-misc)))

    (setq neph-sticky-header-valid-range
          (if (and regionStart regionEnd)
              (cons regionStart regionEnd)
            nil))
    (force-mode-line-update)))

(provide 'neph-modeline-util)
