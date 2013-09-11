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

(provide 'neph-modeline-util)
