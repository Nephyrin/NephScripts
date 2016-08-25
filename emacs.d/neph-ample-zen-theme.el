
(deftheme neph-ample-zen "Neph's customizations to ample-zen")
(add-to-list 'custom-theme-load-path "~/.emacs.d/ample-zen")

(custom-theme-set-faces 'neph-ample-zen
                        '(linum ((t (:background "#222" :foreground "#555" :box nil))))
                        '(mode-line ((t ( :background "#111"
                                          :foreground "#999"
                                          :box '(:line-width 1 :color "#000" :style nil)))))
                        '(header-line ((t ( :background "#111"
                                            :foreground "#999"
                                            :box '(:line-width 1 :color "#000" :style nil)))))
                        '(mode-line-inactive ((t ( :background "#555"
                                                   :foreground "#999"
                                                   :box '(:line-width 1 :color "#333" :style nil)))))
                        '(hl-line ((t ( :background "#302020"
                                        :box '(:line-width -1 :color "#777" :style nil)))))
                        '(whitespace-tab ((t ( :background "#242424" ))))
                        '(region ((t ( :background "#303030" )))))

;; linum
(custom-theme-set-variables 'neph-ample-zen '(linum-format " %5d  "))


;; rtags
(when (featurep 'rtags)
  (custom-theme-set-faces 'neph-ample-zen
                          '(rtags-skippedline ((t ( :background "#323030" ))))
                          '(rtags-errline ((t ( :background "#511411" ))))
                          '(rtags-fixitline ((t ( :background "#513121") )))))

;; ediff
(require 'ediff)
(custom-theme-set-faces 'neph-ample-zen
                        '(ediff-current-diff-A ((t ( :background "#412421" ))))
                        '(ediff-current-diff-B ((t ( :background "#244121" ))))
                        '(ediff-fine-diff-A ((t ( :background "#811411" ))))
                        '(ediff-fine-diff-B ((t ( :background "#148111" )))))

;; Semantic (disabled)
                                        ; (set-face-attribute 'semantic-tag-boundary-face nil :overline "#544")

;; ECB
(custom-theme-set-faces 'neph-ample-zen
                        '(ecb-default-highlight-face ((t ( :background "#448"
                                                           :box '(:line-width -1 :color "#669" )))))
                        '(ecb-source-face ((t ( :background "#484"
                                                :box '(:line-width -1 :color "#696" )))))
                        '(ecb-source-in-directories-buffer-face ((t ( :foreground "#EEE" )))))

;; Company
(when (featurep 'company)
  (custom-theme-set-faces 'neph-ample-zen
                          '(company-tooltip ((t ( :background "#333" :foreground "white" ))))
                          '(company-tooltip-selection ((t ( :background "#555" ))))
                          '(company-tooltip-common-selection ((t ( :foreground "#955" ))))
                          '(company-tooltip-common ((t ( :foreground "#944" ))))
                          '(company-tooltip-annotation ((t ( :foreground "#A99" ))))
                          '(company-scrollbar-fg ((t ( :background "#222" ))))
                          '(company-scrollbar-bg ((t ( :background "#555" )))))
  (custom-theme-set-variables 'neph-ample-zen
                              '(company-tooltip-margin 2)))

(provide-theme 'neph-ample-zen)
