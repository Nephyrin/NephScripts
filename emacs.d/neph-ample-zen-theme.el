
(deftheme neph-ample-zen "Neph's customizations to ample-zen")
(add-to-list 'custom-theme-load-path "~/.emacs.d/ample-zen")

(custom-theme-set-faces 'neph-ample-zen
                        '(linum ((t (:background "#222" :foreground "#555" :box nil))))
                        '(web-mode-block-face ((t (:background "#181818"))))
                        '(mode-line ((t ( :background "#111"
                                          :foreground "#999"
                                          :box (:line-width 1 :color "#000" :style nil)))))
                        '(header-line ((t ( :background "#111"
                                            :foreground "#999"
                                            :box (:line-width 1 :color "#000" :style nil)))))
                        '(mode-line-inactive ((t ( :background "#555"
                                                   :foreground "#999"
                                                   :box (:line-width 1 :color "#333" :style nil)))))
                        '(hl-line ((t :background "#302020"
                                      :box (:line-width -1 :color "#777" :style nil))))
                        '(whitespace-tab ((t  :background "#242424" )))
                        '(region ((t :background "#303030" )))
                        '(whitespace-trailing ((t :background "#764c4c")))
                        '(cursor ((t :background "#D96E26" ))))

;; linum
(custom-theme-set-variables 'neph-ample-zen
                            '(linum-format " %5d  ")
                            '(fci-rule-color "#444"))


;; CCLS rainbow faces
;; Use e.g. to quickly pull out values: (insert (apply 'color-rgb-to-hex (color-values (face-foreground 'foo))))
(custom-theme-set-faces 'neph-ample-zen
                        '(ccls-sem-function-face-0 ((t ( :foreground "#e4b023" ) )) )
                        '(ccls-sem-function-face-1 ((t ( :foreground "#917653" ) )) )
                        '(ccls-sem-function-face-2 ((t ( :foreground "#ea982b" ) )) )
                        '(ccls-sem-function-face-3 ((t ( :foreground "#e1be8e" ) )) )
                        '(ccls-sem-function-face-4 ((t ( :foreground "#d57b16" ) )) )
                        '(ccls-sem-function-face-5 ((t ( :foreground "#87641d" ) )) )
                        '(ccls-sem-function-face-6 ((t ( :foreground "#e3b852" ) )) )
                        '(ccls-sem-function-face-7 ((t ( :foreground "#a26425" ) )) )
                        '(ccls-sem-function-face-8 ((t ( :foreground "#b18826" ) )) )
                        '(ccls-sem-function-face-9 ((t ( :foreground "#d59754" ) )) )

                        '(ccls-sem-macro-face-0 ((t ( :foreground "#e69427" ) )) )
                        '(ccls-sem-macro-face-1 ((t ( :foreground "#c4363c" ) )) )
                        '(ccls-sem-macro-face-2 ((t ( :foreground "#e7a171" ) )) )
                        '(ccls-sem-macro-face-3 ((t ( :foreground "#d74e2a" ) )) )
                        '(ccls-sem-macro-face-4 ((t ( :foreground "#a57144" ) )) )
                        '(ccls-sem-macro-face-5 ((t ( :foreground "#e17932" ) )) )
                        '(ccls-sem-macro-face-6 ((t ( :foreground "#9a4930" ) )) )
                        '(ccls-sem-macro-face-7 ((t ( :foreground "#b5691d" ) )) )
                        '(ccls-sem-macro-face-8 ((t ( :foreground "#e17970" ) )) )
                        '(ccls-sem-macro-face-9 ((t ( :foreground "#ce6c48" ) )) )

                        '(ccls-sem-namespace-face-0 ((t ( :foreground "#419820" ) )) )
                        '(ccls-sem-namespace-face-1 ((t ( :foreground "#57c0a3" ) )) )
                        '(ccls-sem-namespace-face-2 ((t ( :foreground "#5dc547" ) )) )
                        '(ccls-sem-namespace-face-3 ((t ( :foreground "#35805a" ) )) )
                        '(ccls-sem-namespace-face-4 ((t ( :foreground "#82c55c" ) )) )
                        '(ccls-sem-namespace-face-5 ((t ( :foreground "#407a2e" ) )) )
                        '(ccls-sem-namespace-face-6 ((t ( :foreground "#42cb70" ) )) )
                        '(ccls-sem-namespace-face-7 ((t ( :foreground "#7db668" ) )) )
                        '(ccls-sem-namespace-face-8 ((t ( :foreground "#57be88" ) )) )
                        '(ccls-sem-namespace-face-9 ((t ( :foreground "#3d9e49" ) )) )
                        '(ccls-sem-parameter-face-0 ((t ( :foreground "#419820" ) )) )
                        '(ccls-sem-parameter-face-1 ((t ( :foreground "#57c0a3" ) )) )
                        '(ccls-sem-parameter-face-2 ((t ( :foreground "#5dc547" ) )) )
                        '(ccls-sem-parameter-face-3 ((t ( :foreground "#35805a" ) )) )
                        '(ccls-sem-parameter-face-4 ((t ( :foreground "#82c55c" ) )) )
                        '(ccls-sem-parameter-face-5 ((t ( :foreground "#407a2e" ) )) )
                        '(ccls-sem-parameter-face-6 ((t ( :foreground "#42cb70" ) )) )
                        '(ccls-sem-parameter-face-7 ((t ( :foreground "#7db668" ) )) )
                        '(ccls-sem-parameter-face-8 ((t ( :foreground "#57be88" ) )) )
                        '(ccls-sem-parameter-face-9 ((t ( :foreground "#3d9e49" ) )) )

                        '(ccls-sem-type-face-0 ((t ( :foreground "#e0aec2" ) )) )
                        '(ccls-sem-type-face-1 ((t ( :foreground "#d432ba" ) )) )
                        '(ccls-sem-type-face-2 ((t ( :foreground "#9a667e" ) )) )
                        '(ccls-sem-type-face-3 ((t ( :foreground "#e24fb5" ) )) )
                        '(ccls-sem-type-face-4 ((t ( :foreground "#9f425f" ) )) )
                        '(ccls-sem-type-face-5 ((t ( :foreground "#dc81bb" ) )) )
                        '(ccls-sem-type-face-6 ((t ( :foreground "#dd3763" ) )) )
                        '(ccls-sem-type-face-7 ((t ( :foreground "#ac3e86" ) )) )
                        '(ccls-sem-type-face-8 ((t ( :foreground "#dc798f" ) )) )
                        '(ccls-sem-type-face-9 ((t ( :foreground "#df4289" ) )) )
                        '(ccls-sem-variable-face-0 ((t ( :foreground "#419820" ) )) )
                        '(ccls-sem-variable-face-1 ((t ( :foreground "#57c0a3" ) )) )
                        '(ccls-sem-variable-face-2 ((t ( :foreground "#5dc547" ) )) )
                        '(ccls-sem-variable-face-3 ((t ( :foreground "#35805a" ) )) )
                        '(ccls-sem-variable-face-4 ((t ( :foreground "#82c55c" ) )) )
                        '(ccls-sem-variable-face-5 ((t ( :foreground "#407a2e" ) )) )
                        '(ccls-sem-variable-face-6 ((t ( :foreground "#42cb70" ) )) )
                        '(ccls-sem-variable-face-7 ((t ( :foreground "#7db668" ) )) )
                        '(ccls-sem-variable-face-8 ((t ( :foreground "#57be88" ) )) )
                        '(ccls-sem-variable-face-9 ((t ( :foreground "#3d9e49" ) )) )

                        ;; Other
                        ;; '(ccls-sem-member-face (( t () )) )
                        ;; '(ccls-sem-static-face (( t () )) )
                        ;; '(ccls-sem-static-field-face (( t () )) )
                        ;; '(ccls-sem-static-method-face (( t () )) )
                        ;; '(ccls-sem-global-variable-face (( t () )) )
                        ;; '(ccls-sem-local-face (( t () )) )
                        ;; '(ccls-sem-local-function-face (( t () )) )
                        )

;; rtags
(when (featurep 'rtags)
  (custom-theme-set-faces 'neph-ample-zen
                          '(rtags-skippedline ((t ( :background "#2C2A2A" ))))
                          '(rtags-errline ((t ( :background "#511411" ))))
                          '(rtags-fixitline ((t ( :background "#513121") )))))

;; org mode
(custom-theme-set-faces 'neph-ample-zen
                        '(org-block-begin-line
                          ((t (:foreground "#445599" :background "#15141a"))))
                        '(org-block
                          ((t (:background "#181822"))))
                        ;;'(org-code
                        ;;  ((t (:background "#181822"))))
                        '(org-meta-line
                          ((t (:foreground "#445599"))))
                        '(org-block-end-line
                          ((t (:foreground "#445599" :background "#15141a")))))
;; ediff
(require 'ediff)
(custom-theme-set-faces 'neph-ample-zen
                        '(ediff-current-diff-A ((t ( :foreground unspecified :background "#412421" ))))
                        '(ediff-current-diff-B ((t ( :foreground unspecified :background "#143111" ))))
                        '(ediff-fine-diff-A ((t ( :foreground unspecified :background "#811411" ))))
                        '(ediff-fine-diff-B ((t ( :foreground unspecified :background "#148111" )))))

;; Semantic (disabled)
                                        ; (set-face-attribute 'semantic-tag-boundary-face nil :overline "#544")

;; ECB
(custom-theme-set-faces 'neph-ample-zen
                        '(ecb-default-general-face ((t ( :foreground "#EEE" ))))
                        '(ecb-default-highlight-face ((t ( :background "#448"
                                                           :box (:line-width -1 :color "#669" )))))
                        '(ecb-source-face ((t ( :background "#484"
                                                :box (:line-width -1 :color "#696" )))))
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
