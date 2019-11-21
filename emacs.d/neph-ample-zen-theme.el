
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
;; Current color-identifiers generated colors, for reference:
;;   #aac69924d6db
;;   #8edce123bde0
;;   #99257000ffff
;;   #c249adb6bf58
;;   #ffffeb6c7001
;;   #c249bf58adb7
;;   #ffff70007001
;;   #7001c248ffff
;;   #ffff7000eb6d
;;   #9925fffe7001
(custom-theme-set-faces 'neph-ample-zen

;; New set:
;; #ffb2b2 / hue: 0.0, lum: 0.85, sat: 1
;; #efa8a8 / hue: 0.0, lum: 0.8, sat: 0.7
;; #e5b2b2 / hue: 0.0, lum: 0.8, sat: 0.5
;; #ffe0b2 / hue: 0.1, lum: 0.85, sat: 1
;; #efd3a8 / hue: 0.1, lum: 0.8, sat: 0.7
;; #e5d1b2 / hue: 0.1, lum: 0.8, sat: 0.5
;; #efffb2 / hue: 0.2, lum: 0.85, sat: 1
;; #e1efa8 / hue: 0.2, lum: 0.8, sat: 0.7
;; #dbe5b2 / hue: 0.2, lum: 0.8, sat: 0.5
;; #c1ffb2 / hue: 0.3, lum: 0.85, sat: 1
;; #b6efa8 / hue: 0.3, lum: 0.8, sat: 0.7
;; #bce5b2 / hue: 0.3, lum: 0.8, sat: 0.5
;; #b2ffd1 / hue: 0.4, lum: 0.85, sat: 1
;; #a8efc4 / hue: 0.4, lum: 0.8, sat: 0.7
;; #b2e5c6 / hue: 0.4, lum: 0.8, sat: 0.5
;; #b2ffff / hue: 0.5, lum: 0.85, sat: 1
;; #a8efef / hue: 0.5, lum: 0.8, sat: 0.7
;; #b2e5e5 / hue: 0.5, lum: 0.8, sat: 0.5
;; #b2d1ff / hue: 0.6, lum: 0.85, sat: 1
;; #a8c4ef / hue: 0.6, lum: 0.8, sat: 0.7
;; #b2c6e5 / hue: 0.6, lum: 0.8, sat: 0.5
;; #c1b2ff / hue: 0.7, lum: 0.85, sat: 1
;; #b6a8ef / hue: 0.7, lum: 0.8, sat: 0.7
;; #bcb2e5 / hue: 0.7, lum: 0.8, sat: 0.5
;; #efb2ff / hue: 0.8, lum: 0.85, sat: 1
;; #e1a8ef / hue: 0.8, lum: 0.8, sat: 0.7
;; #dbb2e5 / hue: 0.8, lum: 0.8, sat: 0.5
;; #ffb2e0 / hue: 0.9, lum: 0.85, sat: 1
;; #efa8d3 / hue: 0.9, lum: 0.8, sat: 0.7
;; #e5b2d1 / hue: 0.9, lum: 0.8, sat: 0.5

;; Old set:
;; #ff7f7f / hue: 0.0, lum: 0.75, sat: 1
;; #e87c7c / hue: 0.0, lum: 0.7, sat: 0.7
;; #d88c8c / hue: 0.0, lum: 0.7, sat: 0.5
;; #ffcc7f / hue: 0.1, lum: 0.75, sat: 1
;; #e8bd7c / hue: 0.1, lum: 0.7, sat: 0.7
;; #d8ba8c / hue: 0.1, lum: 0.7, sat: 0.5
;; #e5ff7f / hue: 0.2, lum: 0.75, sat: 1
;; #d2e87c / hue: 0.2, lum: 0.7, sat: 0.7
;; #c9d88c / hue: 0.2, lum: 0.7, sat: 0.5
;; #99ff7f / hue: 0.3, lum: 0.75, sat: 1
;; #92e87c / hue: 0.3, lum: 0.7, sat: 0.7
;; #9bd88c / hue: 0.3, lum: 0.7, sat: 0.5
;; #7fffb2 / hue: 0.4, lum: 0.75, sat: 1
;; #7ce8a7 / hue: 0.4, lum: 0.7, sat: 0.7
;; #8cd8aa / hue: 0.4, lum: 0.7, sat: 0.5
;; #7ffeff / hue: 0.5, lum: 0.75, sat: 1
;; #7ce8e8 / hue: 0.5, lum: 0.7, sat: 0.7
;; #8cd8d8 / hue: 0.5, lum: 0.7, sat: 0.5
;; #7fb2ff / hue: 0.6, lum: 0.75, sat: 1
;; #7ca7e8 / hue: 0.6, lum: 0.7, sat: 0.7
;; #8caad8 / hue: 0.6, lum: 0.7, sat: 0.5
;; #987fff / hue: 0.7, lum: 0.75, sat: 1
;; #927ce8 / hue: 0.7, lum: 0.7, sat: 0.7
;; #9b8cd8 / hue: 0.7, lum: 0.7, sat: 0.5
;; #e57fff / hue: 0.8, lum: 0.75, sat: 1
;; #d27ce8 / hue: 0.8, lum: 0.7, sat: 0.7
;; #c98cd8 / hue: 0.8, lum: 0.7, sat: 0.5
;; #ff7fcb / hue: 0.9, lum: 0.75, sat: 1
;; #e87cbd / hue: 0.9, lum: 0.7, sat: 0.7
;; #d88cba / hue: 0.9, lum: 0.7, sat: 0.5

                        '(ccls-skipped-range-face ((t (:background "#29292c"))))

                        ;; Functions -- pretty unique, trying to stay mostly cool themed.
                        '(ccls-sem-function-face-0 ((t (:foreground "#bce5b2"))))
                        '(ccls-sem-function-face-1 ((t (:foreground "#b2ffd1"))))
                        '(ccls-sem-function-face-2 ((t (:foreground "#a8efc4"))))
                        '(ccls-sem-function-face-3 ((t (:foreground "#b2e5c6"))))
                        '(ccls-sem-function-face-4 ((t (:foreground "#b2ffff"))))
                        '(ccls-sem-function-face-5 ((t (:foreground "#a8efef"))))
                        '(ccls-sem-function-face-6 ((t (:foreground "#b2e5e5"))))
                        '(ccls-sem-function-face-7 ((t (:foreground "#b2d1ff"))))
                        '(ccls-sem-function-face-8 ((t (:foreground "#a8c4ef"))))
                        '(ccls-sem-function-face-9 ((t (:foreground "#b2c6e5"))))

                        ;; Macros -- Pretty unique, reddish themed
                        '(ccls-sem-macro-face-0 ((t ( :foreground "#f6a427" ) )) )
                        '(ccls-sem-macro-face-1 ((t ( :foreground "#d4463c" ) )) )
                        '(ccls-sem-macro-face-2 ((t ( :foreground "#f7b171" ) )) )
                        '(ccls-sem-macro-face-3 ((t ( :foreground "#e75e2a" ) )) )
                        '(ccls-sem-macro-face-4 ((t ( :foreground "#b58144" ) )) )
                        '(ccls-sem-macro-face-5 ((t ( :foreground "#f18932" ) )) )
                        '(ccls-sem-macro-face-6 ((t ( :foreground "#aa5930" ) )) )
                        '(ccls-sem-macro-face-7 ((t ( :foreground "#c5791d" ) )) )
                        '(ccls-sem-macro-face-8 ((t ( :foreground "#f18970" ) )) )
                        '(ccls-sem-macro-face-9 ((t ( :foreground "#de9c98" ) )) )

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

                        ;; Parameters, pretty unique, lighter
                        '(ccls-sem-parameter-face-0 ((t (:foreground "#e87c7c"))))
                        '(ccls-sem-parameter-face-1 ((t (:foreground "#d88c8c"))))
                        '(ccls-sem-parameter-face-2 ((t (:foreground "#ffcc7f"))))
                        '(ccls-sem-parameter-face-3 ((t (:foreground "#e8bd7c"))))
                        '(ccls-sem-parameter-face-4 ((t (:foreground "#d8ba8c"))))
                        '(ccls-sem-parameter-face-5 ((t (:foreground "#e5ff7f"))))
                        '(ccls-sem-parameter-face-6 ((t (:foreground "#d2e87c"))))
                        '(ccls-sem-parameter-face-7 ((t (:foreground "#c9d88c"))))
                        '(ccls-sem-parameter-face-8 ((t (:foreground "#99ff7f"))))
                        '(ccls-sem-parameter-face-9 ((t (:foreground "#92e87c"))))

                        '(ccls-sem-type-face-0 ((t (:foreground "#987fff"))))
                        '(ccls-sem-type-face-1 ((t (:foreground "#927ce8"))))
                        '(ccls-sem-type-face-2 ((t (:foreground "#9b8cd8"))))
                        '(ccls-sem-type-face-3 ((t (:foreground "#e57fff"))))
                        '(ccls-sem-type-face-4 ((t (:foreground "#d27ce8"))))
                        '(ccls-sem-type-face-5 ((t (:foreground "#c98cd8"))))
                        '(ccls-sem-type-face-6 ((t (:foreground "#ff7fcb"))))
                        '(ccls-sem-type-face-7 ((t (:foreground "#e87cbd"))))
                        '(ccls-sem-type-face-8 ((t (:foreground "#d88cba"))))
                        '(ccls-sem-type-face-9 ((t (:foreground "#ff7f7f"))))

                        ;; Variables -- most unique, all over the place.
                        '(ccls-sem-variable-face-0 ((t (:foreground "#aac69924d6db"))))
                        '(ccls-sem-variable-face-1 ((t (:foreground "#8edce123bde0"))))
                        '(ccls-sem-variable-face-2 ((t (:foreground "#99257000ffff"))))
                        '(ccls-sem-variable-face-3 ((t (:foreground "#c249adb6bf58"))))
                        '(ccls-sem-variable-face-4 ((t (:foreground "#ffffeb6c7001"))))
                        '(ccls-sem-variable-face-5 ((t (:foreground "#c249bf58adb7"))))
                        '(ccls-sem-variable-face-6 ((t (:foreground "#ffff70007001"))))
                        '(ccls-sem-variable-face-7 ((t (:foreground "#7001c248ffff"))))
                        '(ccls-sem-variable-face-8 ((t (:foreground "#ffff7000eb6d"))))
                        '(ccls-sem-variable-face-9 ((t (:foreground "#9925fffe7001"))))

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
