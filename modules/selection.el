(use-package expand-region
  :bind
  ("C-=" . er/expand-region))

;; (use-package selected
;;   :init
;;   (selected-global-mode)
;;   :bind (:map selected-keymap
;; 			  ("u" . 'upcase-region)
;; 			  ("d" . 'downcase-region)
;; 			  ("w" . 'count-words-region)
;; 			  ("e" . 'er/expand-region)
;; 			  ("q" . 'selected-off)))

;; (use-package google-this
;;   :bind (:map selected-keymap
;; 			  ("g" . 'google-this-region)))

(use-package move-text 
  :bind (:map prog-mode-map 
   ("M-<down>" . move-text-line-down)
   ("M-<up>" . move-text-line-up)))

(general-def
  "C-S-d" 'py-pyment-region)
