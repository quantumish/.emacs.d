(use-package expand-region
  :bind
  ("C-=" . er/expand-region))

;; (use-package selected
;;   :init
;;   (selected-global-mode)
;;   :bind (:map selected-keymap
;;			  ("u" . 'upcase-region)
;;			  ("d" . 'downcase-region)
;;			  ("w" . 'count-words-region)
;;			  ("e" . 'er/expand-region)
;;			  ("q" . 'selected-off)))

;; (use-package google-this
;;   :bind (:map selected-keymap
;;			  ("g" . 'google-this-region)))

(use-package drag-stuff
  :bind (:map prog-mode-map
   ("M-<down>" . drag-stuff-down)
   ("M-<up>" . drag-stuff-up)))

(general-def
  "C-S-d" 'py-pyment-region)
