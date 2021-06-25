(use-package company
  :init
  (setq company-idle-delay 0)
  (setq company-tooltip-maximum-width 40)
  :hook
  (prog-mode . company-mode)
  :bind (:map company-active-map
			  ("<ret>" . 'company-complete-selection)))

(use-package company-quickhelp
  :after company
  :init (company-quickhelp-mode))

(use-package company-quickhelp-terminal
  :after company-quickhelp)

(use-package company-prescient
  :after company prescient
  :init
  (setq-default history-length 1000)
  (setq-default prescient-history-length 1000)
  :init (company-prescient-mode))
