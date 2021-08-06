(use-package hl-todo
  :init
  (global-hl-todo-mode)
  ;; HACK Hardcoded values are bad!
  (setq hl-todo-keyword-faces
		'(("TODO"   . "#29558F")
		  ("FIXME"  . "#447BD0")
		  ("DEBUG"  . "#4776A7")
		  ("HACK"   . "#3367A2")
		  ("NOTE"   . "#a2c5de")))
  ;; We already have todos in Org Mode!
  (add-hook 'org-mode-hook (lambda () (hl-todo-mode -1)))
  (set-face-attribute 'hl-todo nil :italic t)
  :bind (:map hl-todo-mode-map
  ("C-c t p" . hl-todo-previous)
  ("C-c t n" . hl-todo-next)
  ("C-c t i" . hl-todo-insert)))
