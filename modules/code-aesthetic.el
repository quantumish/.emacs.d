(use-package hl-todo
  :init
  (global-hl-todo-mode)
  (setq hl-todo-keyword-faces
		'(("TODO"   . "#99bb66")
		  ("FIXME"  . "#ff6655")
		  ("DEBUG"  . "#a9a1e1")
		  ("HACK"   . "#6c78dd")
		  ("NOTE"   . "#44b9b1")))
  ;; We already have todos in Org Mode!
  (add-hook 'org-mode-hook (lambda () (hl-todo-mode -1)))

  :bind (:map hl-todo-mode-map
  ("C-c p" . hl-todo-previous)
  ("C-c n" . hl-todo-next)
  ("C-c o" . hl-todo-occur)
  ("C-c i" . hl-todo-insert)))
