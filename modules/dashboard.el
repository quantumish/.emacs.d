(use-package dashboard
  :init
  (require 'dashboard-ls)
  (require 'dashboard-hackernews)
  (setq dashboard-center-content t)
  ; (setq dashboard-disable-shortcuts nil)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-projects-backend 'projectile)
  (setq dashboard-footer-messages '("The One True Editor!"
					"Protocol 3: Protect the Pilot"
					"All systems nominal."
					"Democracy... is non negotiable."
					"It's my way or... hell, it's my way!"
					"Death is a preferable alternative to communism."
					"Make life rue the day it though it could give Richard Stallman lemons!"
					"Vi-Vi-Vi, the editor of the beast."
					"Happy hacking!"
					"While any text editor can save your files, only Emacs can save you soul."
					"There's an Emacs package for that."
					"M-x butterfly"
					""))
  (setq dashboard-items '((recents  . 3)
						  (projects . 3)
						  (agenda . 5)
						  ))
  (setq dashboard-startup-banner 'official)
  (setq dashboard-page-separator "\n\n")
  (dashboard-setup-startup-hook)
  :hook
  (dashboard-mode . hide-mode-line-mode)
  (dashboard-mode . turn-off-solaire-mode))

