
(use-package dashboard
  :init
  (setq dashboard-center-content t)
  ; (setq dashboard-disable-shortcuts nil)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-projects-backend 'projectile)
  (setq dashboard-footer-messages '("The One True Editor!"
					"Protocol 3: Protect the Pilot"
					"All systems nominal."
					"Democracy... is non negotiable."
					"It's my way or... hell, it's my way!"
					"Make life rue the day it though it could give Richard Stallman lemons!"
					"Vi-Vi-Vi, the editor of the beast."
					"Happy hacking!"
					"While any text editor can save your files, only Emacs can save your soul."
					"There's an Emacs package for that."
					"Rip and tear, until it is done!"
					"It's time to kick ass and chew bubblegum... and I'm all outta gum."
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

