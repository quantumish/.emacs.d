(use-package system-packages
  :init
  (if (eq system-type 'gnu/linux)
	  (progn 
		(defvar distro-name
		  (substring (shell-command-to-string "source /etc/os-release && echo $NAME") 0 -1))
		(if (string= distro-name "Arch Linux")
x			(add-to-list 'system-packages-supported-package-managers
						 '(yay .
							   ((default-sudo . nil)
								(install . "yay -S")
								(uninstall . "yay -Rs")
								(log . "cat /var/log/pacman.log")
								(change-log . "yay -Qc")
								(get-info . "yay -Qi")
								(get-info-remote . "yay -Si")
								(list-files-provided-by . "yay -Ql")
								(owning-file . "yay -Qo")
								(verify-all-dependencies . "yay -Dk")
								(remove-orphaned . "yay -Rsn $(pacman -Qtdq)")
								(list-installed-packages . "yay -Qe")
								(list-installed-packages-all . "yay -Q")
								(noconfirm . "--noconfirm")))))
		(if (string= distro-name "Debian GNU/Linux")
			(setq system-packages-package-manager 'apt))))
  (if (eq system-type 'darwin)
	  (setq system-packages-package-manager 'brew))
											  	
  (setq system-packages-noconfirm t)
  (setq system-packages-package-manager 'yay)
  (setq system-packages-use-sudo nil))

(use-package use-package-ensure-system-package)

