(defun process-exit-code-and-output (program &rest args)
  "Run PROGRAM with ARGS and return the exit code and output in a list."
  (with-temp-buffer
	(list (apply 'call-process program nil (current-buffer) nil args)
		  (buffer-string))))

(defun get-distro-or-os ()
  "Return the Linux distribution or OS Emacs is running on."
  (if (eq system-type 'darwin)
	  "Darwin")
  (if (eq system-type 'gnu/linux)
	  (progn
		(if (file-exists-p "/etc/os-release")
			(substring (shell-command-to-string "source /etc/os-release && echo $NAME") 0 -1)
		  (substring (car (cdr (process-exit-code-and-output "uname" "-o"))) 0 -1)))))

(use-package system-packages
  :init
  (let (os-name (get-distro-or-os))
	(if (string= os-name "Arch Linux")
		(progn
		  (add-to-list 'system-packages-supported-package-managers
					   '(yay .
							 ((default-sudo . nil)
							  (install . "yay -S")
							  (uninstall . "yay -Rs")
							  (update . "yay -Syu")
							  (log . "cat /var/log/pacman.log")
							  (change-log . "yay -Qc")
							  (clean-cache . "yay -Sc")
							  (get-info . "yay -Qi")
							  (get-info-remote . "yay -Si")
							  (list-files-provided-by . "yay -Ql")
							  (owning-file . "yay -Qo")
							  (verify-all-dependencies . "yay -Dk")
							  (remove-orphaned . "yay -Rsn $(pacman -Qtdq)")
							  (list-installed-packages . "yay -Qe")
							  (list-installed-packages-all . "yay -Q")
							  (noconfirm . "--noconfirm"))))
		  (setq system-packages-package-manager 'yay)))
	(if (string= os-name "Debian GNU/Linux")
		(progn
		  (setq system-packages-use-sudo t)
		  (setq system-packages-package-manager 'apt)))
	(if (string= os-name "Android")
		(progn
		  (add-to-list 'system-packages-supported-package-managers
					   '(pkg .
							 ((default-sudo . nil)
							  (install . "pkg install")
							  (uninstall . "pkg uninstall")
							  (update . "pkg upgrade")
							  (clean-cache . "pkg autoclean")
							  (get-info . "pkg show")
							  (list-files-provided-by . "pkg files")
							  (list-installed-packages . "pkg list-installed"))))
		  (setq system-packages-package-manager 'pkg)))
	(if (string= os-name "Darwin")
		(setq system-packages-package-manager 'brew)))
	(setq system-packages-noconfirm t))

(use-package use-package-ensure-system-package)
