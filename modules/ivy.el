(use-package prescient
  :init (setq prescient-persist-mode t))

(use-package ivy
  :init
  (use-package counsel :config (counsel-mode 1))
  (use-package swiper :defer t)
  (ivy-mode 1)
  :bind
  (("C-s"     . swiper-isearch)
   ("M-x"     . counsel-M-x)
   ("C-x C-f" . counsel-find-file)))

(use-package ivy-rich
  :after ivy
  :init (ivy-rich-mode))

(use-package all-the-icons-ivy-rich
  :after ivy-rich counsel
  :init (all-the-icons-ivy-rich-mode))

(use-package ivy-prescient
  :after ivy prescient
  :init (ivy-prescient-mode))

(use-package marginalia
  :config (marginalia-mode))
