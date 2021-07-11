(use-package flyspell
  :ensure nil
  :ensure-system-package aspell
  :hook (((text-mode outline-mode latex-mode org-mode markdown-mode) . flyspell-mode))
  :custom ;; Maybe use :config?
  (flyspell-issue-message-flag nil)
  (ispell-program-name "aspell")
  (ispell-extra-args
   '("--sug-mode=ultra" "--lang=en_US"))
  :config
  (use-package flyspell-correct-ivy
    :after ivy
    :bind
    (:map flyspell-mode-map
          ([remap flyspell-correct-word-before-point] . flyspell-correct-wrapper)
          ("C-." . flyspell-correct-wrapper))
    :custom (flyspell-correct-interface #'flyspell-correct-ivy)))

(use-package writegood-mode)

(use-package writeroom-mode)

;; TODO flycheck-vale?
;; TODO Better word count solution
