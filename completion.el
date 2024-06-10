(use-package vertico
      ;; Special recipe to load extensions conveniently
      :straight (vertico :files (:defaults "extensions/*")
					 :includes (vertico-indexed
								vertico-flat
								vertico-grid
								vertico-mouse
								vertico-quick
								vertico-buffer
								vertico-repeat
								vertico-reverse
								vertico-directory
								vertico-multiform
								vertico-unobtrusive
								))
      :general
      (:keymaps 'vertico-map
			"<tab>" #'vertico-insert    ; Choose selected candidate
			"<escape>" #'minibuffer-keyboard-quit ; Close minibuffer
			;; NOTE 2022-02-05: Cycle through candidate groups
			"C-M-n" #'vertico-next-group
			"C-M-p" #'vertico-previous-group)
      :custom
      (vertico-count 13)                    ; Number of candidates to display
      (vertico-resize t)
      (vertico-cycle nil) ; Go from last to first candidate and first to last (cycle)?
      :init
      (vertico-mode))


(use-package marginalia
      :init (marginalia-mode))

(use-package all-the-icons-completion
      :after (marginalia all-the-icons vertico)	  
      :init
      (all-the-icons-completion-marginalia-setup)
      (all-the-icons-completion-mode))
