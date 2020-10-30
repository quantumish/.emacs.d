(setq ispell-program-name "aspell")

(define-key org-mode-map (kbd "C-c j") 'pandoc-jump-to-reference)

(defun make-clean-frame ()
  (interactive)
  (setq default-minibuf0fer-frame
        (make-frame
         '((name . "minibuffer")
           (width . 0)
           (height . 0)
           (minibuffer . only)
           (top . 0)
           (left . 0)
           )))
  (setq new-frame
        (make-frame
         '((name . "editor")
           (width . 80)
         (height . 30)
         (minibuffer . nil)
         (top . 50)
         (left . 0)
         )))
  )

(defun word-processing-hook ()
  ;; Makes code buffers look nicer
  (olivetti-mode 1)
  (olivetti-set-width 100)
  (visual-line-mode 1)
  (global-set-key (kbd "C-c C-a") 'flyspell-auto-correct-word))

(add-hook 'org-mode-hook 'word-processing-hook)

(provide 'word-processing)
