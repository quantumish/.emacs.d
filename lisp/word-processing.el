(setq ispell-program-name "aspell")

(add-to-list 'load-path "~/textlint/")
(load "textlint.el")

(load "external/langtool.el")
(setq langtool-language-tool-jar "~/Downloads/LanguageTool-5.0/languagetool-commandline.jar")

(define-key org-mode-map (kbd "C-c j") 'pandoc-jump-to-reference)

(setq default-minibuffer-frame
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

(defun make-clean-frame
  (setq default-minibuffer-frame
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

(define-minor-mode focused-mode
  (make-clean-frame)
  (writeroom-mode 1)
  (focus-mode 1)
  )

(defun word-processing-hook ()
  ;; Makes code buffers look nicer
  (olivetti-mode 1)
  (olivetti-set-width 100)
  (visual-line-mode 1)
  (flycheck-momde
  (global-set-key (kbd "C-c c") 'flyspell-auto-correct-word))

(add-hook 'org-mode-hook 'word-processing-hook)
(add-hook 'org-mode-hook
          (lambda () 
             (add-hook 'after-save-hook 'langtool-check nil 'make-it-local)))

(provide 'word-processing)
