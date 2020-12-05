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
  (global-set-key (kbd "C-c s-a") 'flyspell-auto-correct-word))

(add-hook 'org-mode-hook 'word-processing-hook)

(defun recenter-paragraph ()
  (interactive)
  (save-excursion
    (mark-paragraph)
    (let* ((start (region-beginning))
           (end (region-end))
           (count (count-lines start end))
           (min (/ (window-size) 2)))

      ;; pad buffer with empty lines if necessary
      (when (< (line-number-at-pos start) min)
        (goto-char start)
        (newline min))

      ;; go to paragraph midpoint and recenter
      (goto-char end)
      (previous-line (/ count 2))
      (recenter))))

(defvar ivy-posframe--first-show t)
(defun ivy-posframe-cleanup ()
  "Cleanup ivy's posframe."
  (setq ivy-posframe--first-show t)
  (when (posframe-workable-p)
    (posframe-hide ivy-posframe-buffer)))
(defun ivy-posframe--display (str &optional poshandler)
  "Show STR in ivy's posframe with POSHANDLER."
  (if (not (posframe-workable-p))
      (ivy-display-function-fallback str)
    (with-ivy-window
      (if (not ivy-posframe--first-show)
          (with-current-buffer ivy-posframe-buffer
            (erase-buffer)
            (insert str))
          (setq ivy-posframe--first-show nil)
          (apply #'posframe-show
                 ivy-posframe-buffer
                 :font ivy-posframe-font
                 :string str
                 :position (point)
                 :poshandler poshandler
                 :background-color (face-attribute 'ivy-posframe :background nil t)
                 :foreground-color (face-attribute 'ivy-posframe :foreground nil t)
                 :internal-border-width ivy-posframe-border-width
                 :internal-border-color (face-attribute 'ivy-posframe-border :background nil t)
                 :override-parameters ivy-posframe-parameters
                 (funcall ivy-posframe-size-function)))
      (ivy-posframe--add-prompt 'ignore)))
  (with-current-buffer ivy-posframe-buffer
    (setq-local truncate-lines ivy-truncate-lines)))

(provide 'word-processing)
