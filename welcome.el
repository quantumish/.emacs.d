(setq welcome-messages '("While any text editor can save your files, only Emacs can save your soul."
                         "There's an Emacs package for that."
                         "Eight Megabytes And Constantly Swapping"
                         "Escape Meta Alt Control Super"
                         "M-x butterfly"
                         "The thermonuclear word processor."
                         "The best OS!"))

(defun ar/show-welcome-buffer ()
  "Show *Welcome* buffer."
  (with-current-buffer (get-buffer-create "*Welcome*")
    (setq truncate-lines t)
    (let* ((buffer-read-only)
           (image-path "~/Downloads/splash 1(2).svg")
           (image (create-image image-path 'svg nil :height 350))
           (size (image-size image))
           (height (cdr size))
           (width (car size))
           (top-margin (floor (/ (- (window-height) height) 2)))
           (left-margin (floor (/ (- (window-width) width) 2)))
           (prompt (nth (random (length welcome-messages)) welcome-messages)))
      (erase-buffer)
      (setq mode-line-format nil)
      (goto-char (point-min))
      (insert (make-string top-margin ?\n ))
      (insert (make-string left-margin ?\ ))
      (insert-image image)
      (insert "\n\n\n")
      ;; (insert (make-string (floor (/ (- (window-width) (string-width prompt)) 2)) ?\ ))
      ;; ;; (put-text-property 0 (length prompt) 'face 'font-lock-string-face prompt)
      ;; (insert prompt)
      )
    (setq cursor-type nil)
    (read-only-mode +1)
    (switch-to-buffer (current-buffer))
    (local-set-key (kbd "q") 'kill-this-buffer)))


(ar/show-welcome-buffer)
