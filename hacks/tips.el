(defvar tips-idle-delay 10)

(defvar tips-idle-timer nil)

(defun tips--random-choice (items)
  (let* ((size (length items))
         (index (random size)))
    (nth index items)))

(defun tips--random-func (buf)
  (tips--random-choice
   (let ((cmds ()))
     (mapatoms (lambda (s)
                 (when (and (commandp s)
                            (command-completion-using-modes-and-keymaps-p s buf))
                   (push s cmds))))
     cmds)))

(defun tips--grab-desc (func)
  (downcase (car (split-string (documentation func) "\n"))))
 
(defun posframe-refposhandler-default (&optional frame)
  "The default posframe refposhandler??"
  (cond
   ;; EXWM environment
   (exwm--connection
    (or (ignore-errors
          ;; Need user install xwininfo.
          (let ((dims (posframe-refposhandler-xwininfo frame)))
            (cons (/ (car dims) 2) (/ (car (cdr dims)) 2))))
        ;; FIXME: maybe exwm provide some function,
        ;; Which can get top-left of emacs.
        (cons 0 1)))
   (t nil)))


(defun posframe-refposhandler-test (&optional frame)
  "Parent FRAME poshander function.
Get the position of parent frame (current frame) with the help of
xwininfo."
  (when (executable-find "xwininfo")
    (with-temp-buffer
      (let ((case-fold-search nil))
        (call-process "xwininfo" nil t nil
                      "-display" (frame-parameter frame 'display)
                      "-id"  (frame-parameter frame 'window-id))
        (goto-char (point-min))
        (search-forward "Absolute upper-left")
        (let ((x (string-to-number
                  (buffer-substring-no-properties
                   (search-forward "X: ")
                   (line-end-position))))
              (y (string-to-number
                  (buffer-substring-no-properties
                   (search-forward "Y: ")
                   (line-end-position)))))
          (cons (+ x (/ x 2)) (+ y (/ y 2))))))))

(posframe-refposhandler-test (selected-frame))

(defun tips-show-tip ()
  (let ((buf (current-buffer))
        (tipbuf (get-buffer-create "*tip*")))
    (with-current-buffer tipbuf
      (erase-buffer)
      (org-mode)
      (olivetti-mode)
      (olivetti-set-width 55)
      (let* ((tip-f (tips--random-func buf))
             (tip-d (tips--grab-desc tip-f)))
        (insert (format "\n*Tip!* Use ~%s~ to %s\n " tip-f tip-d))))
    (posframe-show "*tip*"
                   :width 60
                   :border-width 1
                   :border-color "#FFFFFF"
                   :refposhandler 'posframe-refposhandler-test)))
  

(tips-show-tip)
    
    
  (downcase (car (split-string (documentation (tips--random-func)) "\n")))

(defun tips--teardown ()
  (makunbound 'tips-candidates))

(define-minor-mode tips-mode
  "Toggle paranoia for buffer."
  :init-value nil
  :lighter " tips"
  :global t
  (if tips-mode
      (setq tips-idle-timer (run-with-idle-timer tips-idle-delay 
    (tips--teardown)))

