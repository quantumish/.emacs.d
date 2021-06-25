(defun remove-nth-element (nth list)
  (if (zeropth nth) (cdr list)
	(let ((last (nthcdr (1- nth) list)))
	  (setcdr last (cddr last))
	  list)))

(use-package perspective
  :init
  (persp-mode)
  (persp-turn-off-modestring)
  (defvar persp-list-true '())
  (defvar persp-list-initial '())
  (defvar persp-last-true)

  (defun persp-switch-true ()
	(interactive)
	(ivy-read "Perspective to switch to: " persp-list-true
		:preselect (car persp-list-true)
		:action (lambda (name)
				  (let ((frame) (i 0))
					(if (member name persp-list-true)
						(progn
						  (let ((original-frame (selected-frame)))
							(select-frame (nth (cl-position name persp-list-true :test 'equal) persp-list-initial))
							(dolist (frame (visible-frame-list))
							  (persp-switch (concat name (number-to-string i)))
							  (other-frame 1)
							  (setq i (+ i 1)))
							(select-frame original-frame)))
					  (progn
						(add-to-list 'persp-list-true name)
						(dolist (frame (visible-frame-list))
 						  (if (= i 0) (setq persp-list-initial (append persp-list-initial (list frame))))
						  (persp-switch (concat name (number-to-string i)))
						  (other-frame 1)
						  (setq i (+ i 1)))))
				  (setq persp-last-true name)))))

  (defun persp-remove-true ()
	(interactive)
	(ivy-read "Perspective to remove: " persp-list-true
		:preselect (car persp-list-true)
		:action (lambda (name)
				  (dotimes (i (length (visible-frame-list)))
					(persp-kill (concat name (number-to-string i)))
					(setq persp-list-true (remove name persp-list-true))
					(remove-nth-element (cl-position name persp-list-true :test 'equal) persp-list-initial)
					(cl-position name persp-list-true :test 'equal)
					(other-frame 1))
				  (setq persp-last-true "???"))))

  (setq global-mode-string '("" persp-last-true)))
