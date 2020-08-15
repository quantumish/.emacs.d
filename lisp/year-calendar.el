;; Created by Nicolas Rougier
;; Modified by David Freifeld

(require 'calendar)
(require 'holidays)
(require 'material-colors)

(setq org-agenda-start-on-weekday 1)
(setq calendar-mark-holidays-flag t)
(setq material-shade deep-orange)

(defface calendar-face-level-1 nil "")
(defface calendar-face-level-2 nil "")
(defface calendar-face-level-3 nil "")
(defface calendar-face-level-4 nil "")
(defface calendar-face-level-5 nil "")
(defface calendar-face-level-6 nil "")
(defface calendar-face-level-7 nil "")
(defface calendar-face-level-8 nil "")
(defface calendar-face-level-9 nil "")
(defface calendar-face-vacation nil "")
(defface calendar-face-weekend nil "")


(set-face-attribute 'calendar-face-level-1 nil
                    :background (material-color material-shade "L50"))
(set-face-attribute 'calendar-face-level-2 nil
                    :background (material-color material-shade "L100"))
(set-face-attribute 'calendar-face-level-3 nil
                    :background (material-color material-shade "L200"))
(set-face-attribute 'calendar-face-level-4 nil
                    :background (material-color material-shade "L300"))
(set-face-attribute 'calendar-face-level-5 nil
                    :inherit 'face-strong
                    :foreground "white"
                    :background (material-color material-shade "L400"))
(set-face-attribute 'calendar-face-level-6 nil
                    :inherit 'face-strong
                    :foreground "white"
                    :background (material-color material-shade "L500"))
(set-face-attribute 'calendar-face-level-7 nil
                    :inherit 'face-strong
                    :foreground "white"
                    :background (material-color material-shade "L600"))
(set-face-attribute 'calendar-face-level-8 nil
                    :inherit 'face-strong
                    :foreground "white"
                    :background (material-color material-shade "L700"))
(set-face-attribute 'calendar-face-level-9 nil
                    :inherit 'face-strong
                    :foreground "white"
                    :background (material-color material-shade "L800"))
(set-face-attribute 'calendar-face-vacation nil
                    :inherit 'face-strong
                    :background (material-color purple "L50")
                    :foreground (material-color blue-grey "L900"))
(set-face-attribute 'calendar-face-weekend nil
                    :inherit 'default
                    :background "white"
                    :foreground (material-color blue-grey "L300"))



(defadvice calendar-generate-month
  (after highlight-weekend-days (month year indent) activate)
  "Highlight weekend days"
  (dotimes (i 31)
    (let* ((date (list month (1+ i) year))
           (file "~/Dropbox/org/schedule.org")
           (entries (org-agenda-get-day-entries file date))
           (count (length entries)))
       
      (cond ((= count 0) (if (and (not (equal date (calendar-current-date)))
                                  (or (= (calendar-day-of-week date) 0)
                                      (= (calendar-day-of-week date) 6)))
                             (calendar-mark-visible-date date 'calendar-face-weekend)))
            ((= count 1) (calendar-mark-visible-date date 'calendar-face-level-1))
            ((= count 2) (calendar-mark-visible-date date 'calendar-face-level-2))
            ((= count 3) (calendar-mark-visible-date date 'calendar-face-level-3))
            ((= count 4) (calendar-mark-visible-date date 'calendar-face-level-4))
            ((= count 5) (calendar-mark-visible-date date 'calendar-face-level-5))
            ((= count 6) (calendar-mark-visible-date date 'calendar-face-level-6))
            ((= count 7) (calendar-mark-visible-date date 'calendar-face-level-7))
            ((= count 8) (calendar-mark-visible-date date 'calendar-face-level-8))
            (t           (calendar-mark-visible-date date 'calendar-face-level-9)))
      )))

(defun calendar-cursor-to-visible-date (date)
  "Move the cursor to date (if on the screen)"
  (let* ((month        (- (calendar-extract-month date) 1))
         (day          (- (calendar-extract-day date) 1))
         (year         (calendar-extract-year date))
         (month-start  (calendar-day-of-week (list (+ month 1) 1 year)))
         (month-start  (% (+ month-start 6) 7))
         (month-width  25)
         (month-height 9)
         (month-col    (* (% month 3) month-width))
         (month-row    (+ (* (/ month 3) month-height) 3))
         (day-col      (* (% (+ day month-start) 7) 3))
         (day-row      (/ (+ day month-start) 7))
         (row          (+ month-row day-row))
         (col          (+ month-col day-col 1 1)))
    (goto-line      row)
    (move-to-column col)
    ))

(defun new-calendar-frame (char-width char-height)
  ""
  (interactive)
  (select-frame (make-frame))
  (set-frame-width (selected-frame) char-width)
  (set-frame-height (selected-frame) char-height)
  (set-frame-position (selected-frame)
                      (/ (- (display-pixel-width)  (frame-outer-width))  2)
                      (/ (- (display-pixel-height) (frame-outer-height)) 2))
  (x-focus-frame nil)
  (switch-to-buffer (generate-new-buffer "*Year Calendar*"))
  (local-set-key (kbd "C-x C-c") 'kill-and-close)
  (setq header-line-format nil)
  (setq mode-line-format nil))

(defun year-calendar (&optional year)
  ""
  (interactive)
  (new-calendar-frame 74 36)
  (let* ((month 0)
         (year (if year year (string-to-number (format-time-string "%Y" )))))
    (switch-to-buffer (get-buffer-create calendar-buffer))
    (when (not (eq major-mode 'calendar-mode))
      (calendar-mode))
    (setq buffer-read-only nil)
    (erase-buffer)
    (dotimes (j 4)
      (dotimes (i 3)
        (calendar-generate-month
          (setq month (+ month 1))
          year
          (+ 1 (* 25 i))))
      (goto-char (point-max))
      (insert (make-string (- 10 (count-lines (point-min) (point-max))) ?\n))
      (widen)
      (goto-char (point-max))
      (narrow-to-region (point-max) (point-max)))
    (widen)
    (goto-char (point-min))
    (setq buffer-read-only t)
    (setq header-line-format nil)
    (setq mode-line-format nil)
    (let ((displayed-month  2) (displayed-year 2020))  (calendar-mark-holidays))
    (let ((displayed-month  5) (displayed-year 2020))  (calendar-mark-holidays))
    (let ((displayed-month  8) (displayed-year 2020))  (calendar-mark-holidays))
    (let ((displayed-month 11) (displayed-year 2020))  (calendar-mark-holidays))
    (calendar-cursor-to-visible-date (calendar-current-date))))
