;; -------------------------------------------------------------------
;; An extension of the echo area to display static messages
;; Copyright 2020 Nicolas P. Rougier
;; -------------------------------------------------------------------
;; This file is not part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>
;; -------------------------------------------------------------------
(require 'subr-x)

(defun enhanced-message (orig-fun &rest args)
  "This enhanced message displays a regular message in the echo area
   and adds a specific text on the right part of the echo area. This
   is to be used as an advice."
  (let* ((right (propertize
                 ;; Hack: The first space is a thin space, not a regular space
                 (format-time-string "   %A %d %B %Y, %H:%M  ")
                 'face '(:height 0.85
                         :overline t
                         :family "Roboto Condensed"
                         :inherit face-salient)))
         ;; -10 is a crude approximation for compensating the size of displayed
         ;; text because "Roboto Condensed" is not monospaced. We should rather
         ;; compute the actual width in pixel of the displayed text and compute
         ;; the proper amount of spaces needed in the left part. This is beyond
         ;; my current elisp/emacs knowledge.
         (width (- (frame-width) (length right) -12))
         (msg (if (car args) (apply 'format-message args) ""))
         ;; Hack: The space for the split is a thin space, not a regular space
         ;; This way, we get rid of the added part if present (unless an actual
         ;; message uses a thin space.
         (msg (car (split-string msg " ")))
         (msg (string-trim msg))
         (left (truncate-string-to-width msg width nil nil "…"))
         (full (format (format "%%-%ds %%s" width) left right)))
    (if (active-minibuffer-window)
        ;; Regular log and display when minibuffer is active
        (apply orig-fun args)
      (progn
        ;; Log actual message without echo
        (if message-log-max
            (let ((inhibit-message t)) (apply orig-fun (list msg))))
        ;; Display enhanced message without log
        (let ((message-truncate-lines t) (message-log-max nil))
          (apply orig-fun (list full)))
        ;; Set current message explicitely
        (setq current-message msg)))))

(advice-add 'message :around #'enhanced-message)
(add-hook 'post-command-hook (lambda () (let ((message-log-max nil))
                                          (message (current-message)))))

(provide 'echo-message)
