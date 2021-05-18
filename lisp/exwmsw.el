;;; exwmsw.el --- Monitor-independent workspaces for EXWM -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Antonio Ruiz

;; Author: Antonio Ruiz
;; URL: https://github.com/Lemonbreezes/exwm-screen-workspaces
;; Keywords: exwm workspaces
;; Package-Requires: ((exwm "0.22.1") (dash "2.16.0"))
;; Version: 0.1

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides functions for monitor-independent manipulation of workspaces in EXWM.

;;; Code:

(require 'exwm)
(require 'exwm-workspace)
(require 'exwm-randr)
(require 'dash)

;; These are always set modulo (length (exwmsw-get-workspaces-for-randr-output screen))
(defvar exwmsw-active-workspace-plist nil
  "Tracks what workspace we are currently in for a particular monitor")

;; For readability.
(defvar exwmsw-the-left-screen nil)
(defvar exwmsw-the-center-screen nil)
(defvar exwmsw-the-right-screen nil)
;; (with-no-warnings
;;   (define-obsolete-variable-alias 'exwmsw-left-screen
;;     'exwmsw-the-left-screen)
;;   (define-obsolete-variable-alias 'exwmsw-center-screen
;;     'exwmsw-the-center-screen)
;;   (define-obsolete-variable-alias 'exwmsw-right-screen
;;     'exwmsw-the-right-screen))

;; TODO Improve debugging messages
(defvar exwmsw--debug nil)

;;; Interactive functions
;;;###autoload
(defun exwmsw-swap-displayed-workspace-with-center-screen ()
  (interactive)
  (let ((current-screen (exwmsw-get-current-screen)))
	(exwmsw-swap-workspaces-displayed-on-screens (exwmsw-get-current-screen)
												 exwmsw-the-center-screen)
	(exwmsw-focus-screen current-screen)))

;;;###autoload
(defun exwmsw-swap-displayed-workspace-with-right-screen ()
  (interactive)
  (let ((current-screen (exwmsw-get-current-screen)))
	(exwmsw-swap-workspaces-displayed-on-screens (exwmsw-get-current-screen)
												 exwmsw-the-right-screen)
	(exwmsw-focus-screen current-screen)))

;;;###autoload
(defun exwmsw-switch-to-left-screen ()
  (interactive)
  (exwm-workspace-switch
   (nth (exwmsw-get-index-shown-on-screen exwmsw-the-left-screen)
		(exwmsw-get-workspaces-for-randr-output exwmsw-the-left-screen))
   t))

;;;###autoload
(defun exwmsw-switch-to-center-screen ()
  (interactive)
  (exwm-workspace-switch
   (nth (exwmsw-get-index-shown-on-screen exwmsw-the-center-screen)
		(exwmsw-get-workspaces-for-randr-output exwmsw-the-center-screen))
   t))

;;;###autoload
(defun exwmsw-switch-to-right-screen ()
  (interactive)
  (exwm-workspace-switch
   (nth (exwmsw-get-index-shown-on-screen exwmsw-the-right-screen)
		(exwmsw-get-workspaces-for-randr-output exwmsw-the-right-screen))))

;;;###autoload
(defun exwmsw-cycle-workspace-on-screen (screen)
  (interactive)
  (exwmsw-increment-screen-workspace-index screen)
  (exwm-workspace-switch (nth (exwmsw-get-index-shown-on-screen screen)
							  (exwmsw-get-workspaces-for-randr-output screen))))

(defmacro exwmsw-with-current-screen (&rest forms)
  `(let ((curr (exwmsw-get-current-screen)))
	 ,@forms
	 (unless (equal curr (exwmsw-get-current-screen))
	   (exwmsw--debug "exwmsw-with-current-screen: %s"
					  (exwmsw-get-workspaces-for-randr-output curr))
	   (when (not (and (member curr (exwmsw-screen-list))
					   (--any? (< it (length exwm-workspace--list))
							   (exwmsw-get-workspaces-for-randr-output curr))))
		 (exwmsw-create-workspace-on-screen curr))
	   (exwmsw-focus-screen curr))))

;;;###autoload
(defun exwmsw-swap-displayed-workspace-with-left-screen ()
  (interactive)
  (let ((current-screen (exwmsw-get-current-screen)))
	(exwmsw-swap-workspaces-displayed-on-screens (exwmsw-get-current-screen)
												 exwmsw-the-left-screen)
	(exwmsw-focus-screen current-screen)))

(defun exwmsw-get-current-screen ()
  (plist-get exwm-randr-workspace-monitor-plist exwm-workspace-current-index))

;;;###autoload
(defun exwmsw-cycle-workspace-on-left-screen ()
  (interactive)
  (exwmsw-with-current-screen
   (exwmsw-cycle-workspace-on-screen exwmsw-the-left-screen)))

;;;###autoload
(defun exwmsw-cycle-workspace-on-center-screen ()
  (interactive)
  (exwmsw-with-current-screen
   (exwmsw-cycle-workspace-on-screen exwmsw-the-center-screen)))

;;;###autoload
(defun exwmsw-cycle-workspace-on-right-screen ()
  (interactive)
  (exwmsw-with-current-screen
   (exwmsw-cycle-workspace-on-screen exwmsw-the-right-screen)))

;;;###autoload
(defun exwmsw-create-workspace-on-current-screen ()
  (interactive)
  (exwmsw-with-current-screen
   (exwmsw-create-workspace-on-screen (exwmsw-get-current-screen))))

;;;###autoload
(defun exwmsw-delete-workspace-on-current-screen ()
  (interactive)
  (exwmsw-with-current-screen
   (exwmsw-delete-workspace-on-screen (exwmsw-get-current-screen))))

;;; Non-interactive functions
(defun exwmsw--debug (&rest args)
  (when exwmsw--debug
	(apply #'message args)))

(defun exwmsw-create-workspace-on-screen (screen)
  "A wrapper for exwm-workspace-add makes the new workspace active
and adds it to exwm-randr-workspace-monitor-plist."
  (let ((exwm-workspace-list-change-hook nil))
	(ignore exwm-workspace-list-change-hook)
	(setq exwm-randr-workspace-monitor-plist
		  (append (list (length exwm-workspace--list)
						screen)
				  exwm-randr-workspace-monitor-plist))
	(exwm-workspace-add)
	(while (not (eq (exwmsw-get-index-shown-on-screen (exwmsw-get-current-screen))
					(-elem-index (-elem-index exwm-workspace--current exwm-workspace--list)
								 (exwmsw-get-workspaces-for-randr-output (exwmsw-get-current-screen)))))
	  (exwmsw-increment-screen-workspace-index screen))
	(unless (and (-contains? exwm-randr-workspace-monitor-plist 0)
				 (equal screen
						(nth (1- (-elem-index 0 exwm-randr-workspace-monitor-plist))
							 exwm-randr-workspace-monitor-plist)))
	  (exwm-randr-refresh))))

(defun exwmsw-delete-workspace-on-screen (screen)
  "A wrapper for exwm-workspace-delete that ensures the deleted workspace is no longer active
and is garbage-collected from exwm-randr-workspace-monitor-plist."
  (let* ((i (nth (lax-plist-get exwmsw-active-workspace-plist screen)
				 (exwmsw-get-workspaces-for-randr-output screen)))
		 (j (-elem-index i (exwmsw-get-workspaces-for-randr-output screen)))
		 (exwm-workspace-list-change-hook nil))
	(ignore exwm-workspace-list-change-hook)
	;; Ensure our index-to-be-deleted, i, is last on (exwmsw-get-workspaces-for-randr-output screen)
	(while (> j 0)
	  (exwm-workspace-swap (nth i exwm-workspace--list)
						   (nth (nth (1- j) (exwmsw-get-workspaces-for-randr-output screen))
								exwm-workspace--list))
	  (setq j (1- j))
	  (exwmsw-decrement-screen-workspace-index screen))
	(exwmsw--debug "exwmsw-delete-workspace-on-screen: %s %s %s"
				   j
				   (nth j (exwmsw-get-workspaces-for-randr-output screen))
				   (exwmsw-get-workspaces-for-randr-output screen))
	(setq i (nth j (exwmsw-get-workspaces-for-randr-output screen)))
	;; Ensure our index-to-be-deleted, i, is the last workspace in exwm-workspace--list
	(while (< i (1- (length exwm-workspace--list)))
	  (exwm-workspace-swap (nth i exwm-workspace--list)
						   (nth (1+ i) exwm-workspace--list))
	  (let ((first-screen (plist-get exwm-randr-workspace-monitor-plist i))
			(second-screen (plist-get exwm-randr-workspace-monitor-plist (1+ i))))
		(plist-put exwm-randr-workspace-monitor-plist i second-screen)
		(plist-put exwm-randr-workspace-monitor-plist (1+ i) first-screen)
		(setq i (1+ i))
		(exwmsw-decrement-screen-workspace-index second-screen)
		(exwmsw--debug "exwmsw-delete-workspace-on-screen: %s %s %s"
					   i
					   (exwmsw-get-index-shown-on-screen first-screen)
					   (exwmsw-get-index-shown-on-screen second-screen))))
	(setq exwm-randr-workspace-monitor-plist
		  (->> exwm-randr-workspace-monitor-plist
			   (-partition 2)
			   (remove (list i
							 (plist-get exwm-randr-workspace-monitor-plist
										i)))
			   (-flatten)))
	(exwm-workspace-delete i)))

(defun exwmsw-swap-workspaces-displayed-on-screens (screen1 screen2)
  "First updates exwmsw-active-workspace-plist to reflect the new active workspaces.
Then updates exwm-randr-workspace-monitor-plist."
  (when-let ((screen1-workspace-index (nth (lax-plist-get exwmsw-active-workspace-plist screen1)
										   (exwmsw-get-workspaces-for-randr-output screen1)))
			 (screen2-workspace-index (nth (lax-plist-get exwmsw-active-workspace-plist screen2)
										   (exwmsw-get-workspaces-for-randr-output screen2))))
	(when (and (plist-get exwm-randr-workspace-monitor-plist screen1-workspace-index)
			   (plist-get exwm-randr-workspace-monitor-plist screen2-workspace-index)
			   (not (eq screen1-workspace-index screen2-workspace-index)))
	  (plist-put exwm-randr-workspace-monitor-plist screen1-workspace-index screen2)
	  (exwmsw--debug "Swapping screens: %s | %s" screen1-workspace-index screen2)
	  (plist-put exwm-randr-workspace-monitor-plist screen2-workspace-index screen1)
	  (exwmsw--debug "Swapping screens: %s | %s" screen2-workspace-index screen1)
	  ;; If the workspace is an active org-noter workspace, make all org-noter workspaces
	  ;; use the new designated screens.
	  (when (bound-and-true-p exwmsw-org-noter-active-session)
		(if (equal screen1 exwmsw-org-noter-notes-screen)
			(setq exwmsw-org-noter-notes-screen screen2)
		  (when (equal screen2 exwmsw-org-noter-notes-screen)
			(setq exwmsw-org-noter-notes-screen screen1)))
		(if (equal screen1 exwmsw-org-noter-doc-screen)
			(setq exwmsw-org-noter-doc-screen screen2)
		  (when (equal screen2 exwmsw-org-noter-doc-screen)
			(setq exwmsw-org-noter-doc-screen screen1))))
	  (exwm-randr-refresh))))

(defun exwmsw-focus-screen (screen)
  (exwm-workspace-switch
   (nth (exwmsw-get-index-shown-on-screen screen)
		(exwmsw-get-workspaces-for-randr-output screen))))

(defun exwmsw-screen-session-start-pre-hook (&rest _args)
  (exwmsw-create-workspace-on-screen (exwmsw-get-current-screen)))

(defun exwmsw-screen-session-end-hook (oldfun &rest args)
  (exwmsw-with-current-screen
   (let ((curr exwm-workspace--current))
	 (apply oldfun args)
	 (setq exwm-randr-workspace-monitor-plist
		   (->> exwm-randr-workspace-monitor-plist
				(-partition 2)
				(remove (list (-elem-index curr
										   exwm-workspace--list)
							  (plist-get exwm-randr-workspace-monitor-plist
										 (-elem-index curr
													  exwm-workspace--list))))
				(-flatten)))
	 (delete-frame curr))))

(defun exwmsw-get-workspaces-for-randr-output (output)
  (--filter (equal (lax-plist-get exwm-randr-workspace-monitor-plist it) output)
			(-map #'car (-partition 2 exwm-randr-workspace-monitor-plist))))

(defun exwmsw-get-index-shown-on-screen (&optional screen)
  (unless screen (setq screen (exwmsw-get-current-screen)))
  (lax-plist-get exwmsw-active-workspace-plist screen))

(defun exwmsw-increment-screen-workspace-index (&optional screen)
  (unless screen (setq screen (exwmsw-get-current-screen)))
  (lax-plist-put exwmsw-active-workspace-plist
				 screen
				 (mod (1+ (lax-plist-get exwmsw-active-workspace-plist screen))
					  (length (exwmsw-get-workspaces-for-randr-output screen)))))

(defun exwmsw-decrement-screen-workspace-index (&optional screen)
  (unless screen (setq screen (exwmsw-get-current-screen)))
  (lax-plist-put exwmsw-active-workspace-plist
				 screen
				 (mod (1- (lax-plist-get exwmsw-active-workspace-plist screen))
					  (length (exwmsw-get-workspaces-for-randr-output screen)))))

(defun exwmsw-screen-list ()
  (-distinct (-filter #'stringp exwm-randr-workspace-monitor-plist)))

(defun exwmsw-cycle-screens (arg)
  (interactive "p")
  (exwm-workspace-switch
   (nth (exwmsw-get-index-shown-on-screen (exwmsw-cycle-screens--get-next-screen arg))
		(exwmsw-get-workspaces-for-randr-output (exwmsw-cycle-screens--get-next-screen arg)))))

(defun exwmsw-cycle-screens-backward (arg)
  (interactive "p")
  (exwm-workspace-switch
   (nth (exwmsw-get-index-shown-on-screen (exwmsw-cycle-screens--get-prev-screen arg))
		(exwmsw-get-workspaces-for-randr-output (exwmsw-cycle-screens--get-prev-screen arg)))))


(defun exwmsw-cycle-screens--get-next-screen (num)
  (nth (mod (- (-elem-index (exwmsw-get-current-screen) (exwmsw-screen-list))
			   num)
			(length (exwmsw-screen-list)))
	   (exwmsw-screen-list)))

(defun exwmsw-cycle-screens--get-prev-screen (num)
  (nth (mod (+ (-elem-index (exwmsw-get-current-screen) (exwmsw-screen-list))
			   num)
			(length (exwmsw-screen-list)))
	   (exwmsw-screen-list)))

;;; Very simple session management
(defun exwmsw-advise-screen-session (start-fn end-fn)
  "Advises start-fn and end-fn to create new workspaces and delete new workspaces respectively.
Currently there is no system to garbage collect these workspaces if they are not deleted with end-fn."
  (when start-fn (advice-add start-fn :before #'exwmsw-screen-session-start-pre-hook))
  (when end-fn (advice-add end-fn :around #'exwmsw-screen-session-end-hook)))

(defun exwmsw-unadvise-screen-session (start-fn end-fn)
  "See documentation for exwmsw-advise-screen-session"
  (when start-fn (advice-remove start-fn #'exwmsw-screen-session-start-pre-hook))
  (when end-fn (advice-remove end-fn #'exwmsw-screen-session-end-hook)))

(provide 'exwmsw)
