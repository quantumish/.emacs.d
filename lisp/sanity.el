;; Some sane defaults
;; Copyright 2020 Nicolas P. Rougier
;;
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

;; Modified by David Freifeld

(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq initial-major-mode 'org-mode)
(setq-default indent-tabs-mode nil)
(setq pop-up-windows nil)
(tool-bar-mode 0)
(tooltip-mode  0)
(scroll-bar-mode 0)
(global-auto-revert-mode t)
(server-start)

(defun custom/kill-this-buffer ()
  (interactive) (kill-buffer (current-buffer)))
(global-set-key (kbd "C-x k") 'custom/kill-this-buffer)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(save-place-mode 1)

(setq mac-command-modifier 'super mac-option-modifier 'meta)

(ido-mode t)
(setq ido-enable-flex-matching t)

(setq-default tab-width 4)
(setq-default c-basic-offset 4)

(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-x C-x") 'execute-extended-command)
(global-set-key (kbd "C-x C-z") nil)

(global-visual-line-mode 1)

(global-set-key "\C-t" #'transpose-lines)
(define-key ctl-x-map "\C-t" #'transpose-chars)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
