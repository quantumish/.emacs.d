;; -*- lexical-binding: t; -*-
;; make garbage collector less invasive

(setq gc-cons-threshold  most-positive-fixnum
     gc-cons-percentage 0.6)

(setq package-enable-at-startup nil)


(defvar default-gc-cons-threshold 16777216 ; 16mb
  "Default GC cons threshold to use.")
   
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(left-fringe . 0) default-frame-alist)
(push '(right-fringe . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(font . "UW Ttyp0-10") default-frame-alist)
(push '(mouse-color . "#f6e5db") default-frame-alist)
(push '(bottom-divider-width . 20) default-frame-alist)
(push '(right-divider-width . 20) default-frame-alist)

(setq frame-inhibit-implied-resize t)

;; (require 'exwm)

;; (load (expand-file-name "~/.emacs.d/elpaca/repos/exwm/exwm.el") nil t t)

(setq warning-minimum-level :error)

(setq default-file-name-handler-alist file-name-handler-alist
      file-name-handler-alist nil)

(setq initial-scratch-message nil)
(setq inhibit-startup-screen t)
(defun display-startup-echo-area-message ())

(add-hook 'emacs-startup-hook
          (lambda (&rest _)
            (setq gc-cons-threshold default-gc-cons-threshold
                  gc-cons-percentage 0.1
                  file-name-handler-alist default-file-name-handler-alist)

            ;; delete no longer necessary startup variable
            (makunbound 'default-file-name-handler-alist)))

