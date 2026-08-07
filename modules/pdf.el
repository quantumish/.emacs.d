;; -*- lexical-binding: t; -*-
;;; Configuration for PDF related functionality

(use-package pdf-tools
  :init (pdf-loader-install)
  :config
  (defun my/pdf-fit ()
    "Fit the window to the PDF's width and the PDF to the window's height.
     From https://emacs.stackexchange.com/questions/60537/fit-window-to-pdf-upon-loading"
    (interactive)
    (let ((W (- (car (pdf-view-image-size)) (window-pixel-width))))
      (window-resize (get-buffer-window) (+ 10 W) t nil t)
      (pdf-view-fit-height-to-window)))
  :hook
  ((pdf-view-mode . pdf-view-midnight-minor-mode)
   (pdf-view-mode . hide-mode-line-mode)
   (pdf-view-after-change-page . my/pdf-fit)))
