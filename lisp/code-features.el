;; ;; packages
;; (when (>= emacs-major-version 24)
;;   (require 'package)
;;   (package-initialize)
;;   (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
;;   )

(setq company-idle-delay 0)
(setq company-minimum-prefix-length 1)
(company-tng-configure-default)
(let ((map company-active-map))
    (define-key map (kbd "<tab>") 'company-complete-selection)
    (define-key map (kbd "RET") 'nil))
(setq company-tooltip-limit 1) ;; Minimum is 3 :(

;; Add yasnippet support for all company backends
;; https://github.com/syl20bnr/spacemacs/pull/179
(defvar company-mode/enable-yas t "Enable yasnippet for all backends.")

(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas) (and (listp backend)    (member 'company-yasnippet backend)))
  backend
(append (if (consp backend) backend (list backend))
        '(:with company-yasnippet))))

(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

(provide 'code-features)
;;; code-features.el ends here
