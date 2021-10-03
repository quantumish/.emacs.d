
;;; Org Roam

(use-package org-roam
   :init
   (setq org-roam-directory "~/sync/notes")
   (setq org-roam-v2-ack t)
   :bind
   ("C-c n i" . org-roam-node-insert)
   ("C-c n f" . org-roam-node-find)
   ("C-c d s" . org-roam-db-sync))

(use-package org-roam-ui
  :straight
  (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :after org-roam
  ;; :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t)
  (setq org-roam-ui-follow t)
  (setq org-roam-ui-update-on-save t)
  (setq org-roam-ui-open-on-start t))

(use-package ivy-bibtex
  :config
  (setq ivy-re-builders-alist
      '((ivy-bibtex . ivy--regex-ignore-order)
        (t . ivy--regex-plus))))

(use-package org-ref)

(use-package org-roam-bibtex) 

;; TODO - Sketchy packages
;; (use-package vulpea  
;;   :hook ((org-roam-db-autosync-mode . vulpea-db-autosync-enable)))

;; (use-package delve
;;   :straight
;;   (:host github :repo "publicimageltd/delve" :branch "main" :files ("*.el"))
;;   :config
;;   (use-package delve-minor-mode
;;     :config
;;     (add-hook 'org-mode-hook #'delve-minor-mode-maybe-activate)))

;; TODO replace with xeft
(use-package deft
  :init
  (setq deft-directory org-roam-directory)
  (defun my/deft-parse-title (file contents)
     "Parse the given FILE and CONTENTS and determine the title.
  If `deft-use-filename-as-title' is nil, the title is taken to
  be the first non-empty line of the FILE.  Else the base name of the FILE is
  used as title."
     (let ((begin (string-match "^#\\+[tT][iI][tT][lL][eE]: .*$" contents)))
       (if begin
	   (string-trim (substring contents begin (match-end 0)) "#\\+[tT][iI][tT][lL][eE]: *" "[\n\t ]+")
	 (deft-base-filename file))))
   
   (advice-add 'deft-parse-title :override #'my/deft-parse-title)
   
   (setq deft-strip-summary-regexp
	 (concat "\\("
		 "[\n\t]" ;; blank
		 "\\|^#\\+[[:alpha:]_]+:.*$" ;; org-mode metadata
		 "\\|^:PROPERTIES:\n\\(.+\n\\)+:END:\n"
		 "\\)")))

(setq org-roam-capture-templates
      '(("d" "default" plain
         (function org-roam-capture--get-point)
         "%?"
         :file-name "${slug}"
         :head "#+TITLE: ${title}\n#+CREATED: %U\n\n"
	 :if-new
         :unnarrowed t)))

(use-package mathpix.el
  :straight (:host github :repo "jethrokuan/mathpix.el")
  :custom ((mathpix-app-id "app-id")
           (mathpix-app-key "app-key"))
  :bind
  ("C-x m" . mathpix-screenshot))

