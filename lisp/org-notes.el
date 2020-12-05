(use-package org-roam
      :ensure t
      :hook
      (after-init . org-roam-mode)
      :custom
      (org-roam-directory "~/Dropbox/notes/")
      :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n b" . org-roam-db-build-cache))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))
              (("C-c n I" . org-roam-insert-immediate))))
(use-package org-roam-server
  :ensure t
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8080
        org-roam-server-authenticate nil
        org-roam-server-export-inline-images t
        org-roam-server-serve-files nil
        org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
        org-roam-server-network-poll t
        org-roam-server-network-arrows nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20))

(use-package company-org-roam
  :ensure t
  ;; You may want to pin in case the version from stable.melpa.org is not working
  ; :pin melpa
  :config
  (push 'company-org-roam company-backends))

(require 'deft)
(setq deft-directory "~/Dropbox/notes/")

(setq org-roam-capture-templates '(("d" "default" plain (function org-roam--capture-get-point)
     "%?"
     :file-name "%<%Y%m%d%H%M%S>-${slug}"
     :head "\n#+TITLE: ${title}\n#+ROAM_TAGS: unresearched\n#+SETUPFILE:~/Dropbox/setupfile.org\n"
     :unnarrowed t)))

(setq org-html-head "<link rel=\"stylesheet\" href=\"https://sandyuraz.com/styles/org.min.css\">")
(setq org-publish-project-alist
      '(("github.io"
         :base-directory "~/Dropbox/publicnotes/"
         :base-extension "org"
         :publishing-directory "~/richardfeynmanrocks.github.io/notes/"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 4
         :html-extension "html"
         :html-head "<link rel=\"stylesheet\" href=\"https://sandyuraz.com/styles/org.min.css\">"
         )))
(global-set-key (kbd "C-c l") 'org-latex-export-to-pdf)
;;Eliminates the necessity for the save command before compilation is completed
(setq TeX-save-query nil)

(setq yas-triggers-in-field t)
;;Function that combines two commands 1. revert pdfoutput buffer 2. pdf-outline
(defun my-TeX-revert-document-buffer (file)
  (TeX-revert-document-buffer file)
  (pdf-outline))

;; Add custom function to the TeX compilation hook
(add-hook 'TeX-after-compilation-finished-functions #'my-TeX-revert-document-buffer)
(require 'org-roam-protocol)

(with-eval-after-load "ox-latex"
  (add-to-list 'org-latex-classes
               '("koma-article" "\\documentclass{lectures}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))
(require 'ox-latex)
(setq org-latex-to-pdf-process 
  '("xelatex -interaction nonstopmode %f"
     "xelatex -interaction nonstopmode %f")) ;; for multiple passes
(setenv "PATH" "/usr/local/texlive/2020/texmf-dist/tex/latex:$PATH" t)
