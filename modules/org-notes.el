(setq org-html-head "<link rel=\"stylesheet\" href=\"https://quantumish.github.io/org.css\">")
(setq org-publish-project-alist
      '(("github.io"
         :base-directory "~/Dropbox/publicnotes/"
         :base-extension "org"
         :publishing-directory "~/richardfeynmanrocks.github.io/notes/"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 4
         :html-extension "html"
         :with-toc nil
		 :section-numbers nil
		 :html-head "<link rel=\"stylesheet\" href=\"https://richardfeynmanrocks.github.io/org.css\">"
		 :preserve-breaks t
         )))
(setq org-html-postamble nil)

(use-package org-roam
  :init
  (setq org-roam-directory "~/Dropbox/notes"))
