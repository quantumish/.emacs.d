(defun org-babel-exp-code (info type)
  "Return the original code block formatted for export."
  (setf (nth 1 info)
	(if (string= "strip-export" (cdr (assq :noweb (nth 2 info))))
            (let ((a (string-match (org-babel-noweb-wrap) (nth 1 info))))
	      (replace-regexp-in-string
	       (org-babel-noweb-wrap) "" (concat (substring (nth 1 info) 0 (- a 1)) (substring (nth 1 info) a))))
	  (if (org-babel-noweb-p (nth 2 info) :export)
	      (org-babel-expand-noweb-references
	       info org-babel-exp-reference-buffer)
	    (nth 1 info))))
  (org-fill-template
   (if (eq type 'inline)
       org-babel-exp-inline-code-template
     org-babel-exp-code-template)
   `(("lang"  . ,(nth 0 info))
     ;; Inline source code should not be escaped.
     ("body"  . ,(let ((body (nth 1 info)))
                   (if (eq type 'inline) body
                     (org-escape-code-in-string body))))
     ("switches" . ,(let ((f (nth 3 info)))
		      (and (org-string-nw-p f) (concat " " f))))
     ("flags" . ,(let ((f (assq :flags (nth 2 info))))
		   (and f (concat " " (cdr f)))))
     ,@(mapcar (lambda (pair)
		 (cons (substring (symbol-name (car pair)) 1)
		       (format "%S" (cdr pair))))
	       (nth 2 info))
     ("name"  . ,(or (nth 4 info) "")))))

(defun declare-special-block (name color icon default-name default-contents)
  (org-special-block-extras-defblock name (title default-name) (contents default-contents)
            (format
             "<div class=\"admonition\" style=\"--admonition-color: %s;\"><div class=\"admonition-title\"><div class=\"admonition-title-content\"><div class=\"admonition-title-icon\"><i class=\"fas %s\" aria-hidden=\"true\"></i></div><div class=\"admonition-title-markdown\">%s</div></div></div><div class=\"admonition-content-holder\"><div class=\"admonition-content\">%s</div></div></div>" color icon title contents))
  (org-special-block-extras-defblock (concat name "c") (title default-name) (contents default-contents)
	     (format
	      "<details class=\"admonition admonition-note admonition-plugin\" style=\"--admonition-color: %s;\">
<summary class=\"admonition-title \"><div class=\"admonition-title-content\"><div class=\"admonition-title-icon\"><i class=\"fas %s\" aria-hidden=\"true\"></i></div><div class=\"admonition-title-markdown\">%s</div></div><div class=\"collapser\"><div class=\"handle\"></div></div></summary><div class=\"admonition-content-holder\"><div class=\"admonition-content\"><p>%s</p></div></div></details>" color icon title contents)))

(declare-special-block "warning" "235, 195, 52" "fa-exclamation-triangle" "Warning" "")



(use-package org-special-block-extras
      :init
      (org-special-block-extras-mode)
      (org-special-block-extras-defblock test (title "Details") (contents "")
            (format
             "<div class=\"admonition\" style=\"--admonition-color: 108, 173, 96;\"><div class=\"admonition-title\"><div class=\"admonition-title-content\"><div class=\"admonition-title-icon\"><i class=\"fas fa-list\" aria-hidden=\"true\"></i></div><div class=\"admonition-title-markdown\">%s</div></div></div><div class=\"admonition-content-holder\"><div class=\"admonition-content\">%s</div></div></div>" title contents)))

(org-special-block-extras-defblock detailsc (title "Details") (contents "")
				   (format
				    "<details class=\"admonition admonition-note admonition-plugin\" style=\"--admonition-color: 108, 173, 96;\">
<summary class=\"admonition-title \"><div class=\"admonition-title-content\"><div class=\"admonition-title-icon\"><i class=\"fas fa-list\" aria-hidden=\"true\"></i></div><div class=\"admonition-title-markdown\">%s</div></div><div class=\"collapser\"><div class=\"handle\"></div></div></summary><div class=\"admonition-content-holder\"><div class=\"admonition-content\"><p>%s</p></div></div></details>" title contents))


(use-package org-special-block-extras
      :init
      (org-special-block-extras-mode)
      (org-special-block-extras-defblock warning (title "Warning") (contents "")
            (format
             "<div class=\"admonition\" style=\"--admonition-color: 235, 195, 52;\"><div class=\"admonition-title\"><div class=\"admonition-title-content\"><div class=\"admonition-title-icon\"><i class=\"fas fa-exclamation-triangle\" aria-hidden=\"true\"></i></div><div class=\"admonition-title-markdown\">%s</div></div></div><div class=\"admonition-content-holder\"><div class=\"admonition-content\">%s</div></div></div>" title contents)))

(use-package org-special-block-extras
      :init
      (org-special-block-extras-mode)
      (org-special-block-extras-defblock quot (author "") (contents "")
            (format
	     "<div class=\"truequote\"> 
  <blockquote>    
      %s
    </blockquote>
<div class=\"author\">
  %s
</div>  
</div>
</div> " contents author)))


      (use-package calfw
	:init
	(use-package calfw-org)
	(require 'calfw)
 	(require 'calfw-org))


(org-special-block-extras-defblock asidec (title "") (contents "")
				   (format
				    "<details class=\"admonition admonition-note admonition-plugin\" style=\"--admonition-color: 125, 125, 125;\">
<summary class=\"admonition-title \"><div class=\"admonition-title-content\"><div class=\"admonition-title-icon\"><i class=\"fas fa-arrow-right\" aria-hidden=\"true\"></i></div><div class=\"admonition-title-markdown\">%s</div></div><div class=\"collapser\"><div class=\"handle\"></div></div></summary><div class=\"admonition-content-holder\"><div class=\"admonition-content\"><p>%s</p></div></div></details>" title contents))



(use-package htmlize)


(defun org-agenda-skip-if-scheduled-later ()
"If this function returns nil, the current match should not be skipped.
Otherwise, the function must return a position from where the search
should be continued."
  (ignore-errors
    (let ((subtree-end (save-excursion (org-end-of-subtree t)))
          (scheduled-seconds
            (time-to-seconds
              (org-time-string-to-time
                (org-entry-get nil "deferred"))))
          (now (time-to-seconds (current-time))))
       (and scheduled-seconds
            (>= scheduled-seconds now)
            subtree-end))))

(setq org-agenda-skip-function-global 'org-agenda-skip-if-scheduled-later)

(use-package nroam
  :straight '(nroam :host github
                         :branch "master"
                         :repo "NicolasPetton/nroam")
  :after org-roam
  :config
  (add-hook 'org-mode-hook #'nroam-setup-maybe))


(org-special-block-extras-defblock aside (title "") (contents "")
            (format
             "<div class=\"admonition\" style=\"--admonition-color: 173, 173, 173;\"><div class=\"admonition-title\"><div class=\"admonition-title-content\"><div class=\"admonition-title-icon\"><i class=\"fas fa-quote-left\" aria-hidden=\"true\"></i></div><div class=\"admonition-title-markdown\">%s</div></div></div><div class=\"admonition-content-holder\"><div class=\"admonition-content\">%s</div></div></div>" title contents))


(org-special-block-extras-defblock concern (title "") (contents "")
            (format
             "<div class=\"admonition\" style=\"--admonition-color: 235, 195, 52;\"><div class=\"admonition-title\"><div class=\"admonition-title-content\"><div class=\"admonition-title-icon\"><i class=\"fas fa-exclamation-triangle\" aria-hidden=\"true\"></i></div><div class=\"admonition-title-markdown\">%s</div></div></div><div class=\"admonition-content-holder\"><div class=\"admonition-content\">%s</div></div></div>" title contents))

(use-package org-special-block-extras
      :init
      (org-special-block-extras-mode)
      (org-special-block-extras-defblock quott (title "") (contents "")
            (format
             "<div class=\"admonition\" style=\"--admonition-color: 173, 173, 173;\"><div class=\"admonition-title\"><div class=\"admonition-title-content\"><div class=\"admonition-title-icon\"><i class=\"fas fa-quote-left\" aria-hidden=\"true\"></i></div><div class=\"admonition-title-markdown\">%s</div></div></div><div class=\"admonition-content-holder\"><div class=\"admonition-content\">%s</div></div></div>" title contents)))


(org-special-block-extras-defblock concern (title "") (contents "")
				   (format
				    "<details class=\"admonition admonition-note admonition-plugin\" style=\"--admonition-color: 235, 195, 52;\">
<summary class=\"admonition-title \"><div class=\"admonition-title-content\"><div class=\"admonition-title-icon\"><i class=\"fas fa-exclamation-triangle\" aria-hidden=\"true\"></i></div><div class=\"admonition-title-markdown\">%s</div></div><div class=\"collapser\"><div class=\"handle\"></div></div></summary><div class=\"admonition-content-holder\"><div class=\"admonition-content\"><p>%s</p></div></div></details>" title contents))
