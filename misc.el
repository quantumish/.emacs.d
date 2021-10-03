
(defun org-roam-export-all (in-path out-path)
  "Emergency exit from Org Roam v2. 
   Returns list of commands to convert notes in IN-PATH to traditional format in OUT-PATH."
  (let ((sed (if (eq system-type 'darwin) "gsed" "sed")))
    (progn 
      (call-process-shell-command (concat "cp -R " in-path "*.org " out-path))
      (dolist (pair (org-roam-db-query [:select [ID FILE] :from nodes]))      
	(call-process-shell-command (concat sed " -i \"s/id:" (car pair)
					    "/file:" (substring (car (cdr pair)) (length in-path))
					    "/g\" " out-path "*.org")))
      (message (concat sed " -i \"/:PROPERTIES:/d\" " out-path "*.org"))
      (call-process-shell-command (concat sed " -i \"/:PROPERTIES:/d\" " out-path "*.org"))
      (call-process-shell-command (concat sed " -i \"/:ID:/d\" " out-path "*.org"))
      (call-process-shell-command (concat sed " -i \"/:END:/d\" " out-path "*.org")))))

(org-roam-export-all "/Users/davfrei/sync/notes/" "~/taproot3/corners/david/")
(org-roam-export-all "/Users/davfrei/sync/schoolwork/" "~/taproot3/corners/david/")


(setq prettify-symbols-alist '(("[#A]" . "")
                               ("[#B]" . "")
                               ("[#C]" . "")
                               ("[ ]" . "")
                               ("[X]" . "")
                               ("[-]" . "")
                               ("#+begin_src" . "")
                               ("#+end_src" . "―")
                               ("#+begin_collapsible" . "")
                               ("#+end_collapsible" . "―")
                               ("#+begin_aside" . "")
                               ("#+end_aside" . "―")
			       ("#+begin_defn" .  "")
                               ("#+end_defn" . "―")
			       ("#+begin_questionable" .  "")
                               ("#+end_questionable" . "―")
			       ("#+begin_problem" .  "")
                               ("#+end_problem" . "―")
                               (":PROPERTIES:" . "\n")
                               (":END:" . "―")
                               ("#+STARTUP:" . "")
                               ("#+TITLE: " . "")
                               ("#+title: " . "")
                               ("#+RESULTS:" . "")
                               ("#+NAME:" . "")
                               ("#+ROAM_TAGS:" . "")
                               ("#+FILETAGS:" . "")
                               ("#+HTML_HEAD:" . "")
                               ("#+SUBTITLE:" . "")
                               ("#+AUTHOR:" . "")
                               (":Effort:" . "")
                               ("SCHEDULED:" . "")
                               ("DEADLINE:" . "")))

(use-package org-special-block-extras
      :init
      (org-special-block-extras-mode)
      (org-special-block-extras-defblock collapsible (title "Details") (contents "")
                                         (format
                                          (pcase backend     
                                            ('html "<details>
                                           <summary> <i> %s </i> </summary>
                                           %s
                                        </details>")
					    (_ "\\begin{align*}
%s \\
%s \\
                                             \\end{align*}"))
                                          title contents)))


(use-package laas
  :config ; do whatever here
  (aas-set-snippets 'laas-mode
                    ;; set condition!
                    :cond #'texmathp ; expand only while in math
                    "bff" (lambda () (interactive)
                            (yas-expand-snippet "\\mathbf{$1}$0"))                    
                    ;; add accent snippets
                    :cond #'laas-object-on-left-condition
                    "qq" (lambda () (interactive) (laas-wrap-previous-object "sqrt"))
		    ))


(use-package rustic)
