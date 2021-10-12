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
