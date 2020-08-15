;; org-project.el --- Project management with Org Mode

;; Project generation function from Karl Voit
(defun mark-as-project ()
"This function makes sure that the current heading has
(1) the tag :project:
(2) has property COOKIE_DATA set to \"todo recursive\"
(3) has any TODO keyword and
(4) a leading progress indicator"
    (interactive)
    (org-toggle-tag "project" 'on)
    (org-set-property "COOKIE_DATA" "todo recursive")
    (org-back-to-heading t)
    (let* ((title (nth 4 (org-heading-components)))
           (keyword (nth 2 (org-heading-components))))
       (when (and (bound-and-true-p keyword) (string-prefix-p "[" title))
           (message "TODO keyword and progress indicator found")
           )
       (when (and (not (bound-and-true-p keyword)) (string-prefix-p "[" title))
           (message "no TODO keyword but progress indicator found")
           (forward-whitespace 1)
           (insert "NEXT ")
           )
       (when (and (not (bound-and-true-p keyword)) (not (string-prefix-p "[" title)))
           (message "no TODO keyword and no progress indicator found")
           (forward-whitespace 1)
           (insert "NEXT [/] ")
           )
       (when (and (bound-and-true-p keyword) (not (string-prefix-p "[" title)))
           (message "TODO keyword but no progress indicator found")
           (forward-whitespace 2)
           (insert "[/] ")
           )
       )
    )

(defun eos/org-id-new (&optional prefix)
  "Create a new globally unique ID.

An ID consists of two parts separated by a colon:
- a prefix
- a   unique   part   that   will   be   created   according   to
  `org-id-method'.

PREFIX  can specify  the  prefix,  the default  is  given by  the
variable  `org-id-prefix'.  However,  if  PREFIX  is  the  symbol
`none', don't  use any  prefix even if  `org-id-prefix' specifies
one.

So a typical ID could look like \"Org-4nd91V40HI\"."
  (let* ((prefix (if (eq prefix 'none)
                     ""
                   (concat (or prefix org-id-prefix)
                           "-"))) unique)
    (if (equal prefix "-")
        (setq prefix ""))
    (cond
     ((memq org-id-method
            '(uuidgen uuid))
      (setq unique (org-trim (shell-command-to-string org-id-uuid-program)))
      (unless (org-uuidgen-p unique)
        (setq unique (org-id-uuid))))
     ((eq org-id-method 'org)
      (let* ((etime (org-reverse-string (org-id-time-to-b36)))
             (postfix (if org-id-include-domain
                          (progn
                            (require 'message)
                            (concat "@"
                                    (message-make-fqdn))))))
        (setq unique (concat etime postfix))))
     (t (error "Invalid `org-id-method'")))
    (concat prefix (car (split-string unique "-")))))

(defun eos/org-custom-id-get (&optional pom create prefix)
  "Get the CUSTOM_ID property of the entry at point-or-marker POM.

If POM is nil, refer to the entry at point. If the entry does not
have an CUSTOM_ID, the function returns nil. However, when CREATE
is non nil, create a CUSTOM_ID if none is present already. PREFIX
will  be passed  through to  `eos/org-id-new'. In  any case,  the
CUSTOM_ID of the entry is returned."
  (interactive)
  (org-with-point-at pom
    (let* ((orgpath (mapconcat #'identity (org-get-outline-path) "-"))
           (heading (replace-regexp-in-string
                     "/\\|~\\|\\[\\|\\]" ""
                     (replace-regexp-in-string
                      "[[:space:]]+" "_" (if (string= orgpath "")
                                  (org-get-heading t t t t)
                                (concat orgpath "-" (org-get-heading t t t t))))))
           (id (org-entry-get nil "CUSTOM_ID")))
      (cond
       ((and id
             (stringp id)
             (string-match "\\S-" id)) id)
       (create (setq id (eos/org-id-new (concat prefix heading)))
               (org-entry-put pom "CUSTOM_ID" id)
               (org-id-add-location id
                                    (buffer-file-name (buffer-base-buffer)))
               id)))))

(defun eos/org-add-ids-to-headlines-in-file ()
  "Add CUSTOM_ID properties to all headlines in the current file
which do not already have one.

Only adds ids if the `auto-id' option is set to `t' in the file
somewhere. ie, #+OPTIONS: auto-id:t"
  (interactive)
  (save-excursion
    (widen)
    (goto-char (point-min))
    (when (re-search-forward "^#\\+OPTIONS:.*auto-id:t"
                             (point-max)
                             t)
      (org-map-entries (lambda ()
                         (eos/org-custom-id-get (point)
                                                'create))))))

(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'before-save-hook
                      (lambda ()
                        (when (and (eq major-mode 'org-mode)
                                   (eq buffer-read-only nil))
                          (eos/org-add-ids-to-headlines-in-file))))))
