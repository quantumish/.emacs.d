;; org-misc.el --- misc Org config

(setq org-modules (append org-modules '(org-habit))) ;; Habit-tracking with Org Mode
(setq org-modules (append org-modules '(org-crypt))) ;; Encryption
(setq org-modules (append org-modules '(org-id))) ;; Unique headline identifiers

;; Define keywords
(setq org-todo-keywords '((sequence "TODO(t)" "WAIT(w)" "|" "DONE(d)" "NOPE(n)")))

;; Enable Org Babel features 
(org-babel-do-load-languages ;; More languages!
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (latex . t)
   (shell . t)
   (C . t)
   (makefile . t)
   (gnuplot . t)
   (haskell . t)))
(setq org-confirm-babel-evaluate nil) ;; Don't ask me if I want to execute my code or not
(setq org-src-tab-acts-natively t) ;; Indentation fix

;; Enable org link features
(org-link-set-parameters 
 "run"
 :follow #'org-babel-ref-resolve) ;; Allow execution of Org Babel code from links
(add-to-list 'org-file-apps '(directory . emacs)) ;; Allow links to open directories in Dired

(provide 'org-misc)
