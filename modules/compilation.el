(require 'kv)
;; Stolen from https://emacs.stackexchange.com/questions/3197/best-way-to-retrieve-values-in-nested-assoc-lists
(defun assoc-recursive (alist &rest keys)
  "Recursively find KEYs in ALIST."
  (while keys
	(setq alist (cdr (assoc (pop keys) alist))))
  alist)

(defvar custom-compile-cmds
	  '((rustic-mode . ((debug . "cargo build")
						(release . "cargo build --release")
						(test . "cargo test"))
					 )))

(defun compile-dwim ()
  (interactive)
  (let ((list (cdr (assoc major-mode custom-compile-cmds)))) ;; Debugging is for suckers
	  (ivy-read "Compilation preset: " (kvalist->keys list)
				:preselect (car (kvalist->keys list))
				:action (lambda (name)
						  (compile (cdr (assoc (intern-soft name) list)))))))

(use-package compile
  :config
  (setq compilation-scroll-output t)
  (setq compilation-ask-about-save nil)
  (defun compile-project ()
	(interactive)
	(let ((default-directory (projectile-project-root)))
	(call-interactively 'compile)))
  :bind (:map c++-mode-map
			  ("C-;" . compile-project)
			  ("C-c C-;" . recompile))
  :hook
  (compilation-mode . hide-mode-line-mode)
  (compilation-mode . header-line-spacious)
  (compilation-start . olivetti-mode)
  (compilation-start . determine-olivetti))

(general-def c++-mode-map
  "C-x n s" 'narrow-to-defun)

(setq shackle-default-rule '(:other t))
(use-package shackle  
  (setq shackle-rules
		'((compilation-mode :ignore t))))	   

