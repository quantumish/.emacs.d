(defun minimal-browse-url (url)
  "Browse an arbitrary url (as URL) in a new frameless Firefox window."
  (split-window-right)
  (other-window 1)
  (call-process-shell-command (concat "firefox -P default-release --new-window " url) nil 0))

(use-package dash-docs)
(use-package counsel-dash
  :after dash-docs
  :config
  (setq dash-docs-browser-func 'minimal-browse-url)
  (setq dash-docs-enable-debugging nil)
  (defun emacs-lisp-doc ()
	"Restrict dash docsets to Emacs Lisp."
	(interactive)
	(setq-local dash-docs-docsets '("Emacs Lisp")))
  (defun c-doc ()
	"Restrict dash docsets to C."
	(interactive)
	(setq-local dash-docs-docsets '("C")))
  (defun c++-doc ()
	"Restrict dash docsets to C/C++."
	(interactive)
	(setq-local dash-docs-docsets '("C" "C++")))
  (defun rust-doc ()
	"Restrict dash docsets to Rust."
	(interactive)
	(setq-local dash-docs-docsets '("Rust")))
  (defun python-doc ()
	"Restrict dash docsets to Python."
	(interactive)
	(setq-local dash-docs-docsets '("Python 3")))
  :bind (:map prog-mode-map
			  ("C-c d" . 'counsel-dash)
			  ("C-c C-d" . 'counsel-dash-at-point))
  :hook
  (emacs-lisp-mode . emacs-lisp-doc)
  (c-mode . c-doc)
  (c++-mode . c++-doc)
  (python-mode . python-doc)
  (rustic-mode . rust-doc)
  (rust-mode . rust-doc))
