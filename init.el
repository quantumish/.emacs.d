
  (defvar bootstrap-version)
  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))

(straight-use-package 'org)
(require 'ob-tangle)
  (setq org-confirm-babel-evaluate nil)
(defvar quanta-enable-system-level-config nil
  "Whether Emacs should automatically configure programs outside of Emacs at startup.
WARNING: Will override any existing configurations.")

(setq quanta-enable-system-level-config t)

(defun quanta-get-tangle-target (&optional filename)
  "Get target for tangling: yes or FILENAME if allowed.
Utility function for conditional org tangling."
  (if (not (eq filename nil))
      (if (eq quanta-enable-system-level-config t) filename "no")
    "yes"))

(defun config-tangle (&optional filename system)
  "Tangles a block to FILENAME if on SYSTEM and compiled with FEATURE.
Utility function for conditional org-tangling."
  (if (not (eq system nil))
      (if (eq system-type system)
	  (quanta-get-tangle-target filename)
	"no")
    (quanta-get-tangle-target filename)))

(defvar quanta-theme 'default
  "Which higher-level theme to use in loading the configuration.
Options:
- default (ewal, doom-modeline, centaur-tabs, etc.)
- minimal (ewal, powerline, bitmap font, etc.)
- nano (modified Nano Emacs theme)")

(defvar quanta-completion 'vertico
  "Which completion framework to use.
Options:
- ivy
- vertico")

(defvar quanta-load-only-essentials nil
  "Whether quanta should load only the core of its configuration.")

(require 'ob-tangle)
(defun load-org-babel (file)
  (org-babel-tangle-file file)
  (org-babel-load-file file))

(load-org-babel "~/.emacs.d/config.org")
