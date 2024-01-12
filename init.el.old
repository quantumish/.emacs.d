(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(eval-when-compile
  (require 'use-package))
;; Automatically install all packages with straight.el if not present.
(setq straight-use-package-by-default t)
;; Always lazy-load if doable. TODO Properly look into good defer setup
(setq use-package-always-defer t)

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

(defvar quanta-completion 'ivy
  "Which completion framework to use.
Options:
- ivy
- vertico")

(defvar quanta-load-only-essentials nil
  "Whether quanta should load only the core of its configuration.")
(require 'package)
(setq package-enable-at-startup nil) ;; Speed tip taken from Doom Emacs
(setq package-archives '(("ELPA" . "https://tromey.com/elpa/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
;; General convieniences, somewhat questionable
(setq url-http-attempt-keepalives nil)
(setq package-check-signature nil)

(straight-use-package 'general)

(require 'ob-tangle)

(defun load-org-babel (file)
  (org-babel-tangle-file file)
  (org-babel-load-file file))

(load-org-babel "~/.emacs.d/config.org")di
