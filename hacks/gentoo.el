;;; gentoo.el --- Portage interface for Emacs.

;;; Commentary:
;;
;; This is a minimal package that I threw together late one night to let me
;; use most of what I need from Portage from within Emacs. It's pretty nice!

(require 'dash)
(require 'eat)
(require 'consult)

;;; Code:

(defvar gentoo--eix-format
  "<category>/<name>|<description>|{havebest}<bestversion:NAMEVERSION>{else}none\n{}"
  "Format string passed to eix --format flag.")

(defvar gentoo-display-padding 60
  "Padding between package names and info.")
(defvar gentoo-gap-padding 10
  "Padding between various pieces of package info.")
(defvar gentoo-emerge-ask nil
  "Whether or not to run emerge with --ask.")
(defvar gentoo-portage-use-file "/etc/portage/package.use"
  "File path of the portage package.use file.")
(defvar gentoo-portage-accept-file "/etc/portage/package.accept_keywords/xcape"
  "File path of the portage package.accept_keywords file.")
(defvar gentoo-cmd-display-type 'eat
  "What to use to run commands.
Possible values are \\='eat (for using the EAT package) or
\\='compile (for using \\[compile]).")

(defgroup gentoo nil
  "Use portage from Emacs."
  :group 'convenience
  :group 'minibuffer
  :group 'consult
  :prefix "gentoo-")

(defface gentoo-use-stop
  '((t (:inherit default :foreground "green")))
  "Face to display DONE option.")

(defface gentoo-use-enabled
  '((t (:inherit default :foreground "red")))
  "Face to display enabled USE flags.")

(defface gentoo-use-disabled
  '((t (:inherit default :foreground "cyan")))
  "Face to display disabled USE flags.")

(defun gentoo--table-from-lists (keypairs)
  "Create a hash table given list of kv pairs KEYPAIRS."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (x keypairs)
	  (puthash (car x) (cdr x) table))
    table))

(defun gentoo--build-package-dict (&optional installed)
  "Create hash table of possibly INSTALLED packages by querying eix."
  (let* ((cmd (format "EIX_LIMIT_COMPACT=0 eix -Sc%s --format '%s'"
		      (if installed "I" "") ;; HACK
		      gentoo--eix-format))
		 (pkgs (butlast (split-string (shell-command-to-string cmd) "\n"))))
	(gentoo--table-from-lists
	 (--filter (> (length it) 1) (--map (split-string it "|") pkgs)))))

(defun gentoo--query-packages-annotate (cand info)
  "Annotator function for `consult--read' for package CAND and its INFO."
  ;; TODO fix this gross hack of a meta format string
  (let ((fmtstr (format "%%%ds%%%ds%%s" (- gentoo-display-padding (length cand)) gentoo-gap-padding)))
	(format fmtstr (cadr info) "" (car info))))

(defun gentoo--query-packages (&optional &key prompt &key installed)
  "Interactively query possibly INSTALLED gentoo packages with a given PROMPT."
  (let* ((table (gentoo--build-package-dict installed))
		 (sel
		  (consult--read
		   table
		   :prompt (if prompt prompt "Select: ")
		   :annotate (lambda (x) (gentoo--query-packages-annotate x (gethash x table))))))
    (list sel (string= (cadr (gethash sel table)) "none"))))

(defun gentoo--get-useflags (pkg)
  "Get USE flags for Gentoo package PKG using equery."
  (let ((uses (split-string (shell-command-to-string (format "equery -N -C uses %s" pkg)) "\n"))
	(rgx  (rx (seq " " (group (or "+" "-")) " " (or "+" "-") " "
		       (group (one-or-more (or alnum "_" "-" "/")))
		       (zero-or-more blank) ": " (group (zero-or-more any))))))
    (gentoo--table-from-lists
     (-non-nil (-map (lambda (x)
		       (if (string-match rgx x)
			   (list (match-string 2 x) (equal (match-string 1 x) "+") (match-string 3 x))))
		     uses)))))

(defun gentoo--query-useflags-annotate (cand info)
  "Annotator function for `consult--read' for USE flag CAND and its INFO."
  ;; TODO fix this cursed evil hack of a meta format string
  (let ((fmtstr (format "%%%ds%%s" (- gentoo-display-padding (length cand)))))
    (if (not info) (list (propertize cand 'face 'gentoo-use-stop) "" "")
      (list (if (car info)
		(propertize cand 'face 'gentoo-use-enabled)
	      (propertize cand 'face 'gentoo-use-disabled))
	    (if (car info) "+" "-") (format fmtstr "" (cadr info))))))

(defun gentoo--ask-useflags (table pkg)
  "Interactively query USE flags of PKG given TABLE of USE flags."
  (let ((x (consult--read
	    (cons "_DONE_" (map-keys table))
	    :prompt "Toggle USE flags: "
	    :annotate (lambda (x) (gentoo--query-useflags-annotate x (gethash x table))))))
    (unless (string= x "_DONE_")
      (puthash x (list (not (car (gethash x table))) (cadr (gethash x table))) table)
      (gentoo--ask-useflags table pkg))))

(defun gentoo--set-useflags (pkg)
  "Interactively query and set USE flags of package PKG."
  (let* ((flags (gentoo--get-useflags pkg))
	 (default (copy-hash-table flags)))
    (if (> (hash-table-count flags) 0) (gentoo--ask-useflags flags pkg))
    (let* ((diffs (-non-nil (map-apply (lambda (k v)
					 (if (not (eq (car (gethash k default)) (car v)))
					     (concat (if (car v) "" "-") k))) flags)))
	   (path (concat "/sudo::" gentoo-portage-use-file))
	   (rgx (rx-to-string `(seq ,pkg (zero-or-more any)) t))
	   (defn (format "%s %s" pkg (mapconcat (lambda (x) x) diffs " "))))
      (with-temp-buffer
	(insert-file-contents path)
	(if (string-match rgx (buffer-string))
	    (replace-regexp rgx defn)
	  (progn (end-of-buffer) (insert (concat defn "\n"))))
	(write-region (point-min) (point-max) path)))))

(defun gentoo--unmask-pkg (pkg)
  "Unmask package PKG by editing file at `gentoo-portage-accept-file'."
  (if (y-or-n-p (format "Unmask %s?" pkg))
      (write-region (concat pkg "\n")
		    nil (concat "/sudo::" gentoo-portage-accept-file) 'append)))

(defun gentoo--run-cmd (command &optional withsudo)
  "Run command COMMAND possibly WITHSUDO using method in `gentoo-cmd-display-type'."
  (let ((default-directory (if withsudo "/sudo::" default-directory))
	(inhibit-read-only t)
	(eat-buffer-name "*emerge*")
	(compilation-buffer-name-function (lambda (_) "*emerge*"))
	(compilation-environment (if (eq gentoo-cmd-display-type 'compile) '("NO_COLOR=1"))))
    (if (eq gentoo-cmd-display-type 'eat)
	(eat command) (compile command))))

(defun gentoo-install-pkg ()
  "Install a package via emerge and set its USE flags."
  (interactive)
  (let* ((pkgi (gentoo--query-packages))
	(pkg (car pkgi)) (masked (cadr pkgi)))
    (when (or (not masked) (and masked (gentoo--unmask-pkg pkg)))
      (gentoo--set-useflags pkg)
      (gentoo--run-cmd (format "emerge %s %s" (if gentoo-emerge-ask "--ask" "") pkg) t))))
    
(defun gentoo-depclean ()
  "Run emerge --depclean and remove unused packages."
  (interactive)
  (gentoo--run-cmd "emerge --depclean" t))

(defun gentoo-deselect ()
  "Interactively query a package to remove from @world."
  (interactive)
  (let ((pkg (car (gentoo--query-packages :prompt "Deselect: " :installed t))))
    (gentoo--run-cmd (format "emerge --deselect %s" pkg) t)))

(provide 'gentoo)
;;; gentoo.el ends here


