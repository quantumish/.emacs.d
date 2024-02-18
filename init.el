;;; Packages

(setq package-archives '(("ELPA" . "https://tromey.com/elpa/")
                           ("gnu" . "https://elpa.gnu.org/packages/")
                           ("melpa" . "https://melpa.org/packages/")))
  ;; General convieniences, somewhat questionable
(setq url-http-attempt-keepalives nil)
(setq package-check-signature nil)

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

;; (setq straight-recipe-repositories '(melpa gnu-elpa-mirror nongnu-elpa org-elpa el-get))

(straight-use-package 'use-package)
(eval-when-compile
  (require 'use-package))
;; Automatically install all packages with straight.el if not present.
(setq straight-use-package-by-default t)
;; Always lazy-load if doable. TODO Properly look into good defer setup
(setq use-package-always-defer t)

;; Generalizability
;; ;; NOTE this is kind of scuffed. maybe stick to hostname based configuration?
;; (defvar on-thinkpad  
;;   (string= (string-trim (shell-command-to-string
;;                          "cat /sys/devices/virtual/dmi/id/chassis_vendor"))
;;            "LENOVO")
;;   "Whether or not we are on a Thinkpad device.")

;;; Sane Defaults

;; Turn off all unnecessary GUI elements.
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(fringe-mode 0)

;; Unless something is actively exploding, I do not care.
(setq warning-minimum-level :emergency)

;; customize is the worst.
(setq custom-file "/dev/null")
(setq package-selected-packages "/dev/null/")

;; These keybinds suspend Emacs (in order to mimic terminal behavior).
;; This has *only* caused me trouble in GUI Emacs.
(when (display-graphic-p)
  (global-unset-key (kbd "C-z"))
  (global-unset-key (kbd "C-x C-z")))

;; Stop making backup files everywhere, put them all in one place!
(make-directory "~/.saves/" t)
(make-directory "~/.autosaves/" t)
(setq auto-save-file-name-transforms '((".*" "~/.autosaves/" t)))
(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)

;; Stop Emacs from bothering you about disabled commands.
(setq disabled-command-function nil)

;; Prevent any attempts to resize the frame.
(setq frame-inhibit-implied-resize t)

;; Stop Emacs from trying to use dialog boxes.
(setq use-dialog-box nil)

;; Prefer y/n over yes/no.
(fset 'yes-or-no-p 'y-or-n-p)

;; Stuff is often here too.
(add-to-list 'exec-path "~/.local/bin")

;; Don't dump files everywhere if possible.
(use-package no-littering)

;; Visual line mode is just better.
(global-visual-line-mode)

;; PgUp and PgDown are scarily close to arrow keys on my laptop.
(when (string= system-name "qpad")
  (unbind-key "C-<prior>")
  (unbind-key "C-<next>"))

;;;; Misc

(add-to-list 'load-path (concat (getenv "HOME") "/.emacs.d/hacks"))
;; (set-face-attribute 'hl-line nil :background (doom-aAAlighten (doom-color 'bg) 0.015))

;;;; Advice

;; It's kind of annoying to have existing themes still be applied when you apply a new theme.
(define-advice load-theme (:before (&rest _args) theme-dont-propagate)
  "Discard all themes before loading new."
  (mapc #'disable-theme custom-enabled-themes))

;; Make C-w and M-w work on the current line if there's no region.
;; Taken from the Mastering Emacs professor.
(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Single line killed")
     (list (line-beginning-position)
	   (line-beginning-position 2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
	   (line-beginning-position 2)))))

;; ;; Make C-<backspace> kill matching pairs of parens, etc.
;; (defadvice backward-kill-word (around delete-pair activate)
;;   (if (eq (char-syntax (char-before)) ?\()
;;       (progn
;; 	(backward-char 1)
;; 	(save-excursion
;; 	  (forward-sexp 1)
;; 	  (delete-char -1))
;; 	(forward-char 1)
;; 	(append-next-kill)
;; 	(kill-backward-chars 1))
;;     ad-do-it))

;;; EXWM

(use-package exwm
    :init
    (setq exwm-workspace-number 2)
    (setq exwm-input-global-keys
          `(([?\s-r] . exwm-reset)
            ([?\s-w] . exwm-workspace-switch)
            ([?\s-&] . (lambda (command)
                         (interactive (list (read-shell-command "$ "))) ;
                         (start-process-shell-command command nil command)))))
    ;; Set default simulation keys
    (setq exwm-input-simulation-keys
          '(([?\C-a] . [home])
            ([?\C-e] . [end])
            ([?\C-s] . ?\C-f)
            ([?\C-t] . ?\C-n)
            ([?\C-x?\C-s] . ?\C-s)
            ([?\M-v] . [prior])
            ([?\C-v] . [next])
            ([?\C-d] . [delete])            
            ([?\C-k] . [S-end delete])))
    ;; Allow windows to be moved across screens and interacted with normally.

    (setq exwm-layout-show-all-buffers t)
    (setq exwm-workspace-show-all-buffers t)
    (setq exwm-workspace-minibuffer-position 'bottom)
    :config
    (exwm-enable))

(call-process-shell-command "feh --bg-fill ~/.config/wallpapers/firewatch-galaxy.jpg" nil 0)
(call-process-shell-command "bash ~/.config/polybar/launch.sh --material" nil 0)

(use-package exwm
    :init
    (defvar left-screen "HDMI-1")
    (defvar middle-screen "eDP-1")
    ; (defvar right-screen "DP-1")    
    :config
    (require 'exwm-randr)
    (setq exwm-randr-workspace-output-plist `(0 ,middle-screen 1 ,left-screen))
    (exwm-randr-enable))    


(use-package exwm-outer-gaps
  :straight (exmw-outer-gaps :type git :host github :repo "lucasgruss/exwm-outer-gaps")
  :config
  (defun exwm-outer-gaps-redraw ()        
    "exwm-outer gaps sometimes has artifacts in the gap area. Quickly toggling the mode on and off works forces a redraw of the gaps and gets rid of them."
    (interactive)
    (exwm-outer-gaps-mode))
  :hook (exwm-init . (lambda () (exwm-outer-gaps-mode))))

(use-package exwm
    ; :ensure-system-package (xbindkeys xcape dunst flameshot unclutter polybar feh picom)
    :init
    ;; Rebind keys
    (call-process-shell-command "xmodmap ~/.config/X/Xmodmap" nil 0)
    (call-process-shell-command "xbindkeys" nil 0)
    (call-process-shell-command "sh ~/.config/X/xcape.sh" nil 0)
    ;; Notifications w/ dunst
    (call-process-shell-command "dunst &" nil 0)
    (call-process-shell-command "sh ~/.config/dunst/reload_dunst.sh" nil 0)
    ;; Make mouse vanish when not used
    (call-process-shell-command "unclutter &" nil 0)
    ;; The best screenshot utility!
    (call-process-shell-command "flameshot &" nil 0)
    ;; Compositor
    (call-process-shell-command "picom &" nil 0))

(defun exwm-workspace-next ()
  (interactive)
  (if (< exwm-workspace-current-index (- exwm-workspace-number 1))
      (exwm-workspace-switch (+ exwm-workspace-current-index 1))))

(defun exwm-workspace-prev ()
  (interactive)
  (if (> exwm-workspace-current-index 0)
      (exwm-workspace-switch (- exwm-workspace-current-index 1))))

(use-package exwm
  :bind
   (("M-h" . exwm-workspace-next)
    ("M-l" . exwm-workspace-prev)))

;; Make mouse follow focus
(use-package exwm-mff
  :init (exwm-mff-mode))

(use-package exwmsw
  :straight (exwmsw :type git :host github :repo "Lemonbreezes/exwmsw"
                    :fork (:host github :repo "richardfeynmanrocks/exwmsw"))
  :init
  (setq exwmsw-active-workspace-plist `(,middle-screen 0 ,left-screen 0))
  (setq exwmsw-the-center-screen middle-screen)
  (setq exwmsw-the-left-screen left-screen))


(use-package exwm
    :init

    (defun b3n-exwm-set-buffer-name ()
      (if (and exwm-title (string-match "\\`http[^ ]+" exwm-title))
          (let ((url (match-string 0 exwm-title)))
            (setq-local buffer-file-name url)
            (setq-local exwm-title (replace-regexp-in-string
                                    (concat (regexp-quote url) " - ")
                                    ""
                                    exwm-title))))
      (setq-local exwm-title
                  (concat
                   exwm-class-name
                   "<"
                   (if (<= (length exwm-title) 50)
                       exwm-title
                     (concat (substring exwm-title 0 50) "…"))
                   ">"))

      (exwm-workspace-rename-buffer exwm-title))

    (add-hook 'exwm-update-class-hook 'b3n-exwm-set-buffer-name)
    (add-hook 'exwm-update-title-hook 'b3n-exwm-set-buffer-name))

;;; Interface
;;;; Theming

(set-face-attribute 'default nil :family "ttyp0")
(set-face-attribute 'font-lock-comment-face nil :italic t)

(use-package hide-mode-line)

(use-package doom-themes
  :init
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  (doom-themes-visual-bell-config)

  (doom-themes-org-config))

(use-package ewal)
(use-package ewal-doom-themes
  :init
  (load-theme 'ewal-doom-one t))

(use-package doom-modeline
  :init
  (setq doom-modeline-height 40)
  (setq doom-modeline-buffer-encoding nil)
  (doom-modeline-mode))

;; TODO: Contextual solaire
(use-package solaire-mode
  :hook
  (prog-mode . solaire-mode))


;;;; Completion

(use-package prescient
  :init (setq prescient-persist-mode t))

(use-package ivy
  :init
  (use-package counsel :config (counsel-mode 1))  
  (use-package swiper :defer t)
  (ivy-mode 1)
  (setq counsel-search-engine 'google)
  :bind
  (("C-s"     . swiper-isearch)
   ("M-x"     . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-x C-l" . counsel-load-theme)
   ("C-h C-f" . counsel-faces)
   ("M-s g"   . counsel-search)
   ("M-g o"   . counsel-outline)
   ("M-g h"   . counsel-org-goto-all)
   ("M-g i"   . counsel-imenu)
   ("M-g a"   . counsel-linux-app)))

(use-package ivy-prescient
  :init (ivy-prescient-mode))

(use-package counsel-projectile
  :bind
  (("M-g p"   . counsel-projectile-switch-project))) 

(use-package ivy-rich
  :after ivy
  :init (ivy-rich-mode)
  :config
  (setq ivy-rich-parse-remote-buffer nil)
  (setq ivy-rich-parse-remote-path nil))
  
(use-package all-the-icons)

(use-package all-the-icons-ivy-rich
  :after ivy-rich counsel
  :init (all-the-icons-ivy-rich-mode)
  :config
  (ivy-rich-modify-column
   'counsel-find-file
   'all-the-icons-ivy-rich-file-name
   '(:width 45))
  (ivy-rich-modify-column
   'counsel-M-x
   'counsel-M-x-transformer
   '(:width 30))
  (ivy-rich-modify-column
   'ivy-switch-buffer
   'ivy-switch-buffer-transformer
   '(:width 55)))

(use-package ivy-prescient
  :after ivy prescient
  :init (ivy-prescient-mode))

(use-package ivy-posframe
  :after ivy
  :init
  (ivy-posframe-mode)
  (setq ivy-posframe-height 11)
  (setq ivy-posframe-width 100)
  (setq ivy-posframe-border-width 1)
  (set-face-attribute 'ivy-posframe-border nil :background (doom-color 'fg)))

(use-package deadgrep)

;;;; Help
(use-package helpful
  :init
  ;; Advise describe-style functions so that Helpful appears no matter what
  (advice-add 'describe-function :override #'helpful-function)
  (advice-add 'describe-variable :override #'helpful-variable)
  (advice-add 'describe-command :override #'helpful-callable)
  (advice-add 'describe-key :override #'helpful-key)
  (advice-add 'describe-symbol :override #'helpful-symbol)
  :config
  ;; Baseline keybindings, not very opinionated
  (global-set-key (kbd "C-h f") #'counsel-describe-function)
  (global-set-key (kbd "C-h v") #'counsel-describe-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-c C-d") #'helpful-at-point)
  (global-set-key (kbd "C-h F") #'helpful-function)
  (global-set-key (kbd "C-h C") #'helpful-command)

  ;; Counsel integration
  (setq counsel-describe-function-function #'helpful-callable)
  (setq counsel-describe-variable-function #'helpful-variable))

(use-package which-key
  :init (which-key-mode))

;;;; Welcome

(setq welcome-messages '("While any text editor can save your files, only Emacs can save your soul."
                         "There's an Emacs package for that."
                         "Eight Megabytes And Constantly Swapping"
                         "Escape Meta Alt Control Super"
                         "M-x butterfly"
                         "The thermonuclear word processor."
                         "The best OS!"))

(defun ar/show-welcome-buffer ()
  "Show *Welcome* buffer."
  (with-current-buffer (get-buffer-create "*Welcome*")
    (setq truncate-lines t)
    (let* ((buffer-read-only)
           (image-path "~/Downloads/splash 1(2).svg")
           (image (create-image image-path 'svg nil :height 350))
           (size (image-size image))
           (height (cdr size))
           (width (car size))
           (top-margin (floor (/ (- (window-height) height) 2)))
           (left-margin (floor (/ (- (window-width) width) 2)))
           (prompt (nth (random (length welcome-messages)) welcome-messages)))
      (erase-buffer)
      (setq mode-line-format nil)
      (goto-char (point-min))
      (insert (make-string top-margin ?\n ))
      (insert (make-string left-margin ?\ ))
      (insert-image image)
      (insert "\n\n\n")
      (solaire-mode)
      ;; (insert (make-string (floor (/ (- (window-width) (string-width prompt)) 2)) ?\ ))
      ;; ;; (put-text-property 0 (length prompt) 'face 'font-lock-string-face prompt)
      ;; (insert prompt)
      )
    (setq cursor-type nil)
    (read-only-mode +1)
    (switch-to-buffer (current-buffer))
    (local-set-key (kbd "q") 'kill-this-buffer)))

(setq initial-scratch-message nil)
(setq inhibit-startup-screen t)

(defun display-pretty-greeting ()
  (interactive)
  (call-process-shell-command "firefox &" nil 0)
  (sleep-for 0.5)
  (split-window-horizontally)
  (ar/show-welcome-buffer)
  (other-window 1)
  (sleep-for 0.5)
  (hide-mode-line-mode)
  (call-process-shell-command "xdotool getactivewindow key ctrl+r" nil 0))
       
;; (when (display-graphic-p)
;;   (add-hook 'exwm-init-hook 'display-nice-greeting))

;; Movement

;; TODO look into winner-mode
(use-package zygospore)

(defun opposite-other-window ()
  "Cycle buffers in the opposite direction."
  (interactive)
  (other-window -1))

(defun opposite-other-frame ()
  "Cycle frames in the opposite direction."
  (interactive)
  (other-frame -1))

(use-package exwm
  :bind (:map override-global-map
              ("M-k" . other-window)
              ("M-j" . opposite-other-window)
              ("C-M-j" . opposite-other-frame)
              ("C-M-k" . other-frame)
              ("M-m" . zygospore-toggle-delete-other-windows))
  (:map exwm-mode-map ;; HACK
        ("M-k" . other-window)
        ("M-j" . opposite-other-window)
        ("C-M-j" . opposite-other-frame)
        ("C-M-k" . other-frame)
        ("M-m" . zygospore-toggle-delete-other-windows)))

(use-package drag-stuff
  :bind (("M-<up>" . drag-stuff-up)
         ("M-<down>" . drag-stuff-down)))

;;;; Keybindings
(use-package general)
;;; Vanilla++
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq indent-tabs-mode nil)

(setq dired-listing-switches "-alh")

(use-package dtrt-indent
  :init (dtrt-indent-global-mode))

(setq-default c-basic-offset 4)

(use-package crux
  :bind
  (("C-a" . crux-move-beginning-of-line) ;; Move to beginning of text, not line.
   ("C-x 4 t" . crux-transpose-windows)
   ("C-x K" . crux-kill-other-buffers)
   ("C-k" . crux-smart-kill-line)
   ("C-<tab>" . crux-indent-defun))
  :config
  (crux-with-region-or-buffer indent-region)
  (crux-with-region-or-buffer untabify)
  (crux-with-region-or-point-to-eol kill-ring-save)
  (defalias 'rename-file-and-buffer #'crux-rename-file-and-buffer))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package diredfl
  :init (diredfl-global-mode))

(use-package sudo-edit)

(use-package anzu
  :init
  (global-anzu-mode)
  :bind
  (("M-r" . anzu-query-replace)))

(use-package beginend
  :config (beginend-global-mode))

;;; Org

(use-package org
  :config
  (setq org-todo-keywords '((sequence "TODO(t)" "WAIT(w)" "|" "DONE(d)" "NOPE(n)")))
  (setq org-modules (append org-modules '(org-habit org-id))))

;;;; Export
                 
(use-package org
  :config 
  (setq org-html-postamble nil)
  (setq org-export-with-section-numbers nil)
  (setq org-export-with-toc nil)
  (setq org-html-htmlize-output-type 'css)
  (setq org-latex-default-packages-alist
        '(("AUTO" "inputenc" t ("pdflatex"))
          ("T1" "fontenc" t ("pdflatex"))
          (#1="" "graphicx" t)
          (#1# "longtable" nil)
          (#1# "wrapfig" nil)
          (#1# "rotating" nil)
          ("normalem" "ulem" t)
          (#1# "amsmath" t)
          (#1# "amssymb" t)
          (#1# "capt-of" nil)
          ("colorlinks" "hyperref" nil)
          (#1# "geometry" nil)
          ("parfill" "parskip" nil))))

;; Kind of cool feature. TODO experiment with me.
(use-package engrave-faces
  :config
  (setq org-latex-listings 'engraved)
  (setq org-latex-engraved-theme 'ewal-doom-one))

(use-package org-special-block-extras  
  :hook (org-mode . org-special-block-extras-mode)
  :config
  (require 'org-special-block-extras)
  (org-defblock fancyquote (author "Unknown")  
   (pcase backend
     (`latex (format "\\epigraph{%s}{\\emph{%s}}\\vspace{10pt}" contents author))
     (_ "")) ;; TODO handle HTML cases  
   )
  ;; (org-defblock theorem ()  
  ;;  (pcase backend
  ;;    (`latex (format "\\begin{theorem}%s\\end{theorem}" contents author))
  ;;    (_ "")) ;; TODO handle HTML cases  
  ;;   )
  )  
;;;; Aesthetics

(use-package org
  :config
  (set-face-attribute 'org-document-title nil
                      :height 2.0 :weight 'bold)
  (setq org-fontify-quote-and-verse-blocks t)
  (setq org-fontify-emphasized-text t)
  (setq org-hide-emphasis-markers t)
  (setq org-ellipsis " ")
  (setq org-hide-leading-stars t)  
  (setq org-latex-create-formula-image-program 'dvisvgm)
  (setq org-highlight-latex-and-related '(native))
  :hook (org-mode . org-indent-mode))

(use-package org
  :init 
  (defun magic-icon-fix ()
    (let ((fontset (face-attribute 'default :fontset)))
      (set-fontset-font fontset '(?\xf000 . ?\xf2ff) "FontAwesome" nil 'append)      
      ))
  (magic-icon-fix)
  :hook
  (org-mode . (lambda () (interactive)
                (setq prettify-symbols-alist
                      '(("[#A]" . "") ("[#B]" . "") ("[#C]" . "")
                       ("[ ]" . "") ("[X]" . "") ("[-]" . "")
                        ("#+begin_src" . "") ("#+end_src" . "―")
                        ("#+begin_collapsible" . "") ("#+end_collapsible" . "―")
                        ("#+begin_aside" . "") ("#+end_aside" . "―")
                        ("#+begin_quote" . "") ("#+end_quote" . "―")
                        ("#+begin_fancyquote" . "") ("#+end_fancyquote" . "―")
                        ("#+begin_defn" .  "") ("#+end_defn" . "―")
                        ("#+begin_questionable" .  "") ("#+end_questionable" . "―")
                        ("#+begin_problem" .  "") ("#+end_problem" . "―")
                        ("#+EXCLUDE_TAGS:" . "")  (":PROPERTIES:" . "\n")
                        (":END:" . "―") ("#+STARTUP:" . "") ("#+TITLE: " . "")
                        ("#+title: " . "") ("#+RESULTS:" . "") ("#+NAME:" . "")
                        ("#+ROAM_TAGS:" . "") ("#+FILETAGS:" . "")
                        ("#+HTML_HEAD:" . "") ("#+SUBTITLE:" . "")
                        ("#+AUTHOR:" . "") (":Effort:" . "")
                        ("SCHEDULED:" . "") ("DEADLINE:" . "")))
                (prettify-symbols-mode))))




;; Smart mixing of variable pitch and monospace
;; This is preferred over `mixed-pitch` because of small details
;; (use-package org-variable-pitch
;;   :init (org-variable-pitch-setup))

;; Better headline icons
(use-package org-superstar
  :config
  (setq org-superstar-headline-bullets-list '("◉" "○" "◈" "◎"))
  :hook (org-mode . org-superstar-mode))

;; Auto-toggle emphasis
(use-package org-appear
  :straight (:host github :repo "awth13/org-appear")
  :hook (org-mode . org-appear-mode))

;; Auto-toggle LaTeX rendering
(use-package xenops
  :hook (org-mode . xenops-mode)
  :config (defun xenops-handle-paste ()))

;; Natural bulleted lists
(use-package org-autolist
  :hook (org-mode . org-autolist-mode))

;; Centering w/ Olivetti
(use-package olivetti
  :hook (org-mode . (lambda () (interactive) (olivetti-mode) (olivetti-set-width 100))))

;;; Snippets

(use-package yasnippet
  :init (yas-global-mode))

(use-package aas
  :hook (LaTeX-mode . ass-activate-for-major-mode)
  :hook (org-mode . ass-activate-for-major-mode)
  :hook (c-mode . ass-activate-for-major-mode)
  :hook (c++-mode . ass-activate-for-major-mode)
  :config
  (aas-set-snippets 'c-mode
    "u64" "uint64_t"
    "u32" "uint32_t"
    "u16" "uint16_t"
    "u8" "uint8_t"
    "i64" "int64_t"
    "i32" "int32_t"
    "i16" "int16_t"
    "i8" "int8_t")
  (aas-set-snippets 'c++-mode
    "mxf" "Eigen::MatrixXf"
    "mxd" "Eigen::MatrixXd"
    "v2f" "Eigen::Vector2f"
    "v2d" "Eigen::Vector2d"
    "v2i" "Eigen::Vector2i"
    "v3f" "Eigen::Vector3f"
    "v3d" "Eigen::Vector3d"
    "v3i" "Eigen::Vector3i")
  (aas-set-snippets 'org-mode
    "w/" "with"
    "--" "—"))
(use-package laas
  :config ; do whatever here
  (aas-set-snippets 'laas-mode
    ;; set condition!
    "mk" (lambda () (interactive)
           (yas-expand-snippet "\\\\($1\\\\)$0"))                      
    "afsoc" "assume for the sake of contradiction"
    "Afsoc" "Assume for the sake of contradiction"
    :cond #'texmathp ; expand only while in math
    "tt" (lambda () (interactive)
           (yas-expand-snippet "\\text{$1}$0"))
    "dd" (lambda () (interactive)
           (yas-expand-snippet "\\dd{$1}$0"))                      
    "'-" "\\setminus"
    "reals" "\\mathbb{R}"
    "ints" "\\mathbb{Z}"
    "nats" "\\mathbb{N}"
    "pi" "\\pi"
    "bff" (lambda () (interactive)
            (yas-expand-snippet "\\mathbf{$1}$0"))
    "ll" "\\left"
    "rr" "\\right"
    "pm" (lambda () (interactive)
           (yas-expand-snippet "\\begin{pmatrix} $1 \\end{pmatrix} $0"))
    "sm" (lambda () (interactive)
           (yas-expand-snippet "\\left(\\begin{smallmatrix} $1 \\end{smallmatrix}\\right) $0"))
    ;; add accent snippets
    :cond #'laas-object-on-left-condition
    "qq" (lambda () (interactive) (laas-wrap-previous-object "sqrt"))
    ))


;;; Development
;;;; Terminal Emulator
(use-package vterm
  :hook (vterm-mode . hide-mode-line-mode)
  :hook (vterm-mode . rename-uniquely))

;;;;; Eshell
(add-hook 'eshell-mode-hook 'hide-mode-line-mode)

(setq eshell-banner-message "")

(defun dw/get-prompt-path ()
  (let* ((current-path (eshell/pwd))
         (git-output (shell-command-to-string "git rev-parse --show-toplevel"))
         (has-path (not (string-match "^fatal" git-output))))
    (if (not has-path)
        (abbreviate-file-name current-path)
      (string-remove-prefix (file-name-directory git-output) current-path))))

;; This prompt function mostly replicates my custom zsh prompt setup
;; that is powered by github.com/denysdovhan/spaceship-prompt.

(defun dw/eshell-prompt ()
  (concat
   "\n"
   (propertize "davfrei" 'face `(:foreground ,(doom-color 'orange)) 'read-only t)
   (propertize " " 'face `(:foreground ,(doom-color 'fg)) 'read-only t)
   (propertize (dw/get-prompt-path) 'face `(:foreground ,(doom-color 'orange)) 'read-only t)
   (propertize " · " 'face `(:foreground ,(doom-color 'fg)) 'read-only t)
   (propertize (format-time-string "%I:%M:%S %p") 'face `(:foreground ,(doom-color 'cyan)) 'read-only t)
   (if (= (user-uid) 0)
       (propertize "\n#" 'face `(:foreground "red2") 'read-only t)
     (propertize "\nλ" 'face `(:foreground ,(doom-color 'blue)) 'read-only t))
   (propertize " " 'face `(:foreground ,(doom-color 'fg)))
   ))

(defun dw/eshell-configure ()
  (use-package xterm-color)

  (push 'eshell-tramp eshell-modules-list)
  (push 'xterm-color-filter eshell-preoutput-filter-functions)
  (delq 'eshell-handle-ansi-color eshell-output-filter-functions)

  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  (add-hook 'eshell-before-prompt-hook
            (lambda ()
              (setq xterm-color-preserve-properties t)))

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  ;; We want to use xterm-256color when running interactive commands
  ;; in eshell but not during other times when we might be launching
  ;; a shell command to gather its output.
  (add-hook 'eshell-pre-command-hook
            (lambda () (setenv "TERM" "xterm-256color")))
  (add-hook 'eshell-post-command-hook
            (lambda () (setenv "TERM" "dumb")))

  ;; Use completion-at-point to provide completions in eshell
  (define-key eshell-mode-map (kbd "<tab>") 'completion-at-point)

  ;; Initialize the shell history
  (eshell-hist-initialize)

  (setenv "PAGER" "cat")

  (setq eshell-prompt-function      'dw/eshell-prompt
        eshell-prompt-regexp        "^λ "
        eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-highlight-prompt t
        eshell-scroll-to-bottom-on-input t
        eshell-prefer-lisp-functions nil))

(use-package eshell
  :hook (eshell-first-time-mode . dw/eshell-configure)
  :init
  (setq eshell-directory-name "~/.dotfiles/.emacs.d/eshell/"
        eshell-aliases-file (expand-file-name "~/.dotfiles/.emacs.d/eshell/alias")))

(use-package eshell-z
  :hook ((eshell-mode . (lambda () (require 'eshell-z)))
         (eshell-z-change-dir .  (lambda () (eshell/pushd (eshell/pwd))))))

(use-package exec-path-from-shell
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(setq eshell-prompt-function 'dw/eshell-prompt)

(use-package esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode))

(use-package eshell-toggle
  :straight (eshell-toggle :type git :host github :repo "4DA/eshell-toggle")
  :init
  (setq eshell-toggle-size-fraction 4)
  (setq eshell-toggle-use-projectile-root t)
  (setq eshell-toggle-run-command nil))

(use-package eshell-up) ;; TODO eshell-up


;;;; LSP
(use-package lsp-mode
					; :ensure-system-package ccls
					; :ensure-system-package (pyls . "python -m pip install pyls")
					; :ensure-system-package rust-analyzer
  :init
  ;; Disable annoying headerline
  (setq lsp-headerline-breadcrumb-enable nil)
  ;; Don't show unneeded function info in completions
  (setq lsp-completion-show-detail nil)
  ;; Disable annoying autoformatting!
  (setq-default lsp-enable-indentation nil)
  (setq-default lsp-enable-on-type-formatting nil)
  :commands lsp
  ;; Add languages of your choice!
  :hook ((c-mode . lsp)
         (c++-mode . lsp)
         (python-mode . lsp)
         (typescript-mode . lsp)
         (rust-mode . lsp)
         (lsp-mode . (lambda () (lsp-lens-mode 0)))))

(use-package lsp-ui
  :after lsp
  :init
  (setq lsp-ui-doc-delay 5)
  (add-hook 'flycheck-mode-hook 'lsp-ui-mode) ;; HACK
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  ;; (general-def
  ;;   :keymaps 'lsp-mode-map
  ;;   "C-c l p" 'lsp-ui-peek-find-references)
  :config
  (eval `(set-face-attribute 'lsp-ui-doc-background nil :background ,(doom-darken 'bg .2))))


;;;; Company
(use-package company
  :init
  (setq company-idle-delay 0)
  (setq company-tooltip-maximum-width 40)
  :hook
  (lsp-mode . company-mode))

(use-package company-quickhelp
  :after company
  :init (company-quickhelp-mode))

(use-package company-quickhelp-terminal
  :after company-quickhelp)

(use-package company-prescient
  :after company prescient
  :init
  (setq-default history-length 1000)
  (setq-default prescient-history-length 1000)
  :init (company-prescient-mode))


;;;; Compilation
(use-package kv)

(require 'kv)
(defvar custom-compile-cmds
  '((rustic-mode . ((debug . "cargo build")
                    (release . "cargo build --release")
                    (test . "cargo test")))
    (c++-mode . ((cmake . "cmake .")
                 (test . "ctest")
                 (make . "make")
                 (this . "g++ $this.cpp -std=c++17 -o $this")
		 (this-debug . "g++ $this.cpp -std=c++17 -g -o $this")
                 (this-speedy . "g++ $this.cpp -O3 -std=c++17 -o $this")
                 (this-python . "g++ -shared -std=c++17 -undefined_dynamic_lookup `python3 -m pybind11 --includes` $this.cpp -o $this`python3-config --extension-suffix` -D PYTHON -fPIC")))
    (c-mode . ((make . "make")
               (this . "gcc $this.c -o $this")
               (this-speedy . "gcc $this.c -O3 -o $this")
               (this-archive . "gcc $this.c -O -c -g && ar rcs $this.a $this.o")
               (this-mpi . "mpicc $this.c -o $this")))
    (cuda-mode . ((this . "nvcc $this.cu -o $this")))
    (python-mode . ((this-types . "mypy $this.py --ignore-missing-imports --strict")
                    (this-cython . "cython --embed -o $this.c $this.py -3 && sudo gcc $this.c -o $this -I/usr/include/python3.9 -lpython3.9")))
    ))

(defun compile-dwim ()
  (interactive)
  (let ((list (cdr (assoc major-mode custom-compile-cmds)))) ;; Debugging is for suckers
    (ivy-read "Compilation preset: " (kvalist->keys list)
              :preselect (car (kvalist->keys list))
              :action (lambda (name)
                        (compile
                         (replace-regexp-in-string
                          (regexp-quote "$this")
                          (file-name-sans-extension (buffer-file-name))
                          (cdr (assoc (intern-soft name) list))))))))

(use-package compile
  :config
  (setq compilation-scroll-output t)
  (setq compilation-ask-about-save nil)
  (defun compile-project ()
    (interactive)
    (let ((default-directory (projectile-project-root)))
      (call-interactively 'compile-dwim))))


;;;; Tree view

(use-package treemacs
  :init
  (setq treemacs-width 45)
  (treemacs-project-follow-mode)
  (load "treemacs-all-the-faicons.el")
  (treemacs-load-theme "all-the-faicons")    
  :bind
  ("C-c t" . treemacs)
  :hook
  ((treemacs-mode . (lambda () (olivetti-mode) (olivetti-set-width 25)))
   (treemacs-mode . hide-mode-line-mode)
   (treemacs-mode . (lambda ()
                     (setq header-line-format " ")
                     (face-remap-add-relative 'header-line `(:height 20 :background ,(doom-color 'bg)))))))


;;;; Git
(use-package sqlite3)

(use-package magit)

;; (use-package forge
;;   :after magit)

;;;; TODO Documentation

(use-package eldoc
  :ensure nil
  :init
  (defun eldoc-minibuffer-message (format-string &rest args))
  (defun eldoc-display-in-echo-area (docs _interative)))

;; (use-package eldoc-box
;;   :config
;;   (add-to-list 'eldoc-box-frame-parameters '(border-width . 1))
;;   (add-to-list 'eldoc-box-frame-parameters '(alpha-background . 10))

;;   (let ((bg (face-attribute 'solaire-default-face :background)))
;;     (set-face-attribute 'eldoc-box-border nil :background 'unspecified)
;;     (set-face-attribute 'eldoc-box-body nil :background 'unspecified))
  
;;   :hook (eldoc-mode . eldoc-box-hover-mode))

;;;; Aesthetics

(use-package hl-todo
  :config
  (setq hl-todo-keyword-faces
      `(("TODO"   . ,(doom-color 'green))
        ("FIXME"  . ,(doom-color 'red))
        ("DEBUG"  . ,(doom-color 'magenta))
        ("HACK"   . ,(doom-color 'violet))
        ("NOTE"   . ,(doom-color 'cyan))))
  :hook (prog-mode . hl-todo-mode))

(use-package olivetti
    :config
    (setq-default olivetti-body-width 180)
    :hook ((prog-mode . olivetti-mode)))

;; I despise camelCase.
(setq glasses-uncapitalize-p t)
(setq glasses-separate-parentheses-p nil)

;;;; Language Support

(use-package format-all)

;;;; C0 stuff
(setq c0-root "/home/quantumish/c0/cc0/")
(setq c0-bin-root "/home/quantumish/Downloads/c0/cc0/")
(load (concat c0-root "c0-mode/c0.el"))

;;;; Lisp
;; (use-package sly)
(use-package eros
  :init (require 'eros)
  :config
  (set-face-attribute 'eros-result-overlay-face nil
                      :background 'unspecified :box 'unspecified
                      :foreground (face-attribute 'font-lock-comment-face :foreground))
  :bind ("C-x C-e" . eros-eval-last-sexp))

(use-package slime
  :init
  (setq inferior-lisp-program "sbcl")
  (defun slime-eval-last-expression-eros ()
    (interactive)
    (destructuring-bind (output value)
        (slime-eval `(swank:eval-and-grab-output ,(slime-last-expression)))
      (eros--make-result-overlay (concat output value)
        :where (point)
        :duration eros-eval-result-duration)))
  
  ;; (add-hook 'lisp-mode-hook 'slime)
  ;; (add-hook 'lisp-mode-hook 'slime-mode)
  ;; (general-def :keymaps 'slime-mode-map 
  ;;   "C-x C-e" 'cmake-ide-compile))  
  
  :bind (:map slime-mode-map
              ("C-x C-e" . slime-eval-last-expression-eros))
  :hook ((lisp-mode . slime)
         (lisp-mode . slime-mode)))


(use-package lispy)
(use-package outshine
  :config
  (unbind-key "M-<up>" outshine-mode-map)
  (unbind-key "M-<down>" outshine-mode-map) 
  :hook (emacs-lisp-mode . outshine-mode))

;;;; Misc Languages

(use-package rustic)
(use-package cuda-mode)
(use-package nasm-mode)

(use-package rust-mode) ;; for when rustic breaks
(use-package clojure-mode :init (lsp-ensure-server 'clojure-lsp))
(use-package sml-mode)
(use-package json-mode :init (lsp-ensure-server 'json-ls))
(use-package dockerfile-mode :init (lsp-ensure-server 'dockerfile-ls))
(use-package css-mode :init (lsp-ensure-server 'css-ls))
(use-package typescript-mode)

;; Languages that sound cool but I'll likely never use.
(use-package go-mode)
(use-package haskell-mode :init (use-package lsp-haskell))
(use-package nim-mode)
(use-package d-mode)
(use-package zig-mode)
(use-package julia-mode)

;; (add-to-list 'tramp-remote-path 'tramp-own-remote-path)

;; (with-eval-after-load 'lsp-mode
;;   (add-to-list 'lsp-language-id-configuration '(c0-mode . "c0"))
;;   ;; if you are adding the support for your language server in separate repo use
;;   ;; (add-to-list 'lsp-language-id-configuration '(python-mode . "python"))

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection "c0lsp")
                  :activation-fn (lsp-activate-on "c0")
                  :server-id 'c0lsp))

;;   (lsp-register-client
;;    (make-lsp-client :new-connection (lsp-tramp-connection "c0lsp")
;;                     :major-modes '(c02-mode c0-mode)
;;                     :remote? t
;;                     :server-id 'c0lsp-remote)))


;;; PDF
(use-package pdf-tools
  :init
  (pdf-tools-install)
  (defun my/pdf-fit ()
    "Calculates the difference between the pdf image and frame width
     before resizing the frame by the difference"
    (interactive)
    (let ((W (- (car (pdf-view-image-size)) (window-pixel-width))))
      (window-resize (get-buffer-window) W t nil t)
      (pdf-view-fit-height-to-window)))
  
  :hook
  ((pdf-view-mode . pdf-view-midnight-minor-mode)
   (pdf-view-mode . hide-mode-line-mode)
   (pdf-view-after-change-page . my/pdf-fit)))

;;; Writing
(use-package jinx
  ;; turns out to be annoying
  ;; :hook ((org-mode . jinx-mode)
  ;;        (text-mode . jinx-mode))
  :bind (("M-$" . jinx-correct))
  :config
  (set-face-attribute 'jinx-misspelled nil :underline
                      `(:style wave :color ,(doom-color 'blue))))

(use-package lexic)

(use-package doom-modeline
  :config (setq doom-modeline-enable-word-count t))

;;; Elfeed
(use-package elfeed
  :config
  (setq elfeed-feeds  
        '("https://reddit.com/r/cpp.rss"
          "https://reddit.com/r/rust.rss"
          "https://reddit.com/r/emacs.rss"
          "https://reddit.com/r/c_programming.rss"
          "https://reddit.com/r/programming.rss"
          "https://news.ycombinator.com/rss"
          "https://lobste.rs/rss"

          "https://buttondown.email/hillelwayne/rss"
          "https://xkcd.com/atom.xml"

          "https://20mr.substack.com/feed"
          "https://hidonipothan.substack.com/feed"
          )))


;;; CMU

;; (use-package emacs
;;   :bind ("C-x C-a" . (lambda () (counsel-find-file nil "/ssh:afs:./private/"))))


(general-def "C-x C-a" #'(lambda () (interactive) (counsel-find-file nil "/ssh:afs:./private/")))

;;; Done
(exwm-init)
