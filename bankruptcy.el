
(use-package pretty-hydra)
(use-package s)
(use-package major-mode-hydra
  :after hydra
  :preface
  (defun with-alltheicon (icon str &optional height v-adjust face)
    "Display an icon from all-the-icon."
    (s-concat (all-the-icons-alltheicon icon :v-adjust (or v-adjust 0) :height (or height 1) :face face) " " str))

  (defun with-faicon (icon str &optional height v-adjust face)
    "Display an icon from Font Awesome icon."
    (s-concat (all-the-icons-faicon icon ':v-adjust (or v-adjust 0) :height (or height 1) :face face) " " str))

  (defun with-fileicon (icon str &optional height v-adjust face)
    "Display an icon from the Atom File Icons package."
    (s-concat (all-the-icons-fileicon icon :v-adjust (or v-adjust 0) :height (or height 1) :face face) " " str))

  (defun with-octicon (icon str &optional height v-adjust face)
    "Display an icon from the GitHub Octicons."
    (s-concat (all-the-icons-octicon icon :v-adjust (or v-adjust 0) :height (or height 1) :face face) " " str)))

(pretty-hydra-define hydra-flycheck
  (:hint nil :color teal :quit-key "q" :title (with-faicon "plane" "Flycheck" 1 -0.05))
  ("Checker"
   (("?" flycheck-describe-checker "describe")
    ("d" flycheck-disable-checker "disable")
    ("m" flycheck-mode "mode")
    ("s" flycheck-select-checker "select"))
   "Errors"
   (("<" flycheck-previous-error "previous" :color pink)
    (">" flycheck-next-error "next" :color pink)
    ("f" flycheck-buffer "check")
    ("l" flycheck-list-errors "list"))
   "Other"
   (("M" flycheck-manual "manual")
    ("v" flycheck-verify-setup "verify setup"))))

(pretty-hydra-define hydra-go-to-file
  (:hint nil :color teal :quit-key "q" :title (with-octicon "file-symlink-file" "Go To" 1 -0.05))
  ("Org"
   (("oi" (find-file "~/sync/org/inbox.org") "inbox")
    ("oc" (find-file "~/sync/org/completed.org") "home")
    ("op" (find-file "~/sync/org/projects.org") "projects"))
   "Config"
   (("cc" (find-file "~/.emacs.d/config.org") "config.org")
    ("ci" (find-file "~/.emacs.d/init.el") "init.el" ))
   "Notes"
   (("ni" (find-file "~/sync/notes/index.org") "Main Index"))
   ))


(pretty-hydra-define hydra-org-agenda
  (:hint nil :color teal :quit-key "q" :title (with-faicon "list-ol" "Agenda" 1 -0.05))
  ("Standard"
   (("w" (org-agenda)))))


(pretty-hydra-define hydra-launcher
  (:hint nil :color teal :quit-key "q" :title (with-faicon "arrow-right" "Launch" 1 -0.05))
  ("Shell-likes"
   (("v" vterm "Vterm")
    ("e" eshell "Eshell")
    ("l" ielm "IELM")    
    ("k" (call-process-shell-command "open -a Kitty" nil 0) "Kitty"))
   "Messaging"
   (("i" erc "ERC")
    ("d" (call-process-shell-command "open -a Discord" nil 0) "Discord")
    ("t" (call-process-shell-command "open -a Telegram" nil 0) "Telegram"))
   "Misc"
   (("f" (call-process-shell-command "open -a Firefox" nil 0) "Firefox")
    ("s" (call-process-shell-command "open -a Spotify" nil 0) "Spotify")
    ("m" (call-process-shell-command "open -a Spark" nil 0) "Spark"))
   ))


(use-package hydra
  :init
  (global-unset-key (kbd "C-x h"))
  (general-def
    "C-x h l" 'hydra-launcher/body
    "C-x h a" 'hydra-org-agenda/body
    "C-x h f" 'hydra-go-to-file/body))

(define-key global-map (kbd "C-c t") (make-sparse-keymap))
(general-def
  "C-c t e" 'eshell-toggle
  "C-c t t" 'treemacs)  


(use-package lexic)


