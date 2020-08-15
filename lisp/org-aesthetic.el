;; org-aesthetic.el --- Make Org Mode look nicer!

(with-eval-after-load 'org
(set-face-attribute 'org-ellipsis nil
  :foreground "#999999"
  :underline nil
  :weight 'light)
(set-face-attribute 'org-special-keyword nil
  :foreground "#999999"
  :weight 'light)

(set-face-attribute 'org-agenda-date nil
  :foreground "#333333"
  :weight 'regular)

(set-face-attribute 'org-agenda-date-weekend nil
  :foreground "#333333"
  :weight 'regular)

(set-face-attribute 'org-agenda-date-today nil
  :foreground "#333333"
  :weight 'regular)
)

;; Prettify symbols mode is nice
(add-hook 'org-mode-hook (lambda ()
   "Beautify Org Checkbox Symbol"
   (push '("TODO" . "") prettify-symbols-alist)
   (push '("DONE" . "" ) prettify-symbols-alist)
   (push '("WAIT" . "" ) prettify-symbols-alist)
   (push '("NOPE" . "" ) prettify-symbols-alist)
   (push '("[#A]" . "" ) prettify-symbols-alist)
   (push '("[#B]" . "" ) prettify-symbols-alist)
   (push '("[#C]" . "" ) prettify-symbols-alist)
   (push '("#+BEGIN_SRC" . "" ) prettify-symbols-alist)
   (push '("#+END_SRC" . "―" ) prettify-symbols-alist)
   (push '(":PROPERTIES:" . "" ) prettify-symbols-alist)
   (push '(":END:" . "―" ) prettify-symbols-alist)
   (push '("#+STARTUP:" . "" ) prettify-symbols-alist)
   (push '("#+TITLE: " . "" ) prettify-symbols-alist)
   (push '("#+RESULTS:" . "" ) prettify-symbols-alist)
   (push '("#+NAME:" . "" ) prettify-symbols-alist)
   (prettify-symbols-mode)))

(defface org-checkbox-done-text
  '((t (:foreground "#71696A" :strike-through t)))
  "Face for the text part of a checked org-mode checkbox.")

(setq org-priority-faces '((?A . (:foreground "#f5381b" :weight 'bold))
                           (?B . (:foreground "#f5cb22"))
                           (?C . (:foreground "#6cad50"))))

(setq org-todo-keyword-faces
      '(("TODO" . "#999999") ("WAIT" . "#cfd1d1")
        ("DONE" . "#6cad50") ("NOPE" . "#cfd1d1")))

(setq org-fontify-done-headline t)

(custom-set-faces
 '(org-headline-done
            ((((class color) (class color) (min-colors 16))
              (:foreground "#cfd1d1")))))

(provide 'org-aesthetic)
