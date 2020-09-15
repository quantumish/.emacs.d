;; org-aesthetic.el --- Make Org Mode look nicer!

;; Load elisp code for hiding properties drawer
(load "org-hide-properties.el")

(with-eval-after-load 'org
(set-face-attribute 'org-ellipsis nil
                    :foreground "#999999"
                    :underline nil
                    :weight 'light)
(set-face-attribute 'org-special-keyword nil
                    :foreground "#999999"
                    :weight 'light)
(set-face-attribute 'org-document-title nil
                    :weight 'bold)
(set-face-attribute 'org-checkbox-statistics-todo nil
                    :foreground "#f5381b"
                    :weight 'bold)
(set-face-attribute 'org-checkbox-statistics-done nil
                    :foreground "#6cad50"
                    :weight 'bold)
)

(setq org-fontify-quote-and-verse-blocks t)

;; Prettify symbols mode is nice
(add-hook 'org-mode-hook (lambda ()
   "Beautify Org Checkbox Symbol"
   (push '("TODO" . "") prettify-symbols-alist)
   (push '("DONE" . "" ) prettify-symbols-alist)
   (push '("WAIT" . "" ) prettify-symbols-alist)
   (push '("NOPE" . "" ) prettify-symbols-alist)
   (push '("[#A]" . "" ) prettify-symbols-alist)
   (push '("[#B]" . "" ) prettify-symbols-alist)
   (push '("[#C]" . "") prettify-symbols-alist)
   (push '("[ ]" . "" ) prettify-symbols-alist)
   (push '("[X]" . "" ) prettify-symbols-alist)
   (push '("[-]" . "" ) prettify-symbols-alist)
   (push '("#+BEGIN_SRC" . "" ) prettify-symbols-alist)
   (push '("#+END_SRC" . "―" ) prettify-symbols-alist)
   (push '(":PROPERTIES:" . "" ) prettify-symbols-alist)
   (push '(":END:" . "―" ) prettify-symbols-alist)
   (push '("#+STARTUP:" . "" ) prettify-symbols-alist)
   (push '("#+TITLE: " . "" ) prettify-symbols-alist)
   (push '("#+RESULTS:" . "" ) prettify-symbols-alist)
   (push '("#+NAME:" . "" ) prettify-symbols-alist)
   (push '("#+ROAM_TAGS:" . "" ) prettify-symbols-alist)
   (push '("#+HTML_HEAD:" . "" ) prettify-symbols-alist)
   (push '("#+AUTHOR:" . "" ) prettify-symbols-alist)
   (push '("SCHEDULED:" . "" ) prettify-symbols-alist)
   (push '("DEADLINE:" . "" ) prettify-symbols-alist)
   (prettify-symbols-mode)))

(setq org-priority-faces '((?A . (:foreground "#f5381b" :weight 'bold))
                           (?B . (:foreground "#f5cb22"))
                           (?C . (:foreground "#6cad50"))))

(setq org-todo-keyword-faces
      '(("TODO" . "#999999") ("WAIT" . "#cfd1d1")
        ("DONE" . "#6cad50") ("NOPE" . "#cfd1d1")))

(defface org-checkbox-done-text
  '((t (:foreground "#71696A" :strike-through t)))
  "Face for the text part of a checked org-mode checkbox.")

(setq org-fontify-done-headline t)

(custom-set-faces
 '(org-headline-done
            ((((class color) (class color) (min-colors 16))
              (:foreground "#cfd1d1")))))

(require 'calfw)
(require 'calfw-org)

(provide 'org-aesthetic)
