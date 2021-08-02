(org-special-block-extras-defblock collapsible (title "Details") (contents "")
  (format
   (pcase backend     
     (_ "<details>
             <summary> <i> %s </i> </summary>
             %s
         </details>"))
   title contents))

(setq org-html-text-markup-alist
      '((bold . "<b>%s</b>")
        (code . "<code>%s</code>")
        (italic . "<i>%s</i>")
        (strike-through . "<del>%s</del>")
        (underline . "<span class=\"underline\">%s</span>")
        (verbatim . "<kbd>%s</kbd>")))

