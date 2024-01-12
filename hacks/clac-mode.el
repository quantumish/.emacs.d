;;; clac-mode --- mode for .clac files

;;; Commentary:
;; I'm procrastinating the bonus.

;;; Code:
(defconst clac--font-lock-defaults
  (let ((keywords '("quit" "print" "drop" "swap" "rot" "pick" "skip" "if")))
    `((("\\(: comment .* ;\\)" 1 font-lock-comment-face)
       ;; FIXME: cases like "blahdrop" will be colored incorrectly.
       (,(rx-to-string `(: (or ,@keywords))) 0 font-lock-keyword-face)
       (": \\([A-z0-9]*\\).* ;" 1 font-lock-function-name-face)
       (,(rx-to-string `(and ": " (group (any "A-z")) " ;")) 1 font-lock-function-name-face)
       ))))

(defvar clac-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?= ".")
    (modify-syntax-entry ?_ "_")
    (modify-syntax-entry ?% ".")
    (modify-syntax-entry ?/ ".")
    (modify-syntax-entry ?< ".")
    (modify-syntax-entry ?* ".")
    (modify-syntax-entry ?+ ".")
    (modify-syntax-entry ?- ".")
    st))

(define-derived-mode clac-mode prog-mode "clac"
  "Major mode for clac files."
  (setq font-lock-defaults clac--font-lock-defaults)
  (setq-local comment-start ": comment"))

(provide 'clac-mode)
;;; clac-mode.el ends here.
