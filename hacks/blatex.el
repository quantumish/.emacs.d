;;; blatex.el --- LaTeX improvements for Org mode.

;;; Commentary:
;;
;; This package adds some nice utilities for doing math in Org mode.
;;


;;; TODOs
;; - TeXLab LSP support (via polymode?)
;; - Formatter integration somehow
;; - Basic SymPy operations
;; - Loose algebra checking
;; - SLIME-esque repl?

(require 'texmathp)
(require 's)

;;; Code:

(defun blatex-insert-newline (&optional _ _ _)
  "Insert a LaTeX newline at point if in math mode."
    (if (and (texmathp) (not (s-suffix? "\\" (string-trim-right (thing-at-point 'line t)))))
        (insert " \\\\")))

(defun blatex-smart-after-newline (&optional _ _ _)
  "Guess what should be inserted after a new line in math mode."
  (forward-line -1)
  (if (texmathp)
      (let ((line (thing-at-point 'line t)))
        (forward-line)
        (message (substring line 0 1))
        (cond ((string-match "[^\n]\implies\s*&" line) (insert "\\implies& "))
              ((s-contains? "&=" line) (insert "&= "))))
    (forward-line)))

(defun blatex--send-cmd (cmd arg)
  (process-send-string blatex-process (concat cmd " " arg)))

(defun blatex--do-cmd (cmd)
  (cond ((region-active-p)
         (progn
           (blatex--send-cmd cmd (buffer-substring-no-properties
                                  (region-beginning) (region-end)))
           (kill-region (region-beginning) (region-end))))
        ((texmathp)
         (progn
           (blatex--send-cmd cmd (thing-at-point 'line t))
           (kill-whole-line))))
  (insert (with-current-buffer "*blatex*"
            (end-of-buffer)
            (thing-at-point 'line t))))

(defun blatex--do-cmd-multiline (cmd)
  (cond ((region-active-p)
         (let ((end (region-end)))
           (blatex--send-cmd
            cmd (number-to-string (count-lines (region-beginning) (region-end))))
           (while (< (point) end)
             (process-send-string blatex-process (thing-at-point 'line t))
             (forward-line 1)))))
  (message (with-current-buffer "*blatex*"
            (end-of-buffer)
            (thing-at-point 'line t))))


(defun blatex-diff () (interactive) (blatex--do-cmd "diff"))
(defun blatex-int () (interactive) (blatex--do-cmd "int"))
(defun blatex-simp () (interactive) (blatex--do-cmd "simp"))
(defun blatex-doit () (interactive) (blatex--do-cmd "doit"))
(defun blatex-check () (interactive) (blatex--do-cmd-multiline "check"))


(defun blatex--setup ()
  (make-local-variable 'blatex-process)
  (setq blatex-process
        (start-process-shell-command
         "blatex" "*blatex*" "python /home/quantumish/.emacs.d/hacks/blatex-serv.py"))
  (advice-add 'org-return :before #'blatex-insert-newline)
  (advice-add 'org-return :after #'blatex-smart-after-newline))

(defun blatex--teardown ()
  (advice-remove 'org-return #'blatex-insert-newline)
  (advice-remove 'org-return #'blatex-smart-after-newline))

(define-minor-mode blatex-mode
  "Toggle BLaTeX mode."
  :init-value nil
  :lighter " BLaTeX"

  (if blatex-mode (blatex--setup) (blatex--teardown)))



(provide 'blatex)
;;; blatex.el ends here.

