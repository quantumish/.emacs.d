;; -*- lexical-binding: t; -*-
;;; Music-related configuration.

(use-package ready-player :ensure t
  :init (ready-player-mode)
  (defun now-playing ()
    (let ((out (ignore-errors
                 (with-current-buffer (ready-player--active-buffer)
                   (let ((title (ready-player--row-value
                            (ready-player--make-metadata-rows ready-player--metadata)
                            "Title:"))
                         (artist (ready-player--row-value
                            (ready-player--make-metadata-rows ready-player--metadata)
                            "Artist:")))
                     (cond ((and title artist) (format "%s - %s" title artist))
                           (title title)
                           (t "")))))))
      (if out out "")))

  :config
  (setq ready-player-ask-for-project-sustainability nil)
  (setq ready-player-my-media-collection-location (expand-file-name "~/soul"))

  :bind
  (("s-<left>" . ready-player-seek-backward)
   ("s-<right>" . ready-player-seek-forward)
   ("C-c m n" . ready-player-next)
   ("C-c m p" . ready-player-previous)
   ("C-c m SPC" . ready-player-toggle-play-stop)
   ("C-c m /" . ready-player-search)
   ("C-c m i" . ready-player-show-info)
   ("<XF86AudioNext>" . ready-player-next)
   ("<XF86AudioPrev>" . ready-player-previous)
   ("<XF86AudioPlay>" . ready-player-toggle-play-stop)
   ("<XF86AudioPause>" . ready-player-toggle-play-stop)))
