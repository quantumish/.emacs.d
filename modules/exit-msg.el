(setq exit-messages '(
	"Please don't leave, there's more demons to toast!"
	"Let's beat it -- This is turning into a bloodbath!"
	"I wouldn't leave if I were you. Vim is much worse."
	"Don't leave yet -- There's a demon around that corner!"
	"Ya know, next time you come in here I'm gonna toast ya."
	"Go ahead and leave. See if I care."
	"Are you sure you want to quit this great editor?"
	"Emacs will remember that."
	"Emacs, Emacs never changes."
	"Okay, look. We've both said a lot of things you're going to regret..."
	"You are *not* prepared!"
	"Look, bud. You leave now and you forfeit your body count!"
	"Get outta here and go back to your boring editors."
	"You're lucky I don't smack you for thinking about leaving."
	"Don't go now, there's a dimensional shambler waiting at the prompt!"
	"Just leave. When you come back I'll be waiting with a bat."
	"Are you a bad enough dude to stay?"
	"It was worth the risk... I assure you."
	"I'm willing to take full responsibility for the horrible events of the last 24 hours."
	))

(defun random-choice (items)
  (let* ((size (length items))
	 (index (random size)))
	(nth index items)))

(defun save-buffers-kill-emacs-with-confirm ()
  (interactive)
  (if (null current-prefix-arg)
	  (if (y-or-n-p (format "%s Quit? " (random-choice exit-messages)))
	(save-buffers-kill-emacs))
	(save-buffers-kill-emacs)))

(global-set-key "\C-x\C-c" 'save-buffers-kill-emacs-with-confirm)
