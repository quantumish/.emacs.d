(use-package erc
  :ensure nil
  :config
  (setq erc-prompt-for-nickserv-password t)
  (setq erc-autojoin-channels-alist '(("libera.chat" "#sanitycorner")))
  (setq erc-autojoin-timing 'ident)
  (setq erc-default-server "irc.libera.chat")
  (add-to-list 'erc-modules 'spelling)
  (add-to-list 'erc-modules 'notifications)
  (add-to-list 'erc-modules 'log))

(use-package erc-image
  :after erc
  :init
  (add-to-list 'erc-modules 'image) 
  :hook
  (erc-mode . erc-image-enable))

(use-package erc-yt
  :after erc
  :init
  (add-to-list 'erc-modules 'youtube))

(use-package znc)

(load "erc-tex")
(add-to-list 'erc-modules 'latex)

(erc-update-modules)

(defvar erc-status "online")
(defvar erc-away-reason)
(defvar erc-asked-users '())
(defun erc-cmd-AWAY (line)
  "Mark the user as being away, the reason being indicated by LINE.
If no reason is given, unset away status."  
  (when (string-match "^\\s-*\\(.*\\)$" line)
    (let ((reason (match-string 1 line)))
      (erc-log (format "cmd: AWAY: %s" reason))
      (erc-server-send
       (if (string= reason "")
           "AWAY"
         (concat "AWAY :" reason)))
	  (if (string= erc-status "online")
		  (progn
			(if (> (length reason) 0)
				(erc-send-action (erc-default-target) (concat "is away: " reason))
			  (erc-send-action (erc-default-target) "is away."))
			(setq erc-status "away"))
		  (progn 
			(erc-send-action (erc-default-target) "is back.")
			(setq erc-asked-users '())
			(setq erc-status "online")))
	  (setq erc-away-reason reason))
    t))

(defun erc-respond-once-if-away (match-type nickuserhost msg)
  (if (string= erc-status "away")
      (if (eq match-type 'current-nick)
          (unless (member nickuserhost erc-asked-users)
			(if (> (length erc-away-reason) 0)
				(erc-send-action (erc-default-target) (concat "is away: " erc-away-reason))
			  (erc-send-action (erc-default-target) "is away."))
			(add-to-list 'erc-asked-users nickuserhost)))))
(add-hook 'erc-text-matched-hook 'erc-respond-once-if-away)


(defun erc-cmd-CALC (&rest args)
  "calculate value of some expression using bc"
  (let ((expr (mapconcat 'identity args " ")))
    (when (length expr)
      (let ((result (shell-command-to-string (concat "echo '" expr "' | bc "))))
        (when result (erc-send-message (concat expr " = " result)))))))

(require 'tls)
(setq tls-program '("openssl s_client -connect %h:%p -no_ssl2 -ign_eof
                                       -CAfile /dev/null 
                                       -cert /etc/openssl/certs/nick.pem" 
                     "gnutls-cli --priority secure256 
                                 --x509cafile /dev/null 
                                 --x509certfile /etc/openssl/certs/nick.pem -p %p %h" 
                     "gnutls-cli --priority secure256 -p %p %h"))
 (defun start-irc ()
   "Connect to IRC."
   (interactive)
   (erc-tls :server "irc.libera.chat" :port 6697
        :nick "quantumish" :client-certificate '("/etc/openssl/certs/key.pem" "/etc/openssl/certs/nick.pem"))
   (setq erc-autojoin-channels-alist '(("libera.chat" "#sanitycorner"))))
