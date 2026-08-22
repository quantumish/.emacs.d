(require 'sqlite)
(require 'ini)

(defvar firefox-app-name "firefox<"
  "String that firefox window buffers will be prefixed with.")

(defvar firefox--title-name-regex
  ".*<\\([^>]*\\)>"
  "Regex for extracting the title of a Firefox window from its buffer name.")

(defvar firefox-extract-title-name
  (lambda (string)
    (let ((result '())
          (start 0))
      (while (string-match "<\\([^>]*\\)>" string start)
        (push (match-string 1 string) result)
        (setq start (match-end 0)))
      (string-join (nreverse result) " - ")))
  "Function used to the title name of a Firefox window from its buffer name.")

(defvar firefox-mozilla-folder-path
  (expand-file-name "~/.mozilla")
  "Location of the .mozilla folder with Firefox profiles info.")

(defun firefox--profiles-info ()
  (let* ((ini-path (expand-file-name "~/.mozilla/firefox/profiles.ini"))
         (info (ini-decode ini-path))
         (raw-cands
          (mapcar (lambda (x) (cons (cdr (assoc "Name" (cdr x)))
                                    (cdr (assoc "Path" (cdr x)))))
                  info)))
    (seq-filter 'car raw-cands)))

(defvar firefox-current-profile
  (assoc "default-esr" (firefox--profiles-info))
  "The name and folder name of the selected Firefox profile.")

(defun firefox--profile-path ()
  "Get the file path of the the current Firefox profile's directory."
  (expand-file-name (file-name-concat firefox-mozilla-folder-path
                                      "firefox"
                                      (cdr firefox-current-profile))))

(defvar firefox-command
  "env MOZ_USE_XINPUT2=1 firefox --new-window"
  "Shell command to run to launch Firefox.")

(defvar firefox-search-format
  "https://www.google.com/search?q=%s&udm=14"
  "Format string to use for searching non-URL queries.")

(defvar firefox-title-postprocessor
  (lambda (x) (car (split-string x " — ")))
  "Transformation function to run on the title of a page.")

(defvar firefox-title-width 50
  "Number of characters used to display the page title.")

(defvar firefox-url-width 100
  "Number of characters used to display the page URL.")

(defvar firefox--known-tlds '("com" "org" "ai" "sh" "bike" "app")
  "TLDs used for detecting almost-URLS. TODO this is a terrible solution!")

(defvar firefox--places-temp-filename "places.tmp.sqlite"
  "Name for temporary copy of the places.sqlite file.")

(defvar firefox--places-data nil
  "Cached browsing history completion candidates.")

;; (add-to-list 'savehist-additional-variables 'firefox--places-data)

(defvar firefox-history-sql-where
  "last_visit_date IS NOT NULL AND (visit_count > 1 or unixepoch() - last_visit < 2628000)"
  "WHERE expression to use when querying history.")

(defvar firefox-history-sql-order-by
  "frecency DESC"
  "ORDER BY expression to use when querying history.")

(defun firefox--get-browsing-history ()
  "Copy the places.sqlite file and query to find browsing history sorted by frecency."
  (let* ((profile-path (firefox--profile-path))
         (temp-path (file-name-concat (firefox--profile-path) firefox--places-temp-filename)))
    (copy-file (file-name-concat (firefox--profile-path) "places.sqlite") temp-path t)
    (sqlite-execute
     (sqlite-open temp-path)
     (format
      "SELECT url, title, visit_count, frecency,
       datetime(last_visit_date/1000000, 'unixepoch') as last_visit
       FROM moz_places
       WHERE %s ORDER BY %s;"
      firefox-history-sql-where
      firefox-history-sql-order-by))))

(defun firefox--set-data-cache ()
  "Loads latest browsing data into `firefox--places-data`."
  (setq firefox--places-data (firefox--get-browsing-history))
  (dolist (row firefox--places-data)
    (if (cadr row)
        (setf (cadr row) (funcall firefox-title-postprocessor (cadr row)))))
  (setq firefox--places-data (mapcar 'firefox--make-completion firefox--places-data)))

(defun firefox-switch-profile ()
  "Switch the selected Firefox profile."
  (interactive)
  (let* ((cands (firefox--profiles-info))
         (chosen (completing-read "Profile: " cands nil nil)))
    (setq firefox-current-profile (assoc chosen cands))
	(firefox--set-data-cache)))

(defun firefox--urlify (input)
  "Coerces INPUT to a URL by either adding a https prefix or converting to a search query."
  (url-encode-url
   (let ((url (url-generic-parse-url input)))
     (cond ((url-type url) input)
           ((member (car (last (split-string input "\\."))) firefox--known-tlds) (concat "https://" input))
           (t (format firefox-search-format (url-hexify-string input)))))))

(defun firefox--make-completion (x)
  "Generate a completion string for a given row of the table."
  ;; 'invisible trick is taken from ready-player-mode.
  ;; Really we should just use annotation-function properly but I want searchable URLs...
  (if (cadr x)
      (concat (truncate-string-to-width (cadr x) firefox-title-width nil ?\s "…")
              "  "
              (truncate-string-to-width (propertize (car x) 'face 'font-lock-comment-face)
                                        firefox-url-width nil ?\s "…")
              (propertize (concat "firefox-emacs-url:" (car x)) 'invisible t))
    (concat (truncate-string-to-width (car x)  firefox-url-width nil ?\s "…")
            (propertize (concat "firefox-emacs-url:" (car x)) 'invisible t))))

(defun firefox--get-url-from-completion (x)
  "Extract the URL from the completion string."
  (firefox--urlify (car (last (split-string x "firefox-emacs-url:")))))

(defun firefox-new-tab ()
  "Open a new firefox tab, prompting with history."
  (interactive)
  (when (eq firefox--places-data nil)
    (firefox--set-data-cache))
  (let ((vertico-preselect 'prompt)
        (vertico-sort-function nil)
        (cands firefox--places-data))
    (call-process-shell-command
     (format "%s -P %s \"%s\""
             firefox-command
			 (car firefox-current-profile)
             (firefox--get-url-from-completion
              (completing-read "New tab: " cands nil nil)))
     nil 0)))

;; Personal (less package-y) configuration

(defvar time-wasting-websites-keywords
  '("youtube"
    "lobste.rs"
    "news.ycombinator.com"
    "kittensgame.com"
    "hntoplinks.com"
    "hn.algolia.com")
  "Websites I don't want suggested to me.")

(defvar direct-websites-keywords
  '("discord.com"
    "discordapp.com"
    "google.com/search")
  "Websites that shouldn't be opened via firefox.el")

(setq firefox-history-sql-where
      (seq-reduce 'concat
                  (mapcar (lambda (x) (format " AND INSTR(url, \"%s\") == 0" x))
                          (append time-wasting-websites-keywords direct-websites-keywords))
                  firefox-history-sql-where))
