(use-package company
  :init
  (setq company-idle-delay 0)
  (setq company-tooltip-maximum-width 40)
  :hook
  (prog-mode . company-mode)
  :bind (:map company-active-map
			  ("<ret>" . 'company-complete-selection)))

(use-package company-quickhelp
  :after company
  :init (company-quickhelp-mode))

(use-package company-quickhelp-terminal
  :after company-quickhelp)

(use-package company-prescient
  :after company prescient
  :init
  (setq-default history-length 1000)
  (setq-default prescient-history-length 1000)
  :init (company-prescient-mode))

(use-package company-box
  :diminish
  :if (display-graphic-p)
  :defines company-box-icons-all-the-icons
  :hook (company-mode . company-box-mode)
  :custom
  (company-box-backends-colors nil)
  :config
  (with-no-warnings
	;; Prettify icons
	(defun my-company-box-icons--elisp (candidate)
	  (when (derived-mode-p 'emacs-lisp-mode)
		(let ((sym (intern candidate)))
		  (cond ((fboundp sym) 'Function)
				((featurep sym) 'Module)
				((facep sym) 'Color)
				((boundp sym) 'Variable)
				((symbolp sym) 'Text)
				(t . nil)))))
	(advice-add #'company-box-icons--elisp :override #'my-company-box-icons--elisp))

  (when (and (display-graphic-p)
			 (require 'all-the-icons nil t))
	(declare-function all-the-icons-faicon 'all-the-icons)
	(declare-function all-the-icons-material 'all-the-icons)
	(declare-function all-the-icons-octicon 'all-the-icons)
	(setq company-box-icons-all-the-icons
		  `((Unknown . ,(all-the-icons-material "find_in_page" :height 0.8 :v-adjust -0.15))
			(Text . ,(all-the-icons-faicon "text-width" :height 0.8 :v-adjust -0.02))
			(Method . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
			(Function . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
			(Constructor . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
			(Field . ,(all-the-icons-octicon "tag" :height 0.85 :v-adjust 0 :face 'all-the-icons-lblue))
			(Variable . ,(all-the-icons-octicon "tag" :height 0.85 :v-adjust 0 :face 'all-the-icons-lblue))
			(Class . ,(all-the-icons-material "settings_input_component" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
			(Interface . ,(all-the-icons-material "share" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
			(Module . ,(all-the-icons-material "view_module" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
			(Property . ,(all-the-icons-faicon "wrench" :height 0.8 :v-adjust -0.02))
			(Unit . ,(all-the-icons-material "settings_system_daydream" :height 0.8 :v-adjust -0.15))
			(Value . ,(all-the-icons-material "format_align_right" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
			(Enum . ,(all-the-icons-material "storage" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
			(Keyword . ,(all-the-icons-material "filter_center_focus" :height 0.8 :v-adjust -0.15))
			(Snippet . ,(all-the-icons-material "format_align_center" :height 0.8 :v-adjust -0.15))
			(Color . ,(all-the-icons-material "palette" :height 0.8 :v-adjust -0.15))
			(File . ,(all-the-icons-faicon "file-o" :height 0.8 :v-adjust -0.02))
			(Reference . ,(all-the-icons-material "collections_bookmark" :height 0.8 :v-adjust -0.15))
			(Folder . ,(all-the-icons-faicon "folder-open" :height 0.8 :v-adjust -0.02))
			(EnumMember . ,(all-the-icons-material "format_align_right" :height 0.8 :v-adjust -0.15))
			(Constant . ,(all-the-icons-faicon "square-o" :height 0.8 :v-adjust -0.1))
			(Struct . ,(all-the-icons-material "settings_input_component" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
			(Event . ,(all-the-icons-octicon "zap" :height 0.8 :v-adjust 0 :face 'all-the-icons-orange))
			(Operator . ,(all-the-icons-material "control_point" :height 0.8 :v-adjust -0.15))
			(TypeParameter . ,(all-the-icons-faicon "arrows" :height 0.8 :v-adjust -0.02))
			(Template . ,(all-the-icons-material "format_align_left" :height 0.8 :v-adjust -0.15)))
		  company-box-icons-alist 'company-box-icons-all-the-icons)))
