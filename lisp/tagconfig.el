(defface svg-tag-note-face
  '((t :foreground "black" :background "white" :box "black"
       :family "Roboto Mono" :weight light :height 120))
  "Face for note tag" :group nil)

(defface svg-tag-keyboard-face
  '((t :foreground "#333333" :background "#f9f9f9" :box "#333333"
       :family "Roboto Mono" :weight light :height 120))
  "Face for keyboard bindings tag" :group nil)

(setq svg-tag-todo
  (svg-tag-make "TODO" nil 1 1 2))

(setq svg-tag-note
  (svg-tag-make "NOTE" 'svg-tag-note-face 1 1 2))

(defun svg-tag-round (text)
  (svg-tag-make (substring text 1 -1) 'svg-tag-note-face 1 1 12))

(defun svg-tag-quasi-round (text)
  (svg-tag-make (substring text 1 -1) 'svg-tag-note-face 1 1 8))

(defun svg-tag-keyboard (text)
  (svg-tag-make (substring text 1 -1) 'svg-tag-keyboard-face 1 1 2))

(setq svg-tag-tags
        '((":TODO:"                     . svg-tag-todo)
          (":NOTE:"                     . svg-tag-note)
          ("\([0-9a-zA-Z]\)"            . svg-tag-round)
          ("\([0-9a-zA-Z][0-9a-zA-Z]\)" . svg-tag-quasi-round)
          ("|[0-9a-zA-Z- ]+?|"          . svg-tag-keyboard)))

(svg-tag-mode 1)
