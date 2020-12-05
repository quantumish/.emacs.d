(defface svg-tag-bad-face
  '((t :foreground "white" :background "red" :box (:line-width 1 :color "red" :style nil)
       :family "Roboto Mono" :weight light :height 120))
  "Face for bad tag" :group nil)

(defface svg-tag-good-face
  '((t :foreground "white" :background "green" :box (:line-width 1 :color "green" :style nil)
       :family "Roboto Mono" :weight light :height 120))
  "Face for good tag" :group nil)

(defface svg-tag-note-face
  '((t :foreground "black" :background "white" :box "black"
       :family "Roboto Mono" :weight light :height 120))
  "Face for note tag" :group nil)

(defface svg-tag-note-face
  '((t :foreground "black" :background "orange" :box "orange"
       :family "Roboto Mono" :weight light :height 120))
  "Face for warn tag" :group nil)

(defface svg-tag-keyboard-face
  '((t :foreground "#333333" :background "#f9f9f9" :box "#333333"
       :family "Roboto Mono" :weight light :height 120))
  "Face for keyboard bindings tag" :group nil)

(setq svg-tag-todo
      (svg-tag-make "TODO" nil 1 1 2))

(setq svg-tag-note
      (svg-tag-make "NOTE" 'svg-tag-note-face 1 1 2))

(setq svg-tag-fixme
      (svg-tag-make "FIXME" 'svg-tag-bad-face 1 1 2))

(setq svg-tag-warn
      (svg-tag-make "WARN" 'svg-tag-bad-face 1 1 2))

(setq svg-tag-bad
      (svg-tag-make "BAD" 'svg-tag-bad-face 1 1 2))

(setq svg-tag-good
      (svg-tag-make "GOOD" 'svg-tag-good-face 1 1 2))

(defun svg-tag-round (text)
  (svg-tag-make (substring text 1 -1) 'svg-tag-note-face 1 1 12))

(defun svg-tag-quasi-round (text)
  (svg-tag-make (substring text 1 -1) 'svg-tag-note-face 1 1 8))

(defun svg-tag-keyboard (text)
  (svg-tag-make (substring text 1 -1) 'svg-tag-keyboard-face 1 1 2))

(setq svg-tag-tags 
        '((":TODO:"                     . svg-tag-todo)
          (":NOTE:"                     . svg-tag-note)
          (":BAD:"                      . svg-tag-bad)
          (":FIXME:"                    . svg-tag-fixme)
          (":WARN:"                     . svg-tag-warn)
          (":GOOD:"                     . svg-tag-good)
          ("\([0-9a-zA-Z]\)"            . svg-tag-round)
          ("\([0-9a-zA-Z][0-9a-zA-Z]\)" . svg-tag-quasi-round)
          ("|[0-9a-zA-Z- ]+?|"          . svg-tag-keyboard)))

