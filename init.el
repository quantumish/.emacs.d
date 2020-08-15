;; Some sane defaults
;; Copyright 2020 Nicolas P. Rougier
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>

(setq inhibit-startup-screen t)
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
(setq initial-major-mode 'org-mode)
(setq-default indent-tabs-mode nil)
(setq pop-up-windows nil)
(tool-bar-mode 0)
(tooltip-mode  0)
(scroll-bar-mode 0)
(global-auto-revert-mode t)
(server-start)

(defun custom/kill-this-buffer ()
  (interactive) (kill-buffer (current-buffer)))
(global-set-key (kbd "C-x k") 'custom/kill-this-buffer)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(save-place-mode 1)

(setq mac-command-modifier 'super mac-option-modifier 'meta)

(ido-mode t)
(setq ido-enable-flex-matching t)

(setq-default tab-width 4)
(setq-default c-basic-offset 4)

(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-x C-x") 'execute-extended-command)
(global-set-key (kbd "C-x C-z") nil)


(global-set-key "\C-t" #'transpose-lines)
(define-key ctl-x-map "\C-t" #'transpose-chars)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; A very minimal but elegant theme
;; Copyright 2020 Nicolas P. Rougier
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>

;; Only necessary for the splash screen mockup
;; -------------------------------------------------------------------
(with-eval-after-load 'org
  (setq org-display-inline-images t)
  (setq org-redisplay-inline-images t)
  (setq org-startup-with-inline-images "inlineimages")
  (setq org-hide-emphasis-markers t)
  (setq org-confirm-elisp-link-function nil)
  (setq org-ellipsis " …")
  (setq org-link-frame-setup '((file . find-file))))
;; -------------------------------------------------------------------


;; Default font and frame size
(set-face-font 'default "Roboto Mono Light 14")
(setq default-frame-alist
      (append (list '(width  . 73) '(height . 41)
                    '(vertical-scroll-bars . nil)
                    '(internal-border-width . 24)
                    '(font . "Roboto Mono Light 14"))))
(set-frame-parameter (selected-frame)
                     'internal-border-width 24)

;; Line spacing, can be 0 for code and 1 or 2 for text
(setq-default line-spacing 0)

;; Underline line at descent position, not baseline position
(setq x-underline-at-descent-line t)

;; No ugly button for checkboxes
(setq widget-image-enable nil)

;; Line cursor and no blink
(set-default 'cursor-type  '(bar . 1))
(blink-cursor-mode 0)

;; Small fringe on the right only
(fringe-mode '(0 . 6))

;; When we set a face, we take care of removing any previous settings
(defun set-face (face style)
  "Reset a face and make it inherit style."
  (set-face-attribute face nil
   :foreground 'unspecified :background 'unspecified
   :family     'unspecified :slant      'unspecified
   :weight     'unspecified :height     'unspecified
   :underline  'unspecified :overline   'unspecified
   :box        'unspecified :inherit    style))


;; Default face
(set-background-color "#ffffff")
(set-foreground-color "#333333")
(setq frame-background-mode 'light)

(defface face-critical '((t :foreground "#ffffff"
                            :background "#ff6347"))
"Critical face is for information that requires immediate action
or attention. It should be of high constrast when compared to
other faces. This can be realized (for example) by setting an
intense background color, typically a shade of red.")

(defface face-popout '((t :foreground "#ffa07a"))
"Popout face is used for information that need to attract
attention. To achieve such effect, the hue of the face has to be
sufficiently different from other faces such that it attracts
attention through the popout effect.")

(defface face-strong '((t :weight regular))
"Strong face is used for information of a structural nature like
for example titles, keywords, directory, etc. It has to be the
same color as the default color and only the weight differs by
one level (e.g., light/regular or regular/bold).")

(defface face-salient '((t :foreground "#00008b"))
"Salient face is used for information that are more important
than the others while being of the same nature. It is made by
using a different hue with approximately the same intensity as
the default face.")

(defface face-faded '((t :foreground "#999999"))
"Faded face is for information that are less important than the
others while being of the same nature. It is made by using the
same hue as the default but with a smaller intensity than the
default. It can be used for comments, secondary information and
also replace italic (which is generally abused anyway).")

(defface face-subtle '((t :background "#f0f0f0"))
"Subtle face is used to suggest a physical area on the screen
without disturbing the reading of information. This can be made
by setting a very light background color that is barely perceptible.")


;; Structural
(set-face 'bold                                          'face-strong)
(set-face 'italic                                         'face-faded)
(set-face 'bold-italic                                   'face-strong)
(set-face 'region                                        'face-subtle)
(set-face 'highlight                                     'face-subtle)
(set-face 'fixed-pitch                                       'default)
(set-face 'fixed-pitch-serif                                 'default)
(set-face 'variable-pitch                                    'default)
(set-face 'cursor                                            'default)
(set-face-attribute 'cursor nil
                               :background (face-foreground 'default))

;; Modeline
(set-face-attribute 'mode-line nil
		    :height .85
                    :foreground (face-background 'default)
                    :background (face-foreground 'default)
		    :box `(:line-width 2
                           :color ,(face-foreground 'default)
			   :style nil))

(set-face 'mode-line-highlight 'face-popout)
(set-face 'mode-line-emphasis  'face-strong)
(set-face 'mode-line-buffer-id 'face-strong)
(set-face-attribute 'mode-line-inactive nil
		    :height .85
                    :foreground (face-foreground 'face-faded)
                    :background (face-background 'face-subtle)
		    :box `(:line-width 2
                           :color ,(face-background 'face-subtle)
			   :style nil))

;; Semantic
(set-face 'shadow                                         'face-faded)
(set-face 'success                                      'face-salient)
(set-face 'warning                                       'face-popout)
(set-face 'error                                       'face-critical)

;; General
(set-face 'header-line                    '(face-salient face-strong))
(set-face 'buffer-menu-buffer                            'face-strong)
(set-face 'minibuffer-prompt                             'face-strong)
(set-face 'link                                         'face-salient)
(set-face 'fringe                                         'face-faded)
(set-face 'isearch                                       'face-strong)
(set-face 'isearch-fail                                   'face-faded)
(set-face 'lazy-highlight                                'face-subtle)
(set-face 'trailing-whitespace                           'face-subtle)
(set-face 'show-paren-match                              'face-popout)
(set-face 'show-paren-mismatch                           'face-normal)
(set-face-attribute 'tooltip nil                         :height 0.85)

;; Programmation mode
(set-face 'font-lock-comment-face                         'face-faded)
(set-face 'font-lock-doc-face                             'face-faded)
(set-face 'font-lock-string-face                         'face-popout)
(set-face 'font-lock-constant-face                      'face-salient)
(set-face 'font-lock-warning-face                        'face-popout)
(set-face 'font-lock-function-name-face                  'face-strong)
(set-face 'font-lock-variable-name-face                  'face-strong)
(set-face 'font-lock-builtin-face                       'face-salient)
(set-face 'font-lock-type-face                          'face-salient)
(set-face 'font-lock-keyword-face                       'face-salient)

;; Documentation
(with-eval-after-load 'info
  (set-face 'info-menu-header                            'face-strong)
  (set-face 'info-header-node                            'face-normal)
  (set-face 'Info-quoted                                  'face-faded)
  (set-face 'info-title-1                                'face-strong)
  (set-face 'info-title-2                                'face-strong)
  (set-face 'info-title-3                                'face-strong)
  (set-face 'info-title-4                               'face-strong))

;; Message
(with-eval-after-load 'message
  (set-face 'message-cited-text                           'face-faded)
  (set-face 'message-header-cc                               'default)
  (set-face 'message-header-name                         'face-strong)
  (set-face 'message-header-newsgroups                       'default)
  (set-face 'message-header-other                            'default)
  (set-face 'message-header-subject                     'face-salient)
  (set-face 'message-header-to                          'face-salient)
  (set-face 'message-header-xheader                          'default)
  (set-face 'message-mml                                 'face-popout)
  (set-face 'message-separator                           'face-faded))

;; Interface
(with-eval-after-load 'cus-edit
  (set-face 'widget-field                                'face-subtle)
  (set-face 'widget-button                               'face-strong)
  (set-face 'widget-single-line-field                    'face-subtle)
  (set-face 'custom-group-subtitle                       'face-strong)
  (set-face 'custom-group-tag                            'face-strong)
  (set-face 'custom-group-tag-1                          'face-strong)
  (set-face 'custom-comment                               'face-faded)
  (set-face 'custom-comment-tag                           'face-faded)
  (set-face 'custom-changed                             'face-salient)
  (set-face 'custom-modified                            'face-salient)
  (set-face 'custom-face-tag                             'face-strong)
  (set-face 'custom-variable-tag                         'face-strong)
  (set-face 'custom-invalid                              'face-popout)
  (set-face 'custom-visibility                           'face-popout)
  (set-face 'custom-state                               'face-salient)
  (set-face 'custom-link                                'face-salient)
  (set-face-attribute 'custom-button nil
                      :foreground (face-foreground 'face-faded)
                      :background (face-background 'face-subtle)
                      :box `(:line-width 1
                             :color ,(face-foreground 'face-faded)
                             :style nil))
  (set-face-attribute 'custom-button-mouse nil
                      :inherit 'face-subtle
                      :box `(:line-width 1
                             :color ,(face-foreground 'face-subtle)
                             :style nil))
  (set-face-attribute 'custom-button-pressed nil
                      :foreground "white"
                      :background (face-foreground 'face-salient)
                      :inherit 'face-salient
                      :box `(:line-width 1
                             :color ,(face-foreground 'face-salient)
                             :style nil)
                      :inverse-video nil))

;; Package
(with-eval-after-load 'package
  (set-face 'package-description                             'default)
  (set-face 'package-help-section-name                       'default)
  (set-face 'package-name                               'face-salient)
  (set-face 'package-status-avail-obso                    'face-faded)
  (set-face 'package-status-available                        'default)
  (set-face 'package-status-built-in                    'face-salient)
  (set-face 'package-status-dependency                  'face-salient)
  (set-face 'package-status-disabled                      'face-faded)
  (set-face 'package-status-external                         'default)
  (set-face 'package-status-held                             'default)
  (set-face 'package-status-incompat                      'face-faded)
  (set-face 'package-status-installed                   'face-salient)
  (set-face 'package-status-new                              'default)
  (set-face 'package-status-unsigned                         'default)

  ;; Button face is hardcoded, we have to redefine the relevant function
  (defun package-make-button (text &rest properties)
    "Insert button labeled TEXT with button PROPERTIES at point.
PROPERTIES are passed to `insert-text-button', for which this
function is a convenience wrapper used by `describe-package-1'."
    (let ((button-text (if (display-graphic-p)
                           text (concat "[" text "]")))
          (button-face (if (display-graphic-p)
                           '(:box `(:line-width 1
                             :color "#999999":style nil)
                            :foreground "#999999"
                            :background "#F0F0F0")
                         'link)))
      (apply #'insert-text-button button-text
             'face button-face 'follow-link t
             properties)))
  )

;; Flyspell
(with-eval-after-load 'flyspell
  (set-face 'flyspell-duplicate                         'face-popout)
  (set-face 'flyspell-incorrect                         'face-popout))

;; Ido 
(with-eval-after-load 'ido
  (set-face 'ido-first-match                            'face-salient)
  (set-face 'ido-only-match                               'face-faded)
  (set-face 'ido-subdir                                 'face-strong))

;; Diff
(with-eval-after-load 'diff-mode
  (set-face 'diff-header                                  'face-faded)
  (set-face 'diff-file-header                            'face-strong)
  (set-face 'diff-context                                    'default)
  (set-face 'diff-removed                                 'face-faded)
  (set-face 'diff-changed                                'face-popout)
  (set-face 'diff-added                                 'face-salient)
  (set-face 'diff-refine-added            '(face-salient face-strong))
  (set-face 'diff-refine-changed                         'face-popout)
  (set-face 'diff-refine-removed                          'face-faded)
  (set-face-attribute     'diff-refine-removed nil :strike-through t))


;; Org
(with-eval-after-load 'diff-mode
  (set-face 'org-level-3                                 'face-strong)
  (set-face 'org-level-4                                 'face-strong)
  (set-face 'org-level-5                                 'face-strong)
  (set-face 'org-level-6                                 'face-strong)
  (set-face 'org-level-7                                 'face-strong)
  (set-face 'org-level-8                                 'face-strong))

;; Term
(with-eval-after-load 'term
  (setq eterm-256color-disable-bold nil)
  (set-face 'term-bold                                   'face-strong)
  (set-face-attribute 'term-color-black nil
                                :foreground (face-foreground 'default)
                               :background (face-foreground 'default))
  (set-face-attribute 'term-color-white nil
                              :foreground "white" :background "white")
  (set-face-attribute 'term-color-blue nil
                          :foreground "#42A5F5" :background "#BBDEFB")
  (set-face-attribute 'term-color-cyan nil
                          :foreground "#26C6DA" :background "#B2EBF2")
  (set-face-attribute 'term-color-green nil
                          :foreground "#66BB6A" :background "#C8E6C9")
  (set-face-attribute 'term-color-magenta nil
                          :foreground "#AB47BC" :background "#E1BEE7")
  (set-face-attribute 'term-color-red nil
                          :foreground "#EF5350" :background "#FFCDD2")
  (set-face-attribute 'term-color-yellow nil
                         :foreground "#FFEE58" :background "#FFF9C4"))


;; Header and mode line
(set-face-attribute 'header-line nil
		    :height 140
                    :underline t
                    :underline "black"
		    :weight 'light
                    :foreground "black"
		    :background "white"
                    :box `(:line-width 3 :color "white" :style nil))
(set-face-attribute 'mode-line nil
                    :height 10
                    :underline "black"
                    :background "white"
		    :foreground "white"
                    :box nil)
(set-face 'mode-line-inactive 'mode-line)
(set-face 'mode-line-buffer-id 'default)
(set-face 'header-line-highlight 'face-faded)

(defun mode-line-render (left right)
  "Return a string of `window-width' length containing left, and
   right aligned respectively."
  (let* ((available-width (- (window-total-width) (length left) )))
    (format (format "%%s %%%ds" available-width) left right)))
(define-key mode-line-major-mode-keymap [header-line]
  (lookup-key mode-line-major-mode-keymap [mode-line]))

(setq-default mode-line-format '(""))
(setq-default header-line-format
 '(:eval (mode-line-render
  (format-mode-line
   (list
    (propertize "☰"
                'face `(:weight regular)
                'mouse-face 'header-line-highlight
                'help-echo  "Major mode menu"
                'local-map   mode-line-major-mode-keymap)
    " %b "
    '(:eval (if (and buffer-file-name (buffer-modified-p))
                (propertize "(modified)"
             'face `(:foreground ,(face-foreground 'face-faded)))))))
  (format-mode-line
   (propertize "%3l:%2c "
	'face `(:foreground ,(face-foreground 'face-faded)))))))

;; Rounded boxes using SVG:  
(require 'svg)
(defun tag (text &optional foreground background font-size)
 (let* ((font-size   (or font-size 12))
        ;; The char-width ratio depends on the font family
        (char-width  (* font-size 0.58))
        (char-height (+ font-size 1))
        (hmargin char-width)
        (vmargin (* font-size 0.175))
        (radius  (/ font-size 4))
        (background (or background "blue"))
        (foreground (or foreground "white"))
        (width  (+ (* char-width (length text)) (* hmargin 2)))
        (height (+ char-height (* vmargin 2)))
        (svg (svg-create width height)))
 (svg-rectangle svg 0 0 width height :fill background :rx radius)
 (svg-text svg text
           :font-family "Roboto Mono" :font-weight "light"
           :font-size font-size :fill foreground
           :x hmargin :y char-height)
 (insert-image (svg-image svg :ascent 'center)))
 )

(setq org-ellipsis "…")
(set-face-attribute 'org-ellipsis nil
  :foreground "#999999"
  :underline nil
  :weight 'light)
(set-face-attribute 'org-special-keyword nil
  :foreground "#999999"
  :weight 'light)

(set-face-attribute 'org-agenda-date nil
  :foreground "#333333"
  :weight 'regular)

(set-face-attribute 'org-agenda-date-weekend nil
  :foreground "#333333"
  :weight 'regular)

(set-face-attribute 'org-agenda-date-today nil
  :foreground "#333333"
  :weight 'regular)

;; Prettify symbols mode is nice
(add-hook 'org-mode-hook (lambda ()
   "Beautify Org Checkbox Symbol"
   (push '("TODO" . "") prettify-symbols-alist)
   (push '("DONE" . "" ) prettify-symbols-alist)
   (push '("WAIT" . "" ) prettify-symbols-alist)
   (push '("NOPE" . "" ) prettify-symbols-alist)
   (push '("[#A]" . "" ) prettify-symbols-alist)
   (push '("[#B]" . "" ) prettify-symbols-alist)
   (push '("[#C]" . "" ) prettify-symbols-alist)
   (push '("#+BEGIN_SRC" . "" ) prettify-symbols-alist)
   (push '("#+END_SRC" . "―" ) prettify-symbols-alist)
   (push '(":PROPERTIES:" . "" ) prettify-symbols-alist)
   (push '(":END:" . "―" ) prettify-symbols-alist)
   (push '("#+STARTUP:" . "" ) prettify-symbols-alist)
   (push '("#+TITLE: " . "" ) prettify-symbols-alist)
   (push '("#+RESULTS:" . "" ) prettify-symbols-alist)
   (push '("#+NAME:" . "" ) prettify-symbols-alist)
   (prettify-symbols-mode)))

(defface org-checkbox-done-text
  '((t (:foreground "#71696A" :strike-through t)))
  "Face for the text part of a checked org-mode checkbox.")

(setq org-priority-faces '((?A . (:foreground "#f5381b" :weight 'bold))
                           (?B . (:foreground "#f5cb22"))
                           (?C . (:foreground "#6cad50"))))

(setq org-todo-keyword-faces
      '(("TODO" . "#999999") ("WAIT" . "#cfd1d1")
        ("DONE" . "#6cad50") ("NOPE" . "#cfd1d1")))

(setq org-fontify-done-headline t)

(custom-set-faces
 '(org-headline-done
            ((((class color) (class color) (min-colors 16))
              (:foreground "#cfd1d1")))))

;; I'm tired of lsp-ui looking terrible
(lsp-ui-mode 1)
(set-face-attribute 'lsp-ui-sideline-code-action nil
  :foreground "#999999")
(setq lsp-ui-doc-header t)
(custom-set-faces
   '(lsp-ui-doc-background ((t :background "#fafafa")))
   '(lsp-ui-doc-header ((t :background "#f0f0f0")))
   '(lsp-ui-doc-url ((t :inherit link))))
(setq lsp-ui-doc-border "#999999")
(setq lsp-ui-doc-position 'bottom)
(setq lsp-ui-doc-delay 3)
(setq lsp-ui-sideline-delay 1)

;; Some functionality to go with it.
;; David Freifeld

;; Material colors from https://material.io/design/color/
(defconst levels
  (list "L50"  "L100" "L200" "L300" "L400"
        "L500" "L600" "L700" "L800" "L900"
        "A100" "A200" "A400" "A700"))

(defconst red
  (list "#FFEBEE" "#FFCDD2" "#EF9A9A" "#E57373" "#EF5350"
        "#F44336" "#E53935" "#D32F2F" "#C62828" "#B71C1C"
        "#FF8A80" "#FF5252" "#FF1744" "#D50000"))

(defconst pink
  (list "#FCE4EC" "#F8BBD0" "#F48FB1" "#F06292" "#EC407A"
        "#E91E63" "#D81B60" "#C2185B" "#AD1457" "#880E4F"
        "#FF80AB" "#FF4081" "#F50057" "#C51162" ))

(defconst purple
  (list "#F3E5F5" "#E1BEE7" "#CE93D8" "#BA68C8" "#AB47BC"
        "#9C27B0" "#8E24AA" "#7B1FA2" "#6A1B9A" "#4A148C"
        "#EA80FC" "#E040FB" "#D500F9" "#AA00FF" ))

(defconst deep-purple
  (list "#EDE7F6" "#D1C4E9" "#B39DDB" "#9575CD" "#7E57C2"
        "#673AB7" "#5E35B1" "#512DA8" "#4527A0" "#311B92"
        "#B388FF" "#7C4DFF" "#651FFF" "#6200EA" ))

(defconst indigo
  (list "#E8EAF6" "#C5CAE9" "#9FA8DA" "#7986CB" "#5C6BC0"
        "#3F51B5" "#3949AB" "#303F9F" "#283593" "#1A237E"
        "#8C9EFF" "#536DFE" "#3D5AFE" "#304FFE" ))

(defconst blue
  (list "#E3F2FD" "#BBDEFB" "#90CAF9" "#64B5F6" "#42A5F5"
        "#2196F3" "#1E88E5" "#1976D2" "#1565C0" "#0D47A1"
        "#82B1FF" "#448AFF" "#2979FF" "#2962FF" ))

(defconst light-blue
  (list "#E1F5FE" "#B3E5FC" "#81D4FA" "#4FC3F7" "#29B6F6"
        "#03A9F4" "#039BE5" "#0288D1" "#0277BD" "#01579B"
        "#80D8FF" "#40C4FF" "#00B0FF" "#0091EA" ))

(defconst cyan
  (list "#E0F7FA" "#B2EBF2" "#80DEEA" "#4DD0E1" "#26C6DA"
        "#00BCD4" "#00ACC1" "#0097A7" "#00838F" "#006064"
        "#84FFFF" "#18FFFF" "#00E5FF" "#00B8D4" ))

(defconst teal
  (list "#E0F2F1" "#B2DFDB" "#80CBC4" "#4DB6AC" "#26A69A"
        "#009688" "#00897B" "#00796B" "#00695C" "#004D40"
        "#A7FFEB" "#64FFDA" "#1DE9B6" "#00BFA5" ))

(defconst green
  (list "#E8F5E9" "#C8E6C9" "#A5D6A7" "#81C784" "#66BB6A"
        "#4CAF50" "#43A047" "#388E3C" "#2E7D32" "#1B5E20"
        "#B9F6CA" "#69F0AE" "#00E676" "#00C853" ))

(defconst light-green
  (list "#F1F8E9" "#DCEDC8" "#C5E1A5" "#AED581" "#9CCC65"
        "#8BC34A" "#7CB342" "#689F38" "#558B2F" "#33691E"
        "#CCFF90" "#B2FF59" "#76FF03" "#64DD17" ))

(defconst lime
  (list "#F9FBE7" "#F0F4C3" "#E6EE9C" "#DCE775" "#D4E157"
        "#CDDC39" "#C0CA33" "#AFB42B" "#9E9D24" "#827717"
        "#F4FF81" "#EEFF41" "#C6FF00" "#AEEA00" ))

(defconst yellow
  (list "#FFFDE7" "#FFF9C4" "#FFF59D" "#FFF176" "#FFEE58"
        "#FFEB3B" "#FDD835" "#FBC02D" "#F9A825" "#F57F17"
        "#FFFF8D" "#FFFF00" "#FFEA00" "#FFD600" ))

(defconst amber
  (list "#FFF8E1" "#FFECB3" "#FFE082" "#FFD54F" "#FFCA28"
        "#FFC107" "#FFB300" "#FFA000" "#FF8F00" "#FF6F00"
        "#FFE57F" "#FFD740" "#FFC400" "#FFAB00" ))

(defconst orange
  (list "#FFF3E0" "#FFE0B2" "#FFCC80" "#FFB74D" "#FFA726"
        "#FF9800" "#FB8C00" "#F57C00" "#EF6C00" "#E65100"
        "#FFD180" "#FFAB40" "#FF9100" "#FF6D00" ))

(defconst deep-orange
  (list "#FBE9E7" "#FFCCBC" "#FFAB91" "#FF8A65" "#FF7043"
        "#FF5722" "#F4511E" "#E64A19" "#D84315" "#BF360C"
        "#FF9E80" "#FF6E40" "#FF3D00" "#DD2C00" ))

(defconst brown
  (list "#EFEBE9" "#D7CCC8" "#BCAAA4" "#A1887F" "#8D6E63"
        "#795548" "#6D4C41" "#5D4037" "#4E342E" "#3E2723" ))

(defconst grey
  (list "#FAFAFA" "#F5F5F5" "#EEEEEE" "#E0E0E0" "#BDBDBD"
        "#9E9E9E" "#757575" "#616161" "#424242" "#212121" ))

(defconst blue-grey
  (list "#ECEFF1" "#CFD8DC" "#B0BEC5" "#90A4AE" "#78909C"
        "#607D8B" "#546E7A" "#455A64" "#37474F" "#263238" ))


(require 'cl-lib)
(defun material-color (palette level)
   "Return the color from the given palette and specified level."
   (nth (cl-position level levels :test #'equal) palette))

(require 'calendar)
(require 'holidays)
(require 'material-colors)

(setq org-agenda-start-on-weekday 1)
(setq calendar-mark-holidays-flag t)
(setq material-shade deep-orange)

(defface calendar-face-level-1 nil "")
(defface calendar-face-level-2 nil "")
(defface calendar-face-level-3 nil "")
(defface calendar-face-level-4 nil "")
(defface calendar-face-level-5 nil "")
(defface calendar-face-level-6 nil "")
(defface calendar-face-level-7 nil "")
(defface calendar-face-level-8 nil "")
(defface calendar-face-level-9 nil "")
(defface calendar-face-vacation nil "")
(defface calendar-face-weekend nil "")


(set-face-attribute 'calendar-face-level-1 nil
                    :background (material-color material-shade "L50"))
(set-face-attribute 'calendar-face-level-2 nil
                    :background (material-color material-shade "L100"))
(set-face-attribute 'calendar-face-level-3 nil
                    :background (material-color material-shade "L200"))
(set-face-attribute 'calendar-face-level-4 nil
                    :background (material-color material-shade "L300"))
(set-face-attribute 'calendar-face-level-5 nil
                    :inherit 'face-strong
                    :foreground "white"
                    :background (material-color material-shade "L400"))
(set-face-attribute 'calendar-face-level-6 nil
                    :inherit 'face-strong
                    :foreground "white"
                    :background (material-color material-shade "L500"))
(set-face-attribute 'calendar-face-level-7 nil
                    :inherit 'face-strong
                    :foreground "white"
                    :background (material-color material-shade "L600"))
(set-face-attribute 'calendar-face-level-8 nil
                    :inherit 'face-strong
                    :foreground "white"
                    :background (material-color material-shade "L700"))
(set-face-attribute 'calendar-face-level-9 nil
                    :inherit 'face-strong
                    :foreground "white"
                    :background (material-color material-shade "L800"))
(set-face-attribute 'calendar-face-vacation nil
                    :inherit 'face-strong
                    :background (material-color purple "L50")
                    :foreground (material-color blue-grey "L900"))
(set-face-attribute 'calendar-face-weekend nil
                    :inherit 'default
                    :background "white"
                    :foreground (material-color blue-grey "L300"))



(defadvice calendar-generate-month
  (after highlight-weekend-days (month year indent) activate)
  "Highlight weekend days"
  (dotimes (i 31)
    (let* ((date (list month (1+ i) year))
           (file "~/Dropbox/org/schedule.org")
           (entries (org-agenda-get-day-entries file date))
           (count (length entries)))
       
      (cond ((= count 0) (if (and (not (equal date (calendar-current-date)))
                                  (or (= (calendar-day-of-week date) 0)
                                      (= (calendar-day-of-week date) 6)))
                             (calendar-mark-visible-date date 'calendar-face-weekend)))
            ((= count 1) (calendar-mark-visible-date date 'calendar-face-level-1))
            ((= count 2) (calendar-mark-visible-date date 'calendar-face-level-2))
            ((= count 3) (calendar-mark-visible-date date 'calendar-face-level-3))
            ((= count 4) (calendar-mark-visible-date date 'calendar-face-level-4))
            ((= count 5) (calendar-mark-visible-date date 'calendar-face-level-5))
            ((= count 6) (calendar-mark-visible-date date 'calendar-face-level-6))
            ((= count 7) (calendar-mark-visible-date date 'calendar-face-level-7))
            ((= count 8) (calendar-mark-visible-date date 'calendar-face-level-8))
            (t           (calendar-mark-visible-date date 'calendar-face-level-9)))
      )))

(defun calendar-cursor-to-visible-date (date)
  "Move the cursor to date (if on the screen)"
  (let* ((month        (- (calendar-extract-month date) 1))
         (day          (- (calendar-extract-day date) 1))
         (year         (calendar-extract-year date))
         (month-start  (calendar-day-of-week (list (+ month 1) 1 year)))
         (month-start  (% (+ month-start 6) 7))
         (month-width  25)
         (month-height 9)
         (month-col    (* (% month 3) month-width))
         (month-row    (+ (* (/ month 3) month-height) 3))
         (day-col      (* (% (+ day month-start) 7) 3))
         (day-row      (/ (+ day month-start) 7))
         (row          (+ month-row day-row))
         (col          (+ month-col day-col 1 1)))
    (goto-line      row)
    (move-to-column col)
    ))

(defun new-calendar-frame (char-width char-height)
  ""
  (interactive)
  (select-frame (make-frame))
  (set-frame-width (selected-frame) char-width)
  (set-frame-height (selected-frame) char-height)
  (set-frame-position (selected-frame)
                      (/ (- (display-pixel-width)  (frame-outer-width))  2)
                      (/ (- (display-pixel-height) (frame-outer-height)) 2))
  (x-focus-frame nil)
  (switch-to-buffer (generate-new-buffer "*Year Calendar*"))
  (local-set-key (kbd "C-x C-c") 'kill-and-close)
  (setq header-line-format nil)
  (setq mode-line-format nil))

(defun year-calendar (&optional year)
  ""
  (interactive)
  (new-calendar-frame 74 36)
  (let* ((month 0)
         (year (if year year (string-to-number (format-time-string "%Y" )))))
    (switch-to-buffer (get-buffer-create calendar-buffer))
    (when (not (eq major-mode 'calendar-mode))
      (calendar-mode))
    (setq buffer-read-only nil)
    (erase-buffer)
    (dotimes (j 4)
      (dotimes (i 3)
        (calendar-generate-month
          (setq month (+ month 1))
          year
          (+ 1 (* 25 i))))
      (goto-char (point-max))
      (insert (make-string (- 10 (count-lines (point-min) (point-max))) ?\n))
      (widen)
      (goto-char (point-max))
      (narrow-to-region (point-max) (point-max)))
    (widen)
    (goto-char (point-min))
    (setq buffer-read-only t)
    (setq header-line-format nil)
    (setq mode-line-format nil)
    (let ((displayed-month  2) (displayed-year 2020))  (calendar-mark-holidays))
    (let ((displayed-month  5) (displayed-year 2020))  (calendar-mark-holidays))
    (let ((displayed-month  8) (displayed-year 2020))  (calendar-mark-holidays))
    (let ((displayed-month 11) (displayed-year 2020))  (calendar-mark-holidays))
    (calendar-cursor-to-visible-date (calendar-current-date))))

(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(autoload 'gfm-mode "markdown-mode"
   "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

(global-flycheck-mode)
(yas-global-mode 1)

(defun code-visuals-hook ()
  ;; Makes code buffers look nicer
  (olivetti-mode 1)
  (olivetti-set-width 100)
  (focus-mode 1)
  (visual-line-mode 1))
(defun code-features-hook ()
  ;; LSP, flycheck, and co.
  (lsp)
  (lsp-ui-mode 1)
  (flycheck-mode 1)
  (visual-line-mode 1))
(add-hook 'c-mode-common-hook 'code-visuals-hook)
(add-hook 'python-mode-hook 'code-visuals-hook)

;; --------------------------
;; Org Mode functionality
;; --------------------------

;; Load extra modules
(add-to-list 'load-path "~/.emacs.d/lisp/")
(load "org-depend.el") ;; Sequential projects like OmniFocus
(load "org-checklist.el") ;; Reset checkboxes for repeating tasks
(setq org-modules (append org-modules '(org-habit))) ;; Habit-tracking with Org Mode
(setq org-modules (append org-modules '(org-crypt))) ;; Encryption
(setq org-modules (append org-modules '(org-id))) ;; Unique headline identifiers

;; Define keywords
(setq org-todo-keywords '((sequence "TODO(t)" "WAIT(w)" "|" "DONE(d)" "NOPE(n)")))

;; Enable Org Babel features 
(org-babel-do-load-languages ;; More languages!
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (latex . t)
   (shell . t)
   (C . t)
   (makefile . t)
   (gnuplot . t)
   (haskell . t)))
(setq org-confirm-babel-evaluate nil) ;; Don't ask me if I want to execute my code or not
(setq org-src-tab-acts-natively t) ;; Indentation fix

;; Enable org link features
(org-link-set-parameters 
 "run"
 :follow #'org-babel-ref-resolve) ;; Allow execution of Org Babel code from links
(add-to-list 'org-file-apps '(directory . emacs)) ;; Allow links to open directories in Dired


;; Define agenda files
(setq org-agenda-files '("~/Dropbox/org/inbox.org"
                         "~/Dropbox/org/projects.org"
                         "~/Dropbox/org/schedule.org"))

;; Define "perspectives" that are seen throughout all agenda views
(setq org-super-agenda-groups '((:name "Today"
				:time-grid t
				:scheduled today)
			   (:name "Due today"
				:deadline today)
			   (:name "Important"
				:priority "A")
			   (:name "Overdue"
				:deadline past)
			   (:name "Due soon"
				:deadline future)
			   (:name "Waiting"
			       :todo "WAIT")))

;; Define custom agenda views
(setq org-agenda-custom-commands 
      '(("o" "At the office" tags-todo "@office"
         ((org-agenda-overriding-header "Office")))))

;; GTD-ish capture templates
(setq org-capture-templates '(("t" "Todo [inbox]" entry
                               (file+headline "~/Dropbox/org/inbox.org" "Tasks")
                               "* TODO %i%?")
                              ("s" "Schedule" entry
                               (file+headline "~/Dropbox/org/schedule.org" "Schedule")
                               "* %i%? \n %U")))

;; Project generation function from Karl Voit
(defun mark-as-project ()
"This function makes sure that the current heading has
(1) the tag :project:
(2) has property COOKIE_DATA set to \"todo recursive\"
(3) has any TODO keyword and
(4) a leading progress indicator"
    (interactive)
    (org-toggle-tag "project" 'on)
    (org-set-property "COOKIE_DATA" "todo recursive")
    (org-back-to-heading t)
    (let* ((title (nth 4 (org-heading-components)))
           (keyword (nth 2 (org-heading-components))))
       (when (and (bound-and-true-p keyword) (string-prefix-p "[" title))
           (message "TODO keyword and progress indicator found")
           )
       (when (and (not (bound-and-true-p keyword)) (string-prefix-p "[" title))
           (message "no TODO keyword but progress indicator found")
           (forward-whitespace 1)
           (insert "NEXT ")
           )
       (when (and (not (bound-and-true-p keyword)) (not (string-prefix-p "[" title)))
           (message "no TODO keyword and no progress indicator found")
           (forward-whitespace 1)
           (insert "NEXT [/] ")
           )
       (when (and (bound-and-true-p keyword) (not (string-prefix-p "[" title)))
           (message "TODO keyword but no progress indicator found")
           (forward-whitespace 2)
           (insert "[/] ")
           )
       )
    )

(defun eos/org-id-new (&optional prefix)
  "Create a new globally unique ID.

An ID consists of two parts separated by a colon:
- a prefix
- a   unique   part   that   will   be   created   according   to
  `org-id-method'.

PREFIX  can specify  the  prefix,  the default  is  given by  the
variable  `org-id-prefix'.  However,  if  PREFIX  is  the  symbol
`none', don't  use any  prefix even if  `org-id-prefix' specifies
one.

So a typical ID could look like \"Org-4nd91V40HI\"."
  (let* ((prefix (if (eq prefix 'none)
                     ""
                   (concat (or prefix org-id-prefix)
                           "-"))) unique)
    (if (equal prefix "-")
        (setq prefix ""))
    (cond
     ((memq org-id-method
            '(uuidgen uuid))
      (setq unique (org-trim (shell-command-to-string org-id-uuid-program)))
      (unless (org-uuidgen-p unique)
        (setq unique (org-id-uuid))))
     ((eq org-id-method 'org)
      (let* ((etime (org-reverse-string (org-id-time-to-b36)))
             (postfix (if org-id-include-domain
                          (progn
                            (require 'message)
                            (concat "@"
                                    (message-make-fqdn))))))
        (setq unique (concat etime postfix))))
     (t (error "Invalid `org-id-method'")))
    (concat prefix (car (split-string unique "-")))))

(defun eos/org-custom-id-get (&optional pom create prefix)
  "Get the CUSTOM_ID property of the entry at point-or-marker POM.

If POM is nil, refer to the entry at point. If the entry does not
have an CUSTOM_ID, the function returns nil. However, when CREATE
is non nil, create a CUSTOM_ID if none is present already. PREFIX
will  be passed  through to  `eos/org-id-new'. In  any case,  the
CUSTOM_ID of the entry is returned."
  (interactive)
  (org-with-point-at pom
    (let* ((orgpath (mapconcat #'identity (org-get-outline-path) "-"))
           (heading (replace-regexp-in-string
                     "/\\|~\\|\\[\\|\\]" ""
                     (replace-regexp-in-string
                      "[[:space:]]+" "_" (if (string= orgpath "")
                                  (org-get-heading t t t t)
                                (concat orgpath "-" (org-get-heading t t t t))))))
           (id (org-entry-get nil "CUSTOM_ID")))
      (cond
       ((and id
             (stringp id)
             (string-match "\\S-" id)) id)
       (create (setq id (eos/org-id-new (concat prefix heading)))
               (org-entry-put pom "CUSTOM_ID" id)
               (org-id-add-location id
                                    (buffer-file-name (buffer-base-buffer)))
               id)))))

(defun eos/org-add-ids-to-headlines-in-file ()
  "Add CUSTOM_ID properties to all headlines in the current file
which do not already have one.

Only adds ids if the `auto-id' option is set to `t' in the file
somewhere. ie, #+OPTIONS: auto-id:t"
  (interactive)
  (save-excursion
    (widen)
    (goto-char (point-min))
    (when (re-search-forward "^#\\+OPTIONS:.*auto-id:t"
                             (point-max)
                             t)
      (org-map-entries (lambda ()
                         (eos/org-custom-id-get (point)
                                                'create))))))

(add-hook 'org-mode-hook
          (lambda ()
            (add-hook 'before-save-hook
                      (lambda ()
                        (when (and (eq major-mode 'org-mode)
                                   (eq buffer-read-only nil))
                          (eos/org-add-ids-to-headlines-in-file))))))

;; Filing
(setq org-refile-targets '(("~/Dropbox/org/projects.org" :maxlevel . 3)
                           ("~/Dropbox/org/schedule.org" :maxlevel . 2)))

(provide 'init)
;;; init.el ends here
