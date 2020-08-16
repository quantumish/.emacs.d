;; -------------------------------------------------------------------
;; A proof of concept for a multi header or mode line
;;
;; Multi line header or mode line is made possible by generating an
;; SVG image made of two small lines of text. It is certainly memory
;; hungry but it seems to be fast enough to display line/column while
;; typing text. It can probably be extended in a number of ways.
;;
;; Feel free to modify it for your own needs.
;; -------------------------------------------------------------------
(require 'svg)

(defun tag (line-1 font-size-1 font-family-1 foreground-1
            line-2 font-size-2 font-family-2 foreground-2
            left)
  (let* ((font-size-1   (or font-size-1 14))
         (char-width-1  (* font-size-1 0.6))
         (char-height-1 (+ font-size-1 0.0))
         (width-1       (* char-width-1 20))
         (height-1      (+ (* char-height-1 2) 1))

         (font-size-2   (or font-size-2 14))
         (char-width-2  (* font-size-2 0.6))
         (char-height-2 (+ font-size-2 0.0))
         (width-2       (* char-width-2 20))
         (height-2      (+ (* char-height-2 2) 1))

         (width         (max width-1 width-2))
         (height        (max height-1 height-2))
         
         (x1 (if left 0 (- width (* char-width-1 (+ (length line-1) .0)))))
         (x2 (if left 0 (- width (* char-width-2 (+ (length line-2) .0)))))
         (y1 char-height-1)
         (y2 (+ (* char-height-2 2) 1))
         (svg (svg-create (+ width 10) (+ height 5))))
    (svg-text svg line-1
              :font-family font-family-1
              :font-size font-size-1 :fill foreground-1
              :x x1 :y y1)
                
    (svg-text svg line-2
              :font-family font-family-2  
              :font-size font-size-2 :fill foreground-2
              :x x2 :y y2)
    svg))

(define-key mode-line-major-mode-keymap [header-line]
  (lookup-key mode-line-major-mode-keymap [mode-line]))

(defun multiline-mode-line-render (left right)
  (let* ((available-width (+ (- (window-width) (length left) 2))))
    (format (format "%%s %%%ds" available-width) left right)))
(setq-default header-line-format
     '((:eval
       (mode-line-render
        (format-mode-line 
          (propertize (make-string 20 ?\ )
                      'display (svg-image
                        (tag (format-mode-line "%m") 12 "Roboto Mono Light" "#00008b"
                             (format-mode-line "%b") 14 "Roboto Mono"       "black"
                             t) :ascent 100)))
        (format-mode-line
          (propertize (make-string 18 ?\ )
                      'display (svg-image
                        (tag (format-mode-line "GNU Emacs 26.3  ") 12 "Roboto Mono Light" "#00008b"
                             (format-mode-line "%4l:%2c") 12 "Roboto Mono Light" "#999999"
                             nil) :ascent 100)))))))
(provide 'multiline-modeline)
