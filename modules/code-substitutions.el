(defun my/add-visual-replacement (from to)
  "Make `prettify-symbols-mode' replace string (as FROM) with string (as TO).

Updates `prettify-symbols-alist'.  You may need to toggle
`prettify-symbols-mode' to make the changes take effect.

Each character of TO is vertically aligned using the baseline,
such that base-left of the character is aligned with base-right
of the preceding character.  Refer to `reference-point-alist'
for more information."
  (push (cons from (let ((composition nil))
					 (dolist (char (string-to-list to)
								   (nreverse (cdr composition)))
					   (push char composition)
					   (push '(Br . Bl) composition))))
		prettify-symbols-alist))

(add-hook 'c-mode-common-hook
		  (lambda ()
			(my/add-visual-replacement "uint64_t" "u64")
			(my/add-visual-replacement "uint32_t" "u32")
			(my/add-visual-replacement "uint16_t" "u16")
			(my/add-visual-replacement "uint8_t" "u8")
			(my/add-visual-replacement "int64_t" "i64")
			(my/add-visual-replacement "int32_t" "i32")
			(my/add-visual-replacement "int16_t" "i16")
			(my/add-visual-replacement "int8_t" "i8")
			(my/add-visual-replacement "size_t" "sz")
			(my/add-visual-replacement "->" "→")
			(my/add-visual-replacement ">=" "≥")
			(my/add-visual-replacement "<=" "≤")
			(my/add-visual-replacement "!=" "≠")))
(add-hook 'c++-mode-hook
		  (lambda ()
			(c-set-offset 'innamespace 0)
			(my/add-visual-replacement "Eigen::MatrixXf" "mXf")
			(my/add-visual-replacement "Eigen::MatrixXd" "mXd")
			(my/add-visual-replacement "Eigen::Vector2f" "v2f")
			(my/add-visual-replacement "Eigen::Vector2d" "v2d")
			(my/add-visual-replacement "Eigen::Vector2i" "v2i")
			(my/add-visual-replacement "Eigen::Vector3f" "v3f")
			(my/add-visual-replacement "Eigen::Vector3d" "v3d")
			(my/add-visual-replacement "Eigen::Vector3i" "v3i")
			(push '("std::" . "" ) prettify-symbols-alist)))
