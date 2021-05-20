(use-package yasnippet
  :init (yas-global-mode))

(use-package aas
  :hook (LaTeX-mode . ass-activate-for-major-mode)
  :hook (org-mode . ass-activate-for-major-mode)
  :hook (c-mode . ass-activate-for-major-mode)
  :hook (c++-mode . ass-activate-for-major-mode)
  :config
  (aas-set-snippets 'c-mode
                    "u64" "uint64_t"
					"u32" "uint32_t"
					"u16" "uint16_t"
					"u8" "uint8_t"
				    "i64" "int64_t"
					"i32" "int32_t"
					"i16" "int16_t"
					"i8" "int8_t"
					"sz" "size_t")
  (aas-set-snippets 'c++-mode
					"mxf" "Eigen::MatrixXf"
                    "mxd" "Eigen::MatrixXd" 
					"v2f" "Eigen::Vector2f" 
					"v2d" "Eigen::Vector2d" 
					"v2i" "Eigen::Vector2i" 
					"v3f" "Eigen::Vector3f" 
					"v3d" "Eigen::Vector3d" 
					"v3i" "Eigen::Vector3i"))


