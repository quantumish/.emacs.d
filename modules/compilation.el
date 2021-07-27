(use-package kv)
(require 'kv)
(defvar custom-compile-cmds
  '((rustic-mode . ((debug . "cargo build")
					(release . "cargo build --release")
					(test . "cargo test")))
	(c++-mode . ((cmake . "cmake .")
				 (test . "ctest")
				 (make . "make")
				 (this . "g++ $this.cpp -std=c++17 -o $this")
				 (this-speedy . "g++ $this.cpp -O3 -std=c++17 -o $this")
				 (this-python . "g++ -shared -std=c++17 -undefined_dynamic_lookup `python3 -m pybind11 --includes` $this.cpp -o $this`python3-config --extension-suffix` -D PYTHON -fPIC")))
	(c-mode . ((make . "make")
			   (this . "gcc $this.c -o $this")
			   (this-speedy . "gcc $this.c -O3 -o $this")
			   (this-archive . "gcc $this.c -O -c -g && ar rcs $this.a $this.o")
			   (this-mpi . "mpicc $this.c -o $this")))
	(cuda-mode . ((this . "nvcc $this.cu -o $this")))
	(python-mode . ((this-types . "mypy $this.py --ignore-missing-imports --strict")
					(this-cython . "cython --embed -o $this.c $this.py -3 && sudo gcc $this.c -o $this -I/usr/include/python3.9 -lpython3.9")))
	))

(defun compile-dwim ()
  (interactive)
  (let ((list (cdr (assoc major-mode custom-compile-cmds)))) ;; Debugging is for suckers
	(ivy-read "Compilation preset: " (kvalist->keys list)
			  :preselect (car (kvalist->keys list))
			  :action (lambda (name)
						(compile
						 (replace-regexp-in-string
						  (regexp-quote "$this")
						  (file-name-sans-extension (buffer-file-name))
						  (cdr (assoc (intern-soft name) list))))))))

(use-package compile
  :ensure nil
  :config
  (setq compilation-scroll-output t)
  (setq compilation-ask-about-save nil)
  (defun compile-project ()
	(interactive)
	(let ((default-directory (projectile-project-root)))
	(call-interactively 'compile)))
  ;; :bind (:map c++-mode-map
  ;; 			  ("C-;" . compile-project)
  ;; 			  ("C-c C-;" . recompile))
  :hook
  (compilation-mode . hide-mode-line-mode)
  ; (compilation-mode . (lambda () (set-header-line 200)))
  (compilation-start . olivetti-mode)
  (compilation-start . determine-olivetti))

;; (general-def c++-mode-map
;;   "C-x n s" 'narrow-to-defun)

;; (setq shackle-default-rule '(:other t))
;; (use-package shackle  
;;   (setq shackle-rules
;; 		'((compilation-mode :ignore t))))	   

