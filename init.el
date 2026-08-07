;; -*- lexical-binding: t; -*-

(defun load-module (name)
  (let* ((filename (concat name ".el"))
         (path (file-name-concat user-emacs-directory "modules" filename))
         (fullpath (expand-file-name path)))
    (load-file fullpath)))

(load-module "packages")
(load-module "tweaks")

(server-start)

(when (eq system-type 'gnu/linux)
  (load-module "system"))

(load-module "themeing")
(load-module "vompec")
(load-module "introspection")
(load-module "ide")
(load-module "org")
(load-module "firefox")
(load-module "pdf")
(load-module "music")
