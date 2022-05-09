;;;; init.el --- -*- lexical-binding: t -*-


(setq gc-cons-threshold (* 1024 1024 1024))
(defvar warning-minimum-level :emergency)

(defvar config-dir (file-name-directory load-file-name)
  "The root dir of the Emacs config.")

(setq custom-file (expand-file-name "custom.el" config-dir))
(if (file-exists-p custom-file) (load custom-file))


(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths)
      (let ((default-directory (expand-file-name (concat user-emacs-directory path))))
	(add-to-list 'load-path default-directory)
	(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
	    (normal-top-level-add-subdirs-to-load-path))))))

(add-to-load-path (format ".cask/%s" emacs-version))

(eval-when-compile
  (require 'use-package))

(use-package init-loader
  :functions init-loader-load
  :init (setq init-loader-byte-compile t)
  :config (init-loader-load))
