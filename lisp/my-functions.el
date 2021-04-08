;;; -*- mode: emacs-lisp; coding: utf-8 -*-
;;; my-functions.el

(defun my/set-show-trailing-whitespace ()
  (setq show-trailing-whitespace t))

(defun assoc-value  (key addr) (cdr (assoc key attr)))

(defun get-name     (attr) (assoc-value 'name attr))
(defun get-geometry (attr) (assoc-value 'geometry attr))
(defun get-workarea (attr) (assoc-value 'workarea attr))
(defun get-mm-size  (attr) (assoc-value 'mm-size attr))

(defun get-pixels-x  (attr) (caddr  (get-geometry attr)))
(defun get-pixels-y  (attr) (cadddr (get-geometry attr)))
(defun get-mm-size-x (attr) (car    (get-mm-size attr)))
(defun get-mm-size-y (attr) (cadr   (get-mm-size attr)))

(defun dpi-x (attr)
  (let ((px (get-pixels-x attr))
        (x (get-mm-size-x attr)))
    (/ px (/ x 25.4))))

(defun dpi-y (attr)
  (let ((py (get-pixels-y attr))
        (y (get-mm-size-y attr)))
    (/ py (/ y 25.4))))

(defun get-dpy ()
  (let ((attr (car (display-monitor-attributes-list))))
    (truncate (dpi-x attr))))

(defun date-header ()
  (format-time-string "# ================ %Y/%m/%d ================\n"))

(defun insert-date-header ()
  (interactive)
  (insert (date-header)))

(setq account-header-format "
#   %s
# ------------\n\n")

(defun insert-account-header (acc)
  (interactive "MAccount:")
  (insert (format account-header-format acc)))

(provide 'my-functions)
