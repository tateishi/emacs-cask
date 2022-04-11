;;;; -*- mode: emacs-lisp; coding: utf-8 -*-

(defun my/create-fontset (font name)
  (message "%s:%s" font name)
  (condition-case err
      (create-fontset-from-ascii-font font nil name)
    (error
     (message "%s - %s" font (error-message-string err))
     nil)))

(defun my/create-fontset-from-list (font-list name)
  (message "%s:%s" font-list name)
  (if font-list
      (let ((res (my/create-fontset (car font-list) name)))
        (if res
            res
          (my/create-fontset-from-list (cdr font-list) name)))))

(defun my/select-favorite-font (fonts)
  (when (my/create-fontset-from-list fonts "font")
    (set-fontset-font "fontset-font"
                      'unicode "font" nil 'append)
    (add-to-list 'default-frame-alist '(font . "fontset-font"))))

(defvar font-list '(
                    "HackGenNerd Console-12"
                    "Myrica M-12"
                    "Noto Sans Mono CJK JP Regular-11"
                    "Noto Sans Mono CJK JP Regular-12"
                    "Ricty Diminished-12"
                    "Ricty Diminished-14"
                    "Myrica M-14"
                    "ＭＳ ゴシック-14"
                    ))

(use-package fonts
  :if window-system
  :init
  (my/select-favorite-font font-list))

;;;; 20-fonts.el ends here
