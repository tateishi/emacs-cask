;;;; 50-core.el -*- lexical-binding: t -*-

;;;
;;; general setting
;;;

(use-package misc
  :no-require t
  :config
  (setq-default indent-tabs-mode nil)
  (setq vc-follow-symlinks t))

(use-package Japanese
  :no-require t
  :config
  (set-language-environment "Japanese")
  (prefer-coding-system 'utf-8))

(use-package appearance
  :no-require t
  :config
  (column-number-mode t)
  (size-indication-mode t)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

;;;
;;; edit
;;;

(defun my-disable-trailing-whitespace ()
  (setq show-trailing-whitespace nil))

(use-package general
  :no-require t
  :hook ((before-save . delete-trailing-whitespace))
  :init
  (show-paren-mode t)
  (setq-default require-final-newline t)
  (setq-default show-trailing-whitespace t))

(use-package undo-tree
  :bind (("M-/" . undo-tree-redo)
         ("C-x u" . undo-tree-visualize))
  :config (global-undo-tree-mode t))

(use-package eldoc
  :hook ((emacs-lisp-mode-hook . turn-on-eldoc-mode)
	 (lisp-interaction-mode-hook . turn-on-eldoc-mode)
	 (ielm-mode-hook . turn-on-eldoc-mode)))

(use-package auto-complete
  :config
  (global-auto-complete-mode t))

(use-package avy
  :bind
  ("C-c :" . avy-goto-char-timer))

;;;
;;; helm
;;;

(use-package helm
  :bind
  (("C-x C-f" . helm-find-files)
   ("M-x"     . helm-M-x)
   ("C-x b"   . helm-for-files)
   :map helm-map
   ("C-h"     . delete-backward-char)))

;;;
;;; program modes
;;;

(defun my-cc-mode-hook ()
  (google-set-c-style)
  (google-make-newline-indent))

(use-package cc-mode
  :init
  (use-package google-c-style)
  :hook
  (c-mode-common . my-cc-mode-hook))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'"       . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(defun my-ledger-mode-hook ()
  (setq-local tab-always-indent 'complete)
  (setq-local completion-cycle-threshold t)
  (setq-local ledger-complete-in-steps t))

(use-package ledger-mode
  :hook
  (ledger-mode . my-ledger-mode-hook))

(mapc (lambda (package) (use-package package))
      '(cmake-mode
        docker-compose-mode
        dockerfile-mode
        haskell-mode
        json-mode
        meson-mode
        python-mode
        yaml-mode))

;;;
;;; magit
;;;

(use-package magit
  :bind
  (("C-x C-g" . magit-status)
   ("C-x g"   . magit-status)))

;;;
;;; migemo
;;;

(when (executable-find "cmigemo")
  (defvar config-migemo/dir "/usr/share/cmigemo/utf-8")
  (use-package migemo
    :custom
    (migemo-directory
     (if (file-directory-p config-migemo/dir)
         config-migemo/dir
       (format "%s/dict/utf-8" (directory-file-name (file-name-directory (or (executable-find "cmigemo") ""))))))
    :functions (migemo-init)
    :config
    (migemo-init)))

;;;
;;; open junk file
;;;

(use-package open-junk-file
  :bind
  ("C-x j" . open-junk-file)
  :config
  (setq open-junk-file-format "~/junk/%Y/%m/%Y-%m-%d-%H%M%S."))

;;;
;;; user interface
;;;

(use-package zenburn-theme
  :hook (after-init . (lambda () (load-theme 'zenburn t))))

(use-package which-key
  :config (which-key-mode t))

(use-package persp-mode
  :config (persp-mode t))

(use-package treemacs
  :bind (("<f9>" . treemacs-select-window)
         :map treemacs-mode-map
         ("<f9>"  . treemacs-quit))
  :config
  (setq treemacs-width 25))

;;;
;;; window manipulation
;;;

(use-package ace-window
  :bind (("C-x o" . ace-window)))

;;;
;;; key bind
;;;

(use-package key-bind
  :no-require t
  :bind
  (("C-h" . backward-delete-char-untabify)
   ("C-z" . scroll-down-command)
   ("C-c c" . compile)
   ("<C-return>" . other-window)
   ("<M-return>" . other-frame)
   ("C-c o"   . swap-first-window)))

;;;
;;; hydra
;;;

(use-package hydra
  :ensure t
  :functions hydra-main/body hydra-windmove/body
  :bind (("C-c h"   . hydra-main/body)
         ("M-m"     . hydra-main/body)
         ("M-<SPC>" . hydra-main/body)
         ("C-c m"   . hydra-move/body)))

(use-package hydra-main :no-require t
  :after hydra
  :init
  (defhydra hydra-main (:idle 0 :hint nil)
    "
MAIN
--------------------------------------------------------------------------------
_h_: left char   _f_: forward sexp     _0_: treemacs  _m_: MOVE
_l_: right char  _b_: backward sexp    _1_: window 1  _w_: ace-window
_j_: down line   _u_: up list          _2_: window 2  _:_: goto-char
_k_: up line     _U_: backward up list _3_: window 3
_+_: larger      _d_: down list        _4_: window 4
_-_: smaller     _x_: helm-M-x         _5_: window 5
_q_: exit        _o_: helm-for-files   _6_: window 6
               _a_: helm-apropos
"
    ("+" text-scale-increase)
    ("-" text-scale-decrease)
    ("h" backward-char)
    ("l" forward-char)
    ("j" next-line)
    ("k" previous-line)
    ("f" forward-sexp)
    ("b" backward-sexp)
    ("F" forward-list)
    ("B" backward-list)
    ("u" up-list)
    ("U" backward-up-list)
    ("d" down-list)
    ("A" beginning-of-defun)
    ("E" end-of-defun)
    ("x" helm-M-x)
    ("o" helm-for-files)
    ("a" helm-apropos)
    ("w" ace-window)
    (":" avy-goto-char-timer)
    ("m" hydra-move/body        :exit t)
    ("0" treemacs-select-window :exit t)
    ("1" winum-select-window-1  :exit t)
    ("2" winum-select-window-2  :exit t)
    ("3" winum-select-window-3  :exit t)
    ("4" winum-select-window-4  :exit t)
    ("5" winum-select-window-5  :exit t)
    ("6" winum-select-window-6  :exit t)
    ("q" nil                    :exit t)))

(use-package hydra-move :no-require t
  :after hydra
  :init
  (defhydra hydra-move (:idle 0)
    "
MOVE
----------------------------------------------------------------
_h_: left char   _a_: beginning of line
_l_: right char  _e_: end of line
_j_: down line   _g_: goto line
_k_: up line     _:_: avy-goto-char
_H_: left word   _/_: search forward
_L_: right word  _?_: search backward

_q_: exit
"
    ("h" backward-char          "backward-char")
    ("l" forward-char           "forward-char")
    ("j" next-line              "next-line")
    ("k" previous-line          "previous-line")
    ("H" backward-word          "backward-word")
    ("L" forward-word           "forward-word")
    ("a" move-beginning-of-line "bol")
    ("e" move-end-of-line       "eol")
    ("g" goto-line              "goto-line")
    (":" avy-goto-char-timer    "avy-goto-char")
    ("/" isearch-forward        "isearch-forward")
    ("\?" isearch-backward      "isearch-backward")
    ("q" nil                    "exit")))

(use-package hydra-windmove :no-require t
  :after (hydra)
  :init
  (defhydra hydra-windmove (:idle 0)
    "
WINDMOVE
----------------------------------------------------------------
_h_: left  _<return>_: select
_l_: right
_j_: down
_k_: up
_q_: exit
"
    ("h" windmove-left)
    ("l" windmove-right)
    ("j" windmove-down)
    ("k" windmove-up)
    ("<return>" swap-first-window)
    ("q" nil)))


(defun my-kinshu-mode-hook ()
  (setq indent-tabs-mode nil))

(use-package kinshu-mode
  :load-path "lisp"
  :commands (kinshu-mode)
  :hook (kinshu-mode . my-kinshu-mode-hook))

(use-package my-functions
  :load-path "lisp")
