;;; shiwake-mode.el --- My major mode for editing ledger files. -*- lexical-binding: t; -*-

;; Copyright (C) 2021 TATEISHI Tadatoshi

;; Author: TATEISHI Tadatoshi <ishio39@gmail.com>
;; Maintainer: TATEISHI Tadatoshi <ishio39@gmail.com>
;; Created: 2021/04/09
;; Version: 0.0.1

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(require 'ledger-mode)

(defconst shiwake-date-template
  "
# =================== %Y/%m/%d ===================\n")

(defconst shiwake-account-template
  "
#                     %s
# --------------------------------------------------\n\n")

(defun shiwake-date ()
  (interactive)
  (insert (format-time-string shiwake-date-template)))

(defun shiwake-account (account)
  (interactive "MAccount: ")
  (insert (format shiwake-account-template account)))

(defvar shiwake-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-j") #'shiwake-date)
    (define-key map (kbd "C-c C-h") #'shiwake-account)
    map))

(define-derived-mode shiwake-mode ledger-mode "Shiwake"
  "Shiwake-mode is a my major mode for editing ledger data.

\\{shiwake-mode-map}")

(provide 'shiwake-mode)

;;; shiwake-mode.el ends here
