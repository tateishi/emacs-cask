;; kinshu-mode.el  -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'seq)

(defvar kinshu-list
  '(10000 5000 2000 1000 500 100 50 10 5 1))

(defvar kinshu-tabs
  '(0 13 17 21 25 29 33 37 41 45 49))

(defun kinshu (count-list)
  (apply #'+ (cl-mapcar #'* kinshu-list count-list)))

(defun kinshu-read-counts (from)
  (beginning-of-line)
  (read from)
  (let ((res ()))
    (while (not (eolp))
      (push (read from) res)
      (skip-chars-forward " \t"))
    (reverse res)))

(defun kinshu-delete-sum ()
  (end-of-line)
  (while (and (not (bolp)) (not (eq (char-after) ?=))) (backward-char))
  (when (eq (char-after) ?=)
    (skip-chars-backward " \t")
    (kill-line)))

(defun kinshu-next (from tabs)
  (let ((next-list (seq-filter (lambda (m) (> m from)) tabs)))
    (if (null next-list)
        from
      (seq-min next-list))))

(defun before-number (p)
  (skip-chars-backward "0-9")
  (point))

(defun after-number (p)
  (skip-chars-forward "0-9")
  (point))

(defun kinshu-inc ()
  (interactive)
  (save-excursion
    (let ((ch (char-after)))
      (when (and (>= ch ?0) (<= ch ?9))
          (let* ((from (before-number (point)))
                 (to (after-number (point)))
                 (num (1+ (string-to-number (buffer-substring from to))))
                 (len (min (length (format "%d" num)) (- to from))))
            (delete-char (- len))
            (insert (format "%d" num)))))))

(defun kinshu-dec ()
  (interactive)
  (save-excursion
    (let ((ch (char-after)))
      (when (and (>= ch ?0) (<= ch ?9))
          (let* ((from (before-number (point)))
                 (to (after-number (point)))
                 (num (1- (string-to-number (buffer-substring from to))))
                 (len (max (length (format "%d" num)) (- to from))))
            (delete-char (- len))
            (insert (format "%d" num)))))))

(defun kinshu-sum ()
  "一行分の金種から合計金額を計算する"
  (interactive)
  (save-excursion
    (kinshu-delete-sum)
    (end-of-line)
    (insert (format "   =%8d" (kinshu (kinshu-read-counts (current-buffer)))))))

(defun kinshu-sum-region (min max)
  "一行分の金種からの合計金額計算をリージョンに対して行う"
  (interactive "r")
  (save-excursion
    (goto-char min)
    (while (and (not (eobp)) (< (point) max))
      (kinshu-sum)
      (forward-line))))

(defun kinshu-add-header ()
  "金種計算のヘッダーを追加する"
  (interactive)
  (end-of-buffer)
  (insert "#-------------------------------------------------------------\n")
  (insert "#kinshu    10K  5K  2K  1K 500 100  50  10   5   1         sum\n")
  (insert "#-------------------------------------------------------------\n"))

(defun kinshu-add-line ()
  "金種計算の行を追加する"
  (interactive)
  (end-of-buffer)
  (let ((today (format-time-string "%Y/%m/%d")))
    (insert (format "%s   0   0   0   0   0   0   0   0   0   0" today)))
  (beginning-of-line))

(defun kinshu-next-tab ()
  "金種のフォーマットに合わせてカーソル移動する"
  (interactive)
  (let* ((col (current-column))
         (next (kinshu-next col kinshu-tabs)))
    (move-to-column next t)))

(defvar kinshu-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-k") #'kinshu-sum)
    (define-key map (kbd "C-c C-r") #'kinshu-sum-region)
    (define-key map (kbd "C-c C-h") #'kinshu-add-header)
    (define-key map (kbd "C-c C-j") #'kinshu-add-line)
    (define-key map (kbd "+") #'kinshu-inc)
    (define-key map (kbd "=") #'kinshu-inc)
    (define-key map (kbd "-") #'kinshu-dec)
    (define-key map (kbd "_") #'kinshu-dec)
    (define-key map (kbd "C-i") #'kinshu-next-tab)
    map))

(autoload 'text-mode "text-mode")

(define-derived-mode kinshu-mode text-mode "Kinshu"
  "Kinshu-mode is a major mode for editing kinshu data.

\\{kinshu-mode-map}")

(provide 'kinshu-mode)

;; Local Variables:
;; mode: emacs-lisp
;; coding: utf-8
;; End:
