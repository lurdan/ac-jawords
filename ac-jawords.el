;;; ac-jawords.el --- auto-complete source for japanese words in buffer

;; Copyright (C) 2017 KURASHIKI Satoru

;; Filename: ac-jawords.el
;; Description: search written japanese words as completion candidates
;; Author: KURASHIKI Satoru
;; Created: 2017-11-08
;; Version: 0.0.1
;; Package-Version:
;; Package-X-Original-Version:
;; Package-Requires: ((tinysegmenter "0") (auto-complete "0") (s "0"))
;; URL: https://github.com/lurdan/emacs-ac-jawords
;; Keywords:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.

;; It is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:

;;; Code:
(require 's)
(require 'auto-complete)
(require 'tinysegmenter)

;; To avoid duplicating candidates, remove japanese words from ac-word
(defun ac-word-candidates-remove-japanese (ret)
  (let ((contain-japanese (lambda (s) (string-match (rx (category japanese)) s))))
    (cl-remove-if contain-japanese ret)))
(advice-add 'ac-word-candidates :filter-return 'ac-word-candidates-remove-japanese)

;; Variables
(defgroup ac-jawords nil
  "ac-jawords group"
  :group 'applications)

(defcustom ac-jawords-split-function 'tseg-segment
  "分かち書きに使う関数"
  :type 'symbol
  :group 'ac-jawords)

(defcustom ac-jawords-symbol-remove-regexp "[・”’＜＞【】「」『』（）]"
  "候補から除去したい文字/記号の正規表現"
  :type 'string
  :group 'ac-jawords)

(defcustom ac-jawords-symbol-split-regexp "[　：；！？。、，．]"
  "区切り文字として使い、候補から除去する文字/記号の正規表現"
  :type 'string
  :group 'ac-jawords)

;; (defcustom ac-jawords-update-index-frequency nil
;;   "インデックスの更新タイミング (未実装)。any-change, after-save, any-change, timer, ..."
;;   :type 'string
;;   :group 'ac-jawords)

(defvar ac-jawords-index)
(make-variable-buffer-local 'ac-jawords-index)
(setq-default ac-jawords-index nil)

;; Functions
(defun ac-jawords-index (&optional force)
  (interactive "P")
  (if (or current-prefix-arg
          force
          (null ac-jawords-index))
      (setq ac-jawords-index (let ((lines (replace-regexp-in-string
                                           (concat "^\n\\|^..?\n\\|" ac-jawords-symbol-remove-regexp) ""
                                           (replace-regexp-in-string
                                            (concat "\\ca+\\|[[:space:]]+\\|" ac-jawords-symbol-split-regexp) "\n"
                                            (buffer-substring-no-properties (point-min) (point-max))))))
                               (with-temp-buffer
                                 (insert lines)
                                 (delete-duplicate-lines (point-min) (point-max))
                                 (buffer-string))))
    ac-jawords-index))

(defun ac-jawords-candidates (&optional buffer-pred)
  (cl-loop ;;initially (unless ac-fuzzy-enable (ac-incremental-update-word-index)) ;; 要修正
           for buffer in (list (current-buffer)) ;;(buffer-list) ;;(current-buffer)
           if (and (or (not (integerp ac-limit)) (< (length candidates) ac-limit))
                   (if buffer-pred (funcall buffer-pred buffer) t))
           append (funcall ac-match-function
                           ac-prefix
                           (ac-jawords-candidates-internal ac-prefix)
                           ;; (and (local-variable-p 'ac-word-index buffer)
                           ;;      (cdr (buffer-local-value 'ac-word-index buffer)))
                           )
           into candidates
           finally return (cl-remove-if (lambda (elm) (equal elm ac-prefix))
                                     (delete-dups candidates)
                                     )))

(defun ac-jawords-candidates-internal (word)
  (let* ((lines (mapcar 'car (s-match-strings-all
                              (concat word ".*$")
                              (ac-jawords-index)))))
    ;; ここが遅い
    (cl-loop for elm in lines
             append (let ((segs (funcall ac-jawords-split-function elm)))
                      (if (eq 1 (length segs))
                          segs
                        (cl-loop for i from 1 to 2
                                 collect (apply 'concat (cl-subseq segs 0 i)))
                        )))))

(defun ac-jawords-prefix-ignore-p ()
  (cond ((featurep 'skk)
         (eval-when-compile (defvar skk-henkan-mode))
         skk-henkan-mode)
        (t nil)))

(defun ac-jawords-prefix ()
  (unless (ac-jawords-prefix-ignore-p)
    (save-excursion
      (when (aref (char-category-set (char-before)) ?j)
        (looking-back "\\cj+" nil t)
        (looking-back (let ((revs (reverse (funcall ac-jawords-split-function (match-string-no-properties 0)))))
                        (if (and (not (string-match "\\cK" (car revs))) ;;カタカナ以外
                                 ;;(string-match "^\\cC+$" (car revs)) ;;漢字のみ
                                 (> 3 (length (car revs))))
                            (concat (cadr revs) (car revs))
                          (car revs)))
                      nil nil)
        (let ((position (match-beginning 0))
              (str (match-string-no-properties 0)))
          ;;(message "DEBUG-str: %s (%s)" str (< 2 (length str)))
          (if (< 2 (length str))
              position)
          )))))

(ac-define-source jawords-in-buffer
  '((prefix . ac-jawords-prefix)
    (requires . 2)
    (candidates . ac-jawords-candidates)))

;; (ac-define-source jawords
;;   '((prefix . ac-jawords-prefix)
;;     (requires . 2)
;;     (candidates . ac-jawords-candidates)))

;; (ac-define-source jawords-in-same-mode-buffers
;;   '((prefix . ac-jawords-prefix)
;;     (requires . 2)
;;     (candidates . ac-jawords-candidates)))

(when (locate-library "skk")
  (with-eval-after-load "skk"
    (add-to-list 'ac-trigger-commands 'skk-insert)
    (add-to-list 'ac-trigger-commands 'skk-kakutei) ;; minibuffer を除外する advice が必要？ (cf. ac-ja.el)
    ))

(with-eval-after-load "org"
  ;; (add-to-list 'ac-trigger-commands 'org-self-insert-command)
  )

;;;###autoload
(defun ac-jawords-setup ()
  (interactive)
  (add-to-list 'ac-sources 'ac-source-jawords-in-buffer))

(provide 'ac-jawords)

;;; ac-jawords.el ends here

