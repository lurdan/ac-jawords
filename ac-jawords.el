;;; ac-jawords.el --- auto-complete source for japanese words in buffer

;; Copyright (C) 2017 KURASHIKI Satoru

;; Filename: ac-jawords.el
;; Description: Consult various dictionaries and show result using tooltip
;; Author: KURASHIKI Satoru
;; Created: 2017-11-08
;; Version: 0.0.1
;; Package-Version:
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

(defvar ac-jawords-split-function 'tseg-segment)

(defvar ac-jawords-index nil)
(make-variable-buffer-local 'ac-jawords-index)

(defun ac-jawords-index ()
  (if ac-jawords-index
      ac-jawords-index
    (setq ac-jawords-index
          (replace-regexp-in-string "^\n" ""
                                    (replace-regexp-in-string "\\ca+\\|[！？：；。、，．]\\|[[:space:]]+" "\n"
                                                              (buffer-substring-no-properties (point-min) (point-max)))
                                    ))))

(defun ac-jawords-candidates (&optional buffer-pred)
  (cl-loop initially (unless ac-fuzzy-enable (ac-incremental-update-word-index)) ;; 要修正
           for buffer in (buffer-list)
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

(defun ac-jawords-prefix ()
  (save-excursion
    (when (aref (char-category-set (char-before)) ?j)
      (looking-back "\\cj+" nil t)
      (looking-back (let* ((segs (funcall ac-jawords-split-function (match-string-no-properties 0)))
                           (revs (reverse segs)))
                      (if (> 2 (length (car revs)))
                          (concat (cadr revs) (car revs))
                        (car revs)))
                    nil nil)
      (match-beginning 0))))

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

(when (featurep 'skk)
  (with-eval-after-load "skk"
    (add-to-list 'ac-trigger-commands 'skk-insert)
    (add-to-list 'ac-trigger-commands 'skk-kakutei) ;; minibuffer を除外する advice が必要？ (cf. ac-ja.el)
    )
  )
;; (add-to-list 'ac-trigger-commands 'org-self-insert-command)

;;;###autoload
(defun ac-jawords-setup ()
  (interactive)
  (add-to-list 'ac-sources 'ac-source-jawords-in-buffer))

(provide 'ac-jawords)

;;; ac-jawords.el ends here
