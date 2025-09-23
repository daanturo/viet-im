;;; im-viet.el --- Type Vietnamese on Emacs with more freedom -*- lexical-binding: t; -*-

;; Package-Version: 0.1.0-git
;; Package-Requires: ((emacs "30"))

;;; Commentary:
;; Provide a way to type Vietnamese on Emacs, with more extensive rules than the
;; built-in Quail packages (as of 2025-09-23).

;;; Code:

(require 'dash)

(defgroup im-viet nil
  "Type Vietnamese.")

(defvar im-viet--rules--file-format
  (file-name-concat (file-name-parent-directory
                     (or load-file-name buffer-file-name))
                    "generated-im-%s.json"))
(defvar im-viet--rules--val '())
(defun im-viet--rules (im)
  "Get rules of input method IM."
  (with-memoization (map-elt im-viet--rules--val im)
    (let* ((filename (format im-viet--rules--file-format im)))
      (with-temp-buffer
        (insert-file-contents filename)
        (goto-char (point-min))
        (json-parse-buffer :object-type 'hash-table)))))

(defcustom im-viet-input-method 'vni
  "Choice of input method.
- VNI:  https://en.wikipedia.org/wiki/VNI#Input_methods
- Telex: https://en.wikipedia.org/wiki/Telex_(input_method)"
  :type '(choice (const :tag "VNI" vni) (const :tag "Telex" telex)))

(defconst im-viet--triggers '((vni . "0123456789") (telex . "adefjorswxz")))

(defconst im-viet--vowels
  (concat
   "aăâeêioôơuưy"
   "àằầèềìòồờùừỳ"
   "áắấéếíóốớúứý"
   "ãẵẫẽễĩõỗỡũữỹ"
   "ạặậẹệịọộợụựỵ"
   "ảẳẩẻểỉỏổởủửỷ"))

(defconst im-viet--consonants
  (concat
   ;; "đ"
   "bcdfgjklmnpqstvxzhrw"))

(defconst im-viet--candidate-regexp
  (rx-to-string
   `(seq
     ;; word-start
     (?  (or "d" "đ")) ; only "d" and "đ" in prefixing consonants can change
     ;; grouping: check if replacing "d" works first, then rhyme only
     (group
      (* (or ,@(string-to-list im-viet--vowels))) ; vowels
      (* (or ,@(string-to-list im-viet--consonants))) ; suffixing consonants
      (or ,@(--> (map-values im-viet--triggers) (-mapcat #'string-to-list it))) ; include a trigger char
      )
     ;; point
     )
   'no-group))

(defun im-viet--replace-maybe ()
  "Replace some chars before point with apropriate intended diacritics."
  (when (save-excursion
          ;; (re-search-backward im-viet--candidate-regexp (pos-bol) t)
          (looking-back im-viet--candidate-regexp (pos-bol) 'greedy))
    (-some
     (lambda (subexp)
       (when-let* ((cand (downcase (match-string subexp)))
                   (result
                    (map-elt (im-viet--rules im-viet-input-method) cand)))
         (replace-match result nil nil nil subexp)
         t))
     '(0 1))))

(defun im-viet-mode--post-self-insert-h ()
  "Run `im-viet--replace-maybe' after trigger char."
  (when (seq-position
         (map-elt im-viet--triggers im-viet-input-method) last-command-event)
    (im-viet--replace-maybe)))

(defvar-local im-viet-mode--lighter nil)

;;;###autoload
(define-minor-mode im-viet-mode
  "Type Vietnamese."
  :lighter
  (:eval im-viet-mode--lighter)
  (cond
   (im-viet-mode
    (setq-local im-viet-mode--lighter
                (pcase im-viet-input-method
                  (`vni "VNI")
                  (`telex "Telex")))
    (add-hook 'post-self-insert-hook #'im-viet-mode--post-self-insert-h
              nil
              'local))
   (:else
    (remove-hook 'post-self-insert-hook #'im-viet-mode--post-self-insert-h
                 'local))))

;;; im-viet.el ends here

(provide 'im-viet)
