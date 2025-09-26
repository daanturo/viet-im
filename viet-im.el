;;; viet-im.el --- Type Vietnamese on Emacs with more flexibility -*- lexical-binding: t; -*-

;; Author: Dan <@>
;; Version: 0.1.0-git
;; URL: https://gitlab.com/daanturo/viet-im/
;; Package-Requires: ((emacs "29.1"))
;; Keywords: multilingual, input method, Vietnamese

;;; Commentary:

;; Provide a way to type Vietnamese on Emacs, with more extensive rules
;; and typing freedom (1) than the built-in Quail packages.  Unlike
;; Quail-based IMs, this also allows modifying diacritics after the word
;; is completed.

;; (1) See: https://en.wikipedia.org/wiki/VNI#VNI_Tan_Ky, in this
;; package that flexibility of order is applicable to Telex as well.

;;; Code:

(require 'cl-macs)
(require 'map)

(require 'viet-im-rules)

(defgroup viet-im nil
  "Type Vietnamese."
  :group 'i18n)

;;;; Process

(defvar-local viet-im--current-input-method 'vni
  "Choice of input method.
- \\='vni -  VNI:  https://en.wikipedia.org/wiki/VNI#Input_methods
- \\='telex - Telex: https://en.wikipedia.org/wiki/Telex_(input_method)")

(defconst viet-im--triggers '((vni . "0123456789") (telex . "adefjorswxz")
                              (viqr . "(^+`'?~.0")))

(defconst viet-im--vowels
  (string-to-list
   (concat
    "aăâeêioôơuưy"
    "àằầèềìòồờùừỳ"
    "áắấéếíóốớúứý"
    "ãẵẫẽễĩõỗỡũữỹ"
    "ạặậẹệịọộợụựỵ"
    "ảẳẩẻểỉỏổởủửỷ")))

(defconst viet-im--suffix-consonants (string-to-list (concat "bcdfgjklmnpqstvxzhrw")))

;; (defvar viet-im-allow-escape-composition t
;;   "If non-nil, handle backslash + trigger char cases: just insert trigger.
;; For example: \\0 -> 0, \\z -> z.")
;; (dolist (pair viet-im--triggers)
;;   (let* ((im (car pair)))
;;     (puthash "\\\\" "\\" (map-elt viet-im--rules-table im))
;;     (seq-do
;;      (lambda (char)
;;        (puthash
;;         (format "\\%c" char)
;;         (format "%c" char)
;;         (map-elt viet-im--rules-table im)))
;;      (cdr pair))))

(defun viet-im--replace-maybe ()
  "Replace some chars before point with appropriate intended diacritics."
  (let* ((word-beg
          (save-excursion
            (skip-chars-backward
             (concat
              "[:alpha:]"
              (map-elt viet-im--triggers viet-im--current-input-method)))
            (point)))
         (whole-candidate (buffer-substring word-beg (point))))
    ;; Try from longest to shortest potential strings before point until one
    ;; matches a rule, e.g.  "abc"|, "bc"|, "c"|.
    (named-let recur ((try-from 0))
      (when (< try-from (length whole-candidate))
        (let* ((cand (substring whole-candidate try-from))
               (result
                (map-nested-elt
                 viet-im--rules-table
                 (list viet-im--current-input-method cand))))
          (cond
           (result
            (delete-char (- try-from (length whole-candidate)))
            (insert result))
           (:else
            (recur (+ 1 try-from)))))))))

(defun viet-im-mode--post-self-insert-h ()
  "Run `viet-im--replace-maybe' after trigger char."
  ;; Only proceed if a triggering char has been insert
  (when (seq-position
         (map-elt viet-im--triggers viet-im--current-input-method) last-command-event)
    (viet-im--replace-maybe)))

(defun viet-im--deactivate ()
  "For `deactivate-current-input-method-function'."
  (remove-hook 'post-self-insert-hook #'viet-im-mode--post-self-insert-h
               'local))

;;;###autoload
(defun viet-im--activate (_im-id-str im-symbol)
  "Activate IM-SYMBOL, either \\='vni or \\='telex."
  (setq-local viet-im--current-input-method im-symbol)
  (setq-local deactivate-current-input-method-function #'viet-im--deactivate)
  (add-hook 'post-self-insert-hook #'viet-im-mode--post-self-insert-h
            nil
            'local))

;;; viet-im.el ends here

(provide 'viet-im)

;; Local Variables:
;; coding: utf-8
;; End:
