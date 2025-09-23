;;; im-viet.el --- Type Vietnamese on Emacs with more flexibility -*- lexical-binding: t; -*-

;; Author: Dan <@>
;; Version: 0.1.0-git
;; Package-Requires: ((emacs "30"))
;; Keywords: i18n
;; URL: https://gitlab.com/daanturo/im-viet/

;;; Commentary:

;; Provide a way to type Vietnamese on Emacs, with more extensive rules and
;; typing freedom (1) than the built-in Quail package (as of 2025-09).

;; See: https://en.wikipedia.org/wiki/VNI#VNI_Tan_Ky


;;; Code:

(require 'cl-macs)
(require 'map)

(require 'im-viet-rules)

;;;; Customization

(defgroup im-viet nil
  "Type Vietnamese."
  :group 'i18n)

(defcustom im-viet-allow-escape-composition t
  "Handle backslash-trigger cases: just insert trigger.
For example: \\0 -> 0, \\z -> z."
  :type 'boolean)

;;;; Process

(defvar-local im-viet--current-input-method 'vni
  "Choice of input method.
- \\='vni -  VNI:  https://en.wikipedia.org/wiki/VNI#Input_methods
- \\='telex - Telex: https://en.wikipedia.org/wiki/Telex_(input_method)")

(defconst im-viet--triggers '((vni . "0123456789") (telex . "adefjorswxz")))

(defconst im-viet--vowels
  (string-to-list
   (concat
    "aăâeêioôơuưy"
    "àằầèềìòồờùừỳ"
    "áắấéếíóốớúứý"
    "ãẵẫẽễĩõỗỡũữỹ"
    "ạặậẹệịọộợụựỵ"
    "ảẳẩẻểỉỏổởủửỷ")))

(defconst im-viet--suffix-consonants (string-to-list (concat "bcdfgjklmnpqstvxzhrw")))

;; Insert escape composition cases
(dolist (pair im-viet--triggers)
  (let* ((im (car pair)))
    (puthash "\\\\" "\\" (map-elt im-viet--rules-table im))
    (seq-do
     (lambda (char)
       (puthash
        (format "\\%c" char)
        (format "%c" char)
        (map-elt im-viet--rules-table im)))
     (cdr pair))))

(defconst im-viet--escape-composition-regexp
  (let* ((trigger-list
          (seq-mapcat #'string-to-list (map-values im-viet--triggers))))
    (rx-to-string `(seq "\\" (or ,@trigger-list "\\")) 'no-group)))

(defconst im-viet--candidate-regexp
  (let* ((trigger-list
          (seq-mapcat #'string-to-list (map-values im-viet--triggers))))
    (rx-to-string
     `(or
       (seq
        (?  (or "d" "đ")) ; only "d" and "đ" in prefixing consonants can change
        ;; grouping: check if replacing "d" works first, then rhyme only
        (group
         (* (or ,@im-viet--vowels))
         (* (or ,@im-viet--suffix-consonants))
         (or ,@trigger-list) ; include a trigger char
         )))
     'no-group))
  "Regular expression for the rule key before point.")

(defun im-viet--replace-maybe ()
  "Replace some chars before point with appropriate intended diacritics."
  (cl-labels ((match-fn (regexp)
                (and (save-excursion (looking-back regexp (pos-bol) 'greedy))
                     (< 1 (- (match-end 0) (match-beginning 0))))))
    (when (or (match-fn im-viet--candidate-regexp)
              (and im-viet-allow-escape-composition
                   (match-fn im-viet--escape-composition-regexp)))
      ;; Try group 0 (includes "d") first, then if no rule then group 1 (only rhyme)
      (seq-some
       (lambda (subexp)
         (when-let* ((cand (downcase (match-string subexp)))
                     (result
                      (map-nested-elt
                       im-viet--rules-table
                       (list im-viet--current-input-method cand))))
           (replace-match result nil nil nil subexp)
           t))
       [0 1]))))

(defun im-viet-mode--post-self-insert-h ()
  "Run `im-viet--replace-maybe' after trigger char."
  ;; Only proceed if a triggering char has been insert
  (when (seq-position
         (map-elt im-viet--triggers im-viet--current-input-method) last-command-event)
    (im-viet--replace-maybe)))

(defun im-viet--deactivate ()
  "For `deactivate-current-input-method-function'."
  (remove-hook 'post-self-insert-hook #'im-viet-mode--post-self-insert-h
               'local))

;;;###autoload
(defun im-viet--activate (_im-id-str im-symbol)
  "Activate IM-SYMBOL, either \\='vni or \\='telex."
  (setq-local im-viet--current-input-method im-symbol)
  (setq-local deactivate-current-input-method-function #'im-viet--deactivate)
  (add-hook 'post-self-insert-hook #'im-viet-mode--post-self-insert-h
            nil
            'local))

;;;###autoload
(register-input-method "vietnamese-vni-x" "Vietnamese"
                       #'im-viet--activate
                       "VV-X"
                       "Vietnamese VNI input method - extended"
                       'vni)
;;;###autoload
(register-input-method "vietnamese-telex-x" "Vietnamese"
                       #'im-viet--activate
                       "VT-X"
                       "Vietnamese Telex input method - extended"
                       'telex)

;;; im-viet.el ends here

(provide 'im-viet)

;; Local Variables:
;; coding: utf-8
;; End:
