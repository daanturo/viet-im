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

(defconst viet-im--explicit-triggers
  '((vni . "0123456789") (telex . "adefjorswxz") (viqr . "(^+`'?~.0")))

(defconst viet-im--vowels
  (progn
    (concat
     "aăâeêioôơuưy"
     "àằầèềìòồờùừỳ"
     "áắấéếíóốớúứý"
     "ãẵẫẽễĩõỗỡũữỹ"
     "ạặậẹệịọộợụựỵ"
     "ảẳẩẻểỉỏổởủửỷ")))

(defconst viet-im--implicit-triggers
  (mapcar
   (lambda (im)
     (let* ((explicit-triggers
             (split-string (map-elt viet-im--explicit-triggers im) "" t))
            (rule-keys (hash-table-keys (map-elt viet-im--rules-table im)))
            (implicit-chars
             (string-join (sort
                           (seq-uniq
                            (seq-keep
                             (lambda (s)
                               (let* ((char (substring s -1)))
                                 (and (not
                                       (seq-contains-p explicit-triggers char))
                                      char)))
                             rule-keys)))
                          "")))
       (cons im implicit-chars)))
   '(vni telex))
  "Characters that may trigger diacritic changes that aren't IM-defined explicit triggers.")

(defconst viet-im--uncomposed-syllable-regexp
  (mapcar
   (lambda (im)
     (let* ((d-trigger (map-elt '((vni . "9") (telex . "d")) im))
            (triggers
             (split-string (map-elt viet-im--explicit-triggers im) "" t)))
       (cons
        im
        (rx-to-string
         `(seq
           bow ; word start
           ;; Leading consonants
           (? (or ,(concat "d" d-trigger) "qu" "gi" ,@viet-im--prefix-consonants))
           ;; Vowels mixed with triggers
           (* (or ,@viet-im--vowels ,@triggers))
           ;; Trailing consonants
           (?  (or ,@viet-im--suffix-consonants))
           ;; Triggers at end
           (* (or ,@triggers)))
         t))))
   '(vni telex)))

(defun viet-im--replace-maybe ()
  "Replace some chars before point with appropriate intended diacritics."
  (when-let* ((im viet-im--current-input-method)
              ;; Must be a monosyllabic word, as polysyllabic words aren't
              ;; Vietnamese
              (_
               (looking-back (map-elt viet-im--uncomposed-syllable-regexp im)
                             (pos-bol)
                             t))
              (word-beg (match-beginning 0))
              (whole-candidate (buffer-substring word-beg (point))))
    ;; Try from longest to shortest potential strings before point until one
    ;; matches a rule, e.g.  "abc"|, "bc"|, "c"|.
    (named-let recur ((try-from 0))
      (when (< try-from (length whole-candidate))
        (let* ((cand (substring whole-candidate try-from))
               (result (map-nested-elt viet-im--rules-table (list im cand))))
          (cond
           (result
            (delete-char (- try-from (length whole-candidate)))
            (insert result)
            result)
           (:else
            (recur (+ 1 try-from)))))))))

(defun viet-im-mode--post-self-insert-h ()
  "Run `viet-im--replace-maybe' after trigger char."
  ;; Only proceed if a triggering char has been insert
  (when (or (seq-position
             (map-elt
              viet-im--explicit-triggers viet-im--current-input-method)
             last-command-event)
            ;; TODO: should we just consider explicit triggers + all alphabetic?
            (seq-position
             (map-elt
              viet-im--implicit-triggers viet-im--current-input-method)
             last-command-event))
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
