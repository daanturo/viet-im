;; -*- lexical-binding: t; -*-

;; If those get integrated into Emacs core, it should go in leim/leim-ext.el
;; with autoload cookies removed.  The sole purpose of this file is to
;; automatically register input methods, it shouldn't be loaded.

;;;###autoload
(register-input-method
 "vietnamese-vni-x"
 "Vietnamese"
 #'viet-im--activate
 "VV-X"
 "Input method: Vietnamese VNI - extended."
 'vni)

;;;###autoload
(register-input-method
 "vietnamese-telex-x"
 "Vietnamese"
 #'viet-im--activate
 "VT-X"
 "Input method: Vietnamese Telex - extended."
 'telex)
