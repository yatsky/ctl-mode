;;; ctl-mode.el --- Support for the Common Transform Language in CloverDX-*- lexical-binding: t; -*-

;; Copyright (C) 2022 Yaoni

;; Author: Yaoni <y.wang7@uq.net.au>
;; Created: 29 Jan 2022
;; Keywords: languages, CTL

;; This file is not part of GNU Emacs.

;; This file is free software

;;; Commentary:
;;; TODO:
;;; DONE. Syntax coloring.
;;; Comment command.


;;; Code:

(defvar ctl-keywords '("for" "if" "function" "return" "const" "$in." "$out.")
  "Keywords in `ctl-mode'.")

(setq ctl-keywords '("for" "if" "function" "return" "$in." "$out."))

(defvar ctl-datatypes '("boolean" "number" "byte" "string" "cbyte" "list" "date" "map" "decimal" "variant" "integer" "record" "long" "string[]" "integer[]")
  "Datatypes in `ctl-mode'.")

(defvar ctl-negations '("!")
  "Negation in `ctl-mode'.")

(defconst ctl-function-re "transform")

(defun build-regex-string (string-list)
  "Build regex string for font-lock-*-face.
STRING-LIST: The list to be used to build the regex string."
  (mapconcat 'identity (mapcar #'(lambda (x) (concat "\\b" x "\\b")) string-list) "\\|")
  )

(setq yaoni/ctl-highlights
	  `((,(build-regex-string ctl-keywords) . 'font-lock-keyword-face)
		("ALL" . 'font-lock-constant-face)
		(,(build-regex-string ctl-datatypes) . 'font-lock-type-face)
		("[-+*/=<>,;:!|]" . 'font-lock-negation-char-face)
		;; 1 means use the first captured group
		("\\<\\(\\sw+\\) ?(" 1 'font-lock-function-name-face)))

(defvar ctl-mode-syntax-table nil "Syntax table for `ctl-mode'.")

(setq ctl-mode-syntax-table
	  (let ((synTable (make-syntax-table)))
		;; ctl comment: "//"
		(modify-syntax-entry ?\/ ". 12b" synTable)
		(modify-syntax-entry ?\n "> b" synTable)
		synTable))
(define-derived-mode ctl-mode fundamental-mode "ϕCTL"
  "Major mode for editing Common Transform Language code"
  (setq font-lock-defaults '(yaoni/ctl-highlights))
  (set-syntax-table ctl-mode-syntax-table)

  (setq-local comment-start "//")
  (setq-local comment-end ""))

(add-to-list 'auto-mode-alist '("\\.ctl\\'" . ctl-mode))
(provide 'ctl-mode)
;;; ctl-mode.el ends here
