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
;;; DONE. Comment command.
;;; DONE. Keyword completion.


;;; Code:

(defconst ctl-builtin-functions '(
								  "abs"
								  "isBlank"
								  "acos"
								  "isDate"
								  "addResponseHeader"
								  "isDecimal"
								  "append"
								  "isEmpty(container)"
								  "asin"
								  "isEmpty(string)"
								  "atan"
								  "isInteger"
								  "base64byte"
								  "isLong"
								  "binarySearch"
								  "isNumber"
								  "bitAnd"
								  "isNull"
								  "bitIsSet"
								  "isnull"
								  "bitLShift"
								  "isSourceFieldMapped"
								  "bitNegate"
								  "isSubgraphInputPortConnected"
								  "bitOr"
								  "isSubgraphOutputPortConnected"
								  "bitRShift"
								  "isTargetFieldMapped"
								  "bits2str"
								  "isUnicodeNormalized"
								  "bitSet"
								  "isUrl"
								  "bitXor"
								  "isValidCodePoint"
								  "bool2num"
								  "join"
								  "byteAt"
								  "json2xml"
								  "byte2base64"
								  "lastIndexOf"
								  "byte2hex"
								  "left"
								  "byte2str"
								  "length(string)"
								  "cast"
								  "length(container)"
								  "ceil"
								  "length(record)"
								  "charAt"
								  "log"
								  "chop"
								  "log10"
								  "clear"
								  "long2date"
								  "codePointAt"
								  "long2integer"
								  "codePointLength"
								  "long2packDecimal"
								  "codePointToChar"
								  "lowerCase"
								  "compare"
								  "lpad"
								  "concat"
								  "matches"
								  "concatWithSeparator"
								  "matchGroups"
								  "contains"
								  "max"
								  "containsAll"
								  "md5"
								  "containsKey"
								  "metaphone"
								  "containsResponseHeader"
								  "min"
								  "containsValue"
								  "next"
								  "copy"
								  "normalizePath"
								  "copyByName"
								  "num2bool"
								  "copyByPosition"
								  "num2str"
								  "cos"
								  "nvl"
								  "count"
								  "nvl2"
								  "countChar"
								  "NYSIIS"
								  "createDate"
								  "getOAuth2Token"
								  "cut"
								  "packDecimal2long"
								  "date2long"
								  "parseAvro"
								  "date2num"
								  "parseBson"
								  "date2str"
								  "parseJson"
								  "dateAdd"
								  "parseProperties"
								  "dateDiff"
								  "printErr"
								  "decimal2double"
								  "printLog"
								  "decimal2integer"
								  "pi"
								  "decimal2long"
								  "poll"
								  "double2integer"
								  "pop"
								  "double2long"
								  "pow"
								  "e"
								  "push"
								  "editDistance"
								  "put"
								  "endsWith"
								  "raiseError"
								  "escapeUrl"
								  "random"
								  "escapeUrlFragment"
								  "randomBoolean"
								  "exp"
								  "randomDate"
								  "extractDate"
								  "randomGaussian"
								  "extractTime"
								  "randomInteger"
								  "find"
								  "randomLong"
								  "findAllValues"
								  "randomString"
								  "floor"
								  "randomUUID"
								  "get"
								  "record2map"
								  "getAlphanumericChars"
								  "remove"
								  "getAvroSchema"
								  "removeBlankSpace"
								  "getBoolValue"
								  "removeDiacritic"
								  "getByteValue"
								  "removeNonAscii"
								  "getComponentProperty"
								  "removeNonPrintable"
								  "getDateValue"
								  "replace"
								  "getDay"
								  "resetRecord"
								  "getDecimalValue"
								  "resolveParams"
								  "getEnvironmentVariables"
								  "reverse(list)"
								  "getType"
								  "reverse(string)"
								  "getFieldIndex"
								  "right"
								  "getFieldLabel"
								  "round"
								  "getFieldName"
								  "roundHalfToEven"
								  "getFieldProperties"
								  "rpad"
								  "getFieldType"
								  "setBoolValue"
								  "getFileExtension"
								  "setByteValue"
								  "getFileName"
								  "setDateValue"
								  "getFileNameWithoutExtension"
								  "setDecimalValue"
								  "getFilePath"
								  "setIntValue"
								  "getHour"
								  "setLongValue"
								  "getIntValue"
								  "setNumValue"
								  "getJavaProperties"
								  "setRandomSeed"
								  "getKeys"
								  "setRequestEncoding"
								  "getLongValue"
								  "setResponseBody"
								  "getMappedSourceFields"
								  "setResponseContentType"
								  "getMappedTargetFields"
								  "setResponseEncoding"
								  "getMillisecond"
								  "setResponseHeader"
								  "getMinute"
								  "setResponseStatus"
								  "getMonth"
								  "setStringValue"
								  "getNumValue"
								  "setValue"
								  "getParamValue"
								  "sha"
								  "getParamValues"
								  "sha256"
								  "getRawParamValue"
								  "signum"
								  "getRawParamValues"
								  "sin"
								  "getRecordProperties"
								  "sleep"
								  "getRequestBody"
								  "sort"
								  "getRequestClientIPAddress"
								  "soundex"
								  "getRequestContentType"
								  "split"
								  "getRequestEncoding"
								  "sqrt"
								  "getRequestHeader"
								  "startsWith"
								  "getRequestHeaderNames"
								  "str2bits"
								  "getRequestHeaders"
								  "str2bool"
								  "getRequestMethod"
								  "str2byte"
								  "getRequestParameter"
								  "str2date"
								  "getRequestParameterNames"
								  "str2decimal"
								  "getRequestParameters"
								  "str2double"
								  "getRequestPartFilename"
								  "str2integer"
								  "getResponseContentType"
								  "str2long"
								  "getResponseEncoding"
								  "substring"
								  "getSecond"
								  "tan"
								  "getStringValue"
								  "toAbsolutePath"
								  "getSubgraphInputPortsCount"
								  "today"
								  "getSubgraphOutputPortsCount"
								  "toDegrees"
								  "getUrlHost"
								  "toMap"
								  "getUrlPath"
								  "toProjectUrl"
								  "getUrlPort"
								  "toRadians"
								  "getUrlProtocol"
								  "toString"
								  "getUrlQuery"
								  "translate"
								  "getUrlRef"
								  "trim"
								  "getUrlUserInfo"
								  "trunc"
								  "getValue"
								  "truncDate"
								  "getValueAsString"
								  "unescapeUrl"
								  "getValues"
								  "unescapeUrlFragment"
								  "getYear"
								  "unicodeNormalize"
								  "hashCode"
								  "upperCase"
								  "hex2byte"
								  "writeAvro"
								  "iif"
								  "writeBson"
								  "in"
								  "writeExtendedBson"
								  "indexOf"
								  "writeJson"
								  "insert"
								  "xml2json"
								  "isAscii"
								  "zeroDate"
								  )
  "Builtin functions in CTL.")

(defconst ctl-keywords '("for" "foreach" "do" "while" "if" "else" "function" "return" "const" "$in." "$out." "switch" "case" "break" "default" "continue" "return" "try" "catch" "OnError" "dictionary")
  "Keywords in `ctl-mode'.")

(defconst ctl-datatypes '("boolean" "number" "byte" "string" "cbyte" "list" "date" "map" "decimal" "variant" "integer" "record" "long" "string[]" "integer[]")
  "Datatypes in `ctl-mode'.")

(defconst ctl-operator "[-+*/=<>,;:!|]"
  "Punctuation Operators in `ctl-mode'.")

(defconst ctl-completion-keywords (append ctl-builtin-functions ctl-keywords ctl-datatypes)
  "Keywords for auto completion.")

(defconst ctl-function-re "\\<\\(\\sw+\\) ?(")

(defvar  ctl-highlights ""
  "A string of all things that need to be highlighted in ctl.")

(defun build-regex-string (string-list)
  "Build regex string for font-lock-*-face.
STRING-LIST: The list to be used to build the regex string."
  (mapconcat 'identity (mapcar #'(lambda (x) (concat "\\b" x "\\b")) string-list) "\\|"))

(defun ctl-function-completion-at-point ()
  "Function completion for the hook `completion-at-point-functions'."
  (interactive)
  (let* (
		 (bds (bounds-of-thing-at-point 'symbol))
		 (start (car bds))
		 (end (cdr bds)))
	(list start end ctl-completion-keywords . nil)))

(setq ctl-highlights
	  `((,(build-regex-string (append ctl-keywords ctl-builtin-functions)) . 'font-lock-keyword-face)
		("ALL" . 'font-lock-constant-face)
		(,(build-regex-string ctl-datatypes) . 'font-lock-type-face)
		(,ctl-operator . 'font-lock-negation-char-face)
		;; 1 means use the first captured group
		(,ctl-function-re 1 'font-lock-function-name-face)))

(defvar ctl-mode-syntax-table nil "Syntax table for `ctl-mode'.")

(setq ctl-mode-syntax-table
	  (let ((synTable (make-syntax-table)))
		;; ctl comment: "//"
		(modify-syntax-entry ?\/ ". 12b" synTable)
		(modify-syntax-entry ?\n "> b" synTable)
		synTable))
(define-derived-mode ctl-mode fundamental-mode "ϕCTL"
  "Major mode for editing Common Transform Language code"
  (setq font-lock-defaults '(ctl-highlights))
  (set-syntax-table ctl-mode-syntax-table)

  (setq-local comment-start "//")
  (setq-local comment-end "")
  (setq-local tab-width 4)
  (setq-local indent-tabs-mode t)
  (add-hook 'completion-at-point-functions 'ctl-function-completion-at-point nil 'local)
  (rainbow-delimiters-mode-enable))

(add-to-list 'auto-mode-alist '("\\.ctl\\'" . ctl-mode))
(provide 'ctl-mode)
;;; ctl-mode.el ends here
