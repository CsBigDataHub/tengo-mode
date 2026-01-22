;;; tengo-mode.el --- Major mode for the Tengo programming language -*- lexical-binding: t; -*-

;; Copyright (C) 2025
;; Author: Your Name <your.email@example.com>
;; URL: https://github.com/yourusername/tengo-mode
;; Version: 0.2.5
;; Package-Requires: ((emacs "24.3"))
;; Keywords: languages, tengo

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Major mode for editing Tengo code.
;; Tengo is a small, dynamic, fast, secure scripting language for Go.
;;
;; Features:
;; - Syntax highlighting (keywords, builtins, functions, variables, operators, numbers)
;; - Context-aware indentation (ignores braces/parens/brackets in strings/comments)
;; - Comment handling (// and /* */)
;; - Imenu support (top-level functions and variables)
;; - Electric pairing
;; - Defun navigation (works for top-level, named, and nested/anonymous functions)
;;
;; Based on https://github.com/geseq/tengo-vim and official Tengo docs.

;;; Code:

(require 'prog-mode)

;;; Customization

(defgroup tengo nil
  "Major mode for the Tengo programming language."
  :prefix "tengo-"
  :group 'languages)

(defcustom tengo-indent-offset 4
  "Number of spaces to indent per level in Tengo code."
  :type 'integer
  :safe 'integerp
  :group 'tengo)

;;; Syntax Table

(defvar tengo-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Comments
    (modify-syntax-entry ?/ ". 124b" table)
    (modify-syntax-entry ?* ". 23" table)
    (modify-syntax-entry ?\n "> b" table)

    ;; Strings
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?' "\"" table)
    (modify-syntax-entry ?` "\"" table)

    ;; Identifiers
    (modify-syntax-entry ?_ "w" table)

    ;; Parens/braces/brackets
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?{ "(}" table)
    (modify-syntax-entry ?} "){" table)
    (modify-syntax-entry ?\[ "(]" table)
    (modify-syntax-entry ?\] ")[" table)

    table)
  "Syntax table for `tengo-mode'.")

;;; Keywords, Builtins, Constants

(defvar tengo-keywords
  '("if" "else" "for" "in" "break" "continue" "func" "return"
    "import" "export" "immutable")
  "Reserved keywords in Tengo.")

(defvar tengo-builtins
  '("format" "len" "copy" "append" "delete" "splice" "type_name"
    "string" "int" "bool" "float" "char" "bytes" "time" "error" "range"
    "is_string" "is_int" "is_bool" "is_float" "is_char" "is_bytes"
    "is_error" "is_undefined" "is_function" "is_callable" "is_array"
    "is_immutable_array" "is_map" "is_immutable_map" "is_iterable" "is_time")
  "Built-in functions in Tengo.")

(defvar tengo-constants
  '("true" "false" "undefined")
  "Constants in Tengo.")

;; Fallback for older Emacs
(unless (facep 'font-lock-operator-face)
  (defface font-lock-operator-face
    '((t :inherit font-lock-builtin-face))
    "Face for operators."
    :group 'font-lock-faces))

;;; Font Lock

(defvar tengo-font-lock-keywords
  (list
   ;; Shebang
   '("^#!.*" . font-lock-preprocessor-face)

   ;; Keywords
   (cons (regexp-opt tengo-keywords 'words) font-lock-keyword-face)

   ;; Constants
   (cons (regexp-opt tengo-constants 'words) font-lock-constant-face)

   ;; Builtins
   (cons (regexp-opt tengo-builtins 'words) font-lock-builtin-face)

   ;; Named function definitions: name := func
   '("\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\s-*:=\\s-*func" 1 font-lock-function-name-face)

   ;; Variable declarations: name :=
   '("\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\s-*:=" 1 font-lock-variable-name-face)

   ;; Function calls: name(
   '("\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\s-*(" 1 font-lock-function-name-face)

   ;; Numbers
   '("\\b\\(0[xX][0-9a-fA-F]+\\|[0-9]*\\.[0-9]+\\([eE][+-]?[0-9]+\\)?\\|[0-9]+\\(\\.[0-9]*\\)?\\([eE][+-]?[0-9]+\\)?\\)\\b"
     . font-lock-constant-face)

   ;; Operators
   (cons (regexp-opt
          '("==" "!=" "<=" ">=" "&&" "||" "?" ":"
            "+=" "-=" "*=" "/=" "%=" "&=" "|=" "^=" "<<=" ">>=" "&^="
            "<<" ">>" "&^" "+" "-" "*" "/" "%" "&" "|" "^" "!" "<" ">" "=")
          t)
         'font-lock-operator-face))
  "Font lock keywords for Tengo mode.")

;;; Indentation (Context-Aware)

(defun tengo-indent-line ()
  "Indent current line correctly for Tengo.
Ignores braces/parens/brackets inside strings and comments."
  (interactive)
  (let ((indent-col 0))
    (save-excursion
      (beginning-of-line)
      (condition-case nil
          (if (bobp)
              (setq indent-col 0)
            (save-excursion
              (forward-line -1)
              (while (and (not (bobp)) (looking-at "^[ \t]*$"))
                (forward-line -1))
              (setq indent-col (current-indentation))
              (end-of-line)
              (skip-chars-backward " \t")
              (let ((pos (point)))
                (when (and (memq (char-before) '(?{ ?\( ?\[))
                           (save-excursion
                             (goto-char (1- pos))
                             (not (or (nth 3 (syntax-ppss))
                                      (nth 4 (syntax-ppss))))))
                  (setq indent-col (+ indent-col tengo-indent-offset)))))
            (back-to-indentation)
            (when (and (memq (char-after) '(?} ?\) ?\]))
                       (not (or (nth 3 (syntax-ppss))
                                (nth 4 (syntax-ppss)))))
              (setq indent-col (max 0 (- indent-col tengo-indent-offset)))))
        (error (setq indent-col 0))))
    (if (<= (current-column) (current-indentation))
        (indent-line-to indent-col)
      (save-excursion (indent-line-to indent-col)))))

;;; Navigation

(defun tengo-beginning-of-defun (&optional arg)
  "Move backward to the beginning of a Tengo function.
Handles top-level, named, nested, and anonymous functions."
  (interactive "^p")
  (unless arg (setq arg 1))
  (let ((found 0))
    (while (and (< found arg) (re-search-backward "\\<func\\>" nil t))
      (setq found (1+ found)))
    (= found arg)))

(defun tengo-end-of-defun (&optional arg)
  "Move forward to the end of a Tengo function."
  (interactive "^p")
  (unless arg (setq arg 1))
  (dotimes (_ arg)
    (let ((origin (point)))
      (tengo-beginning-of-defun 1)
      (when (/= (point) origin)
        (when (re-search-forward "{" nil t)
          (backward-char)
          (condition-case nil
              (forward-sexp 1)
            (error (end-of-line)))))))
  t)

;;; Imenu

(defvar tengo-imenu-generic-expression
  '(("Functions" "^\\s-*\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\s-*:=\\s-*func" 1)
    ("Variables" "^\\s-*\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\s-*:=" 1))
  "Imenu expression for Tengo.")

;;; Mode Definition

;;;###autoload
(define-derived-mode tengo-mode prog-mode "Tengo"
  "Major mode for editing Tengo code.

\\{tengo-mode-map}"
  :syntax-table tengo-mode-syntax-table
  :group 'tengo

  ;; Font lock
  (setq-local font-lock-defaults '(tengo-font-lock-keywords))

  ;; Indentation
  (setq-local indent-line-function #'tengo-indent-line)
  (setq-local tab-width tengo-indent-offset)
  (setq-local standard-indent tengo-indent-offset)
  (setq-local indent-tabs-mode nil)

  ;; Comments
  (setq-local comment-start "// ")
  (setq-local comment-start-skip "//+\\s-*")
  (setq-local comment-end "")
  (setq-local comment-use-syntax t)

  ;; Navigation
  (setq-local beginning-of-defun-function #'tengo-beginning-of-defun)
  (setq-local end-of-defun-function #'tengo-end-of-defun)

  ;; Imenu
  (setq-local imenu-generic-expression tengo-imenu-generic-expression)

  ;; Electric pairs / auto-indent triggers
  (when (boundp 'electric-indent-chars)
    (setq-local electric-indent-chars
                (append "{}()[]:," electric-indent-chars))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.tengo\\'" . tengo-mode))

(provide 'tengo-mode)

;;; tengo-mode.el ends here
