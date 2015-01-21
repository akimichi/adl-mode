;;; adl-mode --- A minor mode for editing openEHR ADL files.
;;; Commentary:
;; This is an emacs major mode for ADL that does keyword coloring
;; and indentation.
;; Copyright (C) 2013 Akimichi Tatsukawa

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;;
;;; Code:

(defvar adl-mode-syntax-table nil
  "Syntax table used in `adl-mode' buffers.")
(if adl-mode-syntax-table
    nil
  (setq adl-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?-  ". 124b" adl-mode-syntax-table)
;  (modify-syntax-entry ?/  ". 124b" adl-mode-syntax-table)
  (modify-syntax-entry ?*  ". 23"   adl-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" adl-mode-syntax-table)
  (modify-syntax-entry ?\( "()" adl-mode-syntax-table)
  (modify-syntax-entry ?\) ")(" adl-mode-syntax-table)
  (modify-syntax-entry ?\[ "(]" adl-mode-syntax-table)
  (modify-syntax-entry ?\] ")[" adl-mode-syntax-table)
  (modify-syntax-entry ?\{ "(}" adl-mode-syntax-table)
  (modify-syntax-entry ?\} "){" adl-mode-syntax-table)
  ;; Add operator symbols misassigned in the std table
  (modify-syntax-entry ?\$ "."  adl-mode-syntax-table)
  (modify-syntax-entry ?\% "."  adl-mode-syntax-table)
  (modify-syntax-entry ?\& "."  adl-mode-syntax-table)
  (modify-syntax-entry ?\* "."  adl-mode-syntax-table)
  (modify-syntax-entry ?\+ "."  adl-mode-syntax-table)
;  (modify-syntax-entry ?\- "."  adl-mode-syntax-table)
  (modify-syntax-entry ?\/ "."  adl-mode-syntax-table)
  (modify-syntax-entry ?\< "."  adl-mode-syntax-table)
  (modify-syntax-entry ?\= "."  adl-mode-syntax-table)
  (modify-syntax-entry ?\> "."  adl-mode-syntax-table)
  (modify-syntax-entry ?\| "."  adl-mode-syntax-table)

  (modify-syntax-entry ?\_ "w"  adl-mode-syntax-table)
  ;; backquote is open and close paren
  (modify-syntax-entry ?\` "$"  adl-mode-syntax-table)
  ;; comment delimiters
  (modify-syntax-entry ?  "    " adl-mode-syntax-table)
  (modify-syntax-entry ?\t "    " adl-mode-syntax-table)
  (modify-syntax-entry ?\r "    " adl-mode-syntax-table)
  (modify-syntax-entry ?\f "    " adl-mode-syntax-table))

(defvar adl-keywords
  '("archetype" "specialise" "concept" "language" "description" "definition" "ontology")
  "ADL keywords.")

(defvar adl-sections
  '("archetype" "specialise" "concept" "language" "description" "definition" "ontology")
  "ADL keywords for sections.")

(defvar adl-rmtypes
  '("COMPOSITION" "OBSERVATION" "HISTORY" "EVENT" "DV_TEXT" "DV_CODED_TEXT" "SECTION" "EVENT_CONTEXT" "ITEM_TREE" "ELEMENT" "ENTRY" "EVALUATION")
  "RM types.")

(defvar adl-sections-regexp (concat "^" (regexp-opt adl-sections 'words)))
(defvar adl-keywords-regexp (regexp-opt adl-keywords 'words))
(defvar adl-rmtypes-regexp (regexp-opt adl-rmtypes 'words))
(defvar adl-terms-regexp "\\[at[0-9.]+\\]")
(defvar adl-fields-regexp "^[ \t]+\\([a-zA-Z_]+[a-zA-Z0-9_]*\\)")

(defvar adl-font-lock-keywords nil
  "Font-lock keywords used in `adl-mode' buffers.")

(setq adl-font-lock-keywords
  `(
    (,adl-sections-regexp . font-lock-function-name-face)
    (,adl-rmtypes-regexp . font-lock-variable-name-face)
    (,adl-terms-regexp . font-lock-constant-face)
    (,adl-fields-regexp . font-lock-type-face)
    ))

(defvar adl-mode-map ()
  "Keymap used in `adl-mode' buffers.")
(if adl-mode-map
    nil
  (progn
    (setq adl-mode-map (make-sparse-keymap))
    (define-key adl-mode-map "\C-m" 'adl-return)
))

(defun adl-return (&optional arg)
  (interactive)
  (adl-indent-line)
  (newline-and-indent)
)

(defvar adl-indent-expr "[{|]")

(defvar same-indent-expr "&&\\|||")

(defun in-comment()
  (save-excursion
    (if (looking-at "--") ;; if we are at the start of a comment, true
    t
      (let ((startpoint (point)))
    (re-search-backward "--" (- (point) (current-column)) t 1)
    (if (< (point) startpoint) ;; if there is a // before us in the line
        t ;; in comment
      (progn
        (re-search-backward "--" nil t 1)
        (if (< (point) startpoint) ;; if there is a /* somewhere before us
        (let ((opencomm (point)))
          ;;(insert "before")
          (goto-char startpoint)
          (re-search-backward "--" opencomm t 1)
          (if (< (point) startpoint) ;; and a */ between /* and us
              nil ;; not in comment
            t) ;; in comment
          )
        )
        )
      )
    )
      )
    )
  )


(defun adl-indent-line (&optional whole-exp)
  "Indent current line as Lisp code.
With argument, indent any additional lines of the same expression
rigidly along with this one."
  (interactive "P")
  (let ((indentcol -1))
    (save-excursion
      (let ((oldpoint (point)) bolast last-indent last-end bol eol
      (obrace 0) (cbrace 0) (sameindent 0))
    (when (in-comment)
      (setq sameindent 1))
    (beginning-of-line)
    (setq bol (point))
    (condition-case nil
        (re-search-backward "[!-~]")
      (error (setq indentcol 0)))
    (when (< indentcol 0)
      (if (looking-at adl-indent-expr)
          (setq obrace 1)
        (setq obrace 0))
      (when (looking-at ",")
          (setq sameindent 1))
      (when (> (current-column) 0)
        (backward-char))
      (when (looking-at "=>")
          (setq obrace 1))
      (when (looking-at same-indent-expr)
        (setq sameindent 1))
      (beginning-of-line)
      (setq bolast (current-column))
      (re-search-forward "[!-~]")
      (backward-char)
      (setq last-indent (- (current-column) bolast))
      (goto-char oldpoint)
      (end-of-line)
      (setq eol (point))
      (beginning-of-line)
      (re-search-forward "[!-~]" eol t 1)
      (if (> (point) bol) (backward-char) nil)
      (if (looking-at "}")
          (setq cbrace 1)
        (setq cbrace 0))
      (when (looking-at "{")
        (setq sameindent 1))
      (if (= sameindent 1)
          (setq indentcol last-indent)
        (if (= obrace 1)
        (if (= cbrace 1)
            (setq indentcol last-indent)
          (setq indentcol (+ last-indent 3)))
          ;; if there isn't an opening brace at the end of the last row,
          ;; use the nearest enclosing sexp to determine indentation
          ;; if the enclosing sexp starts with ( or [
          (save-excursion
        (condition-case nil
            (progn
              (up-list 1)
              (backward-sexp 1)
              (if (looking-at "[\\(]")
              (setq indentcol (+ 1 (current-column)))
            ;; if enclosing sexp starts with {, indent three from the line
            ;; with the {
            (progn
              (beginning-of-line)
              (re-search-forward "[!-~]")
              (backward-char)
              (if (= cbrace 1)
                  (setq indentcol (current-column))
                (setq indentcol (+ 3 (current-column)))))))
          (error (setq indentcol last-indent)))
        )))

      )
    (if (> (current-column) indentcol)
        (delete-region bol (point))
      ())
    (indent-to indentcol)
    ))
    (if (< (current-column) indentcol)
    (move-to-column indentcol)
      nil
      )
    )
)

;; (defun adl-comment-dwim (arg)
;;   "Comment or uncomment current line or region in a smart way.
;; For detail, see `comment-dwim'."
;;   (interactive "*P")
;;   (require 'newcomment)
;;   (let ((deactivate-mark nil) (comment-start "-- ") (comment-end ""))
;;     (comment-dwim arg)))

;; (defun comment-or-uncomment-region-or-line ()
;;     "Comments or uncomments the region or the current line if there's no active region."
;;     (interactive)
;;     (let (beg end)
;;         (if (region-active-p)
;;             (setq beg (region-beginning) end (region-end))
;;             (setq beg (line-beginning-position) end (line-end-position)))
;;         (comment-or-uncomment-region beg end)
;;         (next-line)))
	 
;; (defun comment-or-uncomment-region-or-line ()
;;     "Comments or uncomments the region or the current line if there's no active region."
;;     (interactive)
;;     (let (beg end)
;;         (if (region-active-p)
;;             (setq beg (region-beginning) end (region-end))
;;             (setq beg (line-beginning-position) end (line-end-position)))
;;         (comment-or-uncomment-region beg end)))
 
(defun adl-mode ()
  (interactive)
  (kill-all-local-variables)

;  (define-key adl-mode-map [remap comment-dwim] 'adl-comment-dwim)

  (make-local-variable 'font-lock-defaults)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'comment-column)
  (make-local-variable 'comment-indent-function)
  (make-local-variable 'indent-line-function)

  (set-syntax-table adl-mode-syntax-table)

  (setq major-mode          'adl-mode
    mode-name               "ADL"
    font-lock-defaults      '(adl-font-lock-keywords)
    comment-start           "--"
;    comment-end             "\n"
    comment-start-skip      "--"
    comment-column          40
    comment-indent-function 'java-comment-indent
    indent-line-function    'adl-indent-line
    indent-tabs-mode        t
    )
  (use-local-map adl-mode-map)
  (run-hooks 'adl-mode-hook)
)

;; (define-derived-mode adl-mode fundamental-mode
;;   "adl-mode is a major mode for editing ADL files."
;;   :syntax-table adl-mode-syntax-table
;; ;(define-derived-mode adl-mode fundamental-mode
;; ;  (setq font-lock-defaults '(adl-font-lock-keywords))
;; ;  (setq adl-keywords-regexp nil)
;; ;  (setq adl-rmtypes-regexp nil)

;;   (make-local-variable 'font-lock-defaults)
;;   (make-local-variable 'comment-start)
;;   (make-local-variable 'comment-end)
;;   (set-syntax-table adl-mode-syntax-table)

;;   (setq major-mode          'adl-mode
;;     mode-name               "Adl"
;;     font-lock-defaults      '(adl-font-lock-keywords)
;;     adl-keywords-regexp     nil
;;     adl-rmtypes-regexp      nil
;;     comment-start           "--"
;;     comment-end             "\n"
;; ;    comment-start-skip      "/\\*+ *"
;;     comment-column          40
;;     comment-indent-function 'java-comment-indent
;;     indent-line-function    'adl-indent-line
;;     indent-tabs-mode        t
;;     )
;;   (use-local-map adl-mode-map)
;; )

(provide 'adl-mode)

;;
;; On Load
;;

;; Run coffee-mode for files ending in .coffee.
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.adl$" . adl-mode))
