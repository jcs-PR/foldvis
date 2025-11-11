;;; foldvis-ts-fold.el --- Display indicators for ts-fold  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Shen, Jen-Chieh

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Display indicators for ts-fold.
;;

;;; Code:

(require 'foldvis)

;;
;;; Externals

(declare-function tsc-query-captures "ext:tsc.el")
(declare-function tsc-make-query "ext:tsc.el")
(declare-function tsc-root-node "ext:tsc.el")
(declare-function tsc-node-end-position "ext:tsc.el")
(declare-function tsc-node-start-position "ext:tsc.el")

(defvar tree-sitter-mode)
(defvar tree-sitter-tree)
(defvar tree-sitter-language)

(defvar ts-fold-mode)
(defvar ts-fold-range-alist)
(declare-function ts-fold--non-foldable-node-p "ext:ts-fold.el")
(declare-function ts-fold--ensure-ts "ext:ts-fold.el")
(declare-function ts-fold-overlay-at "ext:ts-fold.el")
(declare-function ts-fold--get-fold-range "ext:ts-fold.el")
(declare-function ts-fold-toggle "ext:ts-fold.el")

;;
;;; Entry

;;;###autoload
(defun foldvis-ts-fold--enable ()
  "Enable the folding minor mode."
  (add-hook 'tree-sitter-after-change-functions #'foldvis--trigger-render nil t)
  (advice-add 'ts-fold--after-command :after #'foldvis-refresh))

;;;###autoload
(defun foldvis-ts-fold--disable ()
  "Disable the folding minor mode."
  (remove-hook 'tree-sitter-after-change-functions #'foldvis--trigger-render t)
  (advice-remove 'ts-fold--after-command #'foldvis-refresh))

;;;###autoload
(defun foldvis-ts-fold--valid-p ()
  "Return non-nil if the backend is valid."
  (and (featurep 'ts-fold) ts-fold-mode))

;;
;;; Events

;;;###autoload
(defun foldvis-ts-fold--toggle ()
  "Event to toggle folding on and off."
  (ts-fold-toggle))

;;
;;; Core

(defun foldvis-ts-fold--create (node)
  "Create indicators using NODE."
  (when-let* ((range (ts-fold--get-fold-range node))
              (beg (car range)) (end (cdr range)))
    (let ((folded (ts-fold-overlay-at node)))
      (foldvis--create-overlays beg end folded))))

(defun foldvis-ts-fold--within-window (node wend wstart)
  "Return nil if NODE is not within the current window display range.

Arguments WEND and WSTART are the range for caching."
  (when-let*
      ((range (cl-case foldvis-render-method
                (`full
                 (ignore-errors (ts-fold--get-fold-range node)))
                (`partial (cons (tsc-node-start-position node)
                                (tsc-node-end-position node)))
                (t
                 (user-error "Invalid render method: %s" foldvis-render-method))))
       (start (car range))
       (end (cdr range))
       ((or (and (<= wstart start) (<= end wend))    ; with in range
            (and (<= wstart end) (<= start wstart))  ; just one above
            (and (<= wend end) (<= start wend)))))   ; just one below
    node))

;;;###autoload
(defun foldvis-ts-fold--refresh (&rest _)
  "Refresh indicators for all folding range."
  (when tree-sitter-mode
    (ts-fold--ensure-ts
      (when-let*
          ((node (ignore-errors (tsc-root-node tree-sitter-tree)))
           (patterns (seq-mapcat (lambda (fold-range) `((,(car fold-range)) @name))
                                 (alist-get major-mode ts-fold-range-alist)
                                 'vector))
           (query (ignore-errors
                    (tsc-make-query tree-sitter-language patterns)))
           (nodes-to-fold (tsc-query-captures query node #'ignore))
           (wend   (window-end nil t))
           (wstart (window-start))
           (nodes-to-fold
            (cl-remove-if-not (lambda (node)
                                (foldvis-ts-fold--within-window (cdr node) wend wstart))
                              nodes-to-fold))
           (mode-ranges (alist-get major-mode ts-fold-range-alist))
           (nodes-to-fold
            (cl-remove-if (lambda (node)
                            (ts-fold--non-foldable-node-p (cdr node) mode-ranges))
                          nodes-to-fold)))
        (foldvis--remove-ovs)
        (thread-last nodes-to-fold
                     (mapcar #'cdr)
                     (mapc #'foldvis-ts-fold--create))
        (run-hooks 'foldvis-refresh-hook)))))

(provide 'foldvis-ts-fold)
;;; foldvis-ts-fold.el ends here
