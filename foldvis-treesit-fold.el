;;; foldvis-treesit-fold.el --- Display indicators for treesit-fold  -*- lexical-binding: t; -*-

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
;; Display indicators for treesit-fold.
;;

;;; Code:

(require 'foldvis)

;;
;;; Externals

(declare-function treesit-buffer-root-node "ext:treesit.el")
(declare-function treesit-node-language "ext:treesit.el")

(defvar treesit-fold-mode)
(defvar treesit-fold-range-alist)
(declare-function treesit-fold--ensure-ts "ext:treesit-fold.el")
(declare-function treesit-fold--non-foldable-node-p "ext:treesit-fold.el"
                  (node mode-ranges))
(declare-function treesit-fold-overlay-at "ext:treesit-fold.el")
(declare-function treesit-fold--get-fold-range "ext:treesit-fold.el")
(declare-function treesit-fold-toggle "ext:treesit-fold.el")

;;
;;; Entry

;;;###autoload
(defun foldvis-treesit-fold--enable ()
  "Enable the folding minor mode."
  (advice-add 'treesit-fold--after-command :after #'foldvis-refresh))

;;;###autoload
(defun foldvis-treesit-fold--disable ()
  "Disable the folding minor mode."
  (advice-remove 'treesit-fold--after-command #'foldvis-refresh))

;;;###autoload
(defun foldvis-treesit-fold--valid ()
  "Return non-nil if the backend is valid."
  (and (featurep 'treesit-fold) treesit-fold-mode))

;;
;;; Events

;;;###autoload
(defun foldvis-treesit-fold--toggle ()
  "Event to toggle folding on and off."
  (treesit-fold-toggle))

;;
;;; Core

(defun foldvis-treesit-fold--create (node)
  "Create indicators using NODE."
  (when-let* ((range (treesit-fold--get-fold-range node))
              (beg (car range)) (end (cdr range)))
    (let ((folded (treesit-fold-overlay-at node)))
      (foldvis--create-overlays beg end folded))))

(defun foldvis-treesit-fold--within-window (node wend wstart)
  "Return nil if NODE is not within the current window display range.

Arguments WEND and WSTART are the range for caching."
  (when-let*
      ((range (cl-case foldvis-render-method
                (`full
                 (ignore-errors (treesit-fold--get-fold-range node)))
                (`partial (cons (treesit-node-start node)
                                (treesit-node-end node)))
                (t
                 (user-error "Invalid render method: %s" foldvis-render-method))))
       (start (car range))
       (end (cdr range))
       ((or (and (<= wstart start) (<= end wend))    ; with in range
            (and (<= wstart end) (<= start wstart))  ; just one above
            (and (<= wend end) (<= start wend)))))   ; just one below
    node))

;;;###autoload
(defun foldvis-treesit-fold--refresh (&rest _)
  "Refresh indicators for all folding range."
  (when (ignore-errors (treesit-buffer-root-node))
    (treesit-fold--ensure-ts
      (when-let*
          ((node (ignore-errors (treesit-buffer-root-node)))
           (patterns (seq-mapcat (lambda (fold-range) `((,(car fold-range)) @name))
                                 (alist-get major-mode treesit-fold-range-alist)))
           (query (ignore-errors
                    (treesit-query-compile (treesit-node-language node) patterns)))
           (nodes-to-fold (treesit-query-capture node query))
           (wend (window-end nil t))
           (wstart (window-start))
           (nodes-to-fold
            (cl-remove-if-not (lambda (node)
                                (ignore-errors
                                  (foldvis-treesit-fold--within-window (cdr node) wend wstart)))
                              nodes-to-fold))
           (mode-ranges (alist-get major-mode treesit-fold-range-alist))
           (nodes-to-fold
            (cl-remove-if (lambda (node)
                            (treesit-fold--non-foldable-node-p (cdr node) mode-ranges))
                          nodes-to-fold)))
        (foldvis--remove-ovs)
        (thread-last nodes-to-fold
                     (mapcar #'cdr)
                     (mapc #'foldvis-treesit-fold--create))
        (run-hooks 'foldvis-refresh-hook)))))

(provide 'foldvis-treesit-fold)
;;; foldvis-treesit-fold.el ends here
