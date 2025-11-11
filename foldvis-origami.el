;;; foldvis-origami.el --- Display indicators for origami  -*- lexical-binding: t; -*-

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
;; Display indicators for origami.
;;

;;; Code:

(require 'foldvis)

;;
;;; Externals

(defvar origami-mode)

(declare-function origami-get-fold-tree "ext:origami.el")
(declare-function origami-fold-open? "ext:origami.el")
(declare-function origami-fold-end "ext:origami.el")
(declare-function origami-fold-beg "ext:origami.el")
(declare-function origami-toggle-node "ext:origami.el")

;;
;;; Entry

;;;###autoload
(defun foldvis-origami--valid ()
  "Return non-nil if the backend is valid."
  (and (featurep 'origami) origami-mode))

;;
;;; Events

;;;###autoload
(defun foldvis-origami--toggle ()
  "Event to toggle folding on and off."
  (call-interactively #'origami-toggle-node))

;;
;;; Core

(defun foldvis-origami--create (node)
  "Create indicators using NODE."
  (when-let* ((beg (origami-fold-beg node))
              (end (origami-fold-end node)))
    (let ((folded (not (origami-fold-open? node))))
      (foldvis--create-overlays beg end folded))))

(defun foldvis-origami--within-window (node wend wstart)
  "Return nil if NODE is not within the current window display range.

Arguments WEND and WSTART are the range for caching."
  (when-let*
      ((range (cl-case foldvis-render-method
                ((or full partial) (cons (origami-fold-beg node)
                                         (origami-fold-end node)))
                (t
                 (user-error "Invalid render method: %s" foldvis-render-method))))
       (start (car range))
       (end (cdr range))
       ((or (and (<= wstart start) (<= end wend))    ; with in range
            (and (<= wstart end) (<= start wstart))  ; just one above
            (and (<= wend end) (<= start wend)))))   ; just one below
    node))

;;;###autoload
(defun foldvis-origami--refresh (&rest _)
  "Refresh indicators for all folding range."
  (when-let*
      ((tree (ignore-errors (origami-get-fold-tree (current-buffer))))
       (nodes-to-fold (elt tree 4))
       (wend   (window-end nil t))
       (wstart (window-start))
       (nodes-to-fold
        (cl-remove-if-not (lambda (node)
                            (foldvis-origami--within-window node wend wstart))
                          nodes-to-fold)))
    (foldvis--remove-ovs)
    (thread-last nodes-to-fold
                 (mapc #'foldvis-origami--create))
    (run-hooks 'foldvis-refresh-hook)))

(provide 'foldvis-origami)
;;; foldvis-origami.el ends here
