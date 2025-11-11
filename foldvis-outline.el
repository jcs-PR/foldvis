;;; foldvis-outline.el --- Display indicators for outline  -*- lexical-binding: t; -*-

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
;; Display indicators for outline.
;;

;;; Code:

(require 'outline)

(require 'foldvis)

;;
;;; Entry

;;;###autoload
(defun foldvis-outline--enable ()
  "Enable the folding minor mode."
  (add-hook 'outline-view-change-hook #'foldvis-refresh nil t))

;;;###autoload
(defun foldvis-outline--disable ()
  "Disable the folding minor mode."
  (remove-hook 'outline-view-change-hook #'foldvis-refresh t))

;;;###autoload
(defun foldvis-outline--valid-p ()
  "Return non-nil if the backend is valid."
  (and (featurep 'outline) outline-minor-mode))

;;
;;; Events

;;;###autoload
(defun foldvis-outline--toggle ()
  "Event to toggle folding on and off."
  (outline-toggle-children))

;;
;;; Overwrite

(defvar-local foldvis-outline--nodes nil
  "Store a list of nodes to fold.")

(defun foldvis-outline--flag-region (from to flag)
  "To override the function `outline-flag-region'."
  (push (list from to flag) foldvis-outline--nodes))

(defun foldvis-outline--flag-subtree (flag)
  "Modified from the function `outline-flag-subtree'."
  (save-excursion
    (outline-back-to-heading)
    (outline-end-of-heading)
    (foldvis-outline--flag-region (point)
                                  (progn (outline-end-of-subtree) (point))
                                  flag)))

(defun foldvis-outline--nodes ()
  "Return a list of foldable nodes."
  (save-excursion
    (let (outline-view-change-hook
          foldvis-outline--nodes)
      (goto-char (point-min))
      (while (not (eobp))
        (ignore-errors (foldvis-outline--flag-subtree t))
        (forward-line 1))
      (delete-dups foldvis-outline--nodes))))

;;
;;; Core

(defun foldvis-outline--create (node)
  "Create indicators using NODE."
  (when-let* ((beg (nth 0 node))
              (end (nth 1 node)))
    (let ((folded (save-excursion
                    (goto-char beg)
                    (outline-invisible-p (line-end-position)))))
      (foldvis--create-overlays beg end folded))))

(defun foldvis-outline--within-window (node wend wstart)
  "Return nil if NODE is not within the current window display range.

Arguments WEND and WSTART are the range for caching."
  (when-let*
      ((range (cl-case foldvis-render-method
                ((or full partial) (cons (nth 0 node)
                                         (nth 1 node)))
                (t
                 (user-error "Invalid render method: %s" foldvis-render-method))))
       (start (car range))
       (end (cdr range))
       ((or (and (<= wstart start) (<= end wend))    ; with in range
            (and (<= wstart end) (<= start wstart))  ; just one above
            (and (<= wend end) (<= start wend)))))   ; just one below
    node))

;;;###autoload
(defun foldvis-outline--refresh (&rest _)
  "Refresh indicators for all folding range."
  (when outline-minor-mode
    (when-let* ((nodes-to-fold (foldvis-outline--nodes))
                (wend   (window-end nil t))
                (wstart (window-start))
                (nodes-to-fold
                 (cl-remove-if-not (lambda (node)
                                     (foldvis-outline--within-window node wend wstart))
                                   nodes-to-fold)))
      (foldvis--remove-ovs)
      (thread-last nodes-to-fold
                   (mapc #'foldvis-outline--create)))
    (run-hooks 'foldvis-refresh-hook)))

(provide 'foldvis-outline)
;;; foldvis-outline.el ends here
