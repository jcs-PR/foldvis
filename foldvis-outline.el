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
  (add-hook 'outline-view-change-hook #'foldvis-hideshow--refresh nil t))

;;;###autoload
(defun foldvis-outline--disable ()
  "Disable the folding minor mode."
  (remove-hook 'outline-view-change-hook #'foldvis-hideshow--refresh t))

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

(defun foldvis-outline--show-heading ()
  "Modified from the function `outline-show-heading'."
  (foldvis-outline--flag-region (- (point)
                                   (if (bobp) 0
                                     (if (and outline-blank-line
                                              (eq (char-before (1- (point))) ?\n))
                                         2 1)))
                                (progn (outline-end-of-heading) (point))
                                nil))

(defun foldvis-outline--hide-sublevels (levels)
  "Modified from the function `outline-hide-sublevels'."
  (if (< levels 1)
      (error "Must keep at least one level of headers"))
  (save-excursion
    (let* (outline-view-change-hook
           (beg (progn
                  (goto-char (point-min))
                  ;; Skip the prelude, if any.
                  (unless (outline-on-heading-p t) (outline-next-heading))
                  (point)))
           (end (progn
                  (goto-char (point-max))
                  ;; Keep empty last line, if available.
                  (if (bolp) (1- (point)) (point)))))
      (if (< end beg)
          (setq beg (prog1 end (setq end beg))))
      ;; First hide everything.
      (foldvis-outline--flag-region beg end t)
      ;; Then unhide the top level headers.
      (outline-map-region
       (lambda ()
         (if (<= (funcall outline-level) levels)
             (foldvis-outline--show-heading)))
       beg end)
      ;; Finally unhide any trailing newline.
      (goto-char (point-max))
      (if (and (bolp) (not (bobp)) (outline-invisible-p (1- (point))))
          (foldvis-outline--flag-region (1- (point)) (point) nil)))))

(defun foldvis-outline--nodes ()
  "Return a list of foldable nodes."
  (let (outline-view-change-hook
        foldvis-outline--nodes)
    (ignore-errors (foldvis-outline--hide-sublevels 1))
    foldvis-outline--nodes))

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
                                   nodes-to-fold))
                (nodes-to-fold (cdr (reverse nodes-to-fold))))
      (ic nodes-to-fold)
      (foldvis--remove-ovs)
      (thread-last nodes-to-fold
                   (mapc #'foldvis-outline--create)))
    (run-hooks 'foldvis-refresh-hook)))

(provide 'foldvis-outline)
;;; foldvis-outline.el ends here
