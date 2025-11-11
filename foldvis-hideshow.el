;;; foldvis-hideshow.el --- Display indicators for hideshow  -*- lexical-binding: t; -*-

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
;; Display indicators for hideshow.
;;

;;; Code:

(require 'hideshow)

(require 'foldvis)

;;
;;; Entry

;;;###autoload
(defun foldvis-hideshow--enable ()
  "Enable the folding minor mode."
  (add-hook 'hs-show-hook #'foldvis-hideshow--refresh nil t)
  (add-hook 'hs-hide-hook #'foldvis-hideshow--refresh nil t))

;;;###autoload
(defun foldvis-hideshow--disable ()
  "Disable the folding minor mode."
  (remove-hook 'hs-show-hook #'foldvis-hideshow--refresh t)
  (remove-hook 'hs-hide-hook #'foldvis-hideshow--refresh t))

;;;###autoload
(defun foldvis-hideshow--valid ()
  "Return non-nil if the backend is valid."
  (and (featurep 'hideshow) hs-minor-mode))

;;
;;; Events

;;;###autoload
(defun foldvis-hideshow--toggle ()
  "Event to toggle folding on and off."
  (hs-toggle-hiding))

;;
;;; Overwrite

;; XXX: This section is mostly copy-pasted code. It’s admittedly messy,
;; but I’m too lazy to clean it up right now.

(defvar foldvis-hideshow--nodes nil
  "Store a list of nodes to fold.")

(defun foldvis-hideshow--make-overlay (b e kind &optional b-offset e-offset)
  "Modified from the function `hs-make-overlay'."
  (push (list b e kind b-offset e-offset) foldvis-hideshow--nodes))

(defun foldvis-hideshow--hide-comment-region (beg end &optional repos-end)
  "Modified from the function `hs-hide-comment-region'."
  (let ((goal-col (current-column))
        (beg-bol (progn (goto-char beg) (line-beginning-position)))
        (beg-eol (line-end-position))
        (end-eol (progn (goto-char end) (line-end-position))))
    (foldvis-hideshow--make-overlay beg-eol end-eol 'comment beg end)
    (goto-char (if repos-end end (min end (+ beg-bol goal-col))))))

(defun foldvis-hideshow--hide-block-at-point (&optional end comment-reg)
  "Modified from the function `hs-hide-block-at-point'."
  (if comment-reg
      (foldvis-hideshow--hide-comment-region (car comment-reg) (cadr comment-reg) end)
    (when (funcall hs-looking-at-block-start-p-func)
      (let ((mdata (match-data t))
            (header-end (match-end 0))
            p q ov)
        ;; `p' is the point at the end of the block beginning, which
        ;; may need to be adjusted
        (save-excursion
          (goto-char (funcall (or hs-adjust-block-beginning #'identity)
                              header-end))
          (setq p (line-end-position)))
        ;; `q' is the point at the end of the block
        (hs-forward-sexp mdata 1)
        (setq q (if (looking-back hs-block-end-regexp nil)
                    (match-beginning 0)
                  (point)))
        (when (and (< p q) (> (count-lines p q) 1))
          (cond ((and hs-allow-nesting (setq ov (hs-overlay-at p)))
                 (delete-overlay ov))
                ((not hs-allow-nesting)
                 ;;(hs-discard-overlays p q)
                 ))
          (foldvis-hideshow--make-overlay p q 'code (- header-end p)))
        (goto-char (if end q (min p header-end)))))))

(defun foldvis-hideshow--nodes ()
  "Return a list of foldable nodes.

Modified from the function `hs-hide-all'."
  (let (foldvis-hideshow--nodes)
    (hs-life-goes-on
     (save-excursion
       (goto-char (point-min))
       (syntax-propertize (point-max))
       (let ((re (concat "\\("
                         hs-block-start-regexp
                         "\\)"
                         (if hs-hide-comments-when-hiding-all
                             (concat "\\|\\("
                                     hs-c-start-regexp
                                     "\\)")
                           ""))))
         (while (funcall hs-find-next-block-func re (point-max)
                         hs-hide-comments-when-hiding-all)
           (if (match-beginning 1)
               ;; We have found a block beginning.
               (progn
                 (goto-char (match-beginning 1))
                 (unless (if hs-hide-all-non-comment-function
                             (funcall hs-hide-all-non-comment-function)
                           (foldvis-hideshow--hide-block-at-point t))
                   ;; Go to end of matched data to prevent from getting stuck
                   ;; with an endless loop.
                   (when (looking-at hs-block-start-regexp)
                     (goto-char (match-end 0)))))
             ;; found a comment, probably
             (let ((c-reg (hs-inside-comment-p)))
               (when (and c-reg (car c-reg))
                 (if (> (count-lines (car c-reg) (nth 1 c-reg)) 1)
                     (foldvis-hideshow--hide-block-at-point t c-reg)
                   (goto-char (nth 1 c-reg))))))))))
    foldvis-hideshow--nodes))

;;
;;; Core

(defun foldvis-hideshow--create (node)
  "Create indicators using NODE."
  (when-let* ((beg (nth 0 node))
              (end (nth 1 node)))
    (let ((folded (save-excursion
                    (goto-char beg)
                    (hs-already-hidden-p))))
      (foldvis--create-overlays beg end folded))))

(defun foldvis-hideshow--within-window (node wend wstart)
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
(defun foldvis-hideshow--refresh (&rest _)
  "Refresh indicators for all folding range."
  (when hs-minor-mode
    (when-let* ((nodes-to-fold (foldvis-hideshow--nodes))
                (wend   (window-end nil t))
                (wstart (window-start))
                (nodes-to-fold
                 (cl-remove-if-not (lambda (node)
                                     (foldvis-hideshow--within-window node wend wstart))
                                   nodes-to-fold)))
      (foldvis--remove-ovs)
      (thread-last nodes-to-fold
                   (mapc #'foldvis-hideshow--create)))
    (run-hooks 'foldvis-refresh-hook)))

(provide 'foldvis-hideshow)
;;; foldvis-hideshow.el ends here
