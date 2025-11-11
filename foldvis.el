;;; foldvis.el --- Display indicators for various folding systems  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/emacs-vs/foldvis
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (elenv "0.1.0") (fringe-helper "1.0.1"))
;; Keywords: convenience folding

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
;; Display indicators for various folding systems.
;;

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(require 'elenv)
(require 'fringe-helper)

(defgroup foldvis nil
  "Display indicators for various folding systems."
  :group 'convenience
  :prefix "foldvis-")

(defcustom foldvis-backends '( ts-fold treesit-fold
                               origami
                               hideshow
                               outline)
  "List of backends we want to use in order."
  :type '(list symbol)
  :group 'foldvis)

(defcustom foldvis-fringe 'left-fringe
  "Display indicators on the left/right fringe."
  :type '(choice (const :tag "On the right fringe" right-fringe)
                 (const :tag "On the left fringe" left-fringe))
  :group 'foldvis)

(defcustom foldvis-priority 30
  "Indicators overlay's priority."
  :type 'integer
  :group 'foldvis)

;; TODO: We eventually want to remove this. Therefore, we get fast and
;; accurate results!
(defcustom foldvis-render-method 'partial
  "Method used to display indicators."
  :type '(choice (const :tag "Accurate rendering but cost more performance" full)
                 (const :tag "Inaccurate rendering but fast" partial))
  :group 'foldvis)

(defcustom foldvis-commands '( foldvis-click-fringe)
  "A list of commands to refresh render."
  :type '(list symbol)
  :group 'foldvis)

(defcustom foldvis-refresh-hook nil
  "Hook run after indicators refresh."
  :type 'hook
  :group 'foldvis)

(defcustom foldvis-face-function nil
  "Function call when apply to indicators face."
  :type 'function
  :group 'foldvis)

(defface foldvis-fringe-face
  '((t ()))
  "Face used to display fringe contents."
  :group 'foldvis)

(fringe-helper-define 'foldvis-fr-plus nil
  "XXXXXXX"
  "X.....X"
  "X..X..X"
  "X.XXX.X"
  "X..X..X"
  "X.....X"
  "XXXXXXX")

(fringe-helper-define 'foldvis-fr-minus-tail nil
  "........" "........" "........" "........" "........"
  "........" "........" "........" "........" "........"
  "XXXXXXX"
  "X.....X"
  "X.....X"
  "X.XXX.X"
  "X.....X"
  "X.....X"
  "XXXXXXX"
  "...XX..." "...XX..." "...XX..." "...XX..." "...XX..."
  "...XX..." "...XX..." "...XX..." "...XX..." "...XX...")

(fringe-helper-define 'foldvis-fr-center nil
  "...XX..." "...XX..." "...XX..." "...XX..." "...XX..."
  "...XX..." "...XX..." "...XX..." "...XX..." "...XX..."
  "...XX..." "...XX..." "...XX..." "...XX..." "...XX..."
  "...XX..." "...XX..." "...XX..." "...XX..." "...XX..."
  "...XX..." "...XX..." "...XX...")

(fringe-helper-define 'foldvis-fr-end-left nil
  "...XX..." "...XX..." "...XX..." "...XX..." "...XX..."
  "...XX..." "...XX..." "...XX..." "...XX..." "...XX..."
  "...XX..." "...XXXXX" "...XXXXX"
  "........" "........" "........" "........" "........"
  "........" "........" "........" "........" "........")

(fringe-helper-define 'foldvis-fr-end-right nil
  "...XX..." "...XX..." "...XX..." "...XX..." "...XX..."
  "...XX..." "...XX..." "...XX..." "...XX..." "...XX..."
  "...XX..." "XXXXX..." "XXXXX..."
  "........" "........" "........" "........" "........"
  "........" "........" "........" "........" "........")

(defvar-local foldvis--backend nil
  "The current chosen backend.")

;;
;; (@* "Util" )
;;

(defmacro foldvis--with-selected-window (window &rest body)
  "Same with `with-selected-window' but safe.

See macro `with-selected-window' description for arguments WINDOW and BODY."
  (declare (indent 1) (debug t))
  `(when (window-live-p ,window) (with-selected-window ,window ,@body)))

(defun foldvis--overlays-in (prop name &optional beg end)
  "Return overlays with PROP of NAME, from region BEG to END."
  (unless beg (setq beg (point-min))) (unless end (setq end (point-max)))
  (let ((lst)
        (ovs (overlays-in beg end)))
    (dolist (ov ovs)
      (when (eq name (overlay-get ov prop))
        (push ov lst)))
    lst))

;;
;; (@* "Entry" )
;;

(defun foldvis--call-backend (name &optional backend)
  "Call the BACKEND function by NAME."
  (when-let* ((name (format "foldvis-%s-%s" (or backend
                                                foldvis--backend)
                            name))
              (name (intern name))
              ((fboundp name)))
    (funcall name)))

(defun foldvis--choose-backend ()
  "Set the current possible backend."
  (let* ((backend (cl-some (lambda (x)
                             (when (foldvis--call-backend "-valid-p" x)
                               x))
                           foldvis-backends))
         (changed (and foldvis--backend backend
                       (not (equal foldvis--backend backend)))))
    (setq foldvis--backend backend)
    ;; If backend has changed, re-render once.
    (when changed
      (foldvis--render-buffer))
    ;; If backend has become nil, remove it.
    (unless backend
      (foldvis--remove-ovs-buffer))))

(defvar foldvis-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [left-fringe mouse-1] #'foldvis-click-fringe)
    (define-key map [right-fringe mouse-1] #'foldvis-click-fringe)
    map)
  "The keymap for function `foldvis-mode'.")

(defun foldvis--enable ()
  "Start folding minor mode."
  (foldvis--call-backend "-enable")
  (add-hook 'after-save-hook #'foldvis--trigger-render nil t)
  (add-hook 'post-command-hook #'foldvis--post-command nil t)
  (add-hook 'window-size-change-functions #'foldvis--size-change)
  (add-hook 'window-scroll-functions #'foldvis--scroll))

(defun foldvis--disable ()
  "Stop folding minor mode."
  (setq foldvis--backend nil)
  (foldvis--call-backend "-disable")
  (remove-hook 'after-save-hook #'foldvis--trigger-render t)
  (remove-hook 'post-command-hook #'foldvis--post-command t)
  (remove-hook 'window-size-change-functions #'foldvis--size-change)
  (remove-hook 'window-scroll-functions #'foldvis--scroll)
  (foldvis--remove-ovs-buffer))

;;;###autoload
(define-minor-mode foldvis-mode
  "Display indicators for various folding systems."
  :group 'foldvis
  :init-value nil
  :keymap foldvis-mode-map
  :lighter " FoldVis"
  (if foldvis-mode (foldvis--enable) (foldvis--disable)))

(defun foldvis--turn-on-foldvis-mode ()
  "Turn on the `foldvis-mode'."
  (foldvis-mode 1))

;;;###autoload
(define-globalized-minor-mode global-foldvis-mode
  foldvis-mode foldvis--turn-on-foldvis-mode
  :group 'foldvis)

;;
;; (@* "Events" )
;;

;;;###autoload
(defun foldvis-toggle ()
  "Call internal toggle function."
  (foldvis--call-backend "-toggle"))

(defun foldvis-click-fringe (event)
  "Handle the EVENT click on fringe."
  (interactive "e")
  (let ((current-fringe (nth 1 (car (cdr event)))) ovs ov cur-ln)
    (when (eq current-fringe foldvis-fringe)
      (mouse-set-point event)
      (beginning-of-line)
      (setq cur-ln (line-number-at-pos (point)))
      (setq ovs (append (foldvis--overlays-in 'type 'foldvis-fr-plus)
                        (foldvis--overlays-in 'type 'foldvis-fr-minus-tail)))
      (when ovs
        (setq ov (cl-some
                  (lambda (ov) (= cur-ln (line-number-at-pos (overlay-start ov))))
                  ovs))
        (when ov
          (or (save-excursion
                (end-of-line)
                (when (nth 4 (syntax-ppss)) (back-to-indentation))
                (foldvis-toggle))
              (foldvis-toggle)))))))

;;
;; (@* "Core" )
;;

(defun foldvis--create-ov-at-point ()
  "Create indicator overlay at current point."
  (let* ((pos (line-beginning-position))
         (ov (make-overlay pos (1+ pos)))
         (window (selected-window)))
    (overlay-put ov 'foldvis-window window)
    (overlay-put ov 'window window)
    ov))

(defun foldvis--create-overlays (beg end folded)
  "Create indicators overlays in range of BEG to END.

If argument FOLDED is non-nil, means the region is close/hidden (overlay
is created); this is used to determie what indicators' bitmap to use."
  (let (ov-lst)
    (save-excursion
      (goto-char beg)
      (while (and (<= (line-beginning-position) end) (not (eobp)))
        (push (foldvis--create-ov-at-point) ov-lst)
        (forward-line 1)))
    (foldvis--update-overlays (reverse ov-lst) folded)))

(defun foldvis--get-priority (bitmap)
  "Return the priority integer depends on the type of the BITMAP.

This is a static/constant method."
  (let ((prior foldvis-priority))
    (cl-case bitmap
      (foldvis-fr-plus       (+ prior 2))
      (foldvis-fr-minus-tail (+ prior 2))
      (foldvis-fr-end-left   (+ prior 1))
      (foldvis-fr-end-right  (+ prior 1))
      (t                     prior))))

(defun foldvis--get-string (folded ov bitmap)
  "Return a string or nil for indicators overlay (OV).

If argument FOLDED is nil, it must return a string so all indicators are shown
in range.  Otherwise, we should only return string only when BITMAP is the
head (first line) of the region."
  (let* ((face (or (and (functionp foldvis-face-function)
                        (funcall foldvis-face-function (overlay-start ov)))
                   'foldvis-fringe-face))
         (str (propertize "." 'display `(,foldvis-fringe ,bitmap ,face))))
    (if (not folded) str
      (cl-case bitmap
        (foldvis-fr-plus       str)  ; return string only in head
        (foldvis-fr-minus-tail nil)
        (foldvis-fr-end-left   nil)
        (foldvis-fr-end-right  nil)
        (t                     nil)))))

(defun foldvis--active-ov (folded ov bitmap)
  "SHOW the indicator OV with BITMAP.

Argument FOLDED holds folding state; it's a boolean."
  (when (overlayp ov)
    (overlay-put ov 'foldvis-active folded)
    (overlay-put ov 'type bitmap)
    (overlay-put ov 'priority (foldvis--get-priority bitmap))
    (overlay-put ov 'before-string (foldvis--get-string folded ov bitmap))))

(defun foldvis--get-end-fringe ()
  "Return end fringe bitmap according to variable `foldvis-fringe'."
  (cl-case foldvis-fringe
    (left-fringe 'foldvis-fr-end-left)
    (right-fringe 'foldvis-fr-end-right)
    (t (user-error "Invalid indicators fringe type: %s" foldvis-fringe))))

(defun foldvis--update-overlays (ov-lst folded)
  "SHOW indicators overlays OV-LST depends on FOLDED."
  (when-let* ((len (length ov-lst))
              ((> len 1))
              (len-1 (1- len))
              (first-ov (nth 0 ov-lst))
              (last-ov (nth len-1 ov-lst))
              (index 1))
    ;; Head
    (foldvis--active-ov folded first-ov
                        (if folded 'foldvis-fr-plus
                          'foldvis-fr-minus-tail))
    ;; Last
    (foldvis--active-ov folded last-ov (foldvis--get-end-fringe))
    ;; In between `head' and `last'
    (while (< index len-1)
      (foldvis--active-ov folded (nth index ov-lst) 'foldvis-fr-center)
      (cl-incf index)))
  ov-lst)

;;
;; (@* "Update" )
;;

(defvar-local foldvis--render-this-command-p nil
  "Set to non-nil if render current command.")

(defun foldvis--size-change (&optional frame &rest _)
  "Render indicators for all visible windows from FRAME."
  (elenv-with-no-redisplay
    (dolist (win (window-list frame))
      (foldvis--render-window win))))

(defun foldvis--scroll (&optional window &rest _)
  "Render indicators on WINDOW."
  (elenv-with-no-redisplay
    (foldvis--render-window window)))

(defun foldvis--render-buffer ()
  "Render indicators for current buffer."
  (dolist (window (get-buffer-window-list nil nil t))
    (foldvis--render-window window)))

(defun foldvis--render-window (window)
  "Render indicators for WINDOW."
  (foldvis--with-selected-window window
    (ignore-errors (foldvis-refresh))))

(defun foldvis--trigger-render (&rest _)
  "Trigger rendering on the next redisplay."
  (setq foldvis--render-this-command-p t))  ; Trigger render at the end.

(defun foldvis--post-command ()
  "Post command."
  (foldvis--choose-backend)
  (when (or foldvis--render-this-command-p
            (memq this-command foldvis-commands))
    (foldvis-refresh)
    (setq foldvis--render-this-command-p nil)))

(defun foldvis--remove-ovs (&optional window)
  "Remove all indicators overlays in this WINDOW."
  (remove-overlays (point-min) (point-max) 'foldvis-window
                   (or window (selected-window))))

(defun foldvis--remove-ovs-buffer ()
  "Remove all indicators overlays for this buffer."
  (dolist (window (get-buffer-window-list nil nil t))
    (foldvis--remove-ovs window)))

;;;###autoload
(defun foldvis-refresh (&rest _)
  "The refresh event."
  (when foldvis-mode
    (foldvis--call-backend "-refresh")))

(provide 'foldvis)
;;; foldvis.el ends here
