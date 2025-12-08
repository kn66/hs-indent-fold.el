;;; hs-indent-fold.el --- Click-to-fold via indent region detection -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Nobuyuki Kamimoto
;; Version: 0.3
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, folding

;;; Commentary:

;; This package provides click-to-fold functionality for foldable regions.
;; Clicking on the indentation area toggles the fold state using hideshow.
;;
;; Usage:
;;   (add-hook 'prog-mode-hook 'hs-indent-fold-mode)

;;; Code:

(require 'hideshow)

;;;; Customization

(defgroup hs-indent-fold nil
  "Click-to-fold via indent region detection."
  :group 'hideshow
  :prefix "hs-indent-fold-")

(defcustom hs-indent-fold-update-delay 0.1
  "Delay in seconds before updating overlays after buffer changes."
  :type 'number
  :group 'hs-indent-fold)

;;;; Variables

(defvar-local hs-indent-fold--overlays nil
  "List of overlays created by hs-indent-fold-mode.")

(defvar-local hs-indent-fold--update-timer nil
  "Timer for delayed overlay updates.")

;;;; Keymap

(defvar hs-indent-fold-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'hs-indent-fold-click)
    map)
  "Keymap for hs-indent-fold overlays.")

;;;; Core Functions

(defun hs-indent-fold--make-overlay (start end)
  "Create an overlay from START to END."
  (let ((ov (make-overlay start end nil t nil)))
    (overlay-put ov 'hs-indent-fold t)
    (overlay-put ov 'keymap hs-indent-fold-map)
    (overlay-put ov 'mouse-face 'highlight)
    (overlay-put ov 'help-echo "mouse-1: toggle fold")
    (overlay-put ov 'pointer 'hand)
    (push ov hs-indent-fold--overlays)
    ov))

(defun hs-indent-fold--clear-overlays ()
  "Remove all hs-indent-fold overlays."
  (mapc #'delete-overlay hs-indent-fold--overlays)
  (setq hs-indent-fold--overlays nil))

(defun hs-indent-fold--refresh ()
  "Refresh all indent fold overlays."
  (hs-indent-fold--clear-overlays)
  (when hs-minor-mode
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((indent-end
               (progn
                 (back-to-indentation)
                 (point)))
              (line-start (line-beginning-position)))
          (when (> indent-end line-start)
            (hs-indent-fold--make-overlay line-start indent-end)))
        (forward-line 1)))))

(defun hs-indent-fold--schedule-refresh ()
  "Schedule a refresh of overlays after a delay."
  (when hs-indent-fold--update-timer
    (cancel-timer hs-indent-fold--update-timer))
  (let ((buf (current-buffer)))
    (setq hs-indent-fold--update-timer
          (run-with-idle-timer
           hs-indent-fold-update-delay nil
           (lambda ()
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (when (bound-and-true-p hs-indent-fold-mode)
                   (hs-indent-fold--refresh)))))))))

;;;; Interactive Commands

(defun hs-indent-fold-click (event)
  "Handle mouse click EVENT on an indent fold overlay."
  (interactive "e")
  (let ((pos (posn-point (event-start event))))
    (save-excursion
      (goto-char pos)
      (hs-toggle-hiding))))

;;;; After-change handling

(defun hs-indent-fold--after-change (_beg _end _len)
  "Handle buffer changes."
  (hs-indent-fold--schedule-refresh))

;;;; Minor Mode

;;;###autoload
(define-minor-mode hs-indent-fold-mode
  "Minor mode for click-to-fold via indent region detection."
  :lighter " HsIF"
  :group
  'hs-indent-fold
  (if hs-indent-fold-mode
      (progn
        (unless hs-minor-mode
          (hs-minor-mode 1))
        (hs-indent-fold--refresh)
        (add-hook
         'after-change-functions #'hs-indent-fold--after-change
         nil t))
    (when hs-indent-fold--update-timer
      (cancel-timer hs-indent-fold--update-timer)
      (setq hs-indent-fold--update-timer nil))
    (remove-hook
     'after-change-functions #'hs-indent-fold--after-change
     t)
    (hs-indent-fold--clear-overlays)))

(provide 'hs-indent-fold)

;;; hs-indent-fold.el ends here
