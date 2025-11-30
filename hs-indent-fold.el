;;; hs-indent-fold.el --- Click-to-fold via indent region highlighting -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Nobuyuki Kamimoto
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, folding

;;; Commentary:

;; This package provides indent-based visual indicators for foldable regions.
;; Instead of using fringe markers like hideshowvis, it highlights the
;; indentation area of foldable blocks.  Clicking on the highlighted indent
;; region toggles the fold state.
;;
;; Features:
;; - Highlights indent area of foldable blocks detected by hideshow
;; - Click on indent area to fold/unfold
;; - Different colors for expanded vs folded state
;;
;; Usage:
;;   (add-hook 'prog-mode-hook 'hs-indent-fold-mode)

;;; Code:

(require 'hideshow)
(require 'color)

;;;; Compatibility

(defvar hs-indent-fold--forward-sexp-func
  (if (boundp 'hs-forward-sexp-function)
      'hs-forward-sexp-function
    'hs-forward-sexp-func)
  "Symbol for hideshow forward-sexp function (compatibility).")

;;;; Customization

(defgroup hs-indent-fold nil
  "Click-to-fold via indent region highlighting."
  :group 'hideshow
  :prefix "hs-indent-fold-")

(defface hs-indent-fold-face
  '((((background dark)) :background "#2a2a3a")
    (((background light)) :background "#e8e8f0"))
  "Default face for foldable indent regions (used as fallback)."
  :group 'hs-indent-fold)

(defface hs-indent-fold-hover-face
  '((((background dark)) :background "#3a3a4a")
    (((background light)) :background "#d8d8e8"))
  "Default face for indent regions on mouse hover (used as fallback)."
  :group 'hs-indent-fold)

(defface hs-indent-fold-folded-face
  '((((background dark)) :background "#3a2a2a")
    (((background light)) :background "#f0e8e8"))
  "Default face for folded block indicator (used as fallback)."
  :group 'hs-indent-fold)

(defcustom hs-indent-fold-color-by-block t
  "If non-nil, use different colors for different fold blocks."
  :type 'boolean
  :group 'hs-indent-fold)

(defcustom hs-indent-fold-dark-palette
  '("#2d3a4f"
    "#3a2d4f"
    "#2d4f3a"
    "#4f3a2d"
    "#2d4f4f"
    "#4f2d4f"
    "#4f4f2d")
  "List of background colors for fold blocks in dark mode.
These are deep, rich colors suitable for dark backgrounds."
  :type '(repeat color)
  :group 'hs-indent-fold)

(defcustom hs-indent-fold-light-palette
  '("#e8f0ff"
    "#ffe8f0"
    "#e8fff0"
    "#fff0e8"
    "#e8ffff"
    "#ffe8ff"
    "#fffff0")
  "List of background colors for fold blocks in light mode.
These are soft, pastel colors suitable for light backgrounds."
  :type '(repeat color)
  :group 'hs-indent-fold)

(defcustom hs-indent-fold-hover-lighten 10
  "Percentage to adjust colors on hover.
Positive values lighten (for dark mode), negative darken (for light mode)."
  :type 'number
  :group 'hs-indent-fold)

(defcustom hs-indent-fold-dark-folded-color "#5f3a3a"
  "Background color for folded blocks in dark mode."
  :type 'color
  :group 'hs-indent-fold)

(defcustom hs-indent-fold-light-folded-color "#ffdfdf"
  "Background color for folded blocks in light mode."
  :type 'color
  :group 'hs-indent-fold)

(defcustom hs-indent-fold-ignore-same-line t
  "If non-nil, do not highlight blocks that end on the same line."
  :type 'boolean
  :group 'hs-indent-fold)

(defcustom hs-indent-fold-update-delay 0.1
  "Delay in seconds before updating overlays after buffer changes."
  :type 'number
  :group 'hs-indent-fold)

;;;; Variables

(defvar-local hs-indent-fold--overlays nil
  "List of overlays created by hs-indent-fold-mode.")

(defvar-local hs-indent-fold--update-timer nil
  "Timer for delayed overlay updates.")

(defvar-local hs-indent-fold--block-info nil
  "Cache of block information: ((start . end) ...).")

;;;; Keymap

(defvar hs-indent-fold-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'hs-indent-fold-click)
    map)
  "Keymap for hs-indent-fold overlays.")

;;;; Color Utilities

(defun hs-indent-fold--frame-background-dark-p ()
  "Return non-nil if the frame background is dark."
  (eq (frame-parameter nil 'background-mode) 'dark))

(defun hs-indent-fold--lighten-color (color percent)
  "Lighten COLOR by PERCENT."
  (let* ((rgb (color-name-to-rgb color))
         (hsl (apply #'color-rgb-to-hsl rgb))
         (l (nth 2 hsl))
         (new-l (min 1.0 (+ l (/ percent 100.0)))))
    (apply #'color-rgb-to-hex
           (color-hsl-to-rgb (nth 0 hsl) (nth 1 hsl) new-l))))

(defun hs-indent-fold--darken-color (color percent)
  "Darken COLOR by PERCENT."
  (hs-indent-fold--lighten-color color (- percent)))

(defun hs-indent-fold--get-block-color (block-index)
  "Get the background color for BLOCK-INDEX (0-based)."
  (let* ((dark-p (hs-indent-fold--frame-background-dark-p))
         (palette
          (if dark-p
              hs-indent-fold-dark-palette
            hs-indent-fold-light-palette)))
    (if (and hs-indent-fold-color-by-block palette)
        (let* ((len (length palette))
               (idx (mod block-index len)))
          (nth idx palette))
      ;; Fallback to default face
      (face-background 'hs-indent-fold-face nil 'default))))

(defun hs-indent-fold--get-hover-color (block-index)
  "Get the hover color for BLOCK-INDEX."
  (let ((base-color (hs-indent-fold--get-block-color block-index)))
    (if (hs-indent-fold--frame-background-dark-p)
        (hs-indent-fold--lighten-color
         base-color hs-indent-fold-hover-lighten)
      (hs-indent-fold--darken-color
       base-color hs-indent-fold-hover-lighten))))

(defun hs-indent-fold--get-folded-color ()
  "Get the color for folded blocks."
  (if (hs-indent-fold--frame-background-dark-p)
      hs-indent-fold-dark-folded-color
    hs-indent-fold-light-folded-color))

(defvar hs-indent-fold--block-faces (make-hash-table :test 'equal)
  "Cache of dynamically created faces for each block index.")

(defun hs-indent-fold--get-block-face (block-index)
  "Get or create a face for the given BLOCK-INDEX."
  (let ((key
         (cons
          block-index (hs-indent-fold--frame-background-dark-p))))
    (or (gethash key hs-indent-fold--block-faces)
        (let ((face-name
               (intern
                (format "hs-indent-fold-block-%d-face" block-index)))
              (bg-color (hs-indent-fold--get-block-color block-index))
              (hover-color
               (hs-indent-fold--get-hover-color block-index)))
          (face-spec-set face-name `((t :background ,bg-color)))
          (puthash
           key
           (cons face-name hover-color)
           hs-indent-fold--block-faces)
          (cons face-name hover-color)))))

(defun hs-indent-fold--clear-block-faces ()
  "Clear the cached block faces."
  (clrhash hs-indent-fold--block-faces))

;;;; Core Functions

(defun hs-indent-fold--get-indent-end (pos)
  "Get the end position of indentation at line containing POS."
  (save-excursion
    (goto-char pos)
    (back-to-indentation)
    (point)))

(defun hs-indent-fold--detect-blocks ()
  "Detect all foldable blocks in the current buffer.
Returns a list of (block-start block-end indent-column) tuples."
  (when hs-minor-mode
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (let (blocks)
          (while (re-search-forward hs-block-start-regexp nil t)
            (let* ((block-start (match-beginning 0))
                   (start-line (line-number-at-pos block-start))
                   (indent-col
                    (save-excursion
                      (goto-char block-start)
                      (current-indentation))))
              (save-excursion
                (goto-char block-start)
                (ignore-errors
                  (funcall (symbol-value
                            hs-indent-fold--forward-sexp-func)
                           1)
                  (let ((block-end (point))
                        (end-line (line-number-at-pos)))
                    (when (or (not hs-indent-fold-ignore-same-line)
                              (> end-line start-line))
                      (push (list
                             block-start
                             block-end
                             indent-col
                             start-line
                             end-line)
                            blocks)))))))
          (nreverse blocks))))))

(defun hs-indent-fold--make-overlay
    (start end block-start block-end block-index)
  "Create an overlay from START to END for block from BLOCK-START to BLOCK-END.
BLOCK-INDEX is the index of this block (0-based), used for color selection."
  (let* ((ov (make-overlay start end nil t nil))
         (face-info (hs-indent-fold--get-block-face block-index))
         (face-name (car face-info))
         (hover-color (cdr face-info)))
    (overlay-put ov 'hs-indent-fold t)
    (overlay-put ov 'hs-indent-fold-block-start block-start)
    (overlay-put ov 'hs-indent-fold-block-end block-end)
    (overlay-put ov 'hs-indent-fold-block-index block-index)
    (overlay-put ov 'hs-indent-fold-normal-face face-name)
    (overlay-put ov 'face face-name)
    (overlay-put ov 'mouse-face `(:background ,hover-color))
    (overlay-put ov 'keymap hs-indent-fold-map)
    (overlay-put ov 'help-echo "mouse-1: toggle fold")
    (overlay-put ov 'pointer 'hand)
    (push ov hs-indent-fold--overlays)
    ov))

(defun hs-indent-fold--create-overlays-for-block
    (block-info block-index)
  "Create overlays for a single block described by BLOCK-INFO.
BLOCK-INFO is (block-start block-end indent-col start-line end-line).
BLOCK-INDEX is used to select the color for this block.
Creates overlays for the indent region of each line within the block."
  (pcase-let ((`(,block-start
                 ,block-end ,block-indent ,_start-line ,_end-line)
               block-info))
    (save-excursion
      (goto-char block-start)
      (beginning-of-line)
      ;; Loop through all lines in the block
      (while (and (<= (point) block-end) (not (eobp)))
        (let* ((line-start (line-beginning-position))
               (is-blank
                (save-excursion
                  (beginning-of-line)
                  (looking-at-p "^[[:space:]]*$")))
               (line-indent (current-indentation)))
          ;; Only create overlay if:
          ;; - Line is not blank
          ;; - Line has indentation >= block's indent level
          (unless is-blank
            (when (>= line-indent block-indent)
              ;; Highlight from line start to the block's indent width
              ;; This creates a "bar" at the block's indent level
              (let*
                  ((bar-end (+ line-start block-indent))
                   (indent-end
                    (hs-indent-fold--get-indent-end line-start))
                   ;; Use the smaller of bar-end and actual indent-end
                   (ov-end (min bar-end indent-end)))
                ;; Special case: if block-indent is 0, highlight first indent unit
                (when (= block-indent 0)
                  (setq ov-end
                        (min indent-end
                             (+ line-start
                                (or (bound-and-true-p tab-width)
                                    4)))))
                (when (> ov-end line-start)
                  (hs-indent-fold--make-overlay
                   line-start
                   ov-end
                   block-start
                   block-end
                   block-index))))))
        (forward-line 1)))))

(defun hs-indent-fold--clear-overlays ()
  "Remove all hs-indent-fold overlays."
  (mapc #'delete-overlay hs-indent-fold--overlays)
  (setq hs-indent-fold--overlays nil))

(defun hs-indent-fold--refresh ()
  "Refresh all indent fold overlays."
  (hs-indent-fold--clear-overlays)
  (when hs-minor-mode
    (let ((blocks (hs-indent-fold--detect-blocks))
          (index 0))
      (setq hs-indent-fold--block-info blocks)
      (dolist (block blocks)
        (hs-indent-fold--create-overlays-for-block block index)
        (setq index (1+ index))))))

(defun hs-indent-fold-reset-colors ()
  "Reset color cache and refresh overlays.
Call this after changing themes or color settings."
  (interactive)
  (hs-indent-fold--clear-block-faces)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (bound-and-true-p hs-indent-fold-mode)
        (hs-indent-fold--refresh)))))

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
  "Handle mouse click EVENT on an indent fold overlay.
When multiple blocks overlap, prefer the innermost (smallest) block."
  (interactive "e")
  (let* ((pos (posn-point (event-start event)))
         (ovs (overlays-at pos))
         best-ov
         best-size)
    ;; Find the smallest (innermost) block overlay at this position
    (dolist (ov ovs)
      (when (overlay-get ov 'hs-indent-fold)
        (let* ((block-start
                (overlay-get ov 'hs-indent-fold-block-start))
               (block-end (overlay-get ov 'hs-indent-fold-block-end))
               (size (- block-end block-start)))
          (when (or (null best-size) (< size best-size))
            (setq
             best-ov ov
             best-size size)))))
    (when best-ov
      (let ((block-start
             (overlay-get best-ov 'hs-indent-fold-block-start)))
        (save-excursion
          (goto-char block-start)
          (end-of-line)
          (if (hs-already-hidden-p)
              (hs-show-block)
            (hs-hide-block)))
        ;; Update overlay face for folded state
        (hs-indent-fold--update-folded-state)))))

(defun hs-indent-fold--update-folded-state ()
  "Update overlay faces based on fold state."
  (let ((folded-color (hs-indent-fold--get-folded-color)))
    (dolist (ov hs-indent-fold--overlays)
      (when (overlay-buffer ov)
        (let ((block-start
               (overlay-get ov 'hs-indent-fold-block-start))
              (normal-face
               (overlay-get ov 'hs-indent-fold-normal-face)))
          (when block-start
            (save-excursion
              (goto-char block-start)
              (end-of-line)
              (if (hs-already-hidden-p)
                  (overlay-put ov 'face `(:background ,folded-color))
                (overlay-put ov 'face normal-face)))))))))

;;;; After-change handling

(defun hs-indent-fold--after-change (_beg _end _len)
  "Handle buffer changes."
  (hs-indent-fold--schedule-refresh))

;;;; Minor Mode

;;;###autoload
(define-minor-mode hs-indent-fold-mode
  "Minor mode for click-to-fold via indent highlighting."
  :lighter " HsIF"
  :group
  'hs-indent-fold
  (if hs-indent-fold-mode
      (progn
        ;; Ensure hs-minor-mode is enabled
        (unless hs-minor-mode
          (hs-minor-mode 1))
        ;; Initial overlay creation
        (hs-indent-fold--refresh)
        ;; Setup hooks
        (add-hook
         'after-change-functions #'hs-indent-fold--after-change
         nil t)
        (add-hook 'hs-hide-hook #'hs-indent-fold--update-folded-state
                  nil
                  t)
        (add-hook 'hs-show-hook #'hs-indent-fold--update-folded-state
                  nil
                  t))
    ;; Cleanup
    (when hs-indent-fold--update-timer
      (cancel-timer hs-indent-fold--update-timer)
      (setq hs-indent-fold--update-timer nil))
    (remove-hook
     'after-change-functions #'hs-indent-fold--after-change
     t)
    (remove-hook 'hs-hide-hook #'hs-indent-fold--update-folded-state
                 t)
    (remove-hook 'hs-show-hook #'hs-indent-fold--update-folded-state
                 t)
    (hs-indent-fold--clear-overlays)))

;; Reset colors when theme changes
(when (boundp 'enable-theme-functions)
  (add-hook
   'enable-theme-functions
   (lambda (&rest _) (hs-indent-fold-reset-colors))))

(provide 'hs-indent-fold)

;;; hs-indent-fold.el ends here
