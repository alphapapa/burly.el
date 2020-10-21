;;; burly-revive.el --- Partial port of revive.el    -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Adam Porter

;; Author: HIROSE Yuuji [yuuji>at<gentei.org]
;; Author: Adam Porter <adam@alphapapa.net>
;; Keywords: convenience

;; This program is distributed as a free software. The author is not
;; responsible for any possible defects caused by this software.  This
;; software can be treated with: ``The 2-Clause BSD License''(since
;; 2017-09-10, revive.el 2.24).

;;; Commentary:

;; This library allows saving and restoring of window configurations.
;; It has code borrowed from revive.el, originally authored by HIROSE
;; Yuuji [yuuji>at<gentei.org], which is currently hosted at
;; <http://www.gentei.org/~yuuji/software/euc/revive.el>.

;;; Code:

;;;; Requirements

(require 'cl-lib)

;;;; Variables


;;;; Customization


;;;; Commands


;;;; Functions

(declare-function burly-url-buffer "burly.el")
(defun burly-revive-restore-window-configuration (config)
  "Restore the window configuration.
Configuration CONFIG should be created by
`burly-revive--window-configuration'."
  (let ((width (car config))
        (height (nth 1 config))
        (edges (nth 2 config))
        (urls (nth 3 config)))
    (set-buffer (get-buffer-create "*scratch*")) ;; FIXME Probably not the best way.
    (setq edges (burly-revive--normalize-edges width height edges))
    (burly-revive--construct-window-configuration edges)
    (burly-revive--select-window-by-edge 0 (burly-revive--miny))
    (dolist (url urls)
      (let ((new-buffer (burly-url-buffer url)))
        (cl-assert new-buffer nil "ARGH: %s" url)
        (set-window-buffer (selected-window) new-buffer))
      (other-window 1))))

(defun burly-revive--normalize-edges (width height edgelist)
  "Normalize all coordinates for current screen size.
'(WIDTH, HEIGHT) is old screen size and EDGELIST is a list of
window-edges."
  (let (normalized (curw (frame-width))
		   (curh (frame-height)) e n)
    (if (and (equal curw width) (equal curh height))
	edgelist
      (while edgelist
	(setq e (car edgelist)
	      n (list (/ (+ (* curw (nth 0 e)) (/ width 2)) width)
		      (/ (+ (* curh (nth 1 e)) (/ height 2)) height)
		      (/ (+ (* curw (nth 2 e)) (/ width 2)) width)
		      (/ (+ (* curh (nth 3 e)) (/ height 2)) height))
	      normalized (append normalized (list n))
	      edgelist (cdr edgelist)))
      normalized)))

(defun burly-revive--window-configuration (&optional buffer-data-fn)
  "Return the printable current-window-configuration.
This configuration will be stored by restore-window-configuration.
Returned configurations are list of:
'(Screen-Width Screen-Height Edge-List Buffer-List)

Edge-List is a return value of burly-revive--all-window-edges, list of all
window-edges whose first member is always of north west window.

Buffer-List is a list of buffer property list of all windows.  This
property lists are stored in order corresponding to Edge-List.  Buffer
property list is formed as
'((buffer-file-name) (buffer-name) (point) (window-start))."
  (let ((buffer-data-fn (or buffer-data-fn
                            (lambda (_buffer)
                              (list (if (and (buffer-file-name)
                                             (fboundp 'abbreviate-file-name))
                                        (abbreviate-file-name (buffer-file-name))
                                      (buffer-file-name))
                                    (buffer-name)
                                    (point)
                                    (window-start)))))
        (curwin (selected-window))
        (edges (burly-revive--all-window-edges))
        buflist)
    (save-excursion
      (setf buflist (cl-loop for window in (burly-revive--window-list)
                             collect (funcall buffer-data-fn (window-buffer window))))
      (select-window curwin)
      (list (frame-width) (frame-height) edges buflist))))

(defun burly-revive--window-list ()
  "Return the all window list in sorted order."
  (let*((curwin (selected-window)) (win curwin) wlist)
    (if (null
	 (catch 'found
	   (while t
	     (if (and (= 0 (car (window-edges win)))
		      (= (burly-revive--miny) (car (cdr (window-edges win)))))
		 (throw 'found t))
	     (if (eq (setq win (next-window win)) curwin)
		 (throw 'found nil)))))
	(error "Unexpected window configuration."))
    (setq curwin win wlist (list win))
    (while (not (eq curwin (setq win (next-window win "w/o mini"))))
      (setq wlist (append wlist (list win)))) ;use append to preserve order
    wlist))

(defun burly-revive--all-window-edges ()
  "Return the all windows edges by list."
  (let ((wlist (burly-revive--window-list)) edges)
    (while wlist
      (setq edges (append edges (list (window-edges (car wlist))))
	    wlist (cdr wlist)))
    edges))

(defun burly-revive--construct-window-configuration (edges)
  "Restore window configuration by EDGES.
EDGES should be sorted."
  (delete-other-windows)
  (burly-revive-restore-winconf 0 (burly-revive--miny) (frame-width) (1- (frame-height)) edges))

(defun burly-revive-restore-winconf (x1 y1 x2 y2 edges)
  "Restore partial window configuration.
Assume (X1, Y1), (X2, Y2) as diagonal corners of partial window.
EDGES is a list of sub-windows' edges."
  (let* ((topwin (car edges))
         (width (- x2 x1))
         (height (- y2 y1))
         right lower)
    (cond
     ((= (length edges) 1) nil)		;nothing to do.

     ;;if the top window has the same width as whole frame.
     ;; +---------+
     ;; |top      |
     ;; +-----+---+
     ;; |2    |3  |
     ;; +-----+---+
     ((= width (- (nth 2 topwin) (car topwin)))
      (setq lower (cdr edges))
      (burly-revive--select-window-by-edge x1 y1)
      (burly-revive--split-window-safe nil (- (nth 3 topwin) (nth 1 topwin)))
      (burly-revive-restore-winconf
       (car (car lower)) (nth 1 (car lower)) x2 y2 lower))

     ;;if the top window has the same height as whole frame.
     ;; +-----+---+
     ;; |top  |2  |
     ;; |     +---+
     ;; |     |3  |
     ;; +-----+---+
     ((= height (- (nth 3 topwin) (nth 1 topwin)))
      (setq right (cdr edges))
      (burly-revive--select-window-by-edge x1 y1)
      (burly-revive--split-window-safe nil (- (nth 2 topwin) (car topwin)) t)
      (burly-revive-restore-winconf
       (car (car right)) (nth 1 (car right)) x2 y2 right))

     ;;These two cases above are specialized solution of below for speed.

     ;;general cases.
     ;; +------+--+  Detect whether window is mainly divided vertically or
     ;; |top   |2 |  horizontally.  And call this function recursively on
     ;; +---+--+--+  former (that is, upper half in vertical division or
     ;; |3  |4..  |  left half in horizontal) and latter configuration.
     ;; +---+-----+
     (t
      (let ((flist (list topwin))
	    (elist (cdr edges)) divwin div-x div-y former latter)
	(while elist
	  (if (or (and (= x1 (car (car elist)))
		       (not (eq (car divwin) x1)))
		  (and (= y1 (nth 1 (car elist)))
		       (not (eq (nth 1 divwin) y1))))
	      (setq divwin (car elist)
		    former flist
		    latter elist))
	  (setq flist (append flist (list (car elist))))
	  (setq elist (cdr elist)))
	(setq div-x (car divwin) div-y (nth 1 divwin))
	(cond
	 ((= x1 (car divwin))           ;Mainly divided vertically
	  (burly-revive--select-window-by-edge x1 y1)
	  (burly-revive--split-window-safe nil (- div-y y1))
	  (burly-revive-restore-winconf x1 y1 x2 div-y former)
	  (burly-revive-restore-winconf x1 div-y x2 y2 latter)
	  (message "=="))
	 ((= y1 (nth 1 divwin))
	  (burly-revive--select-window-by-edge x1 y1)
	  (burly-revive--split-window-safe nil (- div-x x1) t)
	  (burly-revive-restore-winconf x1 y1 div-x y2 former)
	  (burly-revive-restore-winconf div-x y1 x2 y2 latter)
	  (message "||"))
	 (t (message "dame!"))))))))

(defun burly-revive--miny ()
  (car (cdr (window-edges (frame-first-window nil)))))

(defun burly-revive--select-window-by-edge (x y)
  "Select window whose north west corner is (X, Y).
If the matching window is not found, select the nearest window."
  (let* ((curwin (selected-window))
         (win (next-window curwin))
         (min 99999)
         edges s2 minwin)
    (or (catch 'found
          (while t
            (setq edges (window-edges win)
                  s2 (+ (* (- (car edges) x) (- (car edges) x))
                        (* (- (nth 1 edges) y) (- (nth 1 edges) y))))
            (cond
             ((= s2 0)
              (select-window win)
              (throw 'found t))
             ((< s2 min)
              (setq min s2 minwin win)))
            (if (eq win curwin) (throw 'found nil)) ;select the nearest window
            (setq win (next-window win))))
        (select-window minwin))))

(defun burly-revive--split-window-safe (window size &optional hor-flag)
  "Same as split-window but avoids error."
  (split-window
   window
   (min (max (if hor-flag window-min-width window-min-height) size)
	(if hor-flag (- (frame-width) window-min-width 1)
	  (- (frame-height) window-min-height 1)))
   hor-flag))

;;;; Footer

(provide 'burly-revive)

;;; burly-revive.el ends here
