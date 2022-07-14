;;; burly-tabs.el --- Burly's support for tab-bar    -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; Keywords: tab-bar, tabs, frames

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library provides support for `tab-bar-mode'.

;;; Code:

;; TODO: In Emacs 28, add an entry to the tab context menu.  See
;; <https://lists.gnu.org/archive/html/emacs-devel/2021-09/msg00590.html>.

;;;; Requirements

(require 'cl-lib)
(require 'map)
(require 'tab-bar)

(require 'burly)

;;;; Commands

;;;###autoload
(define-minor-mode burly-tabs-mode
  "Integrate Burly with `tab-bar-mode'.
When active, Burly bookmarks are opened in new tabs and named
accordingly."
  :global t
  :group 'burly
  (unless (version<= "28.1" emacs-version)
    (user-error "`burly-tabs-mode' requires Emacs 28.1 or later"))
  (if burly-tabs-mode
      (progn
	(advice-add #'burly--windows-set :before #'burly-tabs--windows-set-before-advice)
	(advice-add #'burly--windows-set :after #'burly-tabs--windows-set-after-advice))
    ;; Disable mode.
    (advice-remove #'burly--windows-set #'burly-tabs--windows-set-before-advice)
    (advice-remove #'burly--windows-set #'burly-tabs--windows-set-after-advice)))

(cl-defun burly-tabs-reset-tab (&optional (tab (tab-bar--current-tab-find)))
  "Reset TAB to its saved configuration.
Resets TAB to the Burly bookmark that it was created from."
  (interactive)
  (pcase-let* ((`(,_ . ,(map burly-bookmark-name)) tab))
    (unless burly-bookmark-name
      (user-error "Tab has no associated Burly bookmark (`burly-tabs-mode' must be enabled when opening a bookmark)"))
    (burly-open-bookmark burly-bookmark-name)))

(defalias 'burly-reset-tab #'burly-tabs-reset-tab)

;;;; Functions

(defun burly-tabs--windows-set-before-advice (&rest _ignore)
  "Cause bookmark to be opened in a tab by that name.
If a tab already exists named the value of
`burly-opened-bookmark-name', select it; otherwise call
`tab-bar-new-tab'.  To be used as advice to
`burly-open-bookmark'."
  (if (tab-bar--tab-index-by-name burly-opened-bookmark-name)
      (tab-bar-select-tab-by-name burly-opened-bookmark-name)
    (tab-bar-new-tab)))

(defun burly-tabs--windows-set-after-advice (&rest _ignore)
  "Set current tab's `burly-bookmark-name' to BOOKMARK-NAME.
To be used as advice to `burly--windows-set'."
  (tab-rename burly-opened-bookmark-name)
  (let ((current-tab (tab-bar--current-tab-find)))
    (setf (alist-get 'burly-bookmark-name (cdr current-tab))
	  burly-opened-bookmark-name)))

(provide 'burly-tabs)

;;; burly-tabs.el ends here
