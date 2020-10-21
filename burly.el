;;; burly.el --- URLs for buffers and their content  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; Keywords: convenience

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

;; This package provides a way to refer to windows, buffers, and places within the buffers as URLs.

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'map)
(require 'thingatpt)
(require 'url-parse)
(require 'url-util)

(require 'burly-revive)

;;;; Variables

;;;; Customization

(defgroup burly nil
  "Burly."
  :group 'convenience)

(defcustom burly-mode-map
  (list (cons 'org-mode
              (list (cons 'make-url-fn #'burly-make-url-org-mode)
                    (cons 'follow-url-fn #'burly-follow-url-org-mode))))
  "Alist mapping major modes to the appropriate Burly functions.")

;;;; Commands

(defun burly-kill-url (buffer)
  "Copy URL for BUFFER to the kill ring."
  (interactive "b")
  (let ((url (burly-buffer-url (get-buffer buffer))))
    (kill-new url)
    (message "%s" url)))

(defun burly-open-url (url)
  "Open Burly URL."
  ;; FIXME: If point is on an "emacs+burly..." URL, but it's after the "emacs+burly"
  ;; part, `thing-at-point-url-at-point' doesn't pick up the whole URL.
  (interactive (list (or (thing-at-point-url-at-point t)
                         (read-string "URL: "))))
  (cl-assert (string-prefix-p "emacs+burly+" url) nil
             "URL not an emacs+burly one: %s" url)
  (pcase-let* ((urlobj (url-generic-parse-url url))
               ((cl-struct url type) urlobj)
               (subtype (car (last (split-string type "+" 'omit-nulls)))))
    (pcase-exhaustive subtype
      ("bookmark" (pop-to-buffer (burly-follow-url-bookmark urlobj)))
      ("file" (pop-to-buffer (burly-follow-url-file urlobj)))
      ("windows" (burly-windows-set urlobj)))))

;;;; Functions

(defun burly-url-buffer (url)
  "Return buffer for URL."
  (cl-assert (string-prefix-p "emacs+burly+" url) nil
             "URL not an emacs+burly one: %s" url)
  (pcase-let* ((urlobj (url-generic-parse-url url))
               ((cl-struct url type) urlobj)
               (subtype (car (last (split-string type "+" 'omit-nulls)))))
    (pcase-exhaustive subtype
      ("bookmark" (burly-follow-url-bookmark urlobj))
      ("file" (burly-follow-url-file urlobj))
      ("name"
       ;;  (error "ARGH: URLOBJ:%s URL:%s FILENAME:%s" urlobj url (url-filename urlobj))
       (get-buffer (cdr (url-path-and-query urlobj)))))))

(defun burly-buffer-url (buffer)
  "Return URL for BUFFER."
  (let* ((major-mode (buffer-local-value 'major-mode buffer))
         (make-url-fn (map-nested-elt burly-mode-map (list major-mode 'make-url-fn))))
    (cond (make-url-fn (funcall make-url-fn buffer))
          (t (or (with-current-buffer buffer
                   (when-let* ((record (ignore-errors
                                         (bookmark-make-record))))
                     (burly-bookmark-record-url record)))
                 ;; Buffer can't seem to be bookmarked, so record it as
                 ;; a name-only buffer.  For some reason, it works
                 ;; better to use the buffer name in the query string
                 ;; rather than the filename/path part.
                 (url-recreate-url (url-parse-make-urlobj "emacs+burly+name" nil nil nil nil
                                                          (concat "?" (buffer-name buffer)) nil nil 'fullness)))))))

(defun burly-follow-url-file (urlobj)
  "Return buffer for URLOBJ."
  (pcase-let* ((`(,path . ,query-string) (url-path-and-query urlobj))
               (query (url-parse-query-string query-string))
               (buffer (find-file-noselect path))
               (major-mode (buffer-local-value 'major-mode buffer))
               (follow-fn (map-nested-elt burly-mode-map (list major-mode 'follow-url-fn))))
    (cl-assert follow-fn nil "Major mode not in `burly-mode-map': %s" major-mode)
    (funcall follow-fn :buffer buffer :query query)))

;;;;; Windows

(cl-defun burly-windows-url (&optional (frame (selected-frame)))
  "Return URL for window configuration on FRAME."
  (with-selected-frame frame
    (let* ((query (burly-revive--window-configuration #'burly-buffer-url))
           (filename (concat "?" (prin1-to-string query))))
      (url-recreate-url (url-parse-make-urlobj "emacs+burly+windows" nil nil nil nil
                                               filename)))))

(defun burly-windows-set (urlobj)
  "Set window configuration according to URLOBJ."
  (pcase-let* ((`(,_ . ,query-string) (url-path-and-query urlobj))
               (config (read query-string)))
    (burly-revive-restore-window-configuration config)))

;;;;; Bookmarks

(defun burly-bookmark-record-url (record)
  "Return a URL for bookmark RECORD."
  (cl-assert record)
  (pcase-let* ((`(,name . ,props) record)
               (query (cl-loop for prop in props
                               collect (list (car prop) (cdr prop))))
               (filename (concat name "?" (url-build-query-string (remove nil query)))))
    (url-recreate-url (url-parse-make-urlobj "emacs+burly+bookmark" nil nil nil nil
                                             filename nil nil 'fullness))))

(defun burly-follow-url-bookmark (urlobj)
  "Return buffer for bookmark specified by URLOBJ.
URLOBJ should be a URL object as returned by
`url-generic-parse-url'."
  (pcase-let* ((`(,path . ,query-string) (url-path-and-query urlobj))
               (query (url-parse-query-string query-string))
               ;; Convert back to alist.
               (props (cl-loop for prop in query
                               for key = (intern (car prop))
                               for value = (pcase key
                                             ('handler (intern (cadr prop)))
                                             ('position (cl-parse-integer (cadr prop)))
                                             (_ (cadr prop)))
                               do (message "ARGH: %s %s" prop (cons key value))
                               collect (cons key value)))
               (record (cons path props)))
    (save-current-buffer
      (bookmark-jump record)
      (current-buffer))))

;;;;; Org buffers

(defun burly-make-url-org-mode (buffer)
  "Return URL for Org BUFFER."
  (with-current-buffer buffer
    (cl-assert (not (org-before-first-heading-p)) nil
               "Before first heading in buffer: %s" buffer)
    (cl-assert (or (buffer-file-name buffer)
                   (buffer-file-name (buffer-base-buffer buffer)))
               nil "Buffer has no file name: %s" buffer)
    (let* ((narrowed (buffer-narrowed-p))
           (indirect (buffer-base-buffer buffer))
           (outline-path (org-get-outline-path t))
           (pos (point))
           (relative-pos (when outline-path
                           (- (point) (save-excursion
                                        (org-back-to-heading)
                                        (point)))))
           (query (list (list "pos" pos)
                        (when outline-path
                          (list "outline-path" (prin1-to-string outline-path)))
                        (when relative-pos
                          (list "relative-pos" relative-pos))
                        (when indirect
                          (list "indirect" "t"))
                        (when narrowed
                          (list "narrowed" "t"))))
           (buffer-file (or (buffer-file-name buffer)
                            (buffer-file-name (buffer-base-buffer buffer))))
           (filename (concat buffer-file "?" (url-build-query-string (remove nil query)))))
      (url-recreate-url (url-parse-make-urlobj "emacs+burly+file" nil nil nil nil
                                               filename nil nil 'fullness)))))

(cl-defun burly-follow-url-org-mode (&key buffer query)
  "In Org BUFFER, navigate to heading and position in QUERY, and return possibly different buffer.
If QUERY specifies that the buffer should be indirect, a new,
indirect buffer is returned.  Otherwise BUFFER is returned."
  ;; `pcase's map support uses `alist-get', which does not work with string keys
  ;; unless its TESTFN arg is bound to, e.g. `equal', but `map-elt' has deprecated
  ;; its TESTFN arg, and there's no way to pass it or bind it when using `pcase'
  ;; anyway.  So we rebind `alist-get' to a function that uses `assoc-string'.
  (with-current-buffer buffer
    (cl-letf (((symbol-function 'alist-get)
               (lambda (key alist &optional _default _remove _testfn)
                 ;; Only the first value in the list of values is returned, so multiple
                 ;; values are not supported.  I don't expect this to be a problem...
                 (cadr (assoc-string key alist)))))
      (pcase-let* (((map ("pos" pos)
                         ("indirect" indirect)
                         ("narrowed" narrowed)
                         ("outline-path" outline-path)
                         ("relative-pos" relative-pos))
                    query)
                   (heading-pos (when outline-path
                                  (org-find-olp (read outline-path) 'this-buffer))))
        (widen)
        (if heading-pos
            (progn
              (goto-char heading-pos)
              (when relative-pos
                (forward-char (string-to-number relative-pos))))
          (goto-char (string-to-number pos)))
        (cond (indirect (org-tree-to-indirect-buffer))
              (narrowed (org-narrow-to-subtree)))
        (current-buffer)))))


;;;; Footer

(provide 'burly)

;;; burly.el ends here
