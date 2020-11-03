;;; burly.el --- Save and restore window configurations and their buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; URL: https://github.com/alphapapa/burly.el
;; Package-Version: 0.1-pre
;; Package-Requires: ((emacs "26.3") (map "2.1"))
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

;; This package provides tools to save and restore window configurations
;; in Emacs, including buffers that may not be live anymore.  In this
;; way, it's like a lightweight "workspace" manager, allowing you to
;; easily restore a set of windows, their layout in a frame, and the
;; buffers in them.

;; Internally it uses Emacs's bookmarks system to restore buffers to
;; their previous contents and location.  This provides power and
;; extensibility, since many major modes already integrate with
;; Emacs's bookmarks system.  However, in case a mode's bookmarking
;; function isn't satisfactory, Burly allows the user to customize
;; buffer-restoring functions for specific modes.

;; For Org mode, Burly provides such custom functions so that narrowed
;; and indirect Org buffers are properly restored, and headings are
;; located by outline path in case they've moved since a bookmark was
;; made (the org-bookmark-heading package also provides this through
;; the Emacs bookmark system, but users may not have it installed, and
;; the functionality is too useful to not include here by default).

;; Internally, buffers and window configurations are also encoded as
;; URLs, and users may also save and open those URLs instead of using
;; Emacs bookmarks.  (The name "Burly" comes from "buffer URL.")

;;; Code:

;;;; Requirements

(require 'bookmark)
(require 'cl-lib)
(require 'map)
(require 'subr-x)
(require 'thingatpt)
(require 'url-parse)
(require 'url-util)

;;;; Variables

(defvar burly--window-state nil
  "Used to work around `bookmark--jump-via' affecting window configuration.")

;;;; Customization

(defgroup burly nil
  "Save and restore window configurations and their buffers."
  :group 'convenience
  :link '(url-link "https://github.com/alphapapa/burly.el")
  :link '(custom-manual "(Burly)Usage"))

(defcustom burly-bookmark-prefix "Burly: "
  "Prefix string prepended to the name of new Burly bookmarks."
  :type 'string)

(defcustom burly-mode-map
  (list (cons 'org-mode
              (list (cons 'make-url-fn #'burly--org-mode-buffer-url)
                    (cons 'follow-url-fn #'burly-follow-url-org-mode))))
  "Alist mapping major modes to the appropriate Burly functions."
  :type '(alist :key-type symbol
                :value-type (set (cons (const make-url-fn) (function :tag "Make-URL function"))
                                 (cons (const follow-url-fn) (function :tag "Follow-URL function")))))

(defcustom burly-window-persistent-parameters
  (list (cons 'burly-url 'writable)
        (cons 'mode-line-format 'writable))
  "Additional window parameters to persist.
See Info node `(elisp)Window Parameters'."
  :type '(alist :key-type (symbol :tag "Window parameter")
                :value-type (choice (const :tag "Not saved" nil)
                                    (const :tag "Saved" writable))))

;;;; Commands

;;;###autoload
(defun burly-kill-buffer-url (buffer)
  "Copy BUFFER's URL to the kill ring."
  (interactive "b")
  (let ((url (burly-buffer-url (get-buffer buffer))))
    (kill-new url)
    (message "%s" url)))

;;;###autoload
(defun burly-kill-frames-url ()
  "Copy current frameset's URL to the kill ring."
  (interactive)
  (let ((url (burly-frames-url)))
    (kill-new url)
    (message "%s" url)))

;;;###autoload
(defun burly-kill-windows-url ()
  "Copy current frame's window configuration URL to the kill ring."
  (interactive)
  (let ((url (burly-windows-url)))
    (kill-new url)
    (message "%s" url)))

;;;###autoload
(defun burly-open-url (url)
  "Open Burly URL."
  ;; FIXME: If point is on an "emacs+burly..." URL, but it's after the "emacs+burly"
  ;; part, `thing-at-point-url-at-point' doesn't pick up the whole URL.
  (interactive (list (or (thing-at-point-url-at-point t)
                         (read-string "URL: "))))
  (cl-assert (string-prefix-p "emacs+burly+" url) t "burly-open-url: URL not an emacs+burly one:")
  (pcase-let* ((urlobj (url-generic-parse-url url))
               ((cl-struct url type) urlobj)
               (subtype (car (last (split-string type "+" 'omit-nulls)))))
    (pcase-exhaustive subtype
      ((or "bookmark" "file" "name") (pop-to-buffer (burly-url-buffer url)))
      ("frames" (burly--frameset-restore urlobj))
      ("windows" (burly--windows-set urlobj)))))

;;;###autoload
(defun burly-bookmark-frames (name)
  "Bookmark the current frames as NAME."
  (interactive (list (read-string "Save Burly bookmark: " burly-bookmark-prefix)))
  (let ((record (list (cons 'url (burly-frames-url))
                      (cons 'handler #'burly-bookmark-handler))))
    (bookmark-store name record nil)))

;;;###autoload
(defun burly-bookmark-windows (name)
  "Bookmark the current frame's window configuration as NAME."
  (interactive (list (read-string "Save Burly bookmark: " burly-bookmark-prefix)))
  (let ((record (list (cons 'url (burly-windows-url))
                      (cons 'handler #'burly-bookmark-handler))))
    (bookmark-store name record nil)))

;;;###autoload
(defun burly-open-bookmark (bookmark)
  "Restore a window configuration to the current frame from a Burly BOOKMARK."
  (interactive
   (let ((bookmark-names (cl-loop for bookmark in bookmark-alist
                                  for (_name . params) = bookmark
                                  when (equal #'burly-bookmark-handler (alist-get 'handler params))
                                  collect (car bookmark))))
     (list (completing-read "Open Burly bookmark: " bookmark-names
			    nil nil burly-bookmark-prefix))))
  (cl-assert (and bookmark (not (string-empty-p bookmark))) nil
             "(burly-open-bookmark): Invalid Burly bookmark: '%s'" bookmark)
  (bookmark-jump bookmark))

;;;; Functions

;;;;; Buffers

(defun burly-url-buffer (url)
  "Return buffer for URL."
  (cl-assert (string-prefix-p "emacs+burly+" url) t "burly-url-buffer: URL not an emacs+burly one: %s" url)
  (pcase-let* ((urlobj (url-generic-parse-url url))
               ((cl-struct url type) urlobj)
               (subtype (car (last (split-string type "+" 'omit-nulls)))))
    (pcase-exhaustive subtype
      ("bookmark" (burly--bookmark-url-buffer urlobj))
      ("file" (burly--file-url-buffer urlobj))
      ("name" (get-buffer (cdr (url-path-and-query urlobj)))))))

(defun burly-buffer-url (buffer)
  "Return URL for BUFFER."
  (let* ((major-mode (buffer-local-value 'major-mode buffer))
         (make-url-fn (map-nested-elt burly-mode-map (list major-mode 'make-url-fn))))
    (cond (make-url-fn (funcall make-url-fn buffer))
          (t (or (with-current-buffer buffer
                   (when-let* ((record (ignore-errors
                                         (bookmark-make-record))))
                     (burly--bookmark-record-url record)))
                 ;; Buffer can't seem to be bookmarked, so record it as
                 ;; a name-only buffer.  For some reason, it works
                 ;; better to use the buffer name in the query string
                 ;; rather than the filename/path part.
                 (url-recreate-url (url-parse-make-urlobj "emacs+burly+name" nil nil nil nil
                                                          (concat "?" (buffer-name buffer)) nil nil 'fullness)))))))

;;;;; Files

(defun burly--file-url-buffer (urlobj)
  "Return buffer for \"emacs+burly+file:\" URLOBJ."
  (pcase-let* ((`(,path . ,query-string) (url-path-and-query urlobj))
               (query (url-parse-query-string query-string))
               (buffer (find-file-noselect path))
               (major-mode (buffer-local-value 'major-mode buffer))
               (follow-fn (map-nested-elt burly-mode-map (list major-mode 'follow-url-fn))))
    (cl-assert follow-fn nil "Major mode not in `burly-mode-map': %s" major-mode)
    (funcall follow-fn :buffer buffer :query query)))

;;;;; Frames

;; Looks like frameset.el should make this pretty easy.

(require 'frameset)

(cl-defun burly-frames-url (&optional (frames (frame-list)))
  "Return URL for frameset of FRAMES.
FRAMES defaults to all live frames."
  (dolist (frame frames)
    ;; Set URL window parameter for each window before saving state.
    (burly--windows-set-url (window-list frame 'never)))
  (let* ((window-persistent-parameters (append burly-window-persistent-parameters
                                               window-persistent-parameters))
         (query (frameset-save frames))
         (filename (concat "?" (url-hexify-string (prin1-to-string query))))
         (url (url-recreate-url (url-parse-make-urlobj "emacs+burly+frames" nil nil nil nil
                                                       filename))))
    (dolist (frame frames)
      ;; Clear window parameters.
      (burly--windows-set-url (window-list frame 'never) 'nullify))
    url))

(defun burly--frameset-restore (urlobj)
  "Restore FRAMESET according to URLOBJ."
  (pcase-let* ((`(,_ . ,query-string) (url-path-and-query urlobj))
               (frameset (read (url-unhex-string query-string))))
    ;; Restore buffers.  (Apparently `cl-loop''s in-ref doesn't work with
    ;; its destructuring, so we can't just `setf' on `window-state'.)
    (setf (frameset-states frameset)
          (cl-loop for (frame-parameters . window-state) in (frameset-states frameset)
                   collect (cons frame-parameters (burly--bufferize-window-state window-state))))
    (frameset-restore frameset)))

;;;;; Windows

(cl-defun burly-windows-url (&optional (frame (selected-frame)))
  "Return URL for window configuration on FRAME."
  (with-selected-frame frame
    (let* ((query (burly--window-state frame))
           (filename (concat "?" (url-hexify-string (prin1-to-string query)))))
      (url-recreate-url (url-parse-make-urlobj "emacs+burly+windows" nil nil nil nil
                                               filename)))))

(cl-defun burly--window-state (&optional (frame (selected-frame)))
  "Return window state for FRAME.
Sets `burly-url' window parameter in each window before
serializing."
  (with-selected-frame frame
    ;; Set URL window parameter for each window before saving state.
    (burly--windows-set-url (window-list nil 'never))
    (let* ((window-persistent-parameters (append burly-window-persistent-parameters
                                                 window-persistent-parameters))
           (window-state (window-state-get nil 'writable)))
      ;; Clear window parameters we set (because they aren't kept
      ;; current, so leaving them could be confusing).
      (burly--windows-set-url (window-list nil 'never) 'nullify)
      window-state)))

(defun burly--windows-set-url (windows &optional nullify)
  "Set `burly-url' window parameter in WINDOWS.
If NULLIFY, set the parameter to nil."
  (dolist (window windows)
    (let ((value (if nullify nil (burly-buffer-url (window-buffer window)))))
      (set-window-parameter window 'burly-url value))))

(defun burly--windows-set (urlobj)
  "Set window configuration according to URLOBJ."
  (pcase-let* ((`(,_ . ,query-string) (url-path-and-query urlobj))
               ;; FIXME: Remove this condition-case eventually, after giving users time to update their bookmarks.
               (state (condition-case nil
                          (read (url-unhex-string query-string))
                        (invalid-read-syntax (warn "Please recreate that Burly bookmark (storage format changed)")
                                             (read query-string))))
               (state (burly--bufferize-window-state state)))
    (window-state-put state (frame-root-window))
    ;; HACK: Since `bookmark--jump-via' insists on calling a
    ;; buffer-display function after handling the bookmark, we add a
    ;; function to `bookmark-after-jump-hook' to restore the window
    ;; configuration that we just set.
    (setf burly--window-state (window-state-get (frame-root-window) 'writable))
    (push #'burly--bookmark-window-state-hack bookmark-after-jump-hook)))

(defun burly--bufferize-window-state (state)
  "Return window state STATE with its buffers reincarnated."
  (cl-labels ((bufferize-state
               ;; Set windows' buffers in STATE.
               (state) (pcase state
                         (`(leaf . ,_attrs) (bufferize-leaf state))
                         ((pred atom) state)
                         (`(,_key . ,(pred atom)) state)
                         ((pred list) (mapcar #'bufferize-state state))))
              (bufferize-leaf
               (leaf) (pcase-let* ((`(leaf . ,attrs) leaf)
                                   ((map parameters buffer) attrs)
                                   ((map burly-url) parameters)
                                   (`(,_buffer-name . ,buffer-attrs) buffer)
                                   (new-buffer (burly-url-buffer burly-url)))
                        (setf (map-elt attrs 'buffer) (cons new-buffer buffer-attrs))
                        (cons 'leaf attrs))))
    (bufferize-state state)))

;;;;; Bookmarks

(defun burly--bookmark-window-state-hack (&optional _)
  "Put window state from `burly--window-state'.
This function is to be called in `bookmark-after-jump-hook' to
work around `bookmark--jump-via's calling a buffer-display
function which changes the window configuration after
`burly--windows-set' has set it.  This function removes itself
from the hook."
  (unwind-protect
      (progn
        (cl-assert burly--window-state)
        (window-state-put burly--window-state (frame-root-window)))
    (setf bookmark-after-jump-hook (delete #'burly--bookmark-window-state-hack bookmark-after-jump-hook)
          burly--window-state nil)))

;;;###autoload
(defun burly-bookmark-handler (bookmark)
  "Handler function for Burly BOOKMARK."
  (burly-open-url (alist-get 'url (bookmark-get-bookmark-record bookmark))))

(defun burly--bookmark-record-url (record)
  "Return a URL for bookmark RECORD."
  (cl-assert record)
  (pcase-let* ((`(,name . ,props) record)
               (query (cl-loop for prop in props
                               ;; HACK: Remove unreadable values from props.
                               do (cl-loop for value in-ref (cdr prop)
                                           when (or (bufferp value))
                                           do (setf value nil))
                               collect (list (car prop) (prin1-to-string (cdr prop)))))
               (filename (concat name "?" (url-build-query-string (remove nil query)))))
    (url-recreate-url (url-parse-make-urlobj "emacs+burly+bookmark" nil nil nil nil
                                             filename nil nil 'fullness))))

(defun burly--bookmark-url-buffer (urlobj)
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
                                             ('help-args (read (cadr prop)))
                                             ('help-fn (read (cadr prop)))
                                             ('position (cl-parse-integer (cadr prop)))
                                             (_ (read (cadr prop))))
                               collect (cons key value)))
               (record (cons path props)))
    (save-window-excursion
      (bookmark-jump record)
      (current-buffer))))

;;;;; Org buffers

;; We only require Org when compiling the file.  At runtime, Org will
;; be loaded before we call any of its functions, because we load the
;; Org file into a buffer first, which activates `org-mode'.

(eval-when-compile
  (require 'org))

(declare-function org-before-first-heading-p "org")
(declare-function org-get-outline-path "org")
(declare-function org-back-to-heading "org")
(declare-function org-find-olp "org")
(declare-function org-tree-to-indirect-buffer "org")
(declare-function org-narrow-to-subtree "org")

(defun burly--org-mode-buffer-url (buffer)
  "Return URL for Org BUFFER."
  (with-current-buffer buffer
    (cl-assert (or (buffer-file-name buffer)
                   (buffer-file-name (buffer-base-buffer buffer)))
               nil "Buffer has no file name: %s" buffer)
    (let* ((narrowed (buffer-narrowed-p))
           (indirect (buffer-base-buffer buffer))
           (outline-path (unless (org-before-first-heading-p)
                           (org-get-outline-path t)))
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
