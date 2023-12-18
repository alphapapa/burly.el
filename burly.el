;;; burly.el --- Save and restore frame/window configurations with buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; URL: https://github.com/alphapapa/burly.el
;; Version: 0.4-pre
;; Package-Requires: ((emacs "27.1") (map "2.1"))
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

;; This package provides tools to save and restore frame and window
;; configurations in Emacs, including buffers that may not be live
;; anymore.  In this way, it's like a lightweight "workspace" manager,
;; allowing you to easily restore one or more frames, including their
;; windows, the windows' layout, and their buffers.

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

(defvar burly-opened-bookmark-name nil
  "The name of the last bookmark opened by Burly.")

(defvar burly-buffer-local-variables nil
  "Variables whose value are saved and restored by Burly bookmarks.
Intended to be bound around code calling `burly-bookmark-'
commands.")

(defvar burly-window-parameters-translators
  `((window-preserved-size
     (serialize . ,(pcase-lambda (`(,buffer ,direction ,size))
                     `(,(buffer-name buffer) ,direction ,size)))
     (deserialize . ,(pcase-lambda (`(,buffer-name ,direction ,size))
                       `(,(get-buffer buffer-name) ,direction ,size)))))
  "Functions used to serialize and deserialize certain window parameters.
For example, the value of `window-preserved-size' includes a
buffer, which must be serialized to a buffer name, and then
deserialized back to the buffer after it is reincarnated.")

;;;; Customization

(defgroup burly nil
  "Save and restore window configurations and their buffers."
  :group 'convenience
  :link '(url-link "https://github.com/alphapapa/burly.el")
  :link '(custom-manual "(Burly)Usage"))

(defcustom burly-bookmark-prefix "Burly: "
  "Prefix string for the name of new Burly bookmarks."
  :type 'string)

(defcustom burly-major-mode-alist
  (list (cons 'org-mode
              (list (cons 'make-url-fn #'burly--org-mode-buffer-url)
                    (cons 'follow-url-fn #'burly-follow-url-org-mode))))
  "Alist mapping major modes to the appropriate Burly functions."
  :type '(alist :key-type symbol
                :value-type (set (cons (const make-url-fn) (function :tag "Make-URL function"))
                                 (cons (const follow-url-fn) (function :tag "Follow-URL function")))))

(defcustom burly-frameset-filter-alist '((name . nil))
  "Alist of frame parameters and filtering functions.
See variable `frameset-filter-alist'."
  :type '(alist :key-type (symbol :tag "Frame parameter")
                :value-type (choice (const :tag "Always copied" nil)
                                    (const :tag "Never copied" :never)
                                    (function :tag "Filter function"))))

(defcustom burly-window-persistent-parameters
  (list (cons 'burly-url 'writable)
        (cons 'header-line-format 'writable)
	(cons 'mode-line-format 'writable)
	(cons 'tab-line-format 'writable)
        (cons 'no-other-window 'writable)
	(cons 'no-delete-other-windows 'writable)
	(cons 'window-preserved-size 'writable)
	(cons 'window-side 'writable)
	(cons 'window-slot 'writable))
  "Additional window parameters to persist.
See Info node `(elisp)Window Parameters'.  See also option
`burly-set-window-persistent-parameters'."
  :type '(alist :key-type (symbol :tag "Window parameter")
                :value-type (choice (const :tag "Not saved" nil)
                                    (const :tag "Saved" writable))))

(defcustom burly-set-window-persistent-parameters t
  "Sync `window-persistent-parameters' with `burly' option.
When this option is non-nil, `window-persistent-parameters' is
set to the value of `burly-window-persistent-parameters' when
Burly restores a window configuration.

By default, `window-persistent-parameters' does not save many of
the parameters that are in the default value of
`burly-window-persistent-parameters', which causes, e.g. a
built-in command like `window-toggle-side-windows' to not persist
such parameters when side windows are toggled (which could,
e.g. cause a window's `mode-line-format' to not persist).  So
enabling this option solves that.

Note: When this option is non-nil,
`burly-window-persistent-parameters' should be set heeding the
warning in the manual about not using the `writable' value for
parameters whose values do not have a read syntax."
  :type 'boolean)

;;;; Commands

;;;###autoload
(defun burly-open-last-bookmark ()
  "Open the last-opened Burly bookmark.
Helpful for, e.g. quickly restoring an overview while working on
a project."
  (interactive)
  (unless burly-opened-bookmark-name
    (user-error "Use command `burly-open-bookmark' first"))
  (burly-open-bookmark burly-opened-bookmark-name))

;;;###autoload
(defun burly-kill-buffer-url (buffer)
  "Copy BUFFER's URL to the kill ring."
  (interactive "bBuffer: ")
  (let ((url (burly-buffer-url (get-buffer buffer))))
    (kill-new url)
    (message "%s" url)))

;;;###autoload
(defun burly-kill-frames-url ()
  "Copy current frameset URL to the kill ring."
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
  (interactive
   (list (completing-read "Save Burly bookmark: " (burly-bookmark-names)
                          nil nil burly-bookmark-prefix)))
  (let ((record (list (cons 'url (burly-frames-url))
                      (cons 'handler #'burly-bookmark-handler))))
    (bookmark-store name record nil)))

;;;###autoload
(defun burly-bookmark-windows (name)
  "Bookmark the current frame's window configuration as NAME."
  (interactive
   (list (completing-read "Save Burly bookmark: " (burly-bookmark-names)
                          nil nil burly-bookmark-prefix)))
  (let ((record (list (cons 'url (burly-windows-url))
                      (cons 'handler #'burly-bookmark-handler))))
    (bookmark-store name record nil)))

;;;###autoload
(defun burly-open-bookmark (bookmark)
  "Restore a window configuration to the current frame from a Burly BOOKMARK."
  (interactive
   (list (completing-read "Open Burly bookmark: " (burly-bookmark-names)
			  nil nil burly-bookmark-prefix)))
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
      ("name" (let ((buffer-name (decode-coding-string (cdr (url-path-and-query urlobj))
						       'utf-8-unix)))
                (or (get-buffer buffer-name)
                    (with-current-buffer (get-buffer-create (concat "*Burly (error): " buffer-name "*"))
                      (insert "Burly was unable to get a buffer named: " buffer-name "\n"
                              "URL: " url "\n"
                              "Please report this error to the developer\n\n")
                      (current-buffer))))))))

(defun burly-buffer-url (buffer)
  "Return URL for BUFFER."
  (let* ((major-mode (buffer-local-value 'major-mode buffer))
         (make-url-fn (map-nested-elt burly-major-mode-alist (list major-mode 'make-url-fn))))
    (cond (make-url-fn (funcall make-url-fn buffer))
          (t (or (with-current-buffer buffer
                   (when-let* ((record (ignore-errors
                                         (bookmark-make-record))))
                     (when burly-buffer-local-variables
                       ;; TODO: Also do this in non-bookmark URL functions?
                       (setf record (burly--add-buffer-local-variables record burly-buffer-local-variables)))
		     (cl-labels ((encode (element)
				   (cl-typecase element
				     (string (encode-coding-string element 'utf-8-unix))
				     ((satisfies proper-list-p) (mapcar #'encode element))
				     (cons (cons (encode (car element))
						 (encode (cdr element))))
				     (t element))))
		       ;; Encode all strings in record with UTF-8.
		       ;; NOTE: If we stop using URLs in the future, maybe this won't be needed.
		       (setf record (encode record)))
                     (burly--bookmark-record-url record)))
                 ;; Buffer can't seem to be bookmarked, so record it as
                 ;; a name-only buffer.  For some reason, it works
                 ;; better to use the buffer name in the query string
                 ;; rather than the filename/path part.
                 (url-recreate-url (url-parse-make-urlobj "emacs+burly+name" nil nil nil nil
                                                          (concat "?" (encode-coding-string (buffer-name buffer)
											    'utf-8-unix))
							  nil nil 'fullness)))))))

(defun burly--add-buffer-local-variables (record variables)
  "Return bookmark RECORD having added buffer-local VARIABLES to it.
Adds alist of variables and their values to a
`burly-buffer-local-variables' property in RECORD.  Variables
without buffer-local bindings in the current buffer are ignored."
  (cl-loop for variable in variables
           when (buffer-local-boundp variable (current-buffer))
           collect (cons variable (buffer-local-value variable (current-buffer)))
           into map
           finally do (bookmark-prop-set record 'burly-buffer-local-variables map))
  record)

;;;;; Files

(defun burly--file-url-buffer (urlobj)
  "Return buffer for \"emacs+burly+file:\" URLOBJ."
  (pcase-let* ((`(,path . ,query-string) (url-path-and-query urlobj))
               (query (url-parse-query-string query-string))
               (buffer (find-file-noselect path))
               (major-mode (buffer-local-value 'major-mode buffer))
               (follow-fn (map-nested-elt burly-major-mode-alist (list major-mode 'follow-url-fn))))
    (cl-assert follow-fn nil "Major mode not in `burly-major-mode-alist': %s" major-mode)
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
         (frameset-filter-alist (append burly-frameset-filter-alist frameset-filter-alist))
         (query (frameset-save frames))
         (print-length nil)             ; Important!
         (filename (concat "?" (url-hexify-string (prin1-to-string query))))
         (url (url-recreate-url (url-parse-make-urlobj "emacs+burly+frames" nil nil nil nil
                                                       filename))))
    (dolist (frame frames)
      ;; Clear window parameters.
      (burly--windows-set-url (window-list frame 'never) 'nullify))
    url))

(defun burly--frameset-restore (urlobj)
  "Restore FRAMESET according to URLOBJ."
  (setf window-persistent-parameters (copy-sequence burly-window-persistent-parameters))
  (pcase-let* ((`(,_ . ,query-string) (url-path-and-query urlobj))
               (frameset (read (url-unhex-string query-string)))
               (frameset-filter-alist (append burly-frameset-filter-alist frameset-filter-alist)))
    ;; Restore buffers.  (Apparently `cl-loop''s in-ref doesn't work with
    ;; its destructuring, so we can't just `setf' on `window-state'.)
    (setf (frameset-states frameset)
          (cl-loop for (frame-parameters . window-state) in (frameset-states frameset)
                   collect (cons frame-parameters (burly--bufferize-window-state window-state))))
    (condition-case err
        (frameset-restore frameset)
      (error (delay-warning 'burly (format "Error while restoring frameset: ERROR:%S  FRAMESET:%S" err frameset))))))

;;;;; Windows

(cl-defun burly-windows-url (&optional (frame (selected-frame)))
  "Return URL for window configuration on FRAME."
  (with-selected-frame frame
    (let* ((query (burly--window-state frame))
           (print-length nil)             ; Important!
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
      (burly--window-serialized window-state))))

(defun burly--window-serialized (state)
  "Return window STATE having serialized its parameters."
  (cl-labels ((translate-state (state)
                "Set windows' buffers in STATE."
                (pcase state
                  (`(leaf . ,_attrs) (translate-leaf state))
                  ((pred atom) state)
                  (`(,_key . ,(pred atom)) state)
                  ((pred list) (mapcar #'translate-state state))))
              (translate-leaf (leaf)
                "Translate window parameters in LEAF."
                (pcase-let* ((`(leaf . ,attrs) leaf)
                             ((map parameters) attrs))
                  (pcase-dolist (`(,parameter . ,(map serialize))
                                 burly-window-parameters-translators)
                    (when (map-elt parameters parameter)
                      (setf (map-elt parameters parameter)
                            (funcall serialize (map-elt parameters parameter)))))
                  (setf (map-elt attrs 'parameters) parameters)
                  (cons 'leaf attrs))))
    (translate-state state)))

(defun burly--windows-set-url (windows &optional nullify)
  "Set `burly-url' window parameter in WINDOWS.
If NULLIFY, set the parameter to nil."
  (dolist (window windows)
    (let ((value (if nullify nil (burly-buffer-url (window-buffer window)))))
      (set-window-parameter window 'burly-url value))))

(defun burly--windows-set (urlobj)
  "Set window configuration according to URLOBJ."
  (setf window-persistent-parameters (copy-sequence burly-window-persistent-parameters))
  (pcase-let* ((window-persistent-parameters (append burly-window-persistent-parameters
                                                     window-persistent-parameters))
               (`(,_ . ,query-string) (url-path-and-query urlobj))
               ;; FIXME: Remove this condition-case eventually, after giving users time to update their bookmarks.
               (state (condition-case-unless-debug nil
                          (read (url-unhex-string query-string))
                        (invalid-read-syntax
                         (display-warning 'burly "Please recreate that Burly bookmark (storage format changed).  If this warning persists, please file a bug report.")
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
  (cl-labels ((bufferize-state (state)
                "Set windows' buffers in STATE."
                (pcase state
                  (`(leaf . ,_attrs) (translate-leaf (bufferize-leaf state)))
                  ((pred atom) state)
                  (`(,_key . ,(pred atom)) state)
                  ((pred list) (mapcar #'bufferize-state state))))
              (bufferize-leaf (leaf)
                "Recreate buffers in LEAF."
                (pcase-let* ((`(leaf . ,attrs) leaf)
                             ((map parameters buffer) attrs)
                             ((map burly-url) parameters)
                             (`(,_buffer-name . ,buffer-attrs) buffer)
                             (new-buffer (burly-url-buffer burly-url)))
                  (setf (map-elt attrs 'buffer) (cons new-buffer buffer-attrs))
                  (cons 'leaf attrs)))
              (translate-leaf (leaf)
                "Translate window parameters in LEAF."
                (pcase-let* ((`(leaf . ,attrs) leaf)
                             ((map parameters) attrs))
                  (pcase-dolist (`(,parameter . ,(map deserialize))
                                 burly-window-parameters-translators)
                    (when (map-elt parameters parameter)
                      (setf (map-elt parameters parameter)
                            (funcall deserialize (map-elt parameters parameter)))))
                  (setf (map-elt attrs 'parameters) parameters)
                  (cons 'leaf attrs))))
    (if-let ((leaf-pos (cl-position 'leaf state)))
        ;; A one-window frame: the elements following `leaf' are that window's params.
        (append (cl-subseq state 0 leaf-pos)
                (translate-leaf (bufferize-leaf (cl-subseq state leaf-pos))))
      ;; Multi-window frame.
      (bufferize-state state))))

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
  (let ((previous-name burly-opened-bookmark-name))
    ;; Set opened bookmark name before actually opening it so that the
    ;; tabs-mode advice functions can use it beforehand.
    (setf burly-opened-bookmark-name (car bookmark))
    (condition-case err
	(burly-open-url (alist-get 'url (bookmark-get-bookmark-record bookmark)))
      (error (setf burly-opened-bookmark-name previous-name)
	     (signal (car err) (cdr err))))))

(defun burly--bookmark-record-url (record)
  "Return a URL for bookmark RECORD."
  (cl-assert record)
  (pcase-let* ((`(,name . ,props) record)
               (print-length nil)             ; Important!
               (query (cl-loop for prop in props
                               ;; HACK: Remove unreadable values from props.
                               do (cl-loop for value in-ref (cdr prop)
                                           when (or (bufferp value))
                                           do (setf value nil))
                               collect (list (car prop) (prin1-to-string (cdr prop)))))
               (filename (concat (url-hexify-string name) "?" (url-build-query-string (remove nil query)))))
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
                                             ('help-fn (ignore-errors
                                                         ;; NOTE: Due to changes in help-mode.el which serialize natively
                                                         ;; compiled subrs in the bookmark record, which cannot be read
                                                         ;; back (which actually break the entire bookmark system when
                                                         ;; such a record is saved in the bookmarks file), we have to
                                                         ;; workaround a failure to read here.  See bug#56643.
                                                         (read (cadr prop))))
                                             ('position (cl-parse-integer (cadr prop)))
                                             (_ (read (cadr prop))))
                               collect (cons key value)))
               (record (cons path props)))
    (cl-labels ((decode (element)
                  "Return ELEMENT with strings decoded using `utf-8-unix'."
		  (cl-typecase element
		    (string (decode-coding-string element 'utf-8-unix))
		    ((satisfies proper-list-p) (mapcar #'decode element))
		    (cons (cons
			   (decode (car element))
			   (decode (cdr element))))
		    (t element))))
      ;; NOTE: If we stop using URLs in the future, maybe this won't be needed.
      (setf record (decode record)))
    (save-window-excursion
      (condition-case err
          (progn
            (bookmark-jump record)
            (when-let ((local-variable-map (bookmark-prop-get record 'burly-buffer-local-variables)))
              (cl-loop for (variable . value) in local-variable-map
                       do (setf (buffer-local-value variable (current-buffer)) value))))
        (error (delay-warning 'burly (format "Error while opening bookmark: ERROR:%S  RECORD:%S" err record))))
      (current-buffer))))

(defun burly-bookmark-names ()
  "Return list of all Burly bookmark names."
  (bookmark-maybe-load-default-file)
  (cl-loop for bookmark in bookmark-alist
           for (_name . params) = bookmark
           when (equal #'burly-bookmark-handler (alist-get 'handler params))
           collect (car bookmark)))

;;;;; Org buffers

;; We only require Org when compiling the file.  At runtime, Org will
;; be loaded before we call any of its functions, because we load the
;; Org file into a buffer first, which activates `org-mode'.

(eval-when-compile
  (require 'org))

(declare-function org-before-first-heading-p "org")
(declare-function org-back-to-heading "org")
(declare-function org-find-olp "org")
(declare-function org-tree-to-indirect-buffer "org")
(declare-function org-narrow-to-subtree "org")
(declare-function org-heading-components "org")
(declare-function org-up-heading-safe "org")

(defun burly--org-mode-buffer-url (buffer)
  "Return URL for Org BUFFER."
  (with-current-buffer buffer
    (cl-assert (or (buffer-file-name buffer)
                   (buffer-file-name (buffer-base-buffer buffer)))
               nil "Buffer has no file name: %s" buffer)
    (let* ((narrowed (buffer-narrowed-p))
           (indirect (buffer-base-buffer buffer))
           (top-olp
            ;; For narrowing purposes, start with the heading at the top of the buffer.
            (when (buffer-narrowed-p)
              (save-excursion
                (goto-char (point-min))
                ;; `org-get-outline-path' replaces links in headings with their
                ;; descriptions, which prevents using them in regexp searches.
                (when (org-heading-components)
                  (org-with-wide-buffer
                   (nreverse (cl-loop collect (substring-no-properties (nth 4 (org-heading-components)))
                                      while (org-up-heading-safe))))))))
           (point-olp
            (when (ignore-errors (org-heading-components))
              (org-with-wide-buffer
               (nreverse (cl-loop collect (substring-no-properties (nth 4 (org-heading-components)))
                                  while (org-up-heading-safe))))))
           (pos (point))
           (relative-pos (when top-olp
                           (- (point) (save-excursion
                                        (org-back-to-heading)
                                        (point)))))
           (print-length nil)             ; Important!
           (query (list (list "pos" pos)
                        (when top-olp
                          (list "top-olp" (prin1-to-string top-olp)))
                        (when point-olp
                          (list "point-olp" (prin1-to-string point-olp)))
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
  "In BUFFER, jump to heading and position from QUERY, and return a buffer.
If QUERY specifies that the buffer should be indirect, a new,
indirect buffer is returned.  Otherwise BUFFER is returned."
  (with-current-buffer buffer
    ;; Because the query is built with `url-build-query-string', which allows keys
    ;; to have multiple values, we define `query-elt', which handles that and only
    ;; returns the first value (which is fine since we only use single values).
    (cl-labels ((query-elt (query elt)
                  (car (map-elt query elt))))
      (let* ((pos (query-elt query "pos"))
             (indirect (query-elt query "indirect"))
             (narrowed (query-elt query "narrowed"))
             (top-olp (query-elt query "top-olp"))
             (point-olp (query-elt query "point-olp"))
             (relative-pos (query-elt query "relative-pos"))
             (heading-pos (when top-olp
                            (org-find-olp (read top-olp) 'this-buffer))))
        (widen)
        (if heading-pos
            (goto-char heading-pos)
          (goto-char (string-to-number pos)))
        (cond (indirect (org-tree-to-indirect-buffer))
              (narrowed (progn
                          (org-narrow-to-subtree)
                          (goto-char (org-find-olp (read point-olp) 'this-buffer)))))
        (when (and heading-pos relative-pos)
          (forward-char (string-to-number relative-pos)))
        (current-buffer)))))

;;;; Footer

(provide 'burly)

;;; burly.el ends here
