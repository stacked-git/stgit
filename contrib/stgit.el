;;; stgit.el --- major mode for StGit interaction
;;
;; Copyright (C) 2007-2013 David Kågedal
;;
;; Author: David Kågedal <davidk@lysator.liu.se>
;; Homepage: http://stacked-git.github.io
;; Version: 0.17.1
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This is an interactive tool to interact with git branches using
;; StGit.
;;
;; StGit is a command-line tool providing similar functionality to
;; Quilt (i.e. pushing/popping patches to/from a stack) on top of Git.
;; These operations are performed using Git commands and the patches
;; are stored as Git commit objects, allowing easy merging of the
;; StGit patches into other repositories using standard Git
;; functionality.
;;
;; To start using the Emacs interface, run M-x stgit and select the
;; git repository you are working in.
;;
;; To get quick help about the available keybindings in the buffer,
;; press '?'

;;; Installation:
;;
;; To install: put this file on the load-path and place the following
;; in your .emacs file:
;;
;;    (require 'stgit)
;;
;; To start: `M-x stgit'

;;; Code:

(when (< emacs-major-version 22)
  (error "Emacs older than 22 is not supported by stgit.el"))

(require 'git nil t)
(require 'cl)
(require 'comint)
(require 'dired)
(require 'ewoc)
(require 'easymenu)
(require 'format-spec)

(defun stgit-set-default (symbol value)
  "Set default value of SYMBOL to VALUE using `set-default' and
reload all StGit buffers."
  (set-default symbol value)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (derived-mode-p 'stgit-mode)
        (stgit-post-refresh buf :reload)))))

(defgroup stgit nil
  "A user interface for the StGit patch maintenance tool."
  :group 'tools
  :link '(function-link stgit)
  :link '(url-link "http://stacked-git.github.io"))

(defcustom stgit-default-show-worktree t
  "Set to non-nil to by default show the working tree in a new stgit buffer.

Use \\<stgit-mode-map>\\[stgit-toggle-worktree] to toggle this \
setting in an already-started StGit buffer."
  :type 'boolean
  :group 'stgit
  :link '(variable-link stgit-show-worktree))

(defcustom stgit-default-show-unknown nil
  "Set to non-nil to by default show unknown files a new stgit buffer.

Use \\<stgit-mode-map>\\[stgit-toggle-unknown] to toggle this \
setting in an already-started StGit buffer."
  :type 'boolean
  :group 'stgit
  :link '(variable-link stgit-show-unknown))

(defcustom stgit-default-show-ignored nil
  "Set to non-nil to by default show ignored files a new stgit buffer.

Use \\<stgit-mode-map>\\[stgit-toggle-ignored] to toggle this \
setting in an already-started StGit buffer."
  :type 'boolean
  :group 'stgit
  :link '(variable-link stgit-show-ignored))

(defcustom stgit-default-show-patch-names t
  "If non-nil, default to showing patch names in a new stgit buffer.

Use \\<stgit-mode-map>\\[stgit-toggle-patch-names] \
to toggle the this setting in an already-started StGit buffer."
  :type 'boolean
  :group 'stgit
  :link '(variable-link stgit-show-patch-names))

(defcustom stgit-default-show-committed nil
  "Set to nil to inhibit showing of historical git commits by default.

Use \\<stgit-mode-map>\\[stgit-toggle-committed] \
to toggle this setting and to control how many commits are
shown."
  :type 'boolean
  :group 'stgit
  :link '(variable-link stgit-default-committed-count)
  :link '(variable-link stgit-show-committed))

(defcustom stgit-default-committed-count 5
  "The number of historical commits to show when `stgit-show-committed'
is enabled."
  :type 'number
  :group 'stgit
  :link '(variable-link stgit-default-show-committed)
  :link '(variable-link stgit-committed-count))

(defcustom stgit-default-show-svn t
  "Set to non-nil to by default show subversion information in a
new stgit buffer.

Use \\<stgit-mode-map>\\[stgit-toggle-svn] to toggle this \
setting in an already-started StGit buffer."
  :type 'boolean
  :group 'stgit
  :link '(variable-link stgit-show-worktree))

(defcustom stgit-abbreviate-copies-and-renames t
  "If non-nil, abbreviate copies and renames as \"dir/{old -> new}/file\"
instead of \"dir/old/file -> dir/new/file\"."
  :type 'boolean
  :group 'stgit
  :set 'stgit-set-default)

(defcustom stgit-find-copies-harder nil
  "Try harder to find copied files when listing patches.

When not nil, runs git diff-tree with the --find-copies-harder
flag, which reduces performance."
  :type 'boolean
  :group 'stgit
  :set 'stgit-set-default)

(defcustom stgit-show-worktree-mode 'center
  "This variable controls where the \"Index\" and \"Work tree\"
will be shown on in the buffer.

It can be set to 'top (above all patches), 'center (show between
applied and unapplied patches), and 'bottom (below all patches)."
  :type '(radio (const :tag "above all patches (top)" top)
                (const :tag "between applied and unapplied patches (center)"
                       center)
                (const :tag "below all patches (bottom)" bottom))
  :group 'stgit
  :link '(variable-link stgit-show-worktree)
  :set 'stgit-set-default)

(defcustom stgit-patch-line-format "%s%m%-30n %e%d"
  "The format string used to format patch lines.
The format string is passed to `format-spec' and the following
format characters are recognized:

  %s - A '+', '-', '>' or space, depending on whether the patch is
       applied, unapplied, top, or something else.

  %m - An asterisk if the patch is marked, and a space otherwise.

  %n - The patch name.

  %e - The string \"(empty) \" if the patch is empty.

  %d - The short patch description.

  %D - The short patch description, or the patch name.

When `stgit-show-patch-names' is non-nil, the `stgit-noname-patch-line-format'
variable is used instead."
  :type 'string
  :group 'stgit
  :set 'stgit-set-default)

(defcustom stgit-noname-patch-line-format "%s%m%e%D"
  "The alternate format string used to format patch lines.
It has the same semantics as `stgit-patch-line-format', and the
display can be toggled between the two formats using \
\\<stgit-mode-map>\\[stgit-toggle-patch-names].

The alternate form is used when the patch name is hidden."
  :type 'string
  :group 'stgit
  :set 'stgit-set-default)

(defcustom stgit-file-line-format "    %-11s %-2m %n   %c"
  "The format string used to format file lines.
The format string is passed to `format-spec' and the following
format characters are recognized:

  %s - A string describing the status of the file.

  %m - Mode change information

  %n - The file name.

  %c - A description of file changes."
  :type 'string
  :group 'stgit
  :set 'stgit-set-default)

(defcustom stgit-git-program "git"
  "The program used by `stgit-mode' to run git."
  :type 'string
  :group 'stgit)

(defcustom stgit-stg-program "stg"
  "The program used by `stgit-mode' to run StGit."
  :type 'string
  :group 'stgit)

(defgroup stgit-faces nil
  "Faces for `stgit-mode'."
  :group 'stgit)

(defface stgit-branch-name-face
  '((t :inherit bold))
  "The face used for the StGit branch name"
  :group 'stgit-faces)

(defface stgit-top-patch-face
  '((((background dark)) (:weight bold :foreground "yellow"))
    (((background light)) (:weight bold :foreground "purple"))
    (t (:weight bold)))
  "The face used for the top patch names"
  :group 'stgit-faces)

(defface stgit-applied-patch-face
  '((((background dark)) (:foreground "light yellow"))
    (((background light)) (:foreground "purple"))
    (t ()))
  "The face used for applied patch names"
  :group 'stgit-faces)

(defface stgit-unapplied-patch-face
  '((((background dark)) (:foreground "gray80"))
    (((background light)) (:foreground "orchid"))
    (t ()))
  "The face used for unapplied patch names"
  :group 'stgit-faces)

(defface stgit-committed-patch-face
  '((((background dark)) (:foreground "gray50"))
    (((background light)) (:foreground "gray50"))
    (t ()))
  "The face used for already committed patch names"
  :group 'stgit-faces)

(defface stgit-description-face
  '((((background dark)) (:foreground "tan"))
    (((background light)) (:foreground "dark red")))
  "The face used for StGit descriptions"
  :group 'stgit-faces)

(defface stgit-index-work-tree-title-face
  '((((supports :slant italic)) :slant italic)
    (t :inherit bold))
  "StGit mode face used for the \"Index\" and \"Work tree\" titles"
  :group 'stgit-faces)

(defface stgit-unmerged-file-face
  '((((class color) (background light)) (:foreground "red" :bold t))
    (((class color) (background dark)) (:foreground "red" :bold t)))
  "StGit mode face used for unmerged file status"
  :group 'stgit-faces)

(defface stgit-unknown-file-face
  '((((class color) (background light)) (:foreground "goldenrod" :bold t))
    (((class color) (background dark)) (:foreground "goldenrod" :bold t)))
  "StGit mode face used for unknown file status"
  :group 'stgit-faces)

(defface stgit-ignored-file-face
  '((((class color) (background light)) (:foreground "grey60"))
    (((class color) (background dark)) (:foreground "grey40")))
  "StGit mode face used for ignored files")

(defface stgit-file-permission-face
  '((((class color) (background light)) (:foreground "green" :bold t))
    (((class color) (background dark)) (:foreground "green" :bold t)))
  "StGit mode face used for permission changes."
  :group 'stgit-faces)

(defface stgit-modified-file-face
  '((((class color) (background light)) (:foreground "purple"))
    (((class color) (background dark)) (:foreground "salmon")))
  "StGit mode face used for modified file status"
  :group 'stgit-faces)

(defun stgit (dir)
  "Manage StGit patches for the tree in DIR.

See `stgit-mode' for commands available."
  (interactive "DDirectory: \n")
  (switch-to-stgit-buffer (git-get-top-dir dir))
  (stgit-reload))

(defun stgit-assert-mode ()
  "Signal an error if not in an StGit buffer."
  (assert (derived-mode-p 'stgit-mode) nil "Not an StGit buffer"))

(unless (fboundp 'git-get-top-dir)
  (defun git-get-top-dir (dir)
    "Retrieve the top-level directory of a git tree."
    (let ((cdup (with-output-to-string
                  (with-current-buffer standard-output
                    (cd dir)
                    (unless (eq 0 (call-process stgit-git-program nil t nil
                                                "rev-parse" "--show-cdup"))
                      (error "Cannot find top-level git tree for %s" dir))))))
      (expand-file-name (concat (file-name-as-directory dir)
                                (car (split-string cdup "\n")))))))

(defun stgit-refresh-git-status (&optional dir)
  "If it exists, refresh the `git-status' buffer belonging to
directory DIR or `default-directory'"
  (when (and (fboundp 'git-find-status-buffer)
             (fboundp 'git-refresh-status))
    (let* ((top-dir (git-get-top-dir (or dir default-directory)))
           (git-status-buffer (and top-dir (git-find-status-buffer top-dir))))
      (when git-status-buffer
        (with-current-buffer git-status-buffer
          (git-refresh-status))))))

(defun stgit-find-buffer (dir)
  "Return the buffer displaying StGit patches for DIR, or nil if none."
  (setq dir (file-truename (file-name-as-directory dir)))
  (let ((buffers (buffer-list)))
    (while (and buffers
                (not (with-current-buffer (car buffers)
                       (and (eq major-mode 'stgit-mode)
                            (string= (file-truename default-directory) dir)))))
      (setq buffers (cdr buffers)))
    (and buffers (car buffers))))

(defun switch-to-stgit-buffer (dir)
  "Switch to a (possibly new) buffer displaying StGit patches for DIR."
  (setq dir (file-name-as-directory dir))
  (let ((buffer (stgit-find-buffer dir)))
    (switch-to-buffer (or buffer
			  (create-stgit-buffer dir)))))

(defstruct (stgit-patch
            (:conc-name stgit-patch->))
  status name desc empty files-ewoc)

(defun stgit-patch-display-name (patch)
  (let ((name (stgit-patch->name patch)))
    (case name
      (:index "Index")
      (:work "Work Tree")
      (t (symbol-name name)))))

(defun stgit-insert-without-trailing-whitespace (text)
  "Insert TEXT in buffer using `insert', without trailing whitespace.
A newline is appended."
  (unless (string-match "\\(.*?\\) *$" text)
    (error))
  (insert (match-string 1 text) ?\n))

(defun stgit-line-format ()
  "Return the current line format; one of
`stgit-patch-line-format' and `stgit-noname-patch-line-format'"
  (if stgit-show-patch-names
      stgit-patch-line-format
    stgit-noname-patch-line-format))

(defun stgit-patch-pp (patch)
  (let* ((status (stgit-patch->status patch))
         (start (point))
         (name (stgit-patch->name patch))
         (face (cdr (assq status stgit-patch-status-face-alist)))
         (fmt (stgit-line-format))
         (spec (format-spec-make
                ?s (case status
                     ('applied "+")
                     ('top ">")
                     ('unapplied "-")
                     (t " "))
                ?m (if (memq name stgit-marked-patches)
                       "*" " ")
                ?n (propertize (stgit-patch-display-name patch)
                               'face face
                               'syntax-table (string-to-syntax "w"))
                ?e (if (stgit-patch->empty patch) "(empty) " "")
                ?d (propertize (or (stgit-patch->desc patch) "")
                               'face 'stgit-description-face)
                ?D (propertize (let ((desc (stgit-patch->desc patch)))
                                 (if (zerop (length desc))
                                   (stgit-patch-display-name patch)
                                   desc))
                               'face face)))
         (text (format-spec fmt spec)))

    (stgit-insert-without-trailing-whitespace text)
    (put-text-property start (point) 'entry-type 'patch)
    (when (memq name stgit-expanded-patches)
      (stgit-insert-patch-files patch))
    (put-text-property start (point) 'patch-data patch)))

(defun create-stgit-buffer (dir)
  "Create a buffer for showing StGit patches.
Argument DIR is the repository path."
  (let ((buf (create-file-buffer (concat dir "*stgit*")))
        (inhibit-read-only t))
    (with-current-buffer buf
      (setq default-directory dir)
      (stgit-mode)
      (set (make-local-variable 'stgit-ewoc)
           (ewoc-create #'stgit-patch-pp "Branch:\n\n" "--\n" t))
      (setq buffer-read-only t))
    buf))

(def-edebug-spec stgit-capture-output
  (form body))
(defmacro stgit-capture-output (name &rest body)
  "Capture StGit output and, if there was any output, show it in a window
at the end.
Returns the result of the last form in BODY."
  (declare (debug ([&or stringp null] body))
           (indent 1))
  `(let ((output-buf (get-buffer-create ,(or name "*StGit output*")))
         (stgit-dir default-directory)
         (inhibit-read-only t))
     (set-buffer-major-mode output-buf)
     (with-current-buffer output-buf
       (buffer-disable-undo)
       (erase-buffer)
       (setq default-directory stgit-dir)
       (setq buffer-read-only t))
     (prog1
	 (let ((standard-output output-buf))
	   ,@body)
       (with-current-buffer output-buf
	 (set-buffer-modified-p nil)
	 (setq buffer-read-only t)
	 (if (< (point-min) (point-max))
	     (display-buffer output-buf t))))))

(defun stgit-make-run-args (args)
  "Return a copy of ARGS with its elements converted to strings."
  (mapcar (lambda (x)
            ;; don't use (format "%s" ...) to limit type errors
            (cond ((stringp x) x)
                  ((integerp x) (number-to-string x))
                  ((symbolp x) (symbol-name x))
                  (t
                   (error "Bad element in stgit-make-run-args args: %S" x))))
          args))

(defvar stgit-inhibit-messages nil
  "Set to non-nil to inhibit messages when running `stg' commands.
See also `stgit-message'.")
(defun stgit-message (format-spec &rest args)
  "Call `message' on the arguments unless `stgit-inhibit-messages' is non-nil."
  (unless stgit-inhibit-messages
    (apply 'message format-spec args)))

(defmacro stgit-show-task-message (message &rest body)
  "Display \"MESSAGE...\" before executing BODY and then display
\"MESSAGE...done\" when done. MESSAGE will only be evaluated if
necessary and no message will be shown if MESSAGE is nil.

If `stgit-inhibit-messages' is non-nil, messages are
suppressed. See also `stgit-message'. If MESSAGE is non-nil, BODY
will be executed with `stgit-inhibit-messages' set to `t'.

Returns the return value of BODY."
  (declare (indent 1) (debug (form body)))
  (let ((msg (make-symbol "msg")))
    `(let ((,msg (and (not stgit-inhibit-messages)
                      ,message)))
       (when ,msg (message "%s..." ,msg))
       (prog1
           (let ((stgit-inhibit-messages (or ,msg stgit-inhibit-messages)))
             ,@body)
         (when ,msg (message "%s...done" ,msg))))))

(defun stgit-run (&rest args)
  (setq args (stgit-make-run-args args))
  (stgit-show-task-message
      (mapconcat #'identity `("Running" ,stgit-stg-program ,@args) " ")
    (apply 'call-process stgit-stg-program nil standard-output nil args)))

(defun stgit-run-silent (&rest args)
  (let ((stgit-inhibit-messages t))
    (apply 'stgit-run args)))

(defun stgit-run-git (&rest args)
  (setq args (stgit-make-run-args args))
  (stgit-show-task-message
      (mapconcat #'identity `("Running" ,stgit-git-program ,@args) " ")
    (apply 'call-process stgit-git-program nil standard-output nil args)))

(defun stgit-run-git-silent (&rest args)
  (let ((stgit-inhibit-messages t))
    (apply 'stgit-run-git args)))

(defun stgit-index-empty-p ()
  "Returns non-nil if the index contains no changes from HEAD."
  (zerop (stgit-run-git-silent "diff-index" "--cached" "--quiet" "HEAD")))

(defun stgit-work-tree-empty-p ()
  "Returns non-nil if the work tree contains no changes from index."
  (zerop (stgit-run-git-silent "diff-files" "--quiet")))

(defvar stgit-did-advise nil
  "Set to non-nil if appropriate (non-stgit) git functions have
been advised to update the stgit status when necessary.")

(defconst stgit-allowed-branch-name-re
  ;; Disallow control characters, space, del, and "/:@^{}~" in
  ;; "/"-separated parts; parts may not start with a period (.)
  "^[^\0- ./:@^{}~\177][^\0- /:@^{}~\177]*\
\\(/[^\0- ./:@^{}~\177][^\0- /:@^{}~\177]*\\)*$"
  "Regular expression that (new) branch names must match.")

(defun stgit-refresh-index ()
  (when stgit-index-node
    (ewoc-invalidate (car stgit-index-node) (cdr stgit-index-node))))

(defun stgit-refresh-worktree ()
  (when stgit-worktree-node
    (ewoc-invalidate (car stgit-worktree-node) (cdr stgit-worktree-node))))

(defun stgit-run-series-insert-index (ewoc)
  (setq index-node    (cons ewoc (ewoc-enter-last ewoc
                                                  (make-stgit-patch
                                                   :status 'index
                                                   :name :index
                                                   :desc nil
                                                   :empty nil)))
        worktree-node (cons ewoc (ewoc-enter-last ewoc
                                                  (make-stgit-patch
                                                   :status 'work
                                                   :name :work
                                                   :desc nil
                                                   :empty nil)))))

(defun stgit-get-position (&optional position)
  "Return `stgit-mode' position information at POSITION (point by
default) that can be used to restore the point using
`stgit-restore-position'."
  (let ((opoint (point)))
    (and position (goto-char position))
    (prog1
        (list (stgit-patch-name-at-point)
              (let ((f (stgit-patched-file-at-point)))
                (and f (stgit-file->file f)))
              (line-number-at-pos)
              (current-column))
      (goto-char opoint))))

(defun stgit-restore-position (state)
  "Move point to the position in STATE, as returned by
`stgit-get-position'."
  (destructuring-bind (patch file line column) state
    (unless (and patch (case (stgit-goto-patch patch file)
                         ((t) (move-to-column column) t)
                         ((:patch) t)))
      (goto-char (point-min))
      (forward-line (1- line))
      (move-to-column (if patch
                          (stgit-goal-column)
                        column)))))

(defun stgit-get-window-state ()
  "Return the state of the buffer and its windows. Use
`stgit-restore-window-state' to restore the state."
  (list (current-buffer)
        (mapcar (lambda (window)
                  (cons window
                        (stgit-get-position
                         (window-point window))))
                (get-buffer-window-list (current-buffer)
                                        t t))
        (stgit-get-position (point))
        (let ((mark (mark)))
          (and mark
               (stgit-get-position mark)))
        mark-active
        transient-mark-mode))

(defun stgit-restore-window-state (state)
  "Restore the state of the stgit buffer and windows in STATE, as
obtained from `stgit-get-window-state'."
  (destructuring-bind
      (buffer window-states buffer-state mark-state
              old-mark-active old-transient-mark-mode)
      state
    (with-current-buffer buffer
      (mapc (lambda (x) (let ((window (car x))
                              (state  (cdr x)))
                          (when (and (window-live-p window)
                                     (eq (window-buffer window) buffer))
                            (stgit-restore-position state)
                            (set-window-point window (point)))))
            window-states)
      (let ((mark-point (when mark-state
                          (stgit-restore-position mark-state)
                          (point))))
        (stgit-restore-position buffer-state)
        (if (and mark-point (null old-mark-active))
            (set-marker (mark-marker) mark-point)
          (set-mark mark-point))
        (setq mark-active         old-mark-active
              transient-mark-mode old-transient-mark-mode)))))

(defmacro stgit-save-excursion (&rest body)
  "Execute BODY and, for each window displaying the current
buffer, move point and mark back to the file, patch, or line
where they were."
  (declare (indent 0) (debug (body)))
  (let ((state (make-symbol "state")))
    `(let ((,state (stgit-get-window-state))
           deactivate-mark)
       ,@body
       (stgit-restore-window-state ,state))))

(defun stgit-svn-find-rev (sha1 hash)
  "Return the subversion revision corresponding to SHA1 as
reported by git svn.

Cached data is stored in HASH, which must have been created
using (make-hash-table :test 'equal)."
  (let ((result (gethash sha1 hash t)))
    (when (eq result t)
      (let ((svn-rev (with-output-to-string
                       (stgit-run-git-silent "svn" "find-rev"
                                             "--" sha1))))
        (setq result (when (string-match "\\`[0-9]+" svn-rev)
                       (string-to-number (match-string 0 svn-rev))))
        (puthash sha1 result hash)))
    result))

(defun stgit-run-series (ewoc)
  (setq stgit-index-node nil
        stgit-worktree-node nil)
  (let (all-patchsyms base)
    (when (and stgit-show-committed
	       (> stgit-committed-count 0)
	       (setq base (condition-case nil
			      (stgit-id "{base}")
			    (error nil))))
      (let* ((show-svn stgit-show-svn)
             (svn-hash stgit-svn-find-rev-hash)
	     (nentries (format "-%s" stgit-committed-count)))
        (with-temp-buffer
          (let* ((standard-output (current-buffer))
                 (fmt (stgit-line-format))
                 (commit-abbrev (when (string-match "%-\\([0-9]+\\)n" fmt)
                                  (string-to-number (match-string 1 fmt))))
                 (exit-status (stgit-run-git-silent "--no-pager" "log"
                                                    "--reverse"
                                                    "--pretty=oneline"
                                                    nentries
                                                    base)))
            (goto-char (point-min))
            (if (not (zerop exit-status))
                (message "Failed to run git log")
              (while (not (eobp))
                (unless (looking-at
                         "\\([0-9a-f]+\\)\\(\\.\\.\\.\\)? \\(.*\\)")
                  (error "Syntax error in output from git log"))
                (let* ((state 'committed)
                       (name (match-string 1))
                       (desc (match-string 3))
                       (empty nil))

                  (when show-svn
                    (let ((svn-rev (stgit-svn-find-rev name svn-hash)))
                      (when svn-rev
                        (setq desc (format "(r%s) %s" svn-rev desc)))))

                  (and commit-abbrev
                       (< commit-abbrev (length name))
                       (setq name (substring name 0 commit-abbrev)))

                  (setq name (intern name))

                  (setq all-patchsyms (cons name all-patchsyms))
                  (ewoc-enter-last ewoc
                                   (make-stgit-patch
                                    :status state
                                    :name   name
                                    :desc   desc
                                    :empty  empty)))
                (forward-line 1)))))))
    (let ((inserted-index (not stgit-show-worktree))
          index-node
          worktree-node)
      (with-temp-buffer
        (let* ((standard-output (current-buffer))
               (exit-status (stgit-run-silent "series"
                                              "--description" "--empty")))
          (goto-char (point-min))
          (if (not (zerop exit-status))
              (cond ((looking-at "stg series: \\(.*\\)")
                     (setq inserted-index t)
                     (ewoc-set-hf ewoc (car (ewoc-get-hf ewoc))
                                  (substitute-command-keys
                                   "-- not initialized; run \\[stgit-init]")))
                    ((looking-at ".*")
                     (error "Error running stg: %s"
                            (match-string 0))))
            (while (not (eobp))
              (unless (looking-at
                       "\\([0 ]\\)\\([>+-]\\)\\( \\)\\([^ ]+\\) *[|#] \\(.*\\)")
                (error "Syntax error in output from stg series"))
              (let* ((state-str (match-string 2))
                     (state (cond ((string= state-str ">") 'top)
                                  ((string= state-str "+") 'applied)
                                  ((string= state-str "-") 'unapplied)))
                     (name (intern (match-string 4)))
                     (desc (match-string 5))
                     (empty (string= (match-string 1) "0")))
                (unless inserted-index
                  (when (or (eq stgit-show-worktree-mode 'top)
                            (and (eq stgit-show-worktree-mode 'center)
                                 (eq state 'unapplied)))
                    (setq inserted-index t)
                    (stgit-run-series-insert-index ewoc)))
                (setq all-patchsyms (cons name all-patchsyms))
                (ewoc-enter-last ewoc
                                 (make-stgit-patch
                                  :status state
                                  :name   name
                                  :desc   desc
                                  :empty  empty)))
              (forward-line 1)))))
      (unless inserted-index
        (stgit-run-series-insert-index ewoc))
      (setq stgit-index-node     index-node
            stgit-worktree-node  worktree-node
            stgit-marked-patches (intersection stgit-marked-patches
                                               all-patchsyms)))))

(defun stgit-current-branch ()
  "Return the name of the current branch."
  (substring (with-output-to-string
               (stgit-run-silent "branch"))
             0 -1))

(defun stgit-reload (&optional description)
  "Update the contents of the StGit buffer.

If DESCRIPTION is non-nil, it is displayed as a status message
during the operation."
  (interactive)
  (stgit-assert-mode)
  (stgit-show-task-message description
    (let ((inhibit-read-only t))
      (stgit-save-excursion
        (ewoc-filter stgit-ewoc #'(lambda (x) nil))
        (ewoc-set-hf stgit-ewoc
                     (concat "Branch: "
                             (propertize (stgit-current-branch)
                                         'face 'stgit-branch-name-face)
                             "\n\n")
                     (if stgit-show-worktree
                         "--"
                       (propertize
                        (substitute-command-keys "--\n\"\
\\[stgit-toggle-worktree]\" shows the working tree\n")
                        'face 'stgit-description-face)))
        (stgit-run-series stgit-ewoc))
      (stgit-refresh-git-status))))

(defconst stgit-file-status-code-strings
  (mapcar (lambda (arg)
            (cons (car arg)
                  (propertize (cadr arg) 'face (car (cddr arg)))))
          '((add         "Added"       stgit-modified-file-face)
            (copy        "Copied"      stgit-modified-file-face)
            (delete      "Deleted"     stgit-modified-file-face)
            (modify      "Modified"    stgit-modified-file-face)
            (rename      "Renamed"     stgit-modified-file-face)
            (mode-change "Mode change" stgit-modified-file-face)
            (unmerged    "Unmerged"    stgit-unmerged-file-face)
            (unknown     "Unknown"     stgit-unknown-file-face)
            (ignore      "Ignored"     stgit-ignored-file-face)))
  "Alist of code symbols to description strings")

(defconst stgit-patch-status-face-alist
  '((applied   . stgit-applied-patch-face)
    (top       . stgit-top-patch-face)
    (unapplied . stgit-unapplied-patch-face)
    (committed . stgit-committed-patch-face)
    (index     . stgit-index-work-tree-title-face)
    (work      . stgit-index-work-tree-title-face))
  "Alist of face to use for a given patch status")

(defun stgit-file-status-code-as-string (file)
  "Return stgit status code for FILE as a string"
  (let* ((code (assq (stgit-file->status file)
                     stgit-file-status-code-strings))
         (score (stgit-file->cr-score file)))
    (when code
      (if (and score (/= score 100))
          (format "%s %s" (cdr code)
                  (propertize (format "%d%%" score)
                              'face 'stgit-description-face))
        (cdr code)))))

(defun stgit-file-status-code (str &optional score)
  "Return stgit status code from git status string"
  (let ((code (assoc str '(("A" . add)
                           ("C" . copy)
                           ("D" . delete)
                           ("I" . ignore)
                           ("M" . modify)
                           ("R" . rename)
                           ("T" . mode-change)
                           ("U" . unmerged)
                           ("X" . unknown)))))
    (setq code (if code (cdr code) 'unknown))
    (when (stringp score)
      (if (> (length score) 0)
          (setq score (string-to-number score))
        (setq score nil)))
    (if score (cons code score) code)))

(defconst stgit-file-type-strings
  '((#o100 . "file")
    (#o120 . "symlink")
    (#o160 . "subproject"))
  "Alist of names of file types")

(defun stgit-file-type-string (type)
  "Return string describing file type TYPE (the high bits of file permission).
Cf. `stgit-file-type-strings' and `stgit-file-type-change-string'."
  (let ((type-str (assoc type stgit-file-type-strings)))
    (or (and type-str (cdr type-str))
	(format "unknown type %o" type))))

(defun stgit-file-type-change-string (old-perm new-perm)
  "Return string describing file type change from OLD-PERM to NEW-PERM.
Cf. `stgit-file-type-string'."
  (let ((old-type (lsh old-perm -9))
        (new-type (lsh new-perm -9)))
    (cond ((= old-type new-type) "")
          ((zerop new-type) "")
          ((zerop old-type)
           (if (= new-type #o100)
               ""
             (format "(%s)" (stgit-file-type-string new-type))))
          (t (format "(%s -> %s)"
                     (stgit-file-type-string old-type)
                     (stgit-file-type-string new-type))))))

(defun stgit-file-mode-change-string (old-perm new-perm)
  "Return string describing file mode change from OLD-PERM to NEW-PERM.
Cf. `stgit-file-type-change-string'."
  (setq old-perm (logand old-perm #o777)
        new-perm (logand new-perm #o777))
  (if (or (= old-perm new-perm)
          (zerop old-perm)
          (zerop new-perm))
      ""
    (let* ((modified       (logxor old-perm new-perm))
	   (not-x-modified (logand (logxor old-perm new-perm) #o666)))
      (cond ((zerop modified) "")
            ((and (zerop not-x-modified)
                  (or (and (eq #o111 (logand old-perm #o111))
                           (propertize "-x" 'face 'stgit-file-permission-face))
                      (and (eq #o111 (logand new-perm #o111))
                           (propertize "+x" 'face
                                       'stgit-file-permission-face)))))
            (t (concat (propertize (format "%o" old-perm)
                                   'face 'stgit-file-permission-face)
                       (propertize " -> "
                                   'face 'stgit-description-face)
                       (propertize (format "%o" new-perm)
                                   'face 'stgit-file-permission-face)))))))

(defstruct (stgit-file
            (:conc-name stgit-file->))
  old-perm new-perm copy-or-rename cr-score cr-from cr-to status file)

(defun stgit-escape-file-name-p (name)
  "Return non-nil if NAME must be escaped."
  (save-match-data (string-match "[\t\n\"\\]" name)))

(defun stgit-escape-file-name (name &optional no-quotes)
  "Escape NAME if necessary.

If NO-QUOTES is non-nil, do not enclose the result in double quotes."
  (if (stgit-escape-file-name-p name)
    (concat (if no-quotes "" "\"")
	    (mapconcat (lambda (c)
			 (case c
			   (?\t "\\t")
			   (?\n "\\n")
			   (?\" "\\\"")
			   (?\\ "\\\\")
			   (t (char-to-string c))))
		       name "")
	    (if no-quotes "" "\""))
    name))

(defun stgit-describe-copy-or-rename (file)
  (let* ((arrow    (concat " "
			   (propertize "->" 'face 'stgit-description-face)
			   " "))
	 (esc-from (stgit-file->cr-from file))
	 (esc-to   (stgit-file->cr-to   file))
	 (quote    "")
	 from to common-head common-tail)

    (when (or (stgit-escape-file-name-p esc-from)
	      (stgit-escape-file-name-p esc-to))
      (setq esc-from (stgit-escape-file-name esc-from t)
	    esc-to   (stgit-escape-file-name esc-to t)
	    quote    "\""))

    (when stgit-abbreviate-copies-and-renames
      (setq from (split-string esc-from "/")
            to   (split-string esc-to   "/"))

      (while (and from to (cdr from) (cdr to)
                  (string-equal (car from) (car to)))
        (setq common-head (cons (car from) common-head)
              from        (cdr from)
              to          (cdr to)))
      (setq common-head (nreverse common-head)
            from        (nreverse from)
            to          (nreverse to))
      (while (and from to (cdr from) (cdr to)
                  (string-equal (car from) (car to)))
        (setq common-tail (cons (car from) common-tail)
              from        (cdr from)
              to          (cdr to)))
      (setq from (nreverse from)
            to   (nreverse to)))

    (if (or common-head common-tail)
        (concat quote
		(if common-head
                    (mapconcat #'identity common-head "/")
                  "")
                (if common-head "/" "")
                (propertize "{" 'face 'stgit-description-face)
                (mapconcat #'identity from "/")
                arrow
                (mapconcat #'identity to "/")
                (propertize "}" 'face 'stgit-description-face)
                (if common-tail "/" "")
                (if common-tail
                    (mapconcat #'identity common-tail "/")
                  "")
		quote)
      (concat quote esc-from arrow esc-to quote))))

(defun stgit-file-pp (file)
  (let ((start (point))
        (spec (format-spec-make
               ?s (stgit-file-status-code-as-string file)
               ?m (stgit-file-mode-change-string
                   (stgit-file->old-perm file)
                   (stgit-file->new-perm file))
               ?n (if (stgit-file->copy-or-rename file)
                      (stgit-describe-copy-or-rename file)
                    (stgit-escape-file-name (stgit-file->file file)))
               ?c (propertize (stgit-file-type-change-string
                               (stgit-file->old-perm file)
                               (stgit-file->new-perm file))
                              'face 'stgit-description-face))))
    (stgit-insert-without-trailing-whitespace
     (format-spec stgit-file-line-format spec))
    (add-text-properties start (point)
                         (list 'entry-type 'file
                               'file-data file))))

(defun stgit-find-copies-harder-diff-arg ()
  "Return the flag to use with `git-diff' depending on the
`stgit-find-copies-harder' flag."
  (if stgit-find-copies-harder "--find-copies-harder" "-C"))

(defun stgit-insert-ls-files (args file-flag)
  (let ((start (point)))
    (apply 'stgit-run-git
           (append '("ls-files" "--exclude-standard" "-z") args))
    (goto-char start)
    (while (looking-at "\\([^\0]*\\)\0")
      (let ((name-len (- (match-end 0) (match-beginning 0))))
        (insert ":0 0 0000000000000000000000000000000000000000 0000000000000000000000000000000000000000 " file-flag "\0")
        (forward-char name-len)))))

(defun stgit-process-files (callback)
  (goto-char (point-min))
  (when (looking-at "[0-9A-Fa-f]\\{40\\}\0")
    (goto-char (match-end 0)))
  (while (looking-at ":\\([0-7]+\\) \\([0-7]+\\) [0-9A-Fa-f]\\{40\\} [0-9A-Fa-f]\\{40\\} ")
    (let ((old-perm (string-to-number (match-string 1) 8))
          (new-perm (string-to-number (match-string 2) 8)))
      (goto-char (match-end 0))
      (let ((file
             (cond ((looking-at
                     "\\([CR]\\)\\([0-9]*\\)\0\\([^\0]*\\)\0\\([^\0]*\\)\0")
                    (let* ((patch-status (stgit-patch->status patch))
                           (file-subexp  (if (eq patch-status 'unapplied)
                                             3
                                           4))
                           (file         (match-string file-subexp)))
                      (make-stgit-file
                       :old-perm       old-perm
                       :new-perm       new-perm
                       :copy-or-rename t
                       :cr-score       (string-to-number (match-string 2))
                       :cr-from        (match-string 3)
                       :cr-to          (match-string 4)
                       :status         (stgit-file-status-code
                                        (match-string 1))
                       :file           file)))
                   ((looking-at "\\([ABD-QS-Z]\\)\0\\([^\0]*\\)\0")
                    (make-stgit-file
                     :old-perm       old-perm
                     :new-perm       new-perm
                     :copy-or-rename nil
                     :cr-score       nil
                     :cr-from        nil
                     :cr-to          nil
                     :status         (stgit-file-status-code
                                      (match-string 1))
                     :file           (match-string 2))))))
        (goto-char (match-end 0))
        (funcall callback file)))))


(defun stgit-insert-patch-files (patch)
  "Expand (show modification of) the patch PATCH after the line
at point."
  (let* ((patchsym     (stgit-patch->name patch))
         (end          (point-marker))
         (args         (list "-z" (stgit-find-copies-harder-diff-arg)))
         (ewoc         (ewoc-create #'stgit-file-pp nil nil t))
         (show-ignored stgit-show-ignored)
         (show-unknown stgit-show-unknown))
    (set-marker-insertion-type end t)
    (setf (stgit-patch->files-ewoc patch) ewoc)
    (with-temp-buffer
      (let ((standard-output (current-buffer)))
        (apply 'stgit-run-git
               (cond ((eq patchsym :work)
                      (let (standard-output)
                        (stgit-run-git "update-index" "--refresh"))
                      `("diff-files" "-0" ,@args))
                     ((eq patchsym :index)
                      `("diff-index" ,@args "--cached" "HEAD"))
                     (t
                      `("diff-tree" ,@args "-r" ,(stgit-id patchsym)))))

        (when (and (eq patchsym :work))
          (when show-ignored
            (stgit-insert-ls-files '("--ignored" "--others") "I"))
          (when show-unknown
            (stgit-insert-ls-files '("--directory" "--no-empty-directory"
                                     "--others")
                                   "X"))
          (sort-regexp-fields nil ":[^\0]*\0\\([^\0]*\\)\0" "\\1"
                              (point-min) (point-max)))

        (stgit-process-files (lambda (file) (ewoc-enter-last ewoc file)))

        (unless (ewoc-nth ewoc 0)
          (ewoc-set-hf ewoc ""
                       (concat "    "
                               (propertize "<no files>"
                                           'face 'stgit-description-face)
                               "\n")))))
    (goto-char end)))

(defun stgit-find-file-revision (file patchsym &optional other-window)
  (let ((filename (expand-file-name (concat (file-name-nondirectory file)
                                            ".~" (symbol-name patchsym) "~")
                                    (file-name-directory file))))
    (let ((coding-system-for-read 'no-conversion)
          (coding-system-for-write 'no-conversion))
      (with-temp-file filename
        (unless (zerop (stgit-run-git-silent "cat-file"
                                             "blob"
                                             (concat (if (eq patchsym :index)
                                                         ""
                                                       (stgit-id patchsym))
                                                     ":" file)))
          (error "git cat-file failed"))))
    (funcall (if other-window
                 'switch-to-buffer-other-window
               'switch-to-buffer)
             (find-file-noselect filename))
    (set (make-local-variable 'vc-parent-buffer) filename)))

(defun stgit-find-file (&optional other-window this-rev)
  (let* ((file (or (stgit-patched-file-at-point)
                   (error "No file at point")))
         (filename (expand-file-name (stgit-file->file file)))
         (patchsym (stgit-patch-name-at-point)))

    (if (and this-rev (not (eq patchsym :work)))
        (stgit-find-file-revision (stgit-file->file file)
                                  (stgit-patch-name-at-point)
                                  other-window)
      (unless (file-exists-p filename)
        (error "File does not exist"))
      (funcall (if other-window 'find-file-other-window 'find-file)
               filename)
      (when (eq (stgit-file->status file) 'unmerged)
        (smerge-mode 1)))))

(defun stgit-expand (&optional patches collapse)
  "Show the contents of marked patches, or the patch at point.

See also `stgit-collapse'.

Non-interactively, operate on PATCHES, and collapse instead of
expand if COLLAPSE is not nil."
  (interactive (list (stgit-patches-marked-or-at-point t)))
  (stgit-assert-mode)
  (let ((patches-diff (funcall (if collapse #'intersection #'set-difference)
                               patches stgit-expanded-patches)))
    (setq stgit-expanded-patches
          (if collapse
              (set-difference stgit-expanded-patches patches-diff)
            (append stgit-expanded-patches patches-diff)))
    (stgit-show-task-message (concat (if collapse "Collapsing" "Expanding")
                                     " "
                                     (if (= 1 (length patches-diff))
                                         "patch"
                                       "patches"))
      (ewoc-map #'(lambda (patch)
                    (memq (stgit-patch->name patch) patches-diff))
                stgit-ewoc)))
  (move-to-column (stgit-goal-column)))

(defun stgit-collapse (&optional patches)
  "Hide the contents of marked patches, or the patch at point.

See also `stgit-expand'."
  (interactive (list (stgit-patches-marked-or-at-point t)))
  (stgit-assert-mode)
  (stgit-expand patches t))

(defun stgit-select-patch ()
  (let ((patchname (stgit-patch-name-at-point)))
    (stgit-expand (list patchname)
                  (memq patchname stgit-expanded-patches))))

(defun stgit-expand-directory (file)
  (let* ((patch (stgit-patch-at-point))
         (ewoc (stgit-patch->files-ewoc patch))
         (node (ewoc-locate ewoc))
         (filename (stgit-file->file file))
         (start (make-marker))
         (end (make-marker)))

    (save-excursion
      (forward-line 1)
      (set-marker start (point))
      (set-marker end (point))
      (set-marker-insertion-type end t))

    (assert (string-match "/$" filename))
    ;; remove trailing "/"
    (setf (stgit-file->file file) (substring filename 0 -1))
    (ewoc-invalidate ewoc node)

    (with-temp-buffer
      (let ((standard-output (current-buffer)))
        (stgit-insert-ls-files (list "--directory" "--others"
                                     "--no-empty-directory" "--"
                                     filename)
                               "X")
        (stgit-process-files (lambda (f)
                               (setq node (ewoc-enter-after ewoc node f))))))

    (move-to-column (stgit-goal-column))

    (let ((inhibit-read-only t))
      (put-text-property start end 'patch-data patch))))

(defun stgit-select-file ()
  (let* ((file (or (stgit-patched-file-at-point)
                   (error "No file at point")))
         (filename (stgit-file->file file)))
    (if (string-match "/$" filename)
        (stgit-expand-directory file)
      (stgit-find-file))))

(defun stgit-select ()
  "With point on a patch, toggle showing files in the patch.

With point on a file, open the associated file. Opens the target
file for (applied) copies and renames."
  (interactive)
  (stgit-assert-mode)
  (case (get-text-property (point) 'entry-type)
    ('patch
     (stgit-select-patch))
    ('file
     (stgit-select-file))
    (t
     (error "No patch or file on line"))))

(defun stgit-find-file-other-window (&optional this-rev)
  "Open file at point in other window.

With prefix argument, open a buffer with that revision of the file."
  (interactive "p")
  (stgit-assert-mode)
  (stgit-find-file t (> this-rev 1)))

(defun stgit-find-file-merge ()
  "Open file at point and merge it using `smerge-ediff'."
  (interactive)
  (stgit-assert-mode)
  (stgit-find-file t)
  (let ((filename (file-name-nondirectory buffer-file-name)))
    (smerge-ediff (concat "*" filename " GIT*")
		  (concat "*" filename " PATCH*"))))

(defun stgit-quit ()
  "Hide the stgit buffer."
  (interactive)
  (stgit-assert-mode)
  (bury-buffer))

(defun stgit-git-status ()
  "Show status using `git-status'."
  (interactive)
  (stgit-assert-mode)
  (unless (fboundp 'git-status)
    (error "The stgit-git-status command requires git-status"))
  (let ((dir default-directory))
    (save-selected-window
      (pop-to-buffer nil)
      (git-status dir))))

(defun stgit-goal-column ()
  "Return goal column for the current line"
  (case (get-text-property (point) 'entry-type)
    ('patch 2)
    ('file 4)
    (t 0)))

(defun stgit-next-line (&optional arg)
  "Move cursor vertically down ARG lines"
  (interactive "p")
  (stgit-assert-mode)
  (next-line arg)
  (move-to-column (stgit-goal-column)))

(defun stgit-previous-line (&optional arg)
  "Move cursor vertically up ARG lines"
  (interactive "p")
  (stgit-assert-mode)
  (previous-line arg)
  (move-to-column (stgit-goal-column)))

(defun stgit-next-patch (&optional arg)
  "Move cursor down ARG patches."
  (interactive "p")
  (stgit-assert-mode)
  (unless arg (setq arg 1))
  (cond ((< arg 0)
         (stgit-previous-patch (- arg)))
        ((zerop arg)
         (move-to-column (stgit-goal-column)))
        (t
         (when (stgit-at-header-p)
           (ewoc-goto-node stgit-ewoc (ewoc-nth stgit-ewoc 0))
           (setq arg (1- arg)))
         (ewoc-goto-next stgit-ewoc arg)
         (move-to-column goal-column))))

(defun stgit-previous-patch (&optional arg)
  "Move cursor up ARG patches."
  (interactive "p")
  (stgit-assert-mode)
  (unless arg (setq arg 1))
  (cond ((< arg 0)
         (stgit-next-patch (- arg)))
        ((zerop arg)
         (move-to-column (stgit-goal-column)))
        ((stgit-at-header-p)
         (goto-char (point-min)))
        (t
         (let ((opatch (stgit-patch-at-point)))
	   (when (stgit-patched-file-at-point)
	     (setq arg (1- arg)))
           (ewoc-goto-prev stgit-ewoc arg)
           (unless (zerop arg)
             (when (eq opatch (stgit-patch-at-point))
               (goto-char (point-min)))))
         (move-to-column (stgit-goal-column)))))

(defun stgit-previous-patch-group (&optional arg)
  "Move to the previous group of patches.

If ARG is non-nil, do this ARG times. If ARG is negative, move
-ARG groups forward instead; cf. `stgit-next-patch-group'."
  (interactive "p")
  (stgit-assert-mode)
  (if (< arg 0)
      (stgit-previous-patch-group (- arg))
    (while (and (not (bobp))
                (> arg 0))
      (stgit-previous-patch 1)
      (let* ((opoint (point))
             (patch  (stgit-patch-at-point))
             (status (and patch (stgit-patch->status patch))))
        (while (and (not (bobp))
                    (let* ((npatch (stgit-patch-at-point))
                           (nstatus (and npatch (stgit-patch->status npatch))))
                      (eq status nstatus)))
          (setq opoint (point))
          (stgit-previous-patch 1))
        (goto-char opoint))
      (setq arg (1- arg)))))

(defun stgit-next-patch-group (&optional arg)
  "Move to the next group of patches.

If ARG is non-nil, do this ARG times. If ARG is negative, move
-ARG groups backwards instead; cf. `stgit-previous-patch-group'."
  (interactive "p")
  (stgit-assert-mode)
  (if (< arg 0)
      (stgit-previous-patch-group (- arg))
    (while (and (not (eobp))
                (> arg 0))
      (let* ((patch  (stgit-patch-at-point))
             (status (and patch (stgit-patch->status patch))))
        (while (and (not (eobp))
                    (let* ((npatch (stgit-patch-at-point))
                           (nstatus (and npatch (stgit-patch->status npatch))))
                      (eq status nstatus)))
          (stgit-next-patch 1)))
      (setq arg (1- arg)))))

(defvar stgit-mode-hook nil
  "Run after `stgit-mode' is setup.")

(defvar stgit-mode-map nil
  "Keymap for StGit major mode.")

(unless stgit-mode-map
  (let ((diff-map   (make-sparse-keymap))
        (toggle-map (make-sparse-keymap)))
    (mapc (lambda (arg) (define-key diff-map (car arg) (cdr arg)))
          '(("b" .        stgit-diff-base)
            ("c" .        stgit-diff-combined)
            ("m" .        stgit-find-file-merge)
            ("o" .        stgit-diff-ours)
            ("r" .        stgit-diff-range)
            ("t" .        stgit-diff-theirs)))
    (mapc (lambda (arg) (define-key toggle-map (car arg) (cdr arg)))
          '(("n" .        stgit-toggle-patch-names)
            ("t" .        stgit-toggle-worktree)
            ("h" .        stgit-toggle-committed)
            ("i" .        stgit-toggle-ignored)
            ("u" .        stgit-toggle-unknown)
            ("s" .        stgit-toggle-svn)))
    (setq stgit-mode-map (make-keymap))
    (suppress-keymap stgit-mode-map)
    (mapc (lambda (arg) (define-key stgit-mode-map (car arg) (cdr arg)))
          `((" " .        stgit-mark-down)
            ("m" .        stgit-mark-down)
            ("\d" .       stgit-unmark-up)
            ("u" .        stgit-unmark-down)
            ("?" .        stgit-help)
            ("h" .        stgit-help)
            ("\C-p" .     stgit-previous-line)
            ("\C-n" .     stgit-next-line)
            ([up] .       stgit-previous-line)
            ([down] .     stgit-next-line)
            ("p" .        stgit-previous-patch)
            ("n" .        stgit-next-patch)
            ("\M-{" .     stgit-previous-patch-group)
            ("\M-}" .     stgit-next-patch-group)
            ([(control up)] .   stgit-previous-patch-group)
            ([(control down)] . stgit-next-patch-group)
            ("s" .        stgit-git-status)
            ("g" .        stgit-reload-or-repair)
            ("r" .        stgit-refresh)
            ("\C-c\C-r" . stgit-rename)
            ("e" .        stgit-edit)
            ("M" .        stgit-move-patches)
            ("S" .        stgit-squash)
            ("N" .        stgit-new)
            ("c" .        stgit-new-and-refresh)
            ("\C-c\C-c" . stgit-commit)
            ("\C-c\C-u" . stgit-uncommit)
            ("U" .        stgit-revert)
            ("R" .        stgit-resolve-file)
            ("\r" .       stgit-select)
            ("+" .        stgit-expand)
            ("-" .        stgit-collapse)
            ("o" .        stgit-find-file-other-window)
            ("\C-o" .     stgit-new-here)
            ([insertline] . stgit-new-here)
            ("i" .        stgit-toggle-index)
            (">" .        stgit-push-next)
            ("<" .        stgit-pop-next)
            ("P" .        stgit-push-or-pop)
            ("G" .        stgit-goto)
            ("=" .        stgit-diff)
            ("D" .        stgit-delete)
            ([?\C-/] .    stgit-undo)
            ("\C-_" .     stgit-undo)
            ([?\C-c ?\C-/] . stgit-redo)
            ("\C-c\C-_" . stgit-redo)
            ("B" .        stgit-branch)
            ("\C-c\C-b" . stgit-rebase)
            ("t" .        ,toggle-map)
            ("d" .        ,diff-map)
            ("q" .        stgit-quit)
            ("!" .        stgit-execute))))

  (let ((at-unmerged-file '(let ((file (stgit-patched-file-at-point)))
                             (and file (eq (stgit-file->status file)
                                           'unmerged))))
        (patch-collapsed-p '(lambda (p) (not (memq p stgit-expanded-patches)))))
    (easy-menu-define stgit-menu stgit-mode-map
      "StGit Menu"
      `("StGit"
        ["Reload" stgit-reload-or-repair
         :help "Reload StGit status from disk"]
        ["Repair" stgit-repair
         :keys "\\[universal-argument] \\[stgit-reload-or-repair]"
         :help "Repair StGit metadata"]
        "-"
        ["Undo" stgit-undo t]
        ["Redo" stgit-redo t]
        "-"
        ["Git status" stgit-git-status :active (fboundp 'git-status)]
        "-"
        ["New patch" stgit-new-and-refresh
         :help "Create a new patch from changes in index or work tree"
         :active (not (and (stgit-index-empty-p) (stgit-work-tree-empty-p)))]
        ["New empty patch" stgit-new
         :help "Create a new, empty patch"]
        ["(Un)mark patch" stgit-toggle-mark
         :label (if (memq (stgit-patch-name-at-point nil t)
                          stgit-marked-patches)
                    "Unmark patch" "Mark patch")
         :active (stgit-patch-name-at-point nil t)]
        ["Expand/collapse patch"
         (let ((patches (stgit-patches-marked-or-at-point)))
           (if (member-if ,patch-collapsed-p patches)
               (stgit-expand patches)
             (stgit-collapse patches)))
         :label (if (member-if ,patch-collapsed-p
                               (stgit-patches-marked-or-at-point))
                    "Expand patches"
                  "Collapse patches")
         :active (stgit-patches-marked-or-at-point)]
        ["Edit patch" stgit-edit
         :help "Edit patch comment"
         :active (stgit-patch-name-at-point nil t)]
        ["Rename patch" stgit-rename :active (stgit-patch-name-at-point nil t)]
        ["Push/pop patch" stgit-push-or-pop
         :label (if (subsetp (stgit-patches-marked-or-at-point nil t)
                             (stgit-applied-patchsyms t))
                    "Pop patches" "Push patches")]
        ["Delete patches" stgit-delete
         :active (stgit-patches-marked-or-at-point nil t)]
        "-"
        ["Move patches" stgit-move-patches
         :active stgit-marked-patches
         :help "Move marked patch(es) to point"]
        ["Squash patches" stgit-squash
         :active (> (length stgit-marked-patches) 1)
         :help "Merge marked patches into one"]
        "-"
        ["Refresh top patch" stgit-refresh
         :active (not (and (stgit-index-empty-p) (stgit-work-tree-empty-p)))
         :help "Refresh the top patch with changes in index or work tree"]
        ["Refresh this patch" (stgit-refresh t)
         :keys "\\[universal-argument] \\[stgit-refresh]"
         :help "Refresh marked patch with changes in index or work tree"
         :active (and (not (and (stgit-index-empty-p)
                                (stgit-work-tree-empty-p)))
                      (stgit-patch-name-at-point nil t))]
        "-"
        ["Find file" stgit-select
         :active (eq (get-text-property (point) 'entry-type) 'file)]
        ["Open file" stgit-find-file-other-window
         :active (eq (get-text-property (point) 'entry-type) 'file)]
        ["Toggle file index" stgit-toggle-index
         :active (and (eq (get-text-property (point) 'entry-type) 'file)
                      (memq (stgit-patch-name-at-point) '(:work :index)))
         :label (if (eq (stgit-patch-name-at-point) :work)
                    "Move change to index"
                  "Move change to work tree")]
        "-"
        ["Show diff" stgit-diff
         :active (get-text-property (point) 'entry-type)]
        ["Show diff for range of applied patches" stgit-diff-range
         :active (= (length stgit-marked-patches) 1)]
        ("Merge"
         :active (stgit-git-index-unmerged-p)
         ["Combined diff" stgit-diff-combined
          :active (memq (stgit-patch-name-at-point nil nil) '(:work :index))]
         ["Diff against base" stgit-diff-base
          :help "Show diff against the common base"
          :active (memq (stgit-patch-name-at-point nil nil) '(:work :index))]
         ["Diff against ours" stgit-diff-ours
          :help "Show diff against our branch"
          :active (memq (stgit-patch-name-at-point nil nil) '(:work :index))]
         ["Diff against theirs" stgit-diff-theirs
          :help "Show diff against their branch"
          :active (memq (stgit-patch-name-at-point nil nil) '(:work :index))]
         "-"
         ["Interactive merge" stgit-find-file-merge
          :help "Interactively merge the file"
          :active ,at-unmerged-file]
         ["Resolve file" stgit-resolve-file
          :help "Mark file conflict as resolved"
          :active ,at-unmerged-file]
         )
        "-"
        ["Show index & work tree" stgit-toggle-worktree :style toggle
         :selected stgit-show-worktree]
        ["Show unknown files" stgit-toggle-unknown :style toggle
         :selected stgit-show-unknown :active stgit-show-worktree]
        ["Show ignored files" stgit-toggle-ignored :style toggle
         :selected stgit-show-ignored :active stgit-show-worktree]
        ["Show patch names" stgit-toggle-patch-names :style toggle
         :selected stgit-show-patch-names]
        ["Show recent commits" stgit-toggle-committed :style toggle
         :selected stgit-show-committed]
        ["Show subversion info" stgit-toggle-svn :style toggle
         :selected stgit-show-svn]
        "-"
        ["Switch branches" stgit-branch t
         :help "Switch to or create another branch"]
        ["Rebase branch" stgit-rebase t
         :help "Rebase the current branch"]
        "-"
        ["Customize StGit" (customize-group 'stgit)]
        ))))

;; disable tool bar editing buttons
(put 'stgit-mode 'mode-class 'special)

(defun stgit-mode ()
  "Major mode for interacting with StGit.

Start StGit using \\[stgit].

Basic commands:
\\<stgit-mode-map>\
\\[stgit-help]	Show this help text
\\[stgit-quit]	Hide the StGit buffer
\\[describe-bindings]	Show all key bindings

\\[stgit-reload-or-repair]	Reload the StGit buffer
\\[universal-argument] \\[stgit-reload-or-repair]	Repair StGit metadata

\\[stgit-undo]	Undo most recent StGit operation
\\[stgit-redo]	Undo recent undo

\\[stgit-git-status]	Run `git-status' (if available)

\\[stgit-execute]	Run an stg shell command

Movement commands:
\\[stgit-previous-line]	Move to previous line
\\[stgit-next-line]	Move to next line
\\[stgit-previous-patch]	Move to previous patch
\\[stgit-next-patch]	Move to next patch
\\[stgit-previous-patch-group]	Move to previous patch group
\\[stgit-next-patch-group]	Move to next patch group

\\[stgit-mark-down]	Mark patch and move down
\\[stgit-unmark-up]	Unmark patch and move up
\\[stgit-unmark-down]	Unmark patch and move down

Commands for patches:
\\[stgit-select]	Toggle showing changed files in patch
\\[stgit-refresh]	Refresh patch with changes in index or work tree
\\[stgit-diff]	Show the patch log and diff

\\[stgit-expand]	Show changes in marked patches
\\[stgit-collapse]	Hide changes in marked patches

\\[stgit-new-and-refresh]	Create a new patch from index or work tree
\\[stgit-new]	Create a new, empty patch
\\[stgit-new-here]	Create a new, empty patch before patch at point

\\[stgit-rename]	Rename patch
\\[stgit-edit]	Edit patch description
\\[stgit-delete]	Delete patch(es)

\\[stgit-revert]	Revert all changes in index or work tree
\\[stgit-toggle-index]	Toggle all changes between index and work tree

\\[stgit-push-next]	Push next patch onto stack
\\[stgit-pop-next]	Pop current patch from stack
\\[stgit-push-or-pop]	Push or pop marked patches
\\[stgit-goto]	Make patch at point current by popping or pushing

\\[stgit-squash]	Squash (meld together) patches
\\[stgit-move-patches]	Move marked patches to point

\\[stgit-commit]	Commit patch(es)
\\[stgit-uncommit]	Uncommit patch(es)

Commands for files:
\\[stgit-select]	Open the file in this window
\\[stgit-find-file-other-window]	Open the file in another window
\\[stgit-diff]	Show the file's diff

\\[stgit-toggle-index]	Toggle change between index and work tree
\\[stgit-revert]	Revert changes to file

Display commands:
\\[stgit-toggle-patch-names]	Toggle showing patch names
\\[stgit-toggle-worktree]	Toggle showing index and work tree
\\[stgit-toggle-unknown]	Toggle showing unknown files
\\[stgit-toggle-ignored]	Toggle showing ignored files
\\[stgit-toggle-committed]	Toggle showing recent commits
\\[stgit-toggle-svn]	Toggle showing subversion information

Commands for diffs:
\\[stgit-diff]	Show diff of patch or file
\\[stgit-diff-range]	Show diff for range of patches
\\[stgit-diff-base]	Show diff against the merge base
\\[stgit-diff-ours]	Show diff against our branch
\\[stgit-diff-theirs]	Show diff against their branch

  With one prefix argument (e.g., \\[universal-argument] \\[stgit-diff]), \
ignore space changes.
  With two prefix arguments (e.g., \\[universal-argument] \
\\[universal-argument] \\[stgit-diff]), ignore all space changes.

Commands for merge conflicts:
\\[stgit-find-file-merge]	Resolve conflicts using `smerge-ediff'
\\[stgit-resolve-file]	Mark unmerged file as resolved

Commands for branches:
\\[stgit-branch]	Switch to or create another branch
\\[stgit-rebase]	Rebase the current branch

Customization variables:
`stgit-abbreviate-copies-and-renames'
`stgit-default-show-ignored'
`stgit-default-show-patch-names'
`stgit-default-show-unknown'
`stgit-default-show-worktree'
`stgit-default-show-committed'
`stgit-default-show-svn'
`stgit-default-committed-count'
`stgit-find-copies-harder'
`stgit-show-worktree-mode'

See also \\[customize-group] for the \"stgit\" group."
  (kill-all-local-variables)
  (buffer-disable-undo)
  (setq mode-name "StGit"
        major-mode 'stgit-mode
        goal-column 2)
  (use-local-map stgit-mode-map)
  (mapc (lambda (x) (set (make-local-variable (car x)) (cdr x)))
        `((list-buffers-directory       . ,default-directory)
          (parse-sexp-lookup-properties . t)
          (stgit-expanded-patches       . (:work :index))
          (stgit-index-node             . nil)
          (stgit-worktree-node          . nil)
          (stgit-marked-patches         . nil)
          (stgit-svn-find-rev-hash      . ,(make-hash-table :test 'equal))
          (stgit-committed-count        . ,stgit-default-committed-count)
          (stgit-show-committed         . ,stgit-default-show-committed)
          (stgit-show-ignored           . ,stgit-default-show-ignored)
          (stgit-show-patch-names       . ,stgit-default-show-patch-names)
          (stgit-show-svn               . ,stgit-default-show-svn)
          (stgit-show-unknown           . ,stgit-default-show-unknown)
          (stgit-show-worktree          . ,stgit-default-show-worktree)))
  (set-variable 'truncate-lines 't)
  (add-hook 'after-save-hook 'stgit-update-stgit-for-buffer)
  (unless stgit-did-advise
    (stgit-advise)
    (setq stgit-did-advise t))
  (run-hooks 'stgit-mode-hook))

(defun stgit-advise-funlist (funlist)
  "Add advice to the functions in FUNLIST so we can refresh the
stgit buffers as the git status of files change."
  (mapc (lambda (sym)
          (when (fboundp sym)
            (eval `(defadvice ,sym (after stgit-update-stgit-for-buffer)
                     (stgit-update-stgit-for-buffer :index)))
            (ad-activate sym)))
        funlist))

(defun stgit-advise ()
  "Add advice to appropriate (non-stgit) git functions so we can
refresh the stgit buffers as the git status of files change."
  (mapc (lambda (arg)
          (let ((feature (car arg))
                (funlist (cdr arg)))
            (if (featurep feature)
                (stgit-advise-funlist funlist)
              (add-to-list 'after-load-alist
                           `(,feature (stgit-advise-funlist
                                       (quote ,funlist)))))))
        ;; lists of (<feature> <function> <function> ...) to be advised
        '((vc-git vc-git-rename-file vc-git-revert vc-git-register)
          (git    git-add-file git-checkout git-revert-file git-remove-file)
          (dired  dired-delete-file))))

(defvar stgit-pending-refresh-buffers nil
  "Alist of (`buffer' . `mode') of buffers that need to be
refreshed. See `stgit-post-refresh' for the different values of
`mode'.")

(defun stgit-run-pending-refreshs ()
  "Run all pending stgit buffer updates as posted by `stgit-post-refresh'."
  (let ((buffers stgit-pending-refresh-buffers)
        (stgit-inhibit-messages t))
    (setq stgit-pending-refresh-buffers nil)
    (while buffers
      (let* ((elem   (car buffers))
             (buffer (car elem))
             (mode   (cdr elem)))
        (when (buffer-name buffer)
          (with-current-buffer buffer
            (stgit-save-excursion
              (if (eq mode :reload)
                  (stgit-reload)
                (stgit-refresh-worktree)
                (when (eq mode :index)
                  (stgit-refresh-index)))))))
      (setq buffers (cdr buffers)))))

(defun stgit-post-refresh (buffer mode)
  "Update status in BUFFER when Emacs becomes idle.

MODE specifies what to do:
  :work    only update work tree
  :index   update work tree and index
  :reload  reload the entire buffer"
  (unless (memq mode '(:work :index :reload))
    (error "Illegal refresh mode in stgit-post-refresh"))
  (unless stgit-pending-refresh-buffers
    (run-with-idle-timer 0.1 nil 'stgit-run-pending-refreshs))
  (let ((elem (assq buffer stgit-pending-refresh-buffers)))
    (if elem
        ;; if buffer is already present, update its mode if necessary
        (let ((omode (cdr elem)))
          (when (case mode
                  (:index (eq mode :work))
                  (:reload t))
            (setcdr elem mode)))
      ;; new entry
      (setq stgit-pending-refresh-buffers
            (cons (cons buffer mode)
                  stgit-pending-refresh-buffers)))))

(defun stgit-update-stgit-for-buffer (&optional mode)
  "When Emacs becomes idle, update the status in any `stgit-mode'
buffer that shows the status of the current buffer.

MODE specifies how to update the buffer. See `stgit-post-refresh'
for the different values MODE can have."
  (let* ((dir (cond ((derived-mode-p 'stgit-mode 'stgit-status-mode 'dired-mode)
                     default-directory)
                    (buffer-file-name
                     (file-name-directory
                      (expand-file-name buffer-file-name)))))
         (gitdir (and dir (condition-case nil (git-get-top-dir dir)
                            (error nil))))
	 (buffer (and gitdir (stgit-find-buffer gitdir))))
    (when buffer
      (stgit-post-refresh buffer (or mode :work)))))

(defun stgit-add-mark (patchsym)
  "Mark the patch PATCHSYM."
  (setq stgit-marked-patches (cons patchsym stgit-marked-patches)))

(defun stgit-remove-mark (patchsym)
  "Unmark the patch PATCHSYM."
  (setq stgit-marked-patches (delq patchsym stgit-marked-patches)))

(defun stgit-clear-marks ()
  "Unmark all patches."
  (setq stgit-marked-patches '()))

(defun stgit-patch-at-point (&optional cause-error)
  (get-text-property (point) 'patch-data))

(defun stgit-patch-name-at-point (&optional cause-error types)
  "Return the patch name on the current line as a symbol.
If CAUSE-ERROR is not nil, signal an error if none found.

TYPES controls which types of commits and patches can be returned.
If it is t, only allow stgit patches; if 'allow-committed, also
allow historical commits; if nil, also allow work tree and index."
  (let ((patch (stgit-patch-at-point)))
    (and patch
         (memq (stgit-patch->status patch)
               (case types
                 ((nil) nil)
                 ((allow-committed) '(work index))
                 ((t) '(work index committed))
                 (t (error "Bad value"))))
         (setq patch nil))
    (cond (patch
           (stgit-patch->name patch))
          (cause-error
           (error "No patch on this line")))))

(defun stgit-patched-file-at-point ()
  (get-text-property (point) 'file-data))

(defun stgit-patches-marked-or-at-point (&optional cause-error types)
  "Return the symbols of the marked patches, or the patch on the current line.
If CAUSE-ERRROR is not nil, signal an error if none found.

TYPES controls which types of commits and patches can be returned.
If it is t, only allow stgit patches; if 'allow-committed, also
allow historical commits; if nil, also allow work tree and index."
  (if stgit-marked-patches
      stgit-marked-patches
    (let ((patch (stgit-patch-name-at-point nil types)))
      (cond (patch (list patch))
            (cause-error (error "No patches marked or at this line"))
            (t nil)))))

(defun stgit-goto-patch (patchsym &optional file)
  "Move point to the line containing patch PATCHSYM and return t.
If that patch cannot be found, do nothing and return nil.

If the patch was found and FILE is not nil, instead move to that
file's line and return t. If FILE cannot be found, stay on the
line of PATCHSYM and return :patch."
  (let ((node (ewoc-nth stgit-ewoc 0))
        result)
    (while (and node (not (eq (stgit-patch->name (ewoc-data node))
                              patchsym)))
      (setq node (ewoc-next stgit-ewoc node)))
    (cond ((and node file)
           (let* ((file-ewoc (stgit-patch->files-ewoc (ewoc-data node)))
                  (file-node (ewoc-nth file-ewoc 0)))
             (while (and file-node
                         (not (equal (stgit-file->file
                                      (ewoc-data file-node))
                                     file)))
               (setq file-node (ewoc-next file-ewoc file-node)))
             (if file-node
                 (progn
                   (ewoc-goto-node file-ewoc file-node)
                   (setq result t))
               (ewoc-goto-node stgit-ewoc node)
               (setq result :patch))))
          (node
           (ewoc-goto-node stgit-ewoc node)
           (setq result t)))
    (when result
      (move-to-column (stgit-goal-column)))
    result))

(defun stgit-init ()
  "Run stg init to initialize the current branch for use with StGit."
  (interactive)
  (stgit-assert-mode)
  (unless (zerop (stgit-capture-output nil
		   (stgit-run "init")))
    (error "stg init failed"))
  (stgit-reload))

(defun stgit-toggle-mark ()
  "Toggle mark on the patch under point."
  (interactive)
  (stgit-assert-mode)
  (if (memq (stgit-patch-name-at-point t t) stgit-marked-patches)
      (stgit-unmark)
    (stgit-mark)))

(defun stgit-mark ()
  "Mark the patch under point."
  (interactive)
  (stgit-assert-mode)
  (let* ((node (ewoc-locate stgit-ewoc))
         (patch (ewoc-data node)))
    (case (stgit-patch->status patch)
      (work      (error "Cannot mark the work tree"))
      (index     (error "Cannot mark the index"))
      (committed (error "Cannot mark a committed patch")))
    (stgit-add-mark (stgit-patch->name patch))
    (let ((column (current-column)))
      (ewoc-invalidate stgit-ewoc node)
      (move-to-column column))))

(defun stgit-mark-down ()
  "Mark the patch under point and move to the next patch."
  (interactive)
  (stgit-mark)
  (stgit-next-patch))

(defun stgit-unmark ()
  "Remove mark from the patch on the current line."
  (interactive)
  (stgit-assert-mode)
  (let* ((node (ewoc-locate stgit-ewoc))
         (patch (ewoc-data node)))
    (stgit-remove-mark (stgit-patch->name patch))
    (let ((column (current-column)))
      (ewoc-invalidate stgit-ewoc node)
      (move-to-column column))))

(defun stgit-unmark-up ()
  "Remove mark from the patch on the previous line."
  (interactive)
  (stgit-assert-mode)
  (stgit-previous-patch)
  (stgit-unmark))

(defun stgit-unmark-down ()
  "Remove mark from the patch on the current line."
  (interactive)
  (stgit-assert-mode)
  (stgit-unmark)
  (stgit-next-patch))

(defun stgit-rename (name)
  "Rename the patch under point to NAME."
  (interactive (list
                (read-string "Patch name: "
                             (symbol-name (stgit-patch-name-at-point t t)))))
  (stgit-assert-mode)
  (let ((old-patchsym (stgit-patch-name-at-point t t)))
    (unless (string-equal (symbol-name old-patchsym) name)
      (stgit-capture-output nil
	(stgit-run "rename" "--" old-patchsym name))
      (let ((name-sym (intern name)))
	(when (memq old-patchsym stgit-expanded-patches)
	  (setq stgit-expanded-patches
		(cons name-sym (delq old-patchsym stgit-expanded-patches))))
	(when (memq old-patchsym stgit-marked-patches)
	  (setq stgit-marked-patches
		(cons name-sym (delq old-patchsym stgit-marked-patches))))
	(stgit-reload)
	(stgit-goto-patch name-sym)))))

(defun stgit-reload-or-repair (repair)
  "Update the contents of the StGit buffer (`stgit-reload').

With a prefix argument, repair the StGit metadata if the branch
was modified with git commands (`stgit-repair')."
  (interactive "P")
  (stgit-assert-mode)
  (if repair
      (stgit-repair)
    (stgit-reload)))

(defun stgit-repair ()
  "Run stg repair."
  (interactive)
  (stgit-assert-mode)
  (stgit-capture-output nil
    (stgit-run "repair"))
  (stgit-reload))

(defun stgit-available-branches (&optional all skip-current)
  "Returns a list of the names of the available stg branches as strings.

If ALL is not nil, also return non-stgit branches.
If SKIP-CURRENT is not nil, do not include the current branch."
  (let ((output (with-output-to-string
                  (stgit-run "branch" "--list")))
        (pattern (format "^%c\\s-+%c\\s-+\\(\\S-+\\)"
			 (if skip-current ?\  ?.)
                         (if all ?. ?s)))
        (start 0)
        result)
    (while (string-match pattern output start)
      (setq result (cons (match-string 1 output) result))
      (setq start (match-end 0)))
    result))

(defun stgit-branch (branch)
  "Switch to or create branch BRANCH."
  (interactive (list (completing-read "Switch to branch: "
                                      (stgit-available-branches nil t))))
  (stgit-assert-mode)

  (when (equal branch (stgit-current-branch))
    (error "Branch is already current"))

  (let ((merge (not (and (stgit-index-empty-p) (stgit-work-tree-empty-p)))))

    (when (cond ((and merge
		      (not (yes-or-no-p
			    "Attempt to merge uncommitted changes? ")))
		 nil)

		((member branch (stgit-available-branches t))
		 (stgit-capture-output nil
		   (apply 'stgit-run
			  (append '("branch")
				  (and merge '("--merge"))
				  '("--")
				  (list branch))))
		 t)
		((not (string-match stgit-allowed-branch-name-re branch))
		 (error "Invalid branch name"))
		(merge
		 (error "Cannot merge changes into a new branch"))
		((yes-or-no-p (format "Create branch \"%s\"? " branch))
		 (let ((branch-point (completing-read
				      "Branch from (default current branch): "
				      (stgit-available-branches))))
		   (stgit-capture-output nil
		     (apply 'stgit-run
			    `("branch" "--create" "--"
			      ,branch
			      ,@(unless (zerop (length branch-point))
				  (list branch-point)))))
		   t)))

      ;; Do not expand any (normal) patches in the new branch
      (setq stgit-expanded-patches
	    (remove-if-not (lambda (p) (memq p '(:work :index)))
			   stgit-expanded-patches))

      (stgit-reload))))

(defun stgit-available-refs (&optional omit-stgit)
  "Returns a list of the available git refs.
If OMIT-STGIT is not nil, filter out \"resf/heads/*.stgit\"."
  (let* ((output (with-output-to-string
                   (stgit-run-git-silent "for-each-ref" "--format=%(refname)"
                                         "refs/tags" "refs/heads"
                                         "refs/remotes")))
         (result (split-string output "\n" t)))
    (mapcar (lambda (s)
              (if (string-match "^refs/\\(heads\\|tags\\|remotes\\)/" s)
                  (substring s (match-end 0))
                s))
            (if omit-stgit
                (delete-if (lambda (s)
                             (string-match "^refs/heads/.*\\.stgit$" s))
                           result)
              result))))

(defun stgit-parent-branch ()
  "Return the parent branch of the current stg branch as per
git-config setting branch.<branch>.stgit.parentbranch."
  (let ((output (with-output-to-string
                  (stgit-run-git-silent "config"
                                        (format "branch.%s.stgit.parentbranch"
                                                (stgit-current-branch))))))
    (when (string-match ".*" output)
      (match-string 0 output))))

(defun stgit-rebase (new-base)
  "Rebase the current branch to NEW-BASE.

Interactively, first ask which branch to rebase to. Defaults to
what git-config branch.<branch>.stgit.parentbranch is set to."
  (interactive (list (completing-read "Rebase to: "
                                      (stgit-available-refs t)
                                      nil nil
                                      (stgit-parent-branch))))
  (stgit-assert-mode)
  (stgit-capture-output nil (stgit-run "rebase" "--" new-base))
  (stgit-reload))

(defun stgit-commit (count)
  "Run stg commit on (at most) COUNT commits.
Interactively, the prefix argument is used as COUNT.
A negative COUNT will uncommit using `stgit-uncommit' instead."
  (interactive "p")
  (stgit-assert-mode)
  (if (< count 0)
      (stgit-uncommit (- count))
    (setq count (min count (length (stgit-applied-patches t))))
    (stgit-capture-output nil (stgit-run "commit" "-n" count))
    (stgit-reload)))

(defun stgit-uncommit (count)
  "Run stg uncommit on COUNT commits.
Interactively, the prefix argument is used as COUNT.
A negative COUNT will commit using `stgit-commit' instead."
  (interactive "p")
  (stgit-assert-mode)
  (if (< count 0)
      (stgit-commit (- count))
    (stgit-capture-output nil (stgit-run "uncommit" "-n" count))
    (stgit-reload)))

(defun stgit-neighbour-file ()
  "Return the file name of the next file after point, or the
previous file if point is at the last file within a patch."
  (let ((old-point (point))
        neighbour-file)
    (and (zerop (forward-line 1))
         (let ((f (stgit-patched-file-at-point)))
           (and f (setq neighbour-file (stgit-file->file f)))))
    (goto-char old-point)
    (unless neighbour-file
      (and (zerop (forward-line -1))
           (let ((f (stgit-patched-file-at-point)))
             (and f (setq neighbour-file (stgit-file->file f)))))
      (goto-char old-point))
    neighbour-file))

(defun stgit-unmerged-file-stages (file)
  "Returns list of the merge stages that contain FILE, which
must be an unmerged file.

Stage 1, the common ancestor, is 'ancestor.
Stage 2, HEAD, is 'head.
Stage 3, MERGE_HEAD, is 'merge-head."
  (let ((output (with-output-to-string
		  (stgit-run-git-silent "ls-files" "-u" "-z" "--"
					(stgit-file->file file))))
	stages
	start)
    (while (string-match "\\([0-7]*\\) \\([0-9A-Fa-f]\\{40\\}\\) \\([1-3]\\)\t\\([^\0]*\\)\0"
			 output start)
      (setq stages (cons (elt [ancestor head merge-head]
			      (1- (string-to-number (match-string 3 output))))
			 stages)
	    start (match-end 0)))
    stages))

(defun stgit-revert-file ()
  "Revert the file at point, which must be in the index or the
working tree."
  (interactive)
  (stgit-assert-mode)
  (let* ((patched-file (or (stgit-patched-file-at-point)
                           (error "No file on the current line")))
         (patch-name   (stgit-patch-name-at-point))
         (file-status  (stgit-file->status patched-file))
         (rm-file      (cond ((stgit-file->copy-or-rename patched-file)
                              (stgit-file->cr-to patched-file))
                             ((eq file-status 'add)
                              (stgit-file->file patched-file))))
         (co-file      (cond ((eq file-status 'rename)
                              (stgit-file->cr-from patched-file))
                             ((not (memq file-status '(copy add unknown)))
                              (stgit-file->file patched-file))))
         (next-file    (stgit-neighbour-file))
	 (rm-disk-file (when (memq file-status '(ignore unknown))
			 (stgit-file->file patched-file)))
	 add-file)
    (unless (memq patch-name '(:work :index))
      (error "No index or working tree file on this line"))

    (when (eq file-status 'unmerged)
      (let ((stages (stgit-unmerged-file-stages patched-file)))
	(if (memq 'head stages)
	    (setq add-file (stgit-file->file patched-file))
	  (setq rm-file (stgit-file->file patched-file)
		co-file nil))))

    (when (yes-or-no-p (cond (rm-disk-file
			      (format "Delete %s? " rm-disk-file))
			     ((and rm-file co-file)
			      "Revert 2 files? ")
			     (t
			      (format "Revert %s? " (or rm-file co-file)))))
      (when rm-disk-file
	(dired-delete-file rm-disk-file dired-recursive-deletes))

      (stgit-capture-output nil
	(when rm-file
	  (stgit-run-git "rm" "-f" "-q" "--" rm-file))
	(when add-file
	  (stgit-run-git "add" "--" add-file))
	(when co-file
	  (let ((rev (when (or (eq file-status 'unmerged)
			       (eq patch-name :index))
		       '("HEAD"))))
	    (apply #'stgit-run-git
		   "checkout"
		   `(,@rev "--" ,co-file)))))
      (stgit-reload)
      (stgit-goto-patch patch-name next-file))))

(defun stgit-revert ()
  "Revert the change at point, which must be the index, the work
tree, or a single change in either."
  (interactive)
  (stgit-assert-mode)
  (let ((patched-file (stgit-patched-file-at-point)))
    (if patched-file
        (stgit-revert-file)
      (let* ((patch-name (or (stgit-patch-name-at-point)
                             (error "No patch or file at point")))
             (patch-desc (case patch-name
                           (:index "index")
                           (:work  "work tree")
                           (t (error (substitute-command-keys
                                      "Use \\[stgit-delete] to delete a patch"))))))
        (when (if (eq patch-name :work)
                  (stgit-work-tree-empty-p)
                (stgit-index-empty-p))
          (error (format "There are no changes in the %s to revert"
                         patch-desc)))
        (and (eq patch-name :index)
             (not (stgit-work-tree-empty-p))
             (error "Cannot revert index as work tree contains unstaged changes"))

        (when (yes-or-no-p (format "Revert all changes in the %s? "
                                   patch-desc))
          (if (eq patch-name :index)
              (stgit-run-git-silent "reset" "--hard" "-q")
            (stgit-run-git-silent "checkout" "--" "."))
          (stgit-save-excursion
            (stgit-refresh-index)
            (stgit-refresh-worktree)))))))

(defun stgit-resolve-file ()
  "Resolve conflict in the file at point."
  (interactive)
  (stgit-assert-mode)
  (let* ((patched-file (stgit-patched-file-at-point))
         (patch        (stgit-patch-at-point))
         (patch-name   (and patch (stgit-patch->name patch)))
         (status       (and patched-file (stgit-file->status patched-file))))

    (unless (memq patch-name '(:work :index))
      (error "No index or working tree file on this line"))

    (unless (eq status 'unmerged)
      (error "No conflict to resolve at the current line"))

    (stgit-capture-output nil
      (stgit-move-change-to-index (stgit-file->file patched-file)))

    (stgit-reload)))

(defun stgit-push-or-pop-patches (do-push npatches)
  "Push (if DO-PUSH is not nil) or pop (if DO-PUSH is nil)
NPATCHES patches, or all patches if NPATCHES is t."
  (stgit-assert-mode)
  (stgit-capture-output nil
    (apply 'stgit-run
           (if do-push "push" "pop")
           (if (eq npatches t)
               '("--all")
             (list "-n" npatches))))
  (stgit-reload)
  (stgit-refresh-git-status))

(defun stgit-push-next (npatches)
  "Push the first unapplied patch.
With numeric prefix argument, push that many patches."
  (interactive "p")
  (stgit-push-or-pop-patches t npatches))

(defun stgit-pop-next (npatches)
  "Pop the topmost applied patch.
With numeric prefix argument, pop that many patches.

If NPATCHES is t, pop all patches."
  (interactive "p")
  (stgit-push-or-pop-patches nil npatches))

(defun stgit-applied-patches (&optional only-patches)
  "Return a list of the applied patches.

If ONLY-PATCHES is not nil, exclude index and work tree."
  (let ((states (if only-patches
                    '(applied top)
                  '(applied top index work)))
        result)
    (ewoc-map (lambda (patch)
                (when (memq (stgit-patch->status patch) states)
                  (setq result (cons patch result)))
                nil)
              stgit-ewoc)
    result))

(defun stgit-applied-patchsyms (&optional only-patches)
  "Return a list of the symbols of the applied patches.

If ONLY-PATCHES is not nil, exclude index and work tree."
  (mapcar #'stgit-patch->name (stgit-applied-patches only-patches)))

(defun stgit-push-or-pop ()
  "Push or pop the marked patches."
  (interactive)
  (stgit-assert-mode)
  (let* ((patchsyms (stgit-patches-marked-or-at-point t t))
         (applied-syms (stgit-applied-patchsyms t))
         (unapplied (set-difference patchsyms applied-syms)))
    (stgit-capture-output nil
      (apply 'stgit-run
             (if unapplied "push" "pop")
             "--"
             (stgit-sort-patches (if unapplied unapplied patchsyms)))))
  (stgit-reload))

(defun stgit-at-header-p ()
  "Return non-nil if point is in the header area above all patches."
  (not (previous-single-property-change (point) 'patch-data)))

(defun stgit-at-footer-p ()
  "Return non-nil if point is in the footer area below all patches."
  (not (next-single-property-change (point) 'patch-data)))

(defun stgit-goto-target ()
  "Return the goto target at point: a patchsym, :top,
or :bottom."
  (let ((patch (stgit-patch-at-point)))
    (cond (patch
           (case (stgit-patch->status patch)
             ((work index) nil)
             ((committed) :bottom)
             (t (stgit-patch->name patch))))
          ((stgit-at-footer-p)
           :top)
          ((stgit-at-header-p)
           :bottom))))

(defun stgit-goto ()
  "Go to the patch on the current line.

Push or pop patches to make this patch topmost. Push or pop all
patches if used on a line after or before all patches."
  (interactive)
  (stgit-assert-mode)
  (let ((patchsym (stgit-goto-target)))
    (unless patchsym
      (error "No patch to go to on this line"))
    (case patchsym
      (:top    (stgit-push-or-pop-patches t t))
      (:bottom (stgit-push-or-pop-patches nil t))
      (t (stgit-capture-output nil
           (stgit-run "goto" "--" patchsym))
         (stgit-reload)))))

(defun stgit-id (patchsym)
  "Return the git commit id for PATCHSYM.
If PATCHSYM is a keyword, returns PATCHSYM unmodified."
  (if (keywordp patchsym)
      patchsym
    (let ((result (with-output-to-string
		    (stgit-run-silent "id" "--" patchsym))))
      (unless (string-match "^\\([0-9A-Fa-f]\\{40\\}\\)$" result)
	(error "Cannot find commit id for %s" patchsym))
      (match-string 1 result))))

(defun stgit-whitespace-diff-arg (arg)
  (when (numberp arg)
    (cond ((> arg 4) "--ignore-all-space")
          ((> arg 1) "--ignore-space-change"))))

(defun stgit-show-patch (unmerged-stage ignore-whitespace)
  "Show the patch on the current line.

UNMERGED-STAGE is the argument to `git-diff' that that selects
which stage to diff against in the case of unmerged files."
  (let* ((space-arg (stgit-whitespace-diff-arg ignore-whitespace))
         (patch-name (stgit-patch-name-at-point t))
         (entry-type (get-text-property (point) 'entry-type))
         (diff-desc (case entry-type
                      ('file "diff")
                      ('patch "patch")
                      (t (error "No patch or file at point")))))
    (stgit-show-task-message (concat "Showing " diff-desc)
      (stgit-capture-output (concat "*StGit " diff-desc "*")
        (case entry-type
          ('file
           (let* ((patched-file (stgit-patched-file-at-point))
                  (patch-id (let ((id (stgit-id patch-name)))
                              (if (and (eq id :index)
                                       (eq (stgit-file->status patched-file)
                                           'unmerged))
                                  :work
                                id)))
                  (args (append (and space-arg (list space-arg))
                                (and (stgit-file->cr-from patched-file)
                                     (list (stgit-find-copies-harder-diff-arg)))
                                (cond ((eq patch-id :index)
                                       '("--cached"))
                                      ((eq patch-id :work)
                                       (list unmerged-stage))
                                      (t
                                       (list (concat patch-id "^") patch-id)))
                                (and (eq (stgit-file->status patched-file)
                                         'copy)
                                     '("--diff-filter=C"))
                                (if (stgit-file->copy-or-rename patched-file)
                                    (list (stgit-file->cr-from patched-file)
                                          (stgit-file->cr-to patched-file))
                                  (list (stgit-file->file patched-file))))))
             (apply 'stgit-run-git "diff" args)))
          ('patch
           (let* ((patch-id (stgit-id patch-name)))
             (if (or (eq patch-id :index) (eq patch-id :work))
                 (apply 'stgit-run-git "diff"
                        (stgit-find-copies-harder-diff-arg)
                        (append (and space-arg (list space-arg))
                                (if (eq patch-id :index)
                                    '("--cached")
                                  (list unmerged-stage))))
               (let ((args (append '("show" "-O" "--patch-with-stat")
                                   `("-O" ,(stgit-find-copies-harder-diff-arg))
                                   (and space-arg (list "-O" space-arg))
                                   (list (stgit-patch-name-at-point)))))
                 (apply 'stgit-run args))))))
        (with-current-buffer standard-output
          (goto-char (point-min))
          (diff-mode))))))

(defmacro stgit-define-diff (name diff-arg &optional unmerged-action)
  `(defun ,name (&optional ignore-whitespace)
     ,(format "Show the patch on the current line.

%sWith a prefix argument, ignore whitespace. With a prefix argument
greater than four (e.g., \\[universal-argument] \
\\[universal-argument] \\[%s]), ignore all whitespace."
              (if unmerged-action
                  (format "For unmerged files, %s.\n\n" unmerged-action)
                "")
              name)
     (interactive "p")
     (stgit-assert-mode)
     (stgit-show-patch ,diff-arg ignore-whitespace)))

(stgit-define-diff stgit-diff
                   "--ours" nil)
(stgit-define-diff stgit-diff-ours
                   "--ours"
                   "diff against our branch")
(stgit-define-diff stgit-diff-theirs
                   "--theirs"
                   "diff against their branch")
(stgit-define-diff stgit-diff-base
                   "--base"
                   "diff against the merge base")
(stgit-define-diff stgit-diff-combined
                   "--cc"
                   "show a combined diff")

(defun stgit-diff-range (&optional ignore-whitespace)
  "Show diff for the range of patches between point and the marked patch.

With a prefix argument, ignore whitespace. With a prefix argument
greater than four (e.g., \\[universal-argument] \
\\[universal-argument] \\[stgit-diff-range]), ignore all whitespace."
  (interactive "p")
  (stgit-assert-mode)
  (unless (= (length stgit-marked-patches) 1)
    (error "Need exactly one patch marked"))
  (let* ((patches (stgit-sort-patches
                   (cons (stgit-patch-name-at-point t 'allow-committed)
                         stgit-marked-patches)
                   t))
         (first-patch (car patches))
         (second-patch (if (cdr patches) (cadr patches) first-patch))
         (whitespace-arg (stgit-whitespace-diff-arg ignore-whitespace))
         (applied (stgit-applied-patchsyms t)))
    (unless (and (memq first-patch applied) (memq second-patch applied))
      (error "Can only show diff range for applied patches"))
    (stgit-capture-output (format "*StGit diff %s..%s*"
                                  first-patch second-patch)
      (apply 'stgit-run-git
             "diff" "--patch-with-stat"
             (stgit-find-copies-harder-diff-arg)
             (append (and whitespace-arg (list whitespace-arg))
                     (list (format "%s^" (stgit-id first-patch))
                           (stgit-id second-patch))))
      (with-current-buffer standard-output
        (goto-char (point-min))
        (diff-mode)))))

(defun stgit-move-change-to-index (file &optional force)
  "Copies the work tree state of FILE to index, using git add or git rm.

If FORCE is not nil, use --force."
  (let ((op (if (or (file-exists-p file) (file-symlink-p file))
                '("add") '("rm" "-q"))))
    (stgit-capture-output "*git output*"
      (apply 'stgit-run-git (append op (and force '("--force"))
                                    '("--") (list file))))))

(defun stgit-remove-change-from-index (file)
  "Unstages the change in FILE from the index"
  (stgit-capture-output "*git output*"
    (stgit-run-git "reset" "-q" "--" file)))

(defun stgit-git-index-unmerged-p ()
  (let (result)
    (with-output-to-string
      (setq result (not (zerop (stgit-run-git-silent "diff-index" "--cached"
                                                     "--diff-filter=U"
                                                     "--quiet" "HEAD")))))
    result))

(defun stgit-assert-no-unmerged-changes ()
  "Signal an error if there are any unmerged changes in the index."
  (when (stgit-git-index-unmerged-p)
    (error (substitute-command-keys
            "Resolve unmerged changes with \\[stgit-resolve-file] first"))))

(defun stgit-file-toggle-index ()
  "Move modified file in or out of the index.

Leaves the point where it is, but moves the mark to where the
file ended up. You can then jump to the file with \
\\[exchange-point-and-mark]."
  (interactive)
  (stgit-assert-mode)
  (let* ((patched-file   (or (stgit-patched-file-at-point)
			     (error "No file on the current line")))
	 (patched-status (stgit-file->status patched-file)))
    (when (eq patched-status 'unmerged)
      (error (substitute-command-keys "Use \\[stgit-resolve-file] to move an unmerged file to the index")))
    (let* ((patch      (stgit-patch-at-point))
           (patch-name (stgit-patch->name patch))
           (mark-file  (if (eq patched-status 'rename)
			   (stgit-file->cr-to patched-file)
			 (stgit-file->file patched-file)))
           (point-file  (if (eq patched-status 'rename)
                            (stgit-file->cr-from patched-file)
                          (stgit-neighbour-file))))

      (cond ((eq patch-name :work)
             (stgit-move-change-to-index (stgit-file->file patched-file)
                                         (eq patched-status 'ignore)))
            ((eq patch-name :index)
             (stgit-remove-change-from-index (stgit-file->file patched-file)))
            (t
             (error "Can only move files between working tree and index")))
      (stgit-save-excursion
        (stgit-refresh-worktree)
        (stgit-refresh-index))
      (stgit-goto-patch (if (eq patch-name :index) :work :index) mark-file)
      (push-mark nil t t)
      (setq deactivate-mark t)
      (stgit-goto-patch patch-name point-file))))

(defun stgit-toggle-index ()
  "Move change in or out of the index.

Works on index and work tree, as well as files in either.

Leaves the point where it is, but moves the mark to where the
file ended up. You can then jump to the file with \
\\[exchange-point-and-mark]."
  (interactive)
  (stgit-assert-mode)
  (if (stgit-patched-file-at-point)
      (stgit-file-toggle-index)
    (let ((patch-name (stgit-patch-name-at-point)))
      (unless (memq patch-name '(:index :work))
        (error "Can only move changes between working tree and index"))
      (stgit-assert-no-unmerged-changes)
      (if (if (eq patch-name :index)
              (stgit-index-empty-p)
            (stgit-work-tree-empty-p))
          (message "No changes to be moved")
        (stgit-capture-output nil
          (if (eq patch-name :work)
              (stgit-run-git "add" "--update")
            (stgit-run-git "reset" "--mixed" "-q")))
        (stgit-save-excursion
          (stgit-refresh-worktree)
          (stgit-refresh-index)))
      (stgit-goto-patch patch-name)
      (push-mark nil t t)
      (setq deactivate-mark t)
      (stgit-goto-patch (if (eq patch-name :index) :work :index)))))

(defun stgit-edit ()
  "Edit the patch on the current line."
  (interactive)
  (stgit-assert-mode)
  (let ((patchsym (stgit-patch-name-at-point t t))
        (edit-buf (get-buffer-create "*StGit edit*"))
        (dir default-directory))
    (log-edit 'stgit-confirm-edit t nil edit-buf)
    (set (make-local-variable 'stgit-edit-patchsym) patchsym)
    (setq default-directory dir)
    (let ((standard-output edit-buf))
      (save-excursion
        (stgit-run-silent "edit" "--save-template=-" "--" patchsym)))))

(defun stgit-confirm-edit ()
  (interactive)
  (let ((file (make-temp-file "stgit-edit-")))
    (write-region (point-min) (point-max) file)
    (stgit-capture-output nil
      (stgit-run "edit" "-f" file "--" stgit-edit-patchsym))
    (with-current-buffer log-edit-parent-buffer
      (stgit-reload))))

(defun stgit-new-here (add-sign)
  "Create a new patch before the patch at point, asking for a
commit message.

With a prefix argument, include a \"Signed-off-by:\" line at the
end of the patch description.

This works like `stgit-new' followed by `stgit-move'."
  (interactive "P")
  (stgit-assert-mode)
  (let ((patch (stgit-patch-at-point t)))
    (case (stgit-patch->status patch)
      ((index work) (stgit-new add-sign))
      ((applied top)
       (unless (and (stgit-index-empty-p)
                    (stgit-work-tree-empty-p))
         (error "Index and worktree must not contain any changes"))
       (stgit-new add-sign nil (stgit-patch->name patch)))
      (t (error "Can only be used on applied patches")))))

(defun stgit-new (add-sign &optional refresh sink-to)
  "Create a new patch, asking for a commit message.

With a prefix argument, include a \"Signed-off-by:\" line at the
end of the message.

If REFRESH is non-nil, also refresh the patch after creating it.

If SINK-TO is non-nil, sink the created patch to the patch with
that name (a symbol)."
  (interactive "P")
  (stgit-assert-mode)
  (let ((edit-buf (get-buffer-create "*StGit edit*"))
        (dir default-directory))
    (log-edit 'stgit-confirm-new t nil edit-buf)
    (setq default-directory dir)
    (set (make-local-variable 'stgit-refresh-after-new) refresh)
    (set (make-local-variable 'stgit-sink-to) sink-to)
    (when add-sign
      (save-excursion
        (let ((standard-output (current-buffer)))
          (stgit-run-silent "new" "--sign" "--save-template=-"))))))

(defun stgit-confirm-new ()
  (interactive)
  (let ((file (make-temp-file "stgit-edit-"))
        (refresh stgit-refresh-after-new)
        new-patch)
    (write-region (point-min) (point-max) file)
    (stgit-capture-output nil
      (stgit-run "new" "-f" file))

    (let ((top (with-output-to-string (stgit-run "top"))))
      (when (string-match "\\`\\(.+\\)" top)
        (setq new-patch (intern (match-string 1 top)))))

    (when stgit-sink-to
      (stgit-run "sink" "-t" stgit-sink-to))
    (with-current-buffer log-edit-parent-buffer
      (if refresh
          (stgit-refresh)
        (stgit-reload))
      (stgit-goto-patch new-patch))))

(defun stgit-new-and-refresh (add-sign)
  "Create a new patch based on the current changes, asking for a
commit message.

With a prefix argument, include a \"Signed-off-by:\" line at the
end of the patch.

This works just like running `stgit-new' followed by `stgit-refresh'."
  (interactive "P")
  (stgit-assert-mode)
  (stgit-assert-no-unmerged-changes)
  (stgit-new add-sign t))

(defun stgit-create-patch-name (description)
  "Create a patch name from a long description"
  (let ((patch ""))
    (while (> (length description) 0)
      (cond ((string-match "\\`[a-zA-Z_-]+" description)
             (setq patch (downcase (concat patch
                                           (match-string 0 description))))
             (setq description (substring description (match-end 0))))
            ((string-match "\\` +" description)
             (setq patch (concat patch "-"))
             (setq description (substring description (match-end 0))))
            ((string-match "\\`[^a-zA-Z_-]+" description)
             (setq description (substring description (match-end 0))))))
    (cond ((= (length patch) 0)
           "patch")
          ((> (length patch) 20)
           (substring patch 0 20))
          (t patch))))

(defun stgit-delete (patchsyms &optional spill-p)
  "Delete the patches in PATCHSYMS.
Interactively, delete the marked patches, or the patch at point.

With a prefix argument, or SPILL-P, spill the patch contents to
the work tree and index."
  (interactive (list (stgit-patches-marked-or-at-point t t)
                     current-prefix-arg))
  (stgit-assert-mode)
  (unless patchsyms
    (error "No patches to delete"))
  (when (memq :index patchsyms)
    (error "Cannot delete the index"))
  (when (memq :work  patchsyms)
    (error "Cannot delete the work tree"))

  (let ((npatches (length patchsyms)))
    (when (yes-or-no-p (format "Really delete %d patch%s%s? "
			       npatches
			       (if (= 1 npatches) "" "es")
                               (if spill-p
                                   " (spilling contents to index)"
                                 "")))
      (let ((args (append (when spill-p '("--spill"))
                          '("--")
                          patchsyms)))
        (stgit-capture-output nil
          (apply 'stgit-run "delete" args))
        (stgit-reload)))))

(defun stgit-move-patches-target ()
  "Return the patchsym indicating a target patch for
`stgit-move-patches'.

This is either the first unmarked patch at or after point, or one
of :top and :bottom if the point is after or before the applied
patches."

  (save-excursion
    (let (result)
      (while (not result)
        (let ((patchsym (stgit-patch-name-at-point)))
          (cond ((memq patchsym '(:work :index)) (setq result :top))
                (patchsym (if (memq patchsym stgit-marked-patches)
                              (stgit-next-patch)
                            (setq result patchsym)))
                ((re-search-backward "^>" nil t) (setq result :top))
                (t (setq result :bottom)))))
      result)))

(defun stgit-sort-patches (patchsyms &optional allow-duplicates)
  "Returns the list of patches in PATCHSYMS sorted according to
their position in the patch series, bottommost first.

PATCHSYMS must not contain duplicate entries, unless
ALLOW-DUPLICATES is not nil."
  (let (sorted-patchsyms)
    (ewoc-map #'(lambda (patch)
                  (let ((name (stgit-patch->name patch)))
                    (when (memq name patchsyms)
                      (setq sorted-patchsyms (cons name sorted-patchsyms))))
                  nil)
              stgit-ewoc)
    (setq sorted-patchsyms (nreverse sorted-patchsyms))

    (unless allow-duplicates
      (unless (= (length patchsyms) (length sorted-patchsyms))
        (error "Internal error")))

    sorted-patchsyms))

(defun stgit-move-patches (patchsyms target-patch)
  "Move the patches in PATCHSYMS to below TARGET-PATCH.
If TARGET-PATCH is :bottom or :top, move the patches to the
bottom or top of the stack, respectively.

Interactively, move the marked patches to where the point is."
  (interactive (list stgit-marked-patches
                     (stgit-move-patches-target)))
  (stgit-assert-mode)
  (unless patchsyms
    (error "Need at least one patch to move"))

  (unless target-patch
    (error "Point not at a patch"))

  ;; need to have patchsyms sorted by position in the stack
  (let ((sorted-patchsyms (stgit-sort-patches patchsyms)))
    (stgit-capture-output nil
      (if (eq target-patch :top)
          (apply 'stgit-run "float" "--" sorted-patchsyms)
        (apply 'stgit-run
               "sink"
               (append (unless (eq target-patch :bottom)
                         (list "--to" target-patch))
                       '("--")
                       sorted-patchsyms)))))
  (stgit-reload))

(defun stgit-squash (patchsyms)
  "Squash the patches in PATCHSYMS.
Interactively, squash the marked patches.

Unless there are any conflicts, the patches will be merged into
one patch, which will occupy the same spot in the series as the
deepest patch had before the squash."
  (interactive (list stgit-marked-patches))
  (stgit-assert-mode)
  (when (< (length patchsyms) 2)
    (error "Need at least two patches to squash"))
  (let ((stgit-buffer (current-buffer))
        (edit-buf (get-buffer-create "*StGit edit*"))
        (dir default-directory)
        (sorted-patchsyms (stgit-sort-patches patchsyms)))
    (log-edit 'stgit-confirm-squash t nil edit-buf)
    (set (make-local-variable 'stgit-patchsyms) sorted-patchsyms)
    (setq default-directory dir)
    (let ((result (let ((standard-output edit-buf))
                    (save-excursion
                      (apply 'stgit-run-silent "squash"
                             "--save-template=-" "--" sorted-patchsyms)))))

      ;; stg squash may have reordered the patches or caused conflicts
      (with-current-buffer stgit-buffer
        (stgit-reload))

      (unless (eq 0 result)
        (fundamental-mode)
        (rename-buffer "*StGit error*")
        (resize-temp-buffer-window)
        (switch-to-buffer-other-window stgit-buffer)
        (error "stg squash failed")))))

(defun stgit-confirm-squash ()
  (interactive)
  (let ((file (make-temp-file "stgit-edit-")))
    (write-region (point-min) (point-max) file)
    (stgit-capture-output nil
      (apply 'stgit-run "squash" "-f" file "--" stgit-patchsyms))
    (with-current-buffer log-edit-parent-buffer
      (stgit-clear-marks)
      ;; Go to first marked patch and stay there
      (goto-char (point-min))
      (re-search-forward (concat "^[>+-]\\*") nil t)
      (move-to-column goal-column)
      (let ((pos (point)))
        (stgit-reload)
        (goto-char pos)))))

(defun stgit-help ()
  "Display help for the StGit mode."
  (interactive)
  (describe-function 'stgit-mode))

(defun stgit-execute-process-sentinel (process sentinel)
  (let (old-sentinel stgit-buf)
    (with-current-buffer (process-buffer process)
      (setq old-sentinel old-process-sentinel
            stgit-buf    stgit-buffer))
    (and (memq (process-status process) '(exit signal))
         (buffer-live-p stgit-buf)
         (with-current-buffer stgit-buf
           (stgit-reload)))
    (funcall old-sentinel process sentinel)))

(defun stgit-execute-process-filter (process output)
  (with-current-buffer (process-buffer process)
    (let* ((old-point (point))
           (pmark     (process-mark process))
           (insert-at (marker-position pmark))
           (at-pmark  (= insert-at old-point)))
      (goto-char insert-at)
      (insert-before-markers output)
      (comint-carriage-motion insert-at (point))
      (set-marker pmark (point))
      (unless at-pmark
        (goto-char old-point)))))

(defun stgit-execute (&optional git-mode)
  "Prompt for an stg command to execute in a shell.

The names of any marked patches or the patch at point are
inserted in the command to be executed.

With a prefix argument, or if GIT-MODE is non-nil, insert SHA1
sums of the marked patches instead, and prompt for a git command.

If the command ends in an ampersand, run it asynchronously.

When the command has finished, reload the stgit buffer."
  (interactive "P")
  (stgit-assert-mode)
  (let* ((patches (stgit-sort-patches
                   (stgit-patches-marked-or-at-point nil 'allow-committed)))
         (patch-names (mapcar 'symbol-name patches))
         (hyphens (find-if (lambda (s) (string-match "^-" s)) patch-names))
         (program (if git-mode stgit-git-program stgit-stg-program))
         (defaultcmd (concat program
                             " "
                             (and patch-names " ")
                             (and hyphens patch-names "-- ")
                             (mapconcat (if git-mode 'stgit-id 'identity)
                                        patch-names " ")))
         (cmd (read-from-minibuffer "Shell command: "
                                    (cons defaultcmd (+ (length program) 2))
                                    nil nil 'shell-command-history))
         (async (string-match "&[ \t]*\\'" cmd))
         (buffer (get-buffer-create
                  (if async
                      "*Async Shell Command*"
                    "*Shell Command Output*"))))
    ;; cannot use minibuffer as stgit-reload would overwrite it; if we
    ;; show the buffer, shell-command will not use the minibuffer
    (display-buffer buffer)
    (shell-command cmd)
    (if async
        (let ((old-buffer (current-buffer)))
          (with-current-buffer buffer
            (let ((process (get-buffer-process buffer)))
              (set (make-local-variable 'old-process-sentinel)
                   (process-sentinel process))
              (set (make-local-variable 'stgit-buffer)
                   old-buffer)
              (set-process-filter process 'stgit-execute-process-filter)
              (set-process-sentinel process 'stgit-execute-process-sentinel))))
      (with-current-buffer buffer
        (comint-carriage-motion (point-min) (point-max)))
      (shrink-window-if-larger-than-buffer (get-buffer-window buffer))
      (stgit-reload))))

(defun stgit-undo-or-redo (redo hard)
  "Run stg undo or, if REDO is non-nil, stg redo.

If HARD is non-nil, use the --hard flag."
  (stgit-assert-mode)
  (let ((cmd (if redo "redo" "undo")))
    (stgit-capture-output nil
      (if arg
          (when (or (and (stgit-index-empty-p)
                         (stgit-work-tree-empty-p))
                    (y-or-n-p (format "Hard %s may overwrite index/work tree changes. Continue? "
                                      cmd)))
            (stgit-run cmd "--hard"))
        (stgit-run cmd))))
  (stgit-reload))

(defun stgit-undo (&optional arg)
  "Run stg undo.
With prefix argument, run it with the --hard flag.

See also `stgit-redo'."
  (interactive "P")
  (stgit-undo-or-redo nil arg))

(defun stgit-redo (&optional arg)
  "Run stg redo.
With prefix argument, run it with the --hard flag.

See also `stgit-undo'."
  (interactive "P")
  (stgit-undo-or-redo t arg))

(defun stgit-refresh (&optional arg)
  "Run stg refresh.
If the index contains any changes, only refresh from index.

With prefix argument, refresh the marked patch or the patch under point."
  (interactive "P")
  (stgit-assert-mode)
  (stgit-assert-no-unmerged-changes)
  (let ((patchargs (if arg
                       (let ((patches (stgit-patches-marked-or-at-point t t)))
                         (when (> (length patches) 1)
                           (error "Too many patches marked"))
                         (cons "-p" patches))
                     nil)))
    (unless (stgit-index-empty-p)
      (setq patchargs (cons "--index" patchargs)))
    (stgit-capture-output nil
      (apply 'stgit-run "refresh" patchargs))
    (stgit-refresh-git-status))
  (stgit-reload))

(defvar stgit-show-worktree nil
  "If nil, inhibit showing work tree and index in the stgit buffer.

See also `stgit-show-worktree-mode'.")

(defvar stgit-show-ignored nil
  "If nil, inhibit showing files ignored by git.")

(defvar stgit-show-unknown nil
  "If nil, inhibit showing files not registered with git.")

(defvar stgit-show-patch-names t
  "If nil, inhibit showing patch names.")

(defvar stgit-show-committed nil
  "If nil, inhibit showing recent commits.")

(defvar stgit-show-svn nil
  "If nil, inhibit showing git svn information.")

(defvar stgit-committed-count nil
  "The number of recent commits to show.")

(defmacro stgit-define-toggle-view (sym desc help)
  (declare (indent 1) (debug (symbolp stringp stringp)))
  (let* ((name (symbol-name sym))
         (fun  (intern (concat "stgit-toggle-" name)))
         (flag (intern (concat "stgit-show-" name))))
    `(progn
       ;; make help-follow find the correct function
       (put (quote ,fun) 'definition-name 'stgit-define-toggle-view)
       (defun ,fun (&optional arg)
         ,help
         (interactive "P")
         (stgit-assert-mode)
         (setq ,flag (if arg
                         (> (prefix-numeric-value arg) 0)
                       (not ,flag)))
         (stgit-reload (concat (if ,flag "Showing" "Hiding") " " ,desc))))))

(stgit-define-toggle-view worktree
  "work tree and index"
  "Toggle the visibility of the work tree.
With ARG, show the work tree if ARG is positive.

Its initial setting is controlled by `stgit-default-show-worktree'.

`stgit-show-worktree-mode' controls where on screen the index and
work tree will show up.")

(stgit-define-toggle-view ignored
  "ignored files"
  "Toggle the visibility of files ignored by git in the work
tree. With ARG, show these files if ARG is positive.

Its initial setting is controlled by `stgit-default-show-ignored'.

Use \\[stgit-toggle-worktree] to show the work tree.")

(stgit-define-toggle-view unknown
  "unknown files"
  "Toggle the visibility of files not registered with git in the
work tree. With ARG, show these files if ARG is positive.

Its initial setting is controlled by `stgit-default-show-unknown'.

Use \\[stgit-toggle-worktree] to show the work tree.")

(stgit-define-toggle-view patch-names
  "patch names"
  "Toggle the visibility of patch names. With ARG, show patch names
if ARG is positive.

The initial setting is controlled by `stgit-default-show-patch-names'.")

(stgit-define-toggle-view svn
  "subversion revisions"
  "Toggle showing subversion information from git svn. With ARG,
show svn information if ARG is positive.

The initial setting is controlled by `stgit-default-show-svn'.")

(defun stgit-toggle-committed (&optional arg)
  "Toggle the visibility of historical git commits.
With ARG, set the number of commits to show to ARG, and disable
them if ARG is zero.

The initial setting is controlled by `stgit-default-show-committed'."
  (interactive "P")
  (stgit-assert-mode)
  (if (null arg)
      (setq stgit-show-committed (not stgit-show-committed))
    (let ((n (prefix-numeric-value arg)))
      (setq stgit-show-committed (> n 0))
      (setq stgit-committed-count n)))
  (stgit-reload (format "%s historical commits"
                        (if (and stgit-show-committed
                                 (> stgit-committed-count 0))
                            "Showing"
                          "Hiding"))))

(provide 'stgit)
;;; stgit.el ends here
