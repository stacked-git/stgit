;; stgit.el: An emacs mode for StGit
;;
;; Copyright (C) 2007 David Kågedal <davidk@lysator.liu.se>
;;
;; To install: put this file on the load-path and place the following
;; in your .emacs file:
;;
;;    (require 'stgit)
;;
;; To start: `M-x stgit'

(require 'git nil t)

(defun stgit (dir)
  "Manage StGit patches for the tree in DIR."
  (interactive "DDirectory: \n")
  (switch-to-stgit-buffer (git-get-top-dir dir))
  (stgit-reload))

(unless (fboundp 'git-get-top-dir)
  (defun git-get-top-dir (dir)
    "Retrieve the top-level directory of a git tree."
    (let ((cdup (with-output-to-string
                  (with-current-buffer standard-output
                    (cd dir)
                    (unless (eq 0 (call-process "git" nil t nil
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

(defun switch-to-stgit-buffer (dir)
  "Switch to a (possibly new) buffer displaying StGit patches for DIR."
  (setq dir (file-name-as-directory dir))
  (let ((buffers (buffer-list)))
    (while (and buffers
                (not (with-current-buffer (car buffers)
                       (and (eq major-mode 'stgit-mode)
                            (string= default-directory dir)))))
      (setq buffers (cdr buffers)))
    (switch-to-buffer (if buffers
                          (car buffers)
                        (create-stgit-buffer dir)))))

(defun create-stgit-buffer (dir)
  "Create a buffer for showing StGit patches.
Argument DIR is the repository path."
  (let ((buf (create-file-buffer (concat dir "*stgit*")))
        (inhibit-read-only t))
    (with-current-buffer buf
      (setq default-directory dir)
      (stgit-mode)
      (setq buffer-read-only t))
    buf))

(defmacro stgit-capture-output (name &rest body)
  "Capture StGit output and, if there was any output, show it in a window
at the end.
Returns nil if there was no output."
  `(let ((output-buf (get-buffer-create ,(or name "*StGit output*")))
         (stgit-dir default-directory)
         (inhibit-read-only t))
     (with-current-buffer output-buf
       (erase-buffer)
       (setq default-directory stgit-dir)
       (setq buffer-read-only t))
     (let ((standard-output output-buf))
       ,@body)
     (with-current-buffer output-buf
       (set-buffer-modified-p nil)
       (setq buffer-read-only t)
       (if (< (point-min) (point-max))
           (display-buffer output-buf t)))))
(put 'stgit-capture-output 'lisp-indent-function 1)

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

(defun stgit-run-silent (&rest args)
  (setq args (stgit-make-run-args args))
  (apply 'call-process "stg" nil standard-output nil args))

(defun stgit-run (&rest args)
  (setq args (stgit-make-run-args args))
  (let ((msgcmd (mapconcat #'identity args " ")))
    (message "Running stg %s..." msgcmd)
    (apply 'call-process "stg" nil standard-output nil args)
    (message "Running stg %s...done" msgcmd)))

(defun stgit-run-git (&rest args)
  (setq args (stgit-make-run-args args))
  (let ((msgcmd (mapconcat #'identity args " ")))
    (message "Running git %s..." msgcmd)
    (apply 'call-process "git" nil standard-output nil args)
    (message "Running git %s...done" msgcmd)))

(defun stgit-run-git-silent (&rest args)
  (setq args (stgit-make-run-args args))
  (apply 'call-process "git" nil standard-output nil args))

(defun stgit-reload ()
  "Update the contents of the StGit buffer."
  (interactive)
  (let ((inhibit-read-only t)
        (curline (line-number-at-pos))
        (curpatch (stgit-patch-at-point)))
    (erase-buffer)
    (insert "Branch: ")
    (stgit-run-silent "branch")
    (stgit-run-silent "series" "--description" "--empty")
    (stgit-rescan)
    (if curpatch
        (stgit-goto-patch curpatch)
      (goto-line curline)))
  (stgit-refresh-git-status))

(defgroup stgit nil
  "A user interface for the StGit patch maintenance tool."
  :group 'tools)

(defface stgit-description-face
  '((((background dark)) (:foreground "tan"))
    (((background light)) (:foreground "dark red")))
  "The face used for StGit descriptions"
  :group 'stgit)

(defface stgit-top-patch-face
  '((((background dark)) (:weight bold :foreground "yellow"))
    (((background light)) (:weight bold :foreground "purple"))
    (t (:weight bold)))
  "The face used for the top patch names"
  :group 'stgit)

(defface stgit-applied-patch-face
  '((((background dark)) (:foreground "light yellow"))
    (((background light)) (:foreground "purple"))
    (t ()))
  "The face used for applied patch names"
  :group 'stgit)

(defface stgit-unapplied-patch-face
  '((((background dark)) (:foreground "gray80"))
    (((background light)) (:foreground "orchid"))
    (t ()))
  "The face used for unapplied patch names"
  :group 'stgit)

(defface stgit-modified-file-face
  '((((class color) (background light)) (:foreground "purple"))
    (((class color) (background dark)) (:foreground "salmon")))
  "StGit mode face used for modified file status"
  :group 'stgit)

(defface stgit-unmerged-file-face
  '((((class color) (background light)) (:foreground "red" :bold t))
    (((class color) (background dark)) (:foreground "red" :bold t)))
  "StGit mode face used for unmerged file status"
  :group 'stgit)

(defface stgit-unknown-file-face
  '((((class color) (background light)) (:foreground "goldenrod" :bold t))
    (((class color) (background dark)) (:foreground "goldenrod" :bold t)))
  "StGit mode face used for unknown file status"
  :group 'stgit)

(defface stgit-file-permission-face
  '((((class color) (background light)) (:foreground "green" :bold t))
    (((class color) (background dark)) (:foreground "green" :bold t)))
  "StGit mode face used for permission changes."
  :group 'stgit)

(defcustom stgit-expand-find-copies-harder
  nil
  "Try harder to find copied files when listing patches.

When not nil, runs git diff-tree with the --find-copies-harder
flag, which reduces performance."
  :type 'boolean
  :group 'stgit)

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
            (unknown     "Unknown"     stgit-unknown-file-face)))
  "Alist of code symbols to description strings")

(defun stgit-file-status-code-as-string (code)
  "Return stgit status code as string"
  (let ((str (assq (if (consp code) (car code) code)
                   stgit-file-status-code-strings)))
    (when str
      (format "%-11s  "
              (if (and str (consp code) (/= (cdr code) 100))
                  (format "%s %s" (cdr str)
                          (propertize (format "%d%%" (cdr code))
                                      'face 'stgit-description-face))
                (cdr str))))))

(defun stgit-file-status-code (str &optional score)
  "Return stgit status code from git status string"
  (let ((code (assoc str '(("A" . add)
                           ("C" . copy)
                           ("D" . delete)
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
             (format "   (%s)" (stgit-file-type-string new-type))))
          (t (format "   (%s -> %s)"
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

(defun stgit-expand-patch (patchsym)
  "Expand (show modification of) the patch with name PATCHSYM (a
symbol) at point.
`stgit-expand-find-copies-harder' controls how hard to try to
find copied files."
  (save-excursion
    (forward-line)
    (let* ((start (point))
           (result (with-output-to-string
                     (stgit-run-git "diff-tree" "-r" "-z"
                                    (if stgit-expand-find-copies-harder
                                        "--find-copies-harder"
                                      "-C")
                                    (stgit-id patchsym)))))
      (let (mstart)
        (while (string-match "\0:\\([0-7]+\\) \\([0-7]+\\) [0-9A-Fa-f]\\{40\\} [0-9A-Fa-f]\\{40\\} \\(\\([CR]\\)\\([0-9]*\\)\0\\([^\0]*\\)\0\\([^\0]*\\)\\|\\([ABD-QS-Z]\\)\0\\([^\0]*\\)\\)"
                             result mstart)
          (let ((copy-or-rename (match-string 4 result))
                (old-perm       (read (format "#o%s" (match-string 1 result))))
                (new-perm       (read (format "#o%s" (match-string 2 result))))
                (line-start (point))
                status
                change
                properties)
            (insert "    ")
            (if copy-or-rename
                (let ((cr-score       (match-string 5 result))
                      (cr-from-file   (match-string 6 result))
                      (cr-to-file     (match-string 7 result)))
                  (setq status (stgit-file-status-code copy-or-rename
                                                       cr-score)
                        properties (list 'stgit-old-file cr-from-file
                                         'stgit-new-file cr-to-file)
                        change (concat
                                cr-from-file
                                (propertize " -> "
                                            'face 'stgit-description-face)
                                cr-to-file)))
              (setq status (stgit-file-status-code (match-string 8 result))
                    properties (list 'stgit-file (match-string 9 result))
                    change (match-string 9 result)))

            (let ((mode-change (stgit-file-mode-change-string old-perm
                                                              new-perm)))
              (insert (format "%-12s" (stgit-file-status-code-as-string
                                       status))
                      mode-change
                      (if (> (length mode-change) 0) " " "")
                      change
                      (propertize (stgit-file-type-change-string old-perm
                                                                 new-perm)
                                  'face 'stgit-description-face)
                      ?\n))
            (add-text-properties line-start (point) properties))
          (setq mstart (match-end 0))))
      (when (= start (point))
        (insert "    <no files>\n"))
      (put-text-property start (point) 'stgit-file-patchsym patchsym))))

(defun stgit-collapse-patch (patchsym)
  "Collapse the patch with name PATCHSYM after the line at point."
  (save-excursion
    (forward-line)
    (let ((start (point)))
      (while (eq (get-text-property (point) 'stgit-file-patchsym) patchsym)
        (forward-line))
      (delete-region start (point)))))

(defun stgit-rescan ()
  "Rescan the status buffer."
  (save-excursion
    (let ((marked ())
	  found-any)
      (goto-char (point-min))
      (while (not (eobp))
        (cond ((looking-at "Branch: \\(.*\\)")
               (put-text-property (match-beginning 1) (match-end 1)
                                  'face 'bold))
              ((looking-at "\\([0 ]\\)\\([>+-]\\)\\( \\)\\([^ ]+\\) *[|#] \\(.*\\)")
	       (setq found-any t)
               (let ((empty (match-string 1))
		     (state (match-string 2))
                     (patchsym (intern (match-string 4))))
                 (put-text-property
                  (match-beginning 4) (match-end 4) 'face
                  (cond ((string= state ">") 'stgit-top-patch-face)
                        ((string= state "+") 'stgit-applied-patch-face)
                        ((string= state "-") 'stgit-unapplied-patch-face)))
                 (put-text-property (match-beginning 5) (match-end 5)
                                    'face 'stgit-description-face)
                 (when (memq patchsym stgit-marked-patches)
                   (save-excursion
		     (replace-match "*" nil nil nil 3))
                   (setq marked (cons patchsym marked)))
                 (put-text-property (match-beginning 0) (match-end 0)
                                    'stgit-patchsym patchsym)
                 (when (memq patchsym stgit-expanded-patches)
                   (stgit-expand-patch patchsym))
		 (when (equal "0" empty)
		   (save-excursion
		     (goto-char (match-beginning 5))
		     (insert "(empty) ")))
		 (delete-char 1)
                 ))
              ((or (looking-at "stg series: Branch \".*\" not initialised")
                   (looking-at "stg series: .*: branch not initialized"))
	       (setq found-any t)
               (forward-line 1)
               (insert "Run M-x stgit-init to initialise")))
        (forward-line 1))
      (setq stgit-marked-patches (nreverse marked))
      (unless found-any
	(insert "\n  "
		(propertize "no patches in series"
			    'face 'stgit-description-face))))))

(defun stgit-select-file ()
  (let ((patched-file (stgit-patched-file-at-point)))
    (unless patched-file
      (error "No patch or file on the current line"))
    (let ((filename (expand-file-name (cdr patched-file))))
      (unless (file-exists-p filename)
        (error "File does not exist"))
      (find-file filename))))

(defun stgit-toggle-patch-file-list (curpath)
  (let ((inhibit-read-only t))
    (if (memq curpatch stgit-expanded-patches)
        (save-excursion
          (setq stgit-expanded-patches (delq curpatch stgit-expanded-patches))
          (stgit-collapse-patch curpatch))
      (progn
        (setq stgit-expanded-patches (cons curpatch stgit-expanded-patches))
        (stgit-expand-patch curpatch)))))

(defun stgit-select ()
  "Expand or collapse the current entry"
  (interactive)
  (let ((curpatch (stgit-patch-at-point)))
    (if curpatch
        (stgit-toggle-patch-file-list curpatch)
      (stgit-select-file))))


(defun stgit-find-file-other-window ()
  "Open file at point in other window"
  (interactive)
  (let ((patched-file (stgit-patched-file-at-point)))
    (unless patched-file
      (error "No file on the current line"))
    (let ((filename (expand-file-name (cdr patched-file))))
      (unless (file-exists-p filename)
        (error "File does not exist"))
      (find-file-other-window filename))))

(defun stgit-quit ()
  "Hide the stgit buffer."
  (interactive)
  (bury-buffer))

(defun stgit-git-status ()
  "Show status using `git-status'."
  (interactive)
  (unless (fboundp 'git-status)
    (error "The stgit-git-status command requires git-status"))
  (let ((dir default-directory))
    (save-selected-window
      (pop-to-buffer nil)
      (git-status dir))))

(defun stgit-next-line (&optional arg try-vscroll)
  "Move cursor vertically down ARG lines"
  (interactive "p\np")
  (next-line arg try-vscroll)
  (when (looking-at "  \\S-")
    (forward-char 2)))

(defun stgit-previous-line (&optional arg try-vscroll)
  "Move cursor vertically up ARG lines"
  (interactive "p\np")
  (previous-line arg try-vscroll)
  (when (looking-at "  \\S-")
    (forward-char 2)))

(defun stgit-next-patch (&optional arg)
  "Move cursor down ARG patches"
  (interactive "p")
  (unless arg
    (setq arg 1))
  (if (< arg 0)
      (stgit-previous-patch (- arg))
    (while (not (zerop arg))
      (setq arg (1- arg))
      (while (progn (stgit-next-line)
                    (not (stgit-patch-at-point)))))))

(defun stgit-previous-patch (&optional arg)
  "Move cursor up ARG patches"
  (interactive "p")
  (unless arg
    (setq arg 1))
  (if (< arg 0)
      (stgit-next-patch (- arg))
    (while (not (zerop arg))
      (setq arg (1- arg))
      (while (progn (stgit-previous-line)
                    (not (stgit-patch-at-point)))))))

(defvar stgit-mode-hook nil
  "Run after `stgit-mode' is setup.")

(defvar stgit-mode-map nil
  "Keymap for StGit major mode.")

(unless stgit-mode-map
  (setq stgit-mode-map (make-keymap))
  (suppress-keymap stgit-mode-map)
  (mapc (lambda (arg) (define-key stgit-mode-map (car arg) (cdr arg)))
        '((" " .        stgit-mark)
          ("m" .        stgit-mark)
          ("\d" .       stgit-unmark-up)
          ("u" .        stgit-unmark-down)
          ("?" .        stgit-help)
          ("h" .        stgit-help)
          ("p" .        stgit-previous-line)
          ("n" .        stgit-next-line)
          ("\C-p" .     stgit-previous-patch)
          ("\C-n" .     stgit-next-patch)
          ("\M-{" .     stgit-previous-patch)
          ("\M-}" .     stgit-next-patch)
          ("s" .        stgit-git-status)
          ("g" .        stgit-reload)
          ("r" .        stgit-refresh)
          ("\C-c\C-r" . stgit-rename)
          ("e" .        stgit-edit)
          ("M" .        stgit-move-patches)
          ("S" .        stgit-squash)
          ("N" .        stgit-new)
          ("R" .        stgit-repair)
          ("C" .        stgit-commit)
          ("U" .        stgit-uncommit)
          ("\r" .       stgit-select)
          ("o" .        stgit-find-file-other-window)
          (">" .        stgit-push-next)
          ("<" .        stgit-pop-next)
          ("P" .        stgit-push-or-pop)
          ("G" .        stgit-goto)
          ("=" .        stgit-show)
          ("D" .        stgit-delete)
          ([(control ?/)] . stgit-undo)
          ("\C-_" .     stgit-undo)
          ("B" .        stgit-branch)
          ("q" .        stgit-quit))))

(defun stgit-mode ()
  "Major mode for interacting with StGit.
Commands:
\\{stgit-mode-map}"
  (kill-all-local-variables)
  (buffer-disable-undo)
  (setq mode-name "StGit"
        major-mode 'stgit-mode
        goal-column 2)
  (use-local-map stgit-mode-map)
  (set (make-local-variable 'list-buffers-directory) default-directory)
  (set (make-local-variable 'stgit-marked-patches) nil)
  (set (make-local-variable 'stgit-expanded-patches) nil)
  (set-variable 'truncate-lines 't)
  (run-hooks 'stgit-mode-hook))

(defun stgit-add-mark (patchsym)
  "Mark the patch PATCHSYM."
  (setq stgit-marked-patches (cons patchsym stgit-marked-patches))
  (save-excursion
    (when (stgit-goto-patch patchsym)
      (move-to-column 1)
      (let ((inhibit-read-only t))
        (insert-and-inherit ?*)
        (delete-char 1)))))

(defun stgit-remove-mark (patchsym)
  "Unmark the patch PATCHSYM."
  (setq stgit-marked-patches (delq patchsym stgit-marked-patches))
  (save-excursion
    (when (stgit-goto-patch patchsym)
      (move-to-column 1)
      (let ((inhibit-read-only t))
        (insert-and-inherit ? )
        (delete-char 1)))))

(defun stgit-clear-marks ()
  "Unmark all patches."
  (setq stgit-marked-patches '()))

(defun stgit-patch-at-point (&optional cause-error allow-file)
  "Return the patch name on the current line as a symbol.
If CAUSE-ERROR is not nil, signal an error if none found.
If ALLOW-FILE is not nil, also handle when point is on a file of
a patch."
  (or (get-text-property (point) 'stgit-patchsym)
      (and allow-file
           (get-text-property (point) 'stgit-file-patchsym))
      (when cause-error
        (error "No patch on this line"))))

(defun stgit-patched-file-at-point (&optional both-files)
  "Returns a cons of the patchsym and file name at point. For
copies and renames, return the new file if the patch is either
applied. If BOTH-FILES is non-nil, return a cons of the old and
the new file names instead of just one name."
  (let ((patchsym (get-text-property (point) 'stgit-file-patchsym))
        (file     (get-text-property (point) 'stgit-file)))
    (cond ((not patchsym) nil)
          (file (cons patchsym file))
          (both-files
           (cons patchsym (cons (get-text-property (point) 'stgit-old-file)
                                (get-text-property (point) 'stgit-new-file))))
          (t
           (let ((file-sym (save-excursion
                             (stgit-previous-patch)
                             (unless (eq (stgit-patch-at-point)
                                         patchsym)
                               (error "Cannot find the %s patch" patchsym))
                             (beginning-of-line)
                             (if (= (char-after) ?-)
                                 'stgit-old-file 
                               'stgit-new-file))))
             (cons patchsym (get-text-property (point) file-sym)))))))

(defun stgit-patches-marked-or-at-point ()
  "Return the symbols of the marked patches, or the patch on the current line."
  (if stgit-marked-patches
      stgit-marked-patches
    (let ((patch (stgit-patch-at-point)))
      (if patch
          (list patch)
        '()))))

(defun stgit-goto-patch (patchsym)
  "Move point to the line containing patch PATCHSYM.
If that patch cannot be found, return nil."
  (let ((p (text-property-any (point-min) (point-max)
                              'stgit-patchsym patchsym)))
    (when p
      (goto-char p)
      (move-to-column goal-column))))

(defun stgit-init ()
  "Run stg init."
  (interactive)
  (stgit-capture-output nil
    (stgit-run "init"))
  (stgit-reload))

(defun stgit-mark ()
  "Mark the patch under point."
  (interactive)
  (let ((patch (stgit-patch-at-point t)))
    (stgit-add-mark patch))
  (stgit-next-patch))

(defun stgit-unmark-up ()
  "Remove mark from the patch on the previous line."
  (interactive)
  (stgit-previous-patch)
  (stgit-remove-mark (stgit-patch-at-point t)))

(defun stgit-unmark-down ()
  "Remove mark from the patch on the current line."
  (interactive)
  (stgit-remove-mark (stgit-patch-at-point t))
  (stgit-next-patch))

(defun stgit-rename (name)
  "Rename the patch under point to NAME."
  (interactive (list (read-string "Patch name: "
                                  (symbol-name (stgit-patch-at-point t)))))
  (let ((old-patchsym (stgit-patch-at-point t)))
    (stgit-capture-output nil
      (stgit-run "rename" old-patchsym name))
    (let ((name-sym (intern name)))
      (when (memq old-patchsym stgit-expanded-patches)
        (setq stgit-expanded-patches
            (cons name-sym (delq old-patchsym stgit-expanded-patches))))
      (when (memq old-patchsym stgit-marked-patches)
        (setq stgit-marked-patches
            (cons name-sym (delq old-patchsym stgit-marked-patches))))
      (stgit-reload)
      (stgit-goto-patch name-sym))))

(defun stgit-repair ()
  "Run stg repair."
  (interactive)
  (stgit-capture-output nil
    (stgit-run "repair"))
  (stgit-reload))

(defun stgit-available-branches ()
  "Returns a list of the available stg branches"
  (let ((output (with-output-to-string
                  (stgit-run "branch" "--list")))
        (start 0)
        result)
    (while (string-match "^>?\\s-+s\\s-+\\(\\S-+\\)" output start)
      (setq result (cons (match-string 1 output) result))
      (setq start (match-end 0)))
    result))

(defun stgit-branch (branch)
  "Switch to branch BRANCH."
  (interactive (list (completing-read "Switch to branch: "
                                      (stgit-available-branches))))
  (stgit-capture-output nil (stgit-run "branch" "--" branch))
  (stgit-reload))

(defun stgit-commit (count)
  "Run stg commit on COUNT commits.
Interactively, the prefix argument is used as COUNT."
  (interactive "p")
  (stgit-capture-output nil (stgit-run "commit" "-n" count))
  (stgit-reload))

(defun stgit-uncommit (count)
  "Run stg uncommit on COUNT commits.
Interactively, the prefix argument is used as COUNT."
  (interactive "p")
  (stgit-capture-output nil (stgit-run "uncommit" "-n" count))
  (stgit-reload))

(defun stgit-push-next (npatches)
  "Push the first unapplied patch.
With numeric prefix argument, push that many patches."
  (interactive "p")
  (stgit-capture-output nil (stgit-run "push" "-n" npatches))
  (stgit-reload)
  (stgit-refresh-git-status))

(defun stgit-pop-next (npatches)
  "Pop the topmost applied patch.
With numeric prefix argument, pop that many patches."
  (interactive "p")
  (stgit-capture-output nil (stgit-run "pop" "-n" npatches))
  (stgit-reload)
  (stgit-refresh-git-status))

(defun stgit-applied-at-point ()
  "Is the patch on the current line applied?"
  (save-excursion
    (beginning-of-line)
    (looking-at "[>+]")))

(defun stgit-push-or-pop ()
  "Push or pop the patch on the current line."
  (interactive)
  (let ((patchsym (stgit-patch-at-point t))
        (applied (stgit-applied-at-point)))
    (stgit-capture-output nil
      (stgit-run (if applied "pop" "push") patchsym))
    (stgit-reload)))

(defun stgit-goto ()
  "Go to the patch on the current line."
  (interactive)
  (let ((patchsym (stgit-patch-at-point t)))
    (stgit-capture-output nil
      (stgit-run "goto" patchsym))
    (stgit-reload)))

(defun stgit-id (patchsym)
  "Return the git commit id for PATCHSYM."
  (let ((result (with-output-to-string
                  (stgit-run-silent "id" patchsym))))
    (unless (string-match "^\\([0-9A-Fa-f]\\{40\\}\\)$" result)
      (error "Cannot find commit id for %s" patchsym))
    (match-string 1 result)))

(defun stgit-show ()
  "Show the patch on the current line."
  (interactive)
  (stgit-capture-output "*StGit patch*"
    (let ((patchsym (stgit-patch-at-point)))
      (if (not patchsym)
          (let ((patched-file (stgit-patched-file-at-point t)))
            (unless patched-file
              (error "No patch or file at point"))
            (let ((id (stgit-id (car patched-file))))
	      (if (consp (cdr patched-file))
		  ;; two files (copy or rename)
		  (stgit-run-git "diff" "-C" "-C" (concat id "^") id "--"
				 (cadr patched-file) (cddr patched-file))
		;; just one file
		(stgit-run-git "diff" (concat id "^") id "--"
			       (cdr patched-file)))))
        (stgit-run "show" "-O" "--patch-with-stat" "-O" "-M" patchsym))
      (with-current-buffer standard-output
	(goto-char (point-min))
	(diff-mode)))))

(defun stgit-edit ()
  "Edit the patch on the current line."
  (interactive)
  (let ((patchsym (stgit-patch-at-point t))
        (edit-buf (get-buffer-create "*StGit edit*"))
        (dir default-directory))
    (log-edit 'stgit-confirm-edit t nil edit-buf)
    (set (make-local-variable 'stgit-edit-patchsym) patchsym)
    (setq default-directory dir)
    (let ((standard-output edit-buf))
      (stgit-run-silent "edit" "--save-template=-" patchsym))))

(defun stgit-confirm-edit ()
  (interactive)
  (let ((file (make-temp-file "stgit-edit-")))
    (write-region (point-min) (point-max) file)
    (stgit-capture-output nil
      (stgit-run "edit" "-f" file stgit-edit-patchsym))
    (with-current-buffer log-edit-parent-buffer
      (stgit-reload))))

(defun stgit-new (add-sign)
  "Create a new patch.
With a prefix argument, include a \"Signed-off-by:\" line at the
end of the patch."
  (interactive "P")
  (let ((edit-buf (get-buffer-create "*StGit edit*"))
        (dir default-directory))
    (log-edit 'stgit-confirm-new t nil edit-buf)
    (setq default-directory dir)
    (when add-sign
      (save-excursion
        (let ((standard-output (current-buffer)))
          (stgit-run-silent "new" "--sign" "--save-template=-"))))))

(defun stgit-confirm-new ()
  (interactive)
  (let ((file (make-temp-file "stgit-edit-")))
    (write-region (point-min) (point-max) file)
    (stgit-capture-output nil
      (stgit-run "new" "-f" file))
    (with-current-buffer log-edit-parent-buffer
      (stgit-reload))))

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
  (interactive (list (stgit-patches-marked-or-at-point)
                     current-prefix-arg))
  (unless patchsyms
    (error "No patches to delete"))
  (let ((npatches (length patchsyms)))
    (when (yes-or-no-p (format "Really delete %d patch%s%s? "
			       npatches
			       (if (= 1 npatches) "" "es")
                               (if spill-p
                                   " (spilling contents to index)"
                                 "")))
      (let ((args (if spill-p 
                      (cons "--spill" patchsyms)
                    patchsyms)))
        (stgit-capture-output nil
          (apply 'stgit-run "delete" args))
        (stgit-reload)))))

(defun stgit-move-patches-target ()
  "Return the patchsym indicating a target patch for
`stgit-move-patches'.

This is either the patch at point, or one of :top and :bottom, if
the point is after or before the applied patches."

  (let ((patchsym (stgit-patch-at-point)))
    (cond (patchsym patchsym)
	  ((save-excursion (re-search-backward "^>" nil t)) :top)
	  (t :bottom))))

(defun stgit-sort-patches (patchsyms)
  "Returns the list of patches in PATCHSYMS sorted according to
their position in the patch series, bottommost first.

PATCHSYMS may not contain duplicate entries."
  (let (sorted-patchsyms
        (series (with-output-to-string
                  (with-current-buffer standard-output
                    (stgit-run-silent "series" "--noprefix"))))
        start)
    (while (string-match "^\\(.+\\)" series start)
      (let ((patchsym (intern (match-string 1 series))))
        (when (memq patchsym patchsyms)
          (setq sorted-patchsyms (cons patchsym sorted-patchsyms))))
      (setq start (match-end 0)))
    (setq sorted-patchsyms (nreverse sorted-patchsyms))

    (unless (= (length patchsyms) (length sorted-patchsyms))
      (error "Internal error"))

    sorted-patchsyms))

(defun stgit-move-patches (patchsyms target-patch)
  "Move the patches in PATCHSYMS to below TARGET-PATCH.
If TARGET-PATCH is :bottom or :top, move the patches to the
bottom or top of the stack, respectively.

Interactively, move the marked patches to where the point is."
  (interactive (list stgit-marked-patches
                     (stgit-move-patches-target)))
  (unless patchsyms
    (error "Need at least one patch to move"))

  (unless target-patch
    (error "Point not at a patch"))

  (if (eq target-patch :top)
      (stgit-capture-output nil
        (apply 'stgit-run "float" patchsyms))

    ;; need to have patchsyms sorted by position in the stack
    (let ((sorted-patchsyms (stgit-sort-patches patchsyms)))
      (while sorted-patchsyms
        (setq sorted-patchsyms
              (and (stgit-capture-output nil
                     (if (eq target-patch :bottom)
                         (stgit-run "sink" "--" (car sorted-patchsyms))
                       (stgit-run "sink" "--to" target-patch "--"
                                  (car sorted-patchsyms))))
                   (cdr sorted-patchsyms))))))
  (stgit-reload))

(defun stgit-squash (patchsyms)
  "Squash the patches in PATCHSYMS.
Interactively, squash the marked patches.

Unless there are any conflicts, the patches will be merged into
one patch, which will occupy the same spot in the series as the
deepest patch had before the squash."
  (interactive (list stgit-marked-patches))
  (when (< (length patchsyms) 2)
    (error "Need at least two patches to squash"))
  (let ((edit-buf (get-buffer-create "*StGit edit*"))
        (dir default-directory)
        (sorted-patchsyms (stgit-sort-patches patchsyms)))
    (log-edit 'stgit-confirm-squash t nil edit-buf)
    (set (make-local-variable 'stgit-patchsyms) sorted-patchsyms)
    (setq default-directory dir)
    (let ((standard-output edit-buf))
      (apply 'stgit-run-silent "squash" "--save-template=-" sorted-patchsyms))))

(defun stgit-confirm-squash ()
  (interactive)
  (let ((file (make-temp-file "stgit-edit-")))
    (write-region (point-min) (point-max) file)
    (stgit-capture-output nil
      (apply 'stgit-run "squash" "-f" file stgit-patchsyms))
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

(defun stgit-undo (&optional arg)
  "Run stg undo.
With prefix argument, run it with the --hard flag."
  (interactive "P")
  (stgit-capture-output nil
    (if arg
        (stgit-run "undo" "--hard")
      (stgit-run "undo")))
  (stgit-reload))

(defun stgit-refresh (&optional arg)
  "Run stg refresh.
With prefix argument, refresh the marked patch or the patch under point."
  (interactive "P")
  (let ((patchargs (if arg
                       (let ((patches (stgit-patches-marked-or-at-point)))
                         (cond ((null patches)
                                (error "No patch to update"))
                               ((> (length patches) 1)
                                (error "Too many patches selected"))
                               (t
                                (cons "-p" patches))))
                     nil)))
    (stgit-capture-output nil
      (apply 'stgit-run "refresh" patchargs))
    (stgit-refresh-git-status))
  (stgit-reload))

(provide 'stgit)
