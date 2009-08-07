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

(when (< emacs-major-version 22)
  (error "Emacs older than 22 is not supported by stgit.el"))

(require 'git nil t)
(require 'cl)
(require 'ewoc)

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

(defun stgit-find-buffer (dir)
  "Return the buffer displaying StGit patches for DIR, or nil if none."
  (setq dir (file-name-as-directory dir))
  (let ((buffers (buffer-list)))
    (while (and buffers
                (not (with-current-buffer (car buffers)
                       (and (eq major-mode 'stgit-mode)
                            (string= default-directory dir)))))
      (setq buffers (cdr buffers)))
    (and buffers (car buffers))))

(defun switch-to-stgit-buffer (dir)
  "Switch to a (possibly new) buffer displaying StGit patches for DIR."
  (setq dir (file-name-as-directory dir))
  (let ((buffer (stgit-find-buffer dir)))
    (switch-to-buffer (or buffer
			  (create-stgit-buffer dir)))))

(defstruct (stgit-patch)
  status name desc empty files-ewoc)

(defun stgit-patch-pp (patch)
  (let* ((status (stgit-patch-status patch))
         (start (point))
         (name (stgit-patch-name patch))
         (face (cdr (assq status stgit-patch-status-face-alist))))
    (insert (case status
              ('applied "+")
              ('top ">")
              ('unapplied "-")
              (t " "))
            (if (memq name stgit-marked-patches)
                "*" " "))
    (if (memq status '(index work))
        (insert (propertize (if (eq status 'index) "Index" "Work tree")
                            'face face))
      (insert (format "%-30s"
                      (propertize (symbol-name name)
                                  'face face
                                  'syntax-table (string-to-syntax "w")))
              "  "
              (if (stgit-patch-empty patch) "(empty) " "")
              (propertize (or (stgit-patch-desc patch) "")
                          'face 'stgit-description-face)))
    (insert "\n")
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

(defmacro stgit-capture-output (name &rest body)
  "Capture StGit output and, if there was any output, show it in a window
at the end.
Returns nil if there was no output."
  (declare (debug ([&or stringp null] body))
           (indent 1))
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

(defun stgit-index-empty-p ()
  "Returns non-nil if the index contains no changes from HEAD."
  (zerop (stgit-run-git-silent "diff-index" "--cached" "--quiet" "HEAD")))

(defvar stgit-index-node)
(defvar stgit-worktree-node)

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

(defun stgit-run-series (ewoc)
  (setq stgit-index-node nil
        stgit-worktree-node nil)
  (let ((inserted-index (not stgit-show-worktree))
        index-node
        worktree-node
        all-patchsyms)
    (with-temp-buffer
      (let ((exit-status (stgit-run-silent "series" "--description" "--empty")))
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
            (forward-line 1))))
      (unless inserted-index
        (stgit-run-series-insert-index ewoc)))
    (setq stgit-index-node    index-node
          stgit-worktree-node worktree-node
          stgit-marked-patches (intersection stgit-marked-patches
                                             all-patchsyms))))

(defun stgit-reload ()
  "Update the contents of the StGit buffer."
  (interactive)
  (let ((inhibit-read-only t)
        (curline (line-number-at-pos))
        (curpatch (stgit-patch-name-at-point))
        (curfile (stgit-patched-file-at-point)))
    (ewoc-filter stgit-ewoc #'(lambda (x) nil))
    (ewoc-set-hf stgit-ewoc
                 (concat "Branch: "
                         (propertize
                          (with-temp-buffer
                            (stgit-run-silent "branch")
                            (buffer-substring (point-min) (1- (point-max))))
                          'face 'stgit-branch-name-face)
                         "\n\n")
                 (if stgit-show-worktree
                     "--"
                   (propertize
                    (substitute-command-keys "--\n\"\\[stgit-toggle-worktree]\"\
 shows the working tree\n")
                   'face 'stgit-description-face)))
    (stgit-run-series stgit-ewoc)
    (if curpatch
        (stgit-goto-patch curpatch (and curfile (stgit-file-file curfile)))
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

(defface stgit-branch-name-face
  '((t :inherit bold))
  "The face used for the StGit branch name"
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

(defface stgit-ignored-file-face
  '((((class color) (background light)) (:foreground "grey60"))
    (((class color) (background dark)) (:foreground "grey40")))
  "StGit mode face used for ignored files")

(defface stgit-file-permission-face
  '((((class color) (background light)) (:foreground "green" :bold t))
    (((class color) (background dark)) (:foreground "green" :bold t)))
  "StGit mode face used for permission changes."
  :group 'stgit)

(defface stgit-index-work-tree-title-face
  '((((supports :slant italic)) :slant italic)
    (t :inherit bold))
  "StGit mode face used for the \"Index\" and \"Work tree\" titles"
  :group 'stgit)


(defcustom stgit-find-copies-harder
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
            (unknown     "Unknown"     stgit-unknown-file-face)
            (ignore      "Ignored"     stgit-ignored-file-face)))
  "Alist of code symbols to description strings")

(defconst stgit-patch-status-face-alist
  '((applied   . stgit-applied-patch-face)
    (top       . stgit-top-patch-face)
    (unapplied . stgit-unapplied-patch-face)
    (index     . stgit-index-work-tree-title-face)
    (work      . stgit-index-work-tree-title-face))
  "Alist of face to use for a given patch status")

(defun stgit-file-status-code-as-string (file)
  "Return stgit status code for FILE as a string"
  (let* ((code (assq (stgit-file-status file)
                     stgit-file-status-code-strings))
         (score (stgit-file-cr-score file)))
    (when code
      (format "%-11s  "
              (if (and score (/= score 100))
                  (format "%s %s" (cdr code)
                          (propertize (format "%d%%" score)
                                      'face 'stgit-description-face))
                (cdr code))))))

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

(defstruct (stgit-file)
  old-perm new-perm copy-or-rename cr-score cr-from cr-to status file)

(defun stgit-describe-copy-or-rename (file)
  (let (from to common-head common-tail
        (arrow (concat " "
                       (propertize "->" 'face 'stgit-description-face)
                       " ")))

    (when stgit-abbreviate-copies-and-renames
      (setq from (split-string (stgit-file-cr-from file) "/")
            to   (split-string (stgit-file-cr-to   file) "/"))

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
        (concat (if common-head
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
                  ""))
      (concat (stgit-file-cr-from file) arrow (stgit-file-cr-to file)))))

(defun stgit-file-pp (file)
  (let ((status (stgit-file-status file))
        (name (if (stgit-file-copy-or-rename file)
                  (stgit-describe-copy-or-rename file)
                (stgit-file-file file)))
        (mode-change (stgit-file-mode-change-string
                      (stgit-file-old-perm file)
                      (stgit-file-new-perm file)))
        (start (point)))
    (insert (format "    %-12s%s%s%s%s\n"
                    (stgit-file-status-code-as-string file)
                    mode-change
                    (if (zerop (length mode-change)) "" " ")
                    name
                    (propertize (stgit-file-type-change-string
                                 (stgit-file-old-perm file)
                                 (stgit-file-new-perm file))
                                'face 'stgit-description-face)))
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

(defun stgit-insert-patch-files (patch)
  "Expand (show modification of) the patch PATCH after the line
at point."
  (let* ((patchsym (stgit-patch-name patch))
         (end      (point-marker))
         (args     (list "-z" (stgit-find-copies-harder-diff-arg)))
         (ewoc     (ewoc-create #'stgit-file-pp nil nil t)))
    (set-marker-insertion-type end t)
    (setf (stgit-patch-files-ewoc patch) ewoc)
    (with-temp-buffer
      (apply 'stgit-run-git
             (cond ((eq patchsym :work)
                    `("diff-files" "-0" ,@args))
                   ((eq patchsym :index)
                    `("diff-index" ,@args "--cached" "HEAD"))
                   (t
                    `("diff-tree" ,@args "-r" ,(stgit-id patchsym)))))

      (when (and (eq patchsym :work))
        (when stgit-show-ignored
          (stgit-insert-ls-files '("--ignored" "--others") "I"))
        (when stgit-show-unknown
          (stgit-insert-ls-files '("--others") "X"))
        (sort-regexp-fields nil ":[^\0]*\0\\([^\0]*\\)\0" "\\1"
                            (point-min) (point-max)))

      (goto-char (point-min))
      (unless (or (eobp) (memq patchsym '(:work :index)))
        (forward-char 41))
      (while (looking-at ":\\([0-7]+\\) \\([0-7]+\\) [0-9A-Fa-f]\\{40\\} [0-9A-Fa-f]\\{40\\} ")
        (let ((old-perm (string-to-number (match-string 1) 8))
              (new-perm (string-to-number (match-string 2) 8)))
          (goto-char (match-end 0))
          (let ((file
                 (cond ((looking-at
                         "\\([CR]\\)\\([0-9]*\\)\0\\([^\0]*\\)\0\\([^\0]*\\)\0")
                        (let* ((patch-status (stgit-patch-status patch))
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
            (ewoc-enter-last ewoc file))))

      (unless (ewoc-nth ewoc 0)
        (ewoc-set-hf ewoc ""
                     (concat "    "
                             (propertize "<no files>"
                                         'face 'stgit-description-face)
                             "\n"))))
    (goto-char end)))

(defun stgit-find-file (&optional other-window)
  (let* ((file (or (stgit-patched-file-at-point)
                   (error "No file at point")))
         (filename (expand-file-name (stgit-file-file file))))
    (unless (file-exists-p filename)
      (error "File does not exist"))
    (funcall (if other-window 'find-file-other-window 'find-file)
             filename)
    (when (eq (stgit-file-status file) 'unmerged)
      (smerge-mode 1))))

(defun stgit-select-patch ()
  (let ((patchname (stgit-patch-name-at-point)))
    (if (memq patchname stgit-expanded-patches)
        (setq stgit-expanded-patches (delq patchname stgit-expanded-patches))
      (setq stgit-expanded-patches (cons patchname stgit-expanded-patches)))
    (ewoc-invalidate stgit-ewoc (ewoc-locate stgit-ewoc)))
  (move-to-column (stgit-goal-column)))

(defun stgit-select ()
  "With point on a patch, toggle showing files in the patch.

With point on a file, open the associated file. Opens the target
file for (applied) copies and renames."
  (interactive)
  (case (get-text-property (point) 'entry-type)
    ('patch
     (stgit-select-patch))
    ('file
     (stgit-find-file))
    (t
     (error "No patch or file on line"))))

(defun stgit-find-file-other-window ()
  "Open file at point in other window"
  (interactive)
  (stgit-find-file t))

(defun stgit-find-file-merge ()
  "Open file at point and merge it using `smerge-ediff'."
  (interactive)
  (stgit-find-file t)
  (smerge-ediff))

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

(defun stgit-goal-column ()
  "Return goal column for the current line"
  (case (get-text-property (point) 'entry-type)
    ('patch 2)
    ('file 4)
    (t 0)))

(defun stgit-next-line (&optional arg)
  "Move cursor vertically down ARG lines"
  (interactive "p")
  (next-line arg)
  (move-to-column (stgit-goal-column)))

(defun stgit-previous-line (&optional arg)
  "Move cursor vertically up ARG lines"
  (interactive "p")
  (previous-line arg)
  (move-to-column (stgit-goal-column)))

(defun stgit-next-patch (&optional arg)
  "Move cursor down ARG patches."
  (interactive "p")
  (ewoc-goto-next stgit-ewoc (or arg 1))
  (move-to-column goal-column))

(defun stgit-previous-patch (&optional arg)
  "Move cursor up ARG patches."
  (interactive "p")
  (ewoc-goto-prev stgit-ewoc (or arg 1))
  (move-to-column goal-column))

(defvar stgit-mode-hook nil
  "Run after `stgit-mode' is setup.")

(defvar stgit-mode-map nil
  "Keymap for StGit major mode.")

(unless stgit-mode-map
  (let ((diff-map   (make-keymap))
        (toggle-map (make-keymap)))
    (suppress-keymap diff-map)
    (mapc (lambda (arg) (define-key diff-map (car arg) (cdr arg)))
          '(("b" .        stgit-diff-base)
            ("c" .        stgit-diff-combined)
            ("m" .        stgit-find-file-merge)
            ("o" .        stgit-diff-ours)
            ("t" .        stgit-diff-theirs)))
    (suppress-keymap toggle-map)
    (mapc (lambda (arg) (define-key toggle-map (car arg) (cdr arg)))
          '(("t" .        stgit-toggle-worktree)
            ("i" .        stgit-toggle-ignored)
            ("u" .        stgit-toggle-unknown)))
    (setq stgit-mode-map (make-keymap))
    (suppress-keymap stgit-mode-map)
    (mapc (lambda (arg) (define-key stgit-mode-map (car arg) (cdr arg)))
          `((" " .        stgit-mark)
            ("m" .        stgit-mark)
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
            ("\M-{" .     stgit-previous-patch)
            ("\M-}" .     stgit-next-patch)
            ("s" .        stgit-git-status)
            ("g" .        stgit-reload-or-repair)
            ("r" .        stgit-refresh)
            ("\C-c\C-r" . stgit-rename)
            ("e" .        stgit-edit)
            ("M" .        stgit-move-patches)
            ("S" .        stgit-squash)
            ("N" .        stgit-new)
            ("\C-c\C-c" . stgit-commit)
            ("\C-c\C-u" . stgit-uncommit)
            ("U" .        stgit-revert-file)
            ("R" .        stgit-resolve-file)
            ("\r" .       stgit-select)
            ("o" .        stgit-find-file-other-window)
            ("i" .        stgit-file-toggle-index)
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
            ("t" .        ,toggle-map)
            ("d" .        ,diff-map)
            ("q" .        stgit-quit)))))

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
  (set (make-local-variable 'stgit-expanded-patches) (list :work :index))
  (set (make-local-variable 'stgit-show-worktree) stgit-default-show-worktree)
  (set (make-local-variable 'stgit-index-node) nil)
  (set (make-local-variable 'stgit-worktree-node) nil)
  (set (make-local-variable 'parse-sexp-lookup-properties) t)
  (set-variable 'truncate-lines 't)
  (add-hook 'after-save-hook 'stgit-update-saved-file)
  (run-hooks 'stgit-mode-hook))

(defun stgit-update-saved-file ()
  (let* ((file (expand-file-name buffer-file-name))
         (dir (file-name-directory file))
         (gitdir (condition-case nil (git-get-top-dir dir)
                   (error nil)))
	 (buffer (and gitdir (stgit-find-buffer gitdir))))
    (when buffer
      (with-current-buffer buffer
        (stgit-refresh-worktree)))))

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

(defun stgit-patch-name-at-point (&optional cause-error only-patches)
  "Return the patch name on the current line as a symbol.
If CAUSE-ERROR is not nil, signal an error if none found.
If ONLY-PATCHES is not nil, only allow real patches, and not
index or work tree."
  (let ((patch (stgit-patch-at-point)))
    (and patch
         only-patches
         (memq (stgit-patch-status patch) '(work index))
         (setq patch nil))
    (cond (patch
           (stgit-patch-name patch))
          (cause-error
           (error "No patch on this line")))))

(defun stgit-patched-file-at-point ()
  (get-text-property (point) 'file-data))

(defun stgit-patches-marked-or-at-point ()
  "Return the symbols of the marked patches, or the patch on the current line."
  (if stgit-marked-patches
      stgit-marked-patches
    (let ((patch (stgit-patch-name-at-point)))
      (if patch
          (list patch)
        '()))))

(defun stgit-goto-patch (patchsym &optional file)
  "Move point to the line containing patch PATCHSYM.
If that patch cannot be found, do nothing.

If the patch was found and FILE is not nil, instead move to that
file's line. If FILE cannot be found, stay on the line of
PATCHSYM."
  (let ((node (ewoc-nth stgit-ewoc 0)))
    (while (and node (not (eq (stgit-patch-name (ewoc-data node))
                              patchsym)))
      (setq node (ewoc-next stgit-ewoc node)))
    (when (and node file)
      (let* ((file-ewoc (stgit-patch-files-ewoc (ewoc-data node)))
             (file-node (ewoc-nth file-ewoc 0)))
        (while (and file-node (not (equal (stgit-file-file (ewoc-data file-node)) file)))
          (setq file-node (ewoc-next file-ewoc file-node)))
        (when file-node
          (ewoc-goto-node file-ewoc file-node)
          (move-to-column (stgit-goal-column))
          (setq node nil))))
    (when node
      (ewoc-goto-node stgit-ewoc node)
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
  (let* ((node (ewoc-locate stgit-ewoc))
         (patch (ewoc-data node))
         (name (stgit-patch-name patch)))
    (when (eq name :work)
      (error "Cannot mark the work tree"))
    (when (eq name :index)
      (error "Cannot mark the index"))
    (stgit-add-mark (stgit-patch-name patch))
    (ewoc-invalidate stgit-ewoc node))
  (stgit-next-patch))

(defun stgit-unmark-up ()
  "Remove mark from the patch on the previous line."
  (interactive)
  (stgit-previous-patch)
  (let* ((node (ewoc-locate stgit-ewoc))
         (patch (ewoc-data node)))
    (stgit-remove-mark (stgit-patch-name patch))
    (ewoc-invalidate stgit-ewoc node))
  (move-to-column (stgit-goal-column)))

(defun stgit-unmark-down ()
  "Remove mark from the patch on the current line."
  (interactive)
  (let* ((node (ewoc-locate stgit-ewoc))
         (patch (ewoc-data node)))
    (stgit-remove-mark (stgit-patch-name patch))
    (ewoc-invalidate stgit-ewoc node))
  (stgit-next-patch))

(defun stgit-rename (name)
  "Rename the patch under point to NAME."
  (interactive (list
                (read-string "Patch name: "
                             (symbol-name (stgit-patch-name-at-point t t)))))
  (let ((old-patchsym (stgit-patch-name-at-point t t)))
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

(defun stgit-reload-or-repair (repair)
  "Update the contents of the StGit buffer (`stgit-reload').

With a prefix argument, repair the StGit metadata if the branch
was modified with git commands (`stgit-repair')."
  (interactive "P")
  (if repair
      (stgit-repair)
    (stgit-reload)))

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
Interactively, the prefix argument is used as COUNT.
A negative COUNT will uncommit instead."
  (interactive "p")
  (if (< count 0)
      (stgit-uncommit (- count))
    (stgit-capture-output nil (stgit-run "commit" "-n" count))
    (stgit-reload)))

(defun stgit-uncommit (count)
  "Run stg uncommit on COUNT commits.
Interactively, the prefix argument is used as COUNT.
A negative COUNT will commit instead."
  (interactive "p")
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
           (and f (setq neighbour-file (stgit-file-file f)))))
    (goto-char old-point)
    (unless neighbour-file
      (and (zerop (forward-line -1))
           (let ((f (stgit-patched-file-at-point)))
             (and f (setq neighbour-file (stgit-file-file f)))))
      (goto-char old-point))
    neighbour-file))

(defun stgit-revert-file ()
  "Revert the file at point, which must be in the index or the
working tree."
  (interactive)
  (let* ((patched-file (or (stgit-patched-file-at-point)
                           (error "No file on the current line")))
         (patch-name   (stgit-patch-name-at-point))
         (file-status  (stgit-file-status patched-file))
         (rm-file      (cond ((stgit-file-copy-or-rename patched-file)
                              (stgit-file-cr-to patched-file))
                             ((eq file-status 'add)
                              (stgit-file-file patched-file))))
         (co-file      (cond ((eq file-status 'rename)
                              (stgit-file-cr-from patched-file))
                             ((not (memq file-status '(copy add)))
                              (stgit-file-file patched-file))))
         (next-file    (stgit-neighbour-file)))

    (unless (memq patch-name '(:work :index))
      (error "No index or working tree file on this line"))

    (when (eq file-status 'ignore)
      (error "Cannot revert ignored files"))

    (when (eq file-status 'unknown)
      (error "Cannot revert unknown files"))

    (let ((nfiles (+ (if rm-file 1 0) (if co-file 1 0))))
      (when (yes-or-no-p (format "Revert %d file%s? "
                                 nfiles
                                 (if (= nfiles 1) "" "s")))
        (stgit-capture-output nil
          (when rm-file
            (stgit-run-git "rm" "-f" "-q" "--" rm-file))
          (when co-file
            (stgit-run-git "checkout" "HEAD" co-file)))
        (stgit-reload)
        (stgit-goto-patch patch-name next-file)))))

(defun stgit-resolve-file ()
  "Resolve conflict in the file at point."
  (interactive)
  (let* ((patched-file (stgit-patched-file-at-point))
         (patch        (stgit-patch-at-point))
         (patch-name   (and patch (stgit-patch-name patch)))
         (status       (and patched-file (stgit-file-status patched-file))))

    (unless (memq patch-name '(:work :index))
      (error "No index or working tree file on this line"))

    (unless (eq status 'unmerged)
      (error "No conflict to resolve at the current line"))

    (stgit-capture-output nil
      (stgit-move-change-to-index (stgit-file-file patched-file)))

    (stgit-reload)))

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
  (let ((patchsym (stgit-patch-name-at-point t))
        (applied (stgit-applied-at-point)))
    (stgit-capture-output nil
      (stgit-run (if applied "pop" "push") patchsym))
    (stgit-reload)))

(defun stgit-goto ()
  "Go to the patch on the current line."
  (interactive)
  (let ((patchsym (stgit-patch-name-at-point t)))
    (stgit-capture-output nil
      (stgit-run "goto" patchsym))
    (stgit-reload)))

(defun stgit-id (patchsym)
  "Return the git commit id for PATCHSYM.
If PATCHSYM is a keyword, returns PATCHSYM unmodified."
  (if (keywordp patchsym)
      patchsym
    (let ((result (with-output-to-string
		    (stgit-run-silent "id" patchsym))))
      (unless (string-match "^\\([0-9A-Fa-f]\\{40\\}\\)$" result)
	(error "Cannot find commit id for %s" patchsym))
      (match-string 1 result))))

(defun stgit-show-patch (unmerged-stage ignore-whitespace)
  "Show the patch on the current line.

UNMERGED-STAGE is the argument to `git-diff' that that selects
which stage to diff against in the case of unmerged files."
  (let ((space-arg (when (numberp ignore-whitespace)
                     (cond ((> ignore-whitespace 4)
                            "--ignore-all-space")
                           ((> ignore-whitespace 1)
                            "--ignore-space-change"))))
        (patch-name (stgit-patch-name-at-point t)))
    (stgit-capture-output "*StGit patch*"
      (case (get-text-property (point) 'entry-type)
        ('file
         (let* ((patched-file (stgit-patched-file-at-point))
                (patch-id (let ((id (stgit-id patch-name)))
                            (if (and (eq id :index)
                                     (eq (stgit-file-status patched-file)
                                         'unmerged))
                                :work
                              id)))
                (args (append (and space-arg (list space-arg))
                              (and (stgit-file-cr-from patched-file)
                                   (list (stgit-find-copies-harder-diff-arg)))
                              (cond ((eq patch-id :index)
                                     '("--cached"))
                                    ((eq patch-id :work)
                                     (list unmerged-stage))
                                    (t
                                     (list (concat patch-id "^") patch-id)))
                              '("--")
                              (if (stgit-file-copy-or-rename patched-file)
                                  (list (stgit-file-cr-from patched-file)
                                        (stgit-file-cr-to patched-file))
                                (list (stgit-file-file patched-file))))))
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
             (let ((args (append '("show" "-O" "--patch-with-stat" "-O" "-M")
                                 (and space-arg (list "-O" space-arg))
                                 (list (stgit-patch-name-at-point)))))
               (apply 'stgit-run args)))))
         (t
          (error "No patch or file at point")))
      (with-current-buffer standard-output
        (goto-char (point-min))
        (diff-mode)))))

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

(defun stgit-file-toggle-index ()
  "Move modified file in or out of the index.

Leaves the point where it is, but moves the mark to where the
file ended up. You can then jump to the file with \
\\[exchange-point-and-mark]."
  (interactive)
  (let* ((patched-file   (or (stgit-patched-file-at-point)
			     (error "No file on the current line")))
	 (patched-status (stgit-file-status patched-file)))
    (when (eq patched-status 'unmerged)
      (error (substitute-command-keys "Use \\[stgit-resolve-file] to move an unmerged file to the index")))
    (let* ((patch      (stgit-patch-at-point))
           (patch-name (stgit-patch-name patch))
           (mark-file  (if (eq patched-status 'rename)
			   (stgit-file-cr-to patched-file)
			 (stgit-file-file patched-file)))
           (point-file  (if (eq patched-status 'rename)
			   (stgit-file-cr-from patched-file)
			 (stgit-neighbour-file))))

      (cond ((eq patch-name :work)
             (stgit-move-change-to-index (stgit-file-file patched-file)
                                         (eq patched-status 'ignore)))
            ((eq patch-name :index)
             (stgit-remove-change-from-index (stgit-file-file patched-file)))
            (t
             (error "Can only move files between working tree and index")))
      (stgit-refresh-worktree)
      (stgit-refresh-index)
      (stgit-goto-patch (if (eq patch-name :index) :work :index) mark-file)
      (push-mark nil t t)
      (stgit-goto-patch patch-name point-file))))

(defun stgit-edit ()
  "Edit the patch on the current line."
  (interactive)
  (let ((patchsym (stgit-patch-name-at-point t t))
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

  (let ((patchsym (stgit-patch-name-at-point)))
    (cond (patchsym patchsym)
	  ((save-excursion (re-search-backward "^>" nil t)) :top)
	  (t :bottom))))

(defun stgit-sort-patches (patchsyms)
  "Returns the list of patches in PATCHSYMS sorted according to
their position in the patch series, bottommost first.

PATCHSYMS must not contain duplicate entries."
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
  (let ((stgit-buffer (current-buffer))
        (edit-buf (get-buffer-create "*StGit edit*"))
        (dir default-directory)
        (sorted-patchsyms (stgit-sort-patches patchsyms)))
    (log-edit 'stgit-confirm-squash t nil edit-buf)
    (set (make-local-variable 'stgit-patchsyms) sorted-patchsyms)
    (setq default-directory dir)
    (let ((result (let ((standard-output edit-buf))
                    (apply 'stgit-run-silent "squash"
                           "--save-template=-" sorted-patchsyms))))

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
With prefix argument, run it with the --hard flag.

See also `stgit-redo'."
  (interactive "P")
  (stgit-capture-output nil
    (if arg
        (stgit-run "undo" "--hard")
      (stgit-run "undo")))
  (stgit-reload))

(defun stgit-redo (&optional arg)
  "Run stg redo.
With prefix argument, run it with the --hard flag.

See also `stgit-undo'."
  (interactive "P")
  (stgit-capture-output nil
    (if arg
        (stgit-run "redo" "--hard")
      (stgit-run "redo")))
  (stgit-reload))

(defun stgit-refresh (&optional arg)
  "Run stg refresh.
If the index contains any changes, only refresh from index.

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
    (unless (stgit-index-empty-p)
      (setq patchargs (cons "--index" patchargs)))
    (stgit-capture-output nil
      (apply 'stgit-run "refresh" patchargs))
    (stgit-refresh-git-status))
  (stgit-reload))

(defcustom stgit-show-worktree-mode 'center
  "This variable controls where the \"Index\" and \"Work tree\"
will be shown on in the buffer.

It can be set to 'top (above all patches), 'center (show between
applied and unapplied patches), and 'bottom (below all patches).

See also `stgit-show-worktree'."
  :type '(radio (const :tag "above all patches (top)" top)
                (const :tag "between applied and unapplied patches (center)"
                       center)
                (const :tag "below all patches (bottom)" bottom))
  :group 'stgit)

(defcustom stgit-default-show-worktree
  t
  "Set to non-nil to by default show the working tree in a new stgit buffer.

This value is used as the default value for `stgit-show-worktree'."
  :type 'boolean
  :group 'stgit)

(defvar stgit-show-worktree nil
  "If nil, inhibit showing work tree and index in the stgit buffer.

See also `stgit-show-worktree-mode'.")

(defvar stgit-show-ignored nil
  "If nil, inhibit showing files ignored by git.")

(defvar stgit-show-unknown nil
  "If nil, inhibit showing files not registered with git.")

(defun stgit-toggle-worktree (&optional arg)
  "Toggle the visibility of the work tree.
With ARG, show the work tree if ARG is positive.

Its initial setting is controlled by `stgit-default-show-worktree'.

`stgit-show-worktree-mode' controls where on screen the index and
work tree will show up."
  (interactive)
  (setq stgit-show-worktree
        (if (numberp arg)
            (> arg 0)
          (not stgit-show-worktree)))
  (stgit-reload))

(defun stgit-toggle-ignored (&optional arg)
  "Toggle the visibility of files ignored by git in the work
tree. With ARG, show these files if ARG is positive.

Use \\[stgit-toggle-worktree] to show the work tree."
  (interactive)
  (setq stgit-show-ignored
        (if (numberp arg)
            (> arg 0)
          (not stgit-show-ignored)))
  (stgit-reload))

(defun stgit-toggle-unknown (&optional arg)
  "Toggle the visibility of files not registered with git in the
work tree. With ARG, show these files if ARG is positive.

Use \\[stgit-toggle-worktree] to show the work tree."
  (interactive)
  (setq stgit-show-unknown
        (if (numberp arg)
            (> arg 0)
          (not stgit-show-unknown)))
  (stgit-reload))

(defcustom stgit-abbreviate-copies-and-renames
  t
  "If non-nil, abbreviate copies and renames as \"dir/{old -> new}/file\"
instead of \"dir/old/file -> dir/new/file\"."
  :type 'boolean
  :group 'stgit)

(provide 'stgit)
