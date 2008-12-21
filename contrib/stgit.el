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
                      (error "cannot find top-level git tree for %s." dir))))))
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
  "Capture StGit output and show it in a window at the end."
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

(defun stgit-run-silent (&rest args)
  (apply 'call-process "stg" nil standard-output nil args))

(defun stgit-run (&rest args)
  (let ((msgcmd (mapconcat #'identity args " ")))
    (message "Running stg %s..." msgcmd)
    (apply 'call-process "stg" nil standard-output nil args)
    (message "Running stg %s...done" msgcmd)))

(defun stgit-run-git (&rest args)
  (let ((msgcmd (mapconcat #'identity args " ")))
    (message "Running git %s..." msgcmd)
    (apply 'call-process "git" nil standard-output nil args)
    (message "Running git %s...done" msgcmd)))

(defun stgit-reload ()
  "Update the contents of the StGit buffer."
  (interactive)
  (let ((inhibit-read-only t)
        (curline (line-number-at-pos))
        (curpatch (stgit-patch-at-point)))
    (erase-buffer)
    (insert "Branch: ")
    (stgit-run-silent "branch")
    (stgit-run-silent "series" "--description")
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

(defun stgit-expand-patch (patchsym)
  (save-excursion
    (forward-line)
    (let ((start (point)))
      (stgit-run "files" (symbol-name patchsym))
      
      ;; 'stg files' outputs a single newline for empty patches; it
      ;; must be destroyed!
      (when (and (= (1+ start) (point))
                 (= (char-before) ?\n))
        (delete-backward-char 1))

      (let ((end-marker (point-marker)))
        (if (= start (point))
            (insert-string "    <no files>\n")
          (unless (looking-at "^")
            (insert ?\n))
          (while (and (zerop (forward-line -1))
                      (>= (point) start))
            (insert "    ")))
        (put-text-property start end-marker 'stgit-patchsym patchsym)))))

(defun stgit-rescan ()
  "Rescan the status buffer."
  (save-excursion
    (let ((marked ()))
      (goto-char (point-min))
      (while (not (eobp))
        (cond ((looking-at "Branch: \\(.*\\)")
               (put-text-property (match-beginning 1) (match-end 1)
                                  'face 'bold))
              ((looking-at "\\([>+-]\\)\\( \\)\\([^ ]+\\) *[|#] \\(.*\\)")
               (let ((state (match-string 1))
                     (patchsym (intern (match-string 3))))
                 (put-text-property
                  (match-beginning 3) (match-end 3) 'face
                  (cond ((string= state ">") 'stgit-top-patch-face)
                        ((string= state "+") 'stgit-applied-patch-face)
                        ((string= state "-") 'stgit-unapplied-patch-face)))
                 (put-text-property (match-beginning 4) (match-end 4)
                                    'face 'stgit-description-face)
                 (when (memq patchsym stgit-marked-patches)
                   (replace-match "*" nil nil nil 2)
                   (setq marked (cons patchsym marked)))
                 (when (memq patchsym stgit-expanded-patches)
                   (stgit-expand-patch patchsym))
                 ))
              ((or (looking-at "stg series: Branch \".*\" not initialised")
                   (looking-at "stg series: .*: branch not initialized"))
               (forward-line 1)
               (insert "Run M-x stgit-init to initialise")))
        (forward-line 1))
      (setq stgit-marked-patches (nreverse marked)))))

(defun stgit-select ()
  "Expand or collapse the current entry"
  (interactive)
  (let ((curpatch (stgit-patch-at-point)))
    (if (not curpatch)
        (let ((patched-file (stgit-patched-file-at-point)))
          (unless patched-file
            (error "No patch or file on the current line"))
          (let ((filename (expand-file-name (cdr patched-file))))
            (unless (file-exists-p filename)
              (error "File does not exist"))
            (find-file filename)))
      (setq curpatch (intern curpatch))
      (setq stgit-expanded-patches
            (if (memq curpatch stgit-expanded-patches)
                (delq curpatch stgit-expanded-patches)
              (cons curpatch stgit-expanded-patches)))
      (stgit-reload))))

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
    (error "stgit-git-status requires git-status"))
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
          ("c" .        stgit-coalesce)
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
          ("q" . stgit-quit))))

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

(defun stgit-add-mark (patch)
  (let ((patchsym (intern patch)))
    (setq stgit-marked-patches (cons patchsym stgit-marked-patches))))

(defun stgit-remove-mark (patch)
  (let ((patchsym (intern patch)))
    (setq stgit-marked-patches (delq patchsym stgit-marked-patches))))

(defun stgit-clear-marks ()
  (setq stgit-marked-patches '()))

(defun stgit-marked-patches ()
  "Return the names of the marked patches."
  (mapcar 'symbol-name stgit-marked-patches))

(defun stgit-patch-at-point (&optional cause-error allow-file)
  "Return the patch name on the current line.
If CAUSE-ERROR is not nil, signal an error if none found.
If ALLOW-FILE is not nil, also handle when point is on a file of
a patch."
  (or (and allow-file
           (let ((patchsym (get-text-property (point) 'stgit-patchsym)))
             (and patchsym
                  (symbol-name patchsym))))
      (save-excursion
        (beginning-of-line)
        (and (looking-at "[>+-][ *]\\([^ ]*\\)")
             (match-string-no-properties 1)))
      (and cause-error
           (error "No patch on this line"))))

(defun stgit-patched-file-at-point ()
  "Returns a cons of the patchsym and file name at point"
  (let ((patchsym (get-text-property (point) 'stgit-patchsym)))
    (when patchsym
      (save-excursion
        (beginning-of-line)
        (when (looking-at "    [A-Z] \\(.*\\)")
          (cons patchsym (match-string-no-properties 1)))))))

(defun stgit-patches-marked-or-at-point ()
  "Return the names of the marked patches, or the patch on the current line."
  (if stgit-marked-patches
      (stgit-marked-patches)
    (let ((patch (stgit-patch-at-point)))
      (if patch
          (list patch)
        '()))))

(defun stgit-goto-patch (patch)
  "Move point to the line containing PATCH."
  (let ((p (point)))
    (goto-char (point-min))
    (if (re-search-forward (concat "^[>+-][ *]" (regexp-quote patch) " ") nil t)
        (progn (move-to-column goal-column)
               t)
      (goto-char p)
      nil)))

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
    (stgit-add-mark patch)
    (stgit-reload))
  (stgit-next-patch))

(defun stgit-unmark-up ()
  "Remove mark from the patch on the previous line."
  (interactive)
  (stgit-previous-patch)
  (stgit-remove-mark (stgit-patch-at-point t))
  (stgit-reload))

(defun stgit-unmark-down ()
  "Remove mark from the patch on the current line."
  (interactive)
  (stgit-remove-mark (stgit-patch-at-point t))
  (stgit-next-patch)
  (stgit-reload))

(defun stgit-rename (name)
  "Rename the patch under point to NAME."
  (interactive (list (read-string "Patch name: " (stgit-patch-at-point t))))
  (let ((old-name (stgit-patch-at-point t)))
    (stgit-capture-output nil
      (stgit-run "rename" old-name name))
    (let ((old-name-sym (intern old-name))
          (name-sym (intern name)))
      (when (memq old-name-sym stgit-expanded-patches)
        (setq stgit-expanded-patches
            (cons name-sym (delq old-name-sym stgit-expanded-patches))))
      (when (memq old-name-sym stgit-marked-patches)
        (setq stgit-marked-patches
            (cons name-sym (delq old-name-sym stgit-marked-patches)))))
    (stgit-reload)
    (stgit-goto-patch name)))

(defun stgit-repair ()
  "Run stg repair."
  (interactive)
  (stgit-capture-output nil
    (stgit-run "repair"))
  (stgit-reload))

(defun stgit-commit ()
  "Run stg commit."
  (interactive)
  (stgit-capture-output nil (stgit-run "commit"))
  (stgit-reload))

(defun stgit-uncommit (arg)
  "Run stg uncommit. Numeric arg determines number of patches to uncommit."
  (interactive "p")
  (stgit-capture-output nil (stgit-run "uncommit" "-n" (number-to-string arg)))
  (stgit-reload))

(defun stgit-push-next (npatches)
  "Push the first unapplied patch.
With numeric prefix argument, push that many patches."
  (interactive "p")
  (stgit-capture-output nil (stgit-run "push" "-n"
                                       (number-to-string npatches)))
  (stgit-reload)
  (stgit-refresh-git-status))

(defun stgit-pop-next (npatches)
  "Pop the topmost applied patch.
With numeric prefix argument, pop that many patches."
  (interactive "p")
  (stgit-capture-output nil (stgit-run "pop" "-n" (number-to-string npatches)))
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
  (let ((patch (stgit-patch-at-point t))
        (applied (stgit-applied-at-point)))
    (stgit-capture-output nil
      (stgit-run (if applied "pop" "push") patch))
    (stgit-reload)))

(defun stgit-goto ()
  "Go to the patch on the current line."
  (interactive)
  (let ((patch (stgit-patch-at-point t)))
    (stgit-capture-output nil
      (stgit-run "goto" patch))
    (stgit-reload)))

(defun stgit-id (patch)
  "Return the git commit id for PATCH"
  (let ((result (with-output-to-string
                  (stgit-run-silent "id" patch))))
    (unless (string-match "^\\([0-9A-Fa-f]\\{40\\}\\)$" result)
      (error "Cannot find commit id for %s" patch))
    (match-string 1 result)))

(defun stgit-show ()
  "Show the patch on the current line."
  (interactive)
  (stgit-capture-output "*StGit patch*"
    (let ((patch (stgit-patch-at-point)))
      (if (not patch)
          (let ((patched-file (stgit-patched-file-at-point)))
            (unless patched-file
              (error "No patch or file at point"))
            (let ((id (stgit-id (symbol-name (car patched-file)))))
              (with-output-to-temp-buffer "*StGit diff*"
                (stgit-run-git "diff" (concat id "^") id (cdr patched-file))
                (with-current-buffer standard-output
                  (diff-mode)))))
        (stgit-run "show" (stgit-patch-at-point))
        (with-current-buffer standard-output
          (goto-char (point-min))
          (diff-mode))))))

(defun stgit-edit ()
  "Edit the patch on the current line."
  (interactive)
  (let ((patch (stgit-patch-at-point t))
        (edit-buf (get-buffer-create "*StGit edit*"))
        (dir default-directory))
    (log-edit 'stgit-confirm-edit t nil edit-buf)
    (set (make-local-variable 'stgit-edit-patch) patch)
    (setq default-directory dir)
    (let ((standard-output edit-buf))
      (stgit-run-silent "edit" "--save-template=-" patch))))

(defun stgit-confirm-edit ()
  (interactive)
  (let ((file (make-temp-file "stgit-edit-")))
    (write-region (point-min) (point-max) file)
    (stgit-capture-output nil
      (stgit-run "edit" "-f" file stgit-edit-patch))
    (with-current-buffer log-edit-parent-buffer
      (stgit-reload))))

(defun stgit-new ()
  "Create a new patch."
  (interactive)
  (let ((edit-buf (get-buffer-create "*StGit edit*"))
        (dir default-directory))
    (log-edit 'stgit-confirm-new t nil edit-buf)
    (setq default-directory dir)))

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
             (setq patch (downcase (concat patch (match-string 0 description))))
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

(defun stgit-delete (patch-names)
  "Delete the named patches."
  (interactive (list (stgit-patches-marked-or-at-point)))
  (if (zerop (length patch-names))
      (error "No patches to delete")
    (when (yes-or-no-p (format "Really delete %d patches? "
                               (length patch-names)))
      (stgit-capture-output nil
        (apply 'stgit-run "delete" patch-names))
      (stgit-reload))))

(defun stgit-coalesce (patch-names)
  "Run stg coalesce on the named patches."
  (interactive (list (stgit-marked-patches)))
  (let ((edit-buf (get-buffer-create "*StGit edit*"))
        (dir default-directory))
    (log-edit 'stgit-confirm-coalesce t nil edit-buf)
    (set (make-local-variable 'stgit-patches) patch-names)
    (setq default-directory dir)
    (let ((standard-output edit-buf))
      (apply 'stgit-run-silent "coalesce" "--save-template=-" patch-names))))

(defun stgit-confirm-coalesce ()
  (interactive)
  (let ((file (make-temp-file "stgit-edit-")))
    (write-region (point-min) (point-max) file)
    (stgit-capture-output nil
      (apply 'stgit-run "coalesce" "-f" file stgit-patches))
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
                                (error "no patch to update"))
                               ((> (length patches) 1)
                                (error "too many patches selected"))
                               (t
                                (cons "-p" patches))))
                     nil)))
    (stgit-capture-output nil
      (apply 'stgit-run "refresh" patchargs))
    (stgit-refresh-git-status))
  (stgit-reload))

(provide 'stgit)
