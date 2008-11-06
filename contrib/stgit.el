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

(defun stgit (dir)
  "Manage stgit patches"
  (interactive "DDirectory: \n")
  (switch-to-stgit-buffer (git-get-top-dir dir))
  (stgit-refresh))

(defun git-get-top-dir (dir)
  "Retrieve the top-level directory of a git tree."
  (let ((cdup (with-output-to-string
                (with-current-buffer standard-output
                  (cd dir)
                  (unless (eq 0 (call-process "git" nil t nil
                                              "rev-parse" "--show-cdup"))
                    (error "cannot find top-level git tree for %s." dir))))))
    (expand-file-name (concat (file-name-as-directory dir)
                              (car (split-string cdup "\n"))))))

(defun switch-to-stgit-buffer (dir)
  "Switch to a (possibly new) buffer displaying StGit patches for DIR"
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
  "Capture StGit output and show it in a window at the end"
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

(defun stgit-run (&rest args)
  (apply 'call-process "stg" nil standard-output nil args))

(defun stgit-refresh ()
  "Update the contents of the stgit buffer"
  (interactive)
  (let ((inhibit-read-only t)
        (curline (line-number-at-pos))
        (curpatch (stgit-patch-at-point)))
    (erase-buffer)
    (insert "Branch: ")
    (stgit-run "branch")
    (stgit-run "series" "--description")
    (stgit-rescan)
    (if curpatch
        (stgit-goto-patch curpatch)
      (goto-line curline))))

(defface stgit-description-face
  '((((background dark)) (:foreground "tan"))
    (((background light)) (:foreground "dark red")))
  "The face used for StGit desriptions")

(defface stgit-top-patch-face
  '((((background dark)) (:weight bold :foreground "yellow"))
    (((background light)) (:weight bold :foreground "purple"))
    (t (:weight bold)))
  "The face used for the top patch names")

(defface stgit-applied-patch-face
  '((((background dark)) (:foreground "light yellow"))
    (((background light)) (:foreground "purple"))
    (t ()))
  "The face used for applied patch names")

(defface stgit-unapplied-patch-face
  '((((background dark)) (:foreground "gray80"))
    (((background light)) (:foreground "orchid"))
    (t ()))
  "The face used for unapplied patch names")

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
                   (setq marked (cons patchsym marked)))))
              ((looking-at "stg series: Branch \".*\" not initialised")
               (forward-line 1)
               (insert "Run M-x stgit-init to initialise")))
        (forward-line 1))
      (setq stgit-marked-patches (nreverse marked)))))

(defvar stgit-mode-hook nil
  "Run after `stgit-mode' is setup.")

(defvar stgit-mode-map nil
  "Keymap for StGit major mode.")

(unless stgit-mode-map
  (setq stgit-mode-map (make-keymap))
  (suppress-keymap stgit-mode-map)
  (define-key stgit-mode-map " "   'stgit-mark)
  (define-key stgit-mode-map "\d" 'stgit-unmark)
  (define-key stgit-mode-map "?"   'stgit-help)
  (define-key stgit-mode-map "h"   'stgit-help)
  (define-key stgit-mode-map "p"   'previous-line)
  (define-key stgit-mode-map "n"   'next-line)
  (define-key stgit-mode-map "g"   'stgit-refresh)
  (define-key stgit-mode-map "r"   'stgit-rename)
  (define-key stgit-mode-map "e"   'stgit-edit)
  (define-key stgit-mode-map "c"   'stgit-coalesce)
  (define-key stgit-mode-map "N"   'stgit-new)
  (define-key stgit-mode-map "R"   'stgit-repair)
  (define-key stgit-mode-map "C"   'stgit-commit)
  (define-key stgit-mode-map "U"   'stgit-uncommit)
  (define-key stgit-mode-map ">"   'stgit-push-next)
  (define-key stgit-mode-map "<"   'stgit-pop-next)
  (define-key stgit-mode-map "P"   'stgit-push-or-pop)
  (define-key stgit-mode-map "G"   'stgit-goto)
  (define-key stgit-mode-map "="   'stgit-show)
  (define-key stgit-mode-map "D"   'stgit-delete))

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
  (set-variable 'truncate-lines 't)
  (run-hooks 'stgit-mode-hook))

(defun stgit-add-mark (patch)
  (let ((patchsym (intern patch)))
    (setq stgit-marked-patches (cons patchsym stgit-marked-patches))))

(defun stgit-remove-mark (patch)
  (let ((patchsym (intern patch)))
    (setq stgit-marked-patches (delq patchsym stgit-marked-patches))))

(defun stgit-marked-patches ()
  "Return the names of the marked patches."
  (mapcar 'symbol-name stgit-marked-patches))

(defun stgit-patch-at-point ()
  "Return the patch name on the current line"
  (save-excursion
    (beginning-of-line)
    (if (looking-at "[>+-][ *]\\([^ ]*\\)")
        (match-string-no-properties 1)
      nil)))

(defun stgit-patches-marked-or-at-point ()
  "Return the names of the marked patches, or the patch on the current line."
  (if stgit-marked-patches
      (stgit-marked-patches)
    (let ((patch (stgit-patch-at-point)))
      (if patch
          (list patch)
        '()))))

(defun stgit-goto-patch (patch)
  "Move point to the line containing PATCH"
  (let ((p (point)))
    (goto-char (point-min))
    (if (re-search-forward (concat "^[>+-][ *]" (regexp-quote patch) " ") nil t)
        (progn (move-to-column goal-column)
               t)
      (goto-char p)
      nil)))

(defun stgit-init ()
  "Run stg init"
  (interactive)
  (stgit-capture-output nil
   (stgit-run "init"))
  (stgit-refresh))

(defun stgit-mark ()
  "Mark the patch under point"
  (interactive)
  (let ((patch (stgit-patch-at-point)))
    (stgit-add-mark patch)
    (stgit-refresh))
  (next-line))

(defun stgit-unmark ()
  "Mark the patch on the previous line"
  (interactive)
  (forward-line -1)
  (let ((patch (stgit-patch-at-point)))
    (stgit-remove-mark patch)
    (stgit-refresh)))

(defun stgit-rename (name)
  "Rename the patch under point"
  (interactive (list (read-string "Patch name: " (stgit-patch-at-point))))
  (let ((old-name (stgit-patch-at-point)))
    (unless old-name
      (error "No patch on this line"))
    (stgit-capture-output nil
      (stgit-run "rename" old-name name))
    (stgit-refresh)
    (stgit-goto-patch name)))

(defun stgit-repair ()
  "Run stg repair"
  (interactive)
  (stgit-capture-output nil
   (stgit-run "repair"))
  (stgit-refresh))

(defun stgit-commit ()
  "Run stg commit."
  (interactive)
  (stgit-capture-output nil (stgit-run "commit"))
  (stgit-refresh))

(defun stgit-uncommit (arg)
  "Run stg uncommit. Numeric arg determines number of patches to uncommit."
  (interactive "p")
  (stgit-capture-output nil (stgit-run "uncommit" "-n" (number-to-string arg)))
  (stgit-refresh))

(defun stgit-push-next (npatches)
  "Push the first unapplied patch.
With numeric prefix argument, push that many patches."
  (interactive "p")
  (stgit-capture-output nil (stgit-run "push" "-n"
                                       (number-to-string npatches)))
  (stgit-refresh))

(defun stgit-pop-next (npatches)
  "Pop the topmost applied patch.
With numeric prefix argument, pop that many patches."
  (interactive "p")
  (stgit-capture-output nil (stgit-run "pop" "-n" (number-to-string npatches)))
  (stgit-refresh))

(defun stgit-applied-at-point ()
  "Is the patch on the current line applied?"
  (save-excursion
    (beginning-of-line)
    (looking-at "[>+]")))

(defun stgit-push-or-pop ()
  "Push or pop the patch on the current line"
  (interactive)
  (let ((patch (stgit-patch-at-point))
        (applied (stgit-applied-at-point)))
    (stgit-capture-output nil
       (stgit-run (if applied "pop" "push") patch))
    (stgit-refresh)))

(defun stgit-goto ()
  "Go to the patch on the current line"
  (interactive)
  (let ((patch (stgit-patch-at-point)))
    (stgit-capture-output nil
       (stgit-run "goto" patch))
    (stgit-refresh)))

(defun stgit-show ()
  "Show the patch on the current line"
  (interactive)
  (stgit-capture-output "*StGit patch*"
    (stgit-run "show" (stgit-patch-at-point))
    (with-current-buffer standard-output
      (goto-char (point-min))
      (diff-mode))))

(defun stgit-edit ()
  "Edit the patch on the current line"
  (interactive)
  (let ((patch (stgit-patch-at-point))
        (edit-buf (get-buffer-create "*StGit edit*"))
        (dir default-directory))
    (log-edit 'stgit-confirm-edit t nil edit-buf)
    (set (make-local-variable 'stgit-edit-patch) patch)
    (setq default-directory dir)
    (let ((standard-output edit-buf))
      (stgit-run "edit" "--save-template=-" patch))))

(defun stgit-confirm-edit ()
  (interactive)
  (let ((file (make-temp-file "stgit-edit-")))
    (write-region (point-min) (point-max) file)
    (stgit-capture-output nil
      (stgit-run "edit" "-f" file stgit-edit-patch))
    (with-current-buffer log-edit-parent-buffer
      (stgit-refresh))))

(defun stgit-new ()
  "Create a new patch"
  (interactive)
  (let ((edit-buf (get-buffer-create "*StGit edit*")))
    (log-edit 'stgit-confirm-new t nil edit-buf)))

(defun stgit-confirm-new ()
  (interactive)
  (let ((file (make-temp-file "stgit-edit-")))
    (write-region (point-min) (point-max) file)
    (stgit-capture-output nil
      (stgit-run "new" "-f" file))
    (with-current-buffer log-edit-parent-buffer
      (stgit-refresh))))

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
  "Delete the named patches"
  (interactive (list (stgit-patches-marked-or-at-point)))
  (if (zerop (length patch-names))
      (error "No patches to delete")
    (when (yes-or-no-p (format "Really delete %d patches? "
                               (length patch-names)))
      (stgit-capture-output nil
        (apply 'stgit-run "delete" patch-names))
      (stgit-refresh))))

(defun stgit-coalesce (patch-names)
  "Run stg coalesce on the named patches"
  (interactive (list (stgit-marked-patches)))
  (let ((edit-buf (get-buffer-create "*StGit edit*"))
        (dir default-directory))
    (log-edit 'stgit-confirm-coalesce t nil edit-buf)
    (set (make-local-variable 'stgit-patches) patch-names)
    (setq default-directory dir)
    (let ((standard-output edit-buf))
      (apply 'stgit-run "coalesce" "--save-template=-" patch-names))))

(defun stgit-confirm-coalesce ()
  (interactive)
  (let ((file (make-temp-file "stgit-edit-")))
    (write-region (point-min) (point-max) file)
    (stgit-capture-output nil
      (apply 'stgit-run "coalesce" "-f" file stgit-patches))
    (with-current-buffer log-edit-parent-buffer
      (stgit-refresh))))

(defun stgit-help ()
  "Display help for the StGit mode."
  (interactive)
  (describe-function 'stgit-mode))

(provide 'stgit)
