;; Copyright (C) 1998-2021 Free Software Foundation, Inc.
(eval-when-compile (require 'subr-x))
(autoload 'vc-find-revision "vc")
(autoload 'vc-find-revision-no-save "vc")
(defvar vc-find-revision-no-save)
  :type 'boolean)
  :type 'boolean)
  :type 'boolean)
  :type 'boolean)
  :options '(diff-delete-empty-files diff-make-unified))

(defcustom diff-refine 'font-lock
  "If non-nil, enable hunk refinement.

The value `font-lock' means to refine during font-lock.
The value `navigation' means to refine each hunk as you visit it
with `diff-hunk-next' or `diff-hunk-prev'.

You can always manually refine a hunk with `diff-refine-hunk'."
  :version "27.1"
  :type '(choice (const :tag "Don't refine hunks" nil)
                 (const :tag "Refine hunks during font-lock" font-lock)
                 (const :tag "Refine hunks during navigation" navigation)))

(defcustom diff-font-lock-prettify nil
  "If non-nil, font-lock will try and make the format prettier.

This mimics the Magit's diff format by making the hunk header
less cryptic, and on GUI frames also displays insertion and
deletion indicators on the left fringe (if it's available)."
  :version "27.1"
  :type 'boolean)

(defcustom diff-font-lock-syntax t
  "If non-nil, diff hunk font-lock includes source language syntax highlighting.
This highlighting is the same as added by `font-lock-mode'
when corresponding source files are visited normally.
Syntax highlighting is added over diff-mode's own highlighted changes.

If t, the default, highlight syntax only in Diff buffers created by Diff
commands that compare files or by VC commands that compare revisions.
These provide all necessary context for reliable highlighting.  This value
requires support from a VC backend to find the files being compared.
For diffs against the working-tree version of a file, the highlighting is
based on the current file contents.  File-based fontification tries to
infer fontification from the compared files.

If `hunk-only' fontification is based on hunk alone, without full source.
It tries to highlight hunks without enough context that sometimes might result
in wrong fontification.  This is the fastest option, but less reliable.

If `hunk-also', use reliable file-based syntax highlighting when available
and hunk-based syntax highlighting otherwise as a fallback."
  :version "27.1"
  :type '(choice (const :tag "Don't highlight syntax" nil)
                 (const :tag "Hunk-based only" hunk-only)
                 (const :tag "Highlight syntax" t)
                 (const :tag "Allow hunk-based fallback" hunk-also)))
(defvar diff-vc-revisions nil
  "The VC revisions compared in the current Diff buffer, if any.")

(defvar-local diff-default-directory nil
  "The default directory where the current Diff buffer was created.")

  :type '(choice (string "\e") (string "C-c=") string))
  "Toggle automatic diff hunk finer highlighting (Diff Auto Refine mode).
  :group 'diff-mode :init-value nil :lighter nil ;; " Auto-Refine"
  (if diff-auto-refine-mode
      (progn
        (customize-set-variable 'diff-refine 'navigation)
        (condition-case-unless-debug nil (diff-refine-hunk) (error nil)))
    (customize-set-variable 'diff-refine nil)))
(make-obsolete 'diff-auto-refine-mode "set `diff-refine' instead." "27.1")
(make-obsolete-variable 'diff-auto-refine-mode
                        "set `diff-refine' instead." "27.1")
;; Note: The colors used in a color-rich environments (a GUI or in a
;; terminal supporting 24 bit colors) doesn't render well in terminal
;; supporting only 256 colors.  Concretely, both #ffeeee
;; (diff-removed) and #eeffee (diff-added) are mapped to the same
;; greyish color.  "min-colors 257" ensures that those colors are not
;; used terminals supporting only 256 colors.  However, any number
;; between 257 and 2^24 (16777216) would do.

     :background "grey85" :extend t)
     :background "grey45" :extend t)
     :foreground "blue1" :weight bold :extend t)
    (t :weight bold :extend t))
  "`diff-mode' face inherited by hunk and index header faces.")
     :background "grey75" :weight bold :extend t)
     :background "grey60" :weight bold :extend t)
     :foreground "cyan" :weight bold :extend t)
    (t :weight bold :extend t))			; :height 1.3
  "`diff-mode' face used to highlight file header lines.")
  "`diff-mode' face used to highlight index header lines.")
  "`diff-mode' face used to highlight hunk header lines.")
    (((class color) (min-colors 257) (background light))
     :background "#ffeeee" :extend t)
     :background "#ffdddd" :extend t)
     :background "#553333" :extend t)
     :foreground "red" :extend t))
  "`diff-mode' face used to highlight removed lines.")
    (((class color) (min-colors 257) (background light))
     :background "#eeffee" :extend t)
     :background "#ddffdd" :extend t)
     :background "#335533" :extend t)
     :foreground "green" :extend t))
  "`diff-mode' face used to highlight added lines.")
  :version "25.1")
  '((default :inherit diff-removed)
    (((class color) (min-colors 88))
     :foreground "#aa2222"))
  '((default :inherit diff-added)
    (((class color) (min-colors 88))
     :foreground "#22aa22"))
  '((default :inherit diff-changed)
    (((class color) (min-colors 88))
     :foreground "#aaaa22"))
  "`diff-mode' face used to highlight function names produced by \"diff -p\".")
  '((t :extend t))
  :version "27.1")
  "`diff-mode' face used to highlight nonexistent files in recursive diffs.")
    ("^\\(?:index .*\\.\\.\\|diff \\).*\n" . 'diff-header)
    ("^\\(?:new\\|deleted\\) file mode .*\n" . 'diff-header)
    ("^Binary files .* differ\n" . 'diff-file-header)
    ("^[^-=+*!<>#].*\n" (0 'diff-context))
    (,#'diff--font-lock-syntax)
    (,#'diff--font-lock-prettify)
    (,#'diff--font-lock-refined)))
(defvar diff-buffer-type nil)
(defun diff-prev-line-if-patch-separator ()
  "Return previous line if it has patch separator as produced by git."
  (pcase diff-buffer-type
    ('git
     (save-excursion
       (let ((old-point (point)))
         (forward-line -1)
         (if (looking-at "^-- $")
             (point)
           old-point))))
    (_ (point))))

                        ('unified
                        ('context (if diff-valid-unified-empty-line
                                      "^[^-+#! \n\\]" "^[^-+#! \\]"))
                        ('normal "^[^<>#\\]")
          (setq end (point))))
      (setq end (diff-prev-line-if-patch-separator)))
 (when (and (eq diff-refine 'navigation) (called-interactively-p 'interactive))
         (cl-do* ((files fs (delq nil (mapcar #'diff-filename-drop-dir files)))
		  (if (not (save-excursion (re-search-forward "^\\+" nil t)))
(defun diff--font-lock-cleanup ()
  (remove-overlays nil nil 'diff-mode 'fine)
  (remove-overlays nil nil 'diff-mode 'syntax)
  (when font-lock-mode
    (make-local-variable 'font-lock-extra-managed-props)
    ;; Added when diff--font-lock-prettify is non-nil!
    (cl-pushnew 'display font-lock-extra-managed-props)))

\\{diff-mode-map}"
  (add-hook 'font-lock-mode-hook #'diff--font-lock-cleanup nil 'local)
  (set (make-local-variable 'next-error-function) #'diff-next-error)
       #'diff-beginning-of-file-and-junk)
       #'diff-end-of-file)
      (add-hook 'write-contents-functions #'diff-write-contents-hooks nil t)
    (add-hook 'after-change-functions #'diff-after-change-function nil t)
    (add-hook 'post-command-hook #'diff-post-command-hook nil t))
       #'diff-current-defun)
  (add-function :filter-return (local 'filter-buffer-substring-function)
                #'diff--filter-substring)
  (unless buffer-file-name
    (hack-dir-local-variables-non-file-buffer))
  (save-excursion
    (setq-local diff-buffer-type
                (if (re-search-forward "^diff --git" nil t)
                    'git
                  nil))))
      (add-hook 'write-contents-functions #'diff-write-contents-hooks nil t)
    (add-hook 'after-change-functions #'diff-after-change-function nil t)
    (add-hook 'post-command-hook #'diff-post-command-hook nil t)))
	     (eq 0 (file-attribute-size (file-attributes buffer-file-name))))
  (add-hook 'after-save-hook #'diff-delete-if-empty nil t))
          ;; Also skip lines like "\ No newline at end of file"
	  (let ((kill-chars (list (if destp ?- ?+) ?\\)))
	      (if (memq (char-after) kill-chars)
		     (mapconcat #'regexp-quote (split-string text) "[ \t\n]+")
(define-obsolete-function-alias 'diff-xor 'xor "27.1")
  "Find current diff location within the source file.
OTHER-FILE, if non-nil, means to look at the diff's name and line
  numbers for the old file.  Furthermore, use `diff-vc-revisions'
  if it's available.  If `diff-jump-to-old-file' is non-nil, the
  sense of this parameter is reversed.  If the prefix argument is
  8 or more, `diff-jump-to-old-file' is set to OTHER-FILE.
REVERSE, if non-nil, switches the sense of SRC and DST (see below).
NOPROMPT, if non-nil, means not to prompt the user.
Return a list (BUF LINE-OFFSET (BEG . END) SRC DST SWITCHED).
\(BEG . END) is a pair indicating the position of the text in the buffer.
SWITCHED is non-nil if the patch is already applied."
    (let* ((other (xor other-file diff-jump-to-old-file))
	   (revision (and other diff-vc-backend
                          (if reverse (nth 1 diff-vc-revisions)
                            (or (nth 0 diff-vc-revisions)
                                ;; When diff shows changes in working revision
                                (vc-working-revision file)))))
	   (buf (if revision
                    (let ((vc-find-revision-no-save t))
                      (vc-find-revision (expand-file-name file) revision diff-vc-backend))
                  (find-file-noselect file))))
      (diff-hunk-status-msg line-offset (xor switched reverse) nil)
    (diff-hunk-status-msg line-offset (xor reverse switched) t)))
then `diff-jump-to-old-file' is also set, for the next invocations.

Under version control, the OTHER-FILE prefix arg means jump to the old
revision of the file if point is on an old changed line, or to the new
revision of the file otherwise."
  (let ((buffer (when event (current-buffer)))
        (reverse (not (save-excursion (beginning-of-line) (looking-at "[-<]")))))
                 (diff-find-source-location other-file reverse)))
      (when buffer (next-error-found buffer (current-buffer)))
      (diff-hunk-status-msg line-offset (xor reverse switched) t))))
  "Face used for char-based changes shown by `diff-refine-hunk'.")
    (((class color) (min-colors 257) (background light))
     :background "#ffcccc")
    (((class color) (min-colors 257) (background light))
     :background "#bbffbb")
      (let ((beg (diff-beginning-of-hunk t))
            ;; Be careful to start from the hunk header so diff-end-of-hunk
            ;; gets to read the hunk header's line info.
            (end (progn (diff-end-of-hunk) (point))))
        (diff--refine-hunk beg end)))))
(defun diff--refine-hunk (start end)
  (require 'smerge-mode)
  (goto-char start)
  (let* ((style (diff-hunk-style))      ;Skips the hunk header as well.
         (beg (point))
         (props-c '((diff-mode . fine) (face . diff-refine-changed)))
         (props-r '((diff-mode . fine) (face . diff-refine-removed)))
         (props-a '((diff-mode . fine) (face . diff-refine-added))))

    (remove-overlays beg end 'diff-mode 'fine)

    (goto-char beg)
    (pcase style
      ('unified
       (while (re-search-forward "^-" end t)
         (let ((beg-del (progn (beginning-of-line) (point)))
               beg-add end-add)
           (when (and (diff--forward-while-leading-char ?- end)
                      ;; Allow for "\ No newline at end of file".
                      (progn (diff--forward-while-leading-char ?\\ end)
                             (setq beg-add (point)))
                      (diff--forward-while-leading-char ?+ end)
                      (progn (diff--forward-while-leading-char ?\\ end)
                             (setq end-add (point))))
             (smerge-refine-regions beg-del beg-add beg-add end-add
                                    nil #'diff-refine-preproc props-r props-a)))))
      ('context
       (let* ((middle (save-excursion (re-search-forward "^---" end)))
              (other middle))
         (while (re-search-forward "^\\(?:!.*\n\\)+" middle t)
           (smerge-refine-regions (match-beginning 0) (match-end 0)
                                  (save-excursion
                                    (goto-char other)
                                    (re-search-forward "^\\(?:!.*\n\\)+" end)
                                    (setq other (match-end 0))
                                    (match-beginning 0))
                                  other
                                  (if diff-use-changed-face props-c)
                                  #'diff-refine-preproc
                                  (unless diff-use-changed-face props-r)
                                  (unless diff-use-changed-face props-a)))))
      (_ ;; Normal diffs.
       (let ((beg1 (1+ (point))))
         (when (re-search-forward "^---.*\n" end t)
           ;; It's a combined add&remove, so there's something to do.
           (smerge-refine-regions beg1 (match-beginning 0)
                                  (match-end 0) end
                                  nil #'diff-refine-preproc props-r props-a)))))))

(defun diff--iterate-hunks (max fun)
  "Iterate over all hunks between point and MAX.
Call FUN with two args (BEG and END) for each hunk."
  (save-excursion
    (let* ((beg (or (ignore-errors (diff-beginning-of-hunk))
                    (ignore-errors (diff-hunk-next) (point))
                    max)))
      (while (< beg max)
        (cl-assert (looking-at diff-hunk-header-re))
        (let ((end
               (save-excursion (diff-end-of-hunk) (point))))
          (cl-assert (< beg end))
          (funcall fun beg end)
          (goto-char end)
          (setq beg (if (looking-at diff-hunk-header-re)
                        end
                      (or (ignore-errors (diff-hunk-next) (point))
                          max))))))))

(defun diff--font-lock-refined (max)
  "Apply hunk refinement from font-lock."
  (when (eq diff-refine 'font-lock)
    (when (get-char-property (point) 'diff--font-lock-refined)
      ;; Refinement works over a complete hunk, whereas font-lock limits itself
      ;; to highlighting smallish chunks between point..max, so we may be
      ;; called N times for a large hunk in which case we don't want to
      ;; rehighlight that hunk N times (especially since each highlighting
      ;; of a large hunk can itself take a long time, adding insult to injury).
      ;; So, after refining a hunk (including a failed attempt), we place an
      ;; overlay over the whole hunk to mark it as refined, to avoid redoing
      ;; the job redundantly when asked to highlight subsequent parts of the
      ;; same hunk.
      (goto-char (next-single-char-property-change
                  (point) 'diff--font-lock-refined nil max)))
    (diff--iterate-hunks
     max
     (lambda (beg end)
       (unless (get-char-property beg 'diff--font-lock-refined)
         (diff--refine-hunk beg end)
         (let ((ol (make-overlay beg end)))
           (overlay-put ol 'diff--font-lock-refined t)
           (overlay-put ol 'diff-mode 'fine)
           (overlay-put ol 'evaporate t)
           (overlay-put ol 'modification-hooks
                        '(diff--overlay-auto-delete))))))))

(defun diff--overlay-auto-delete (ol _after _beg _end &optional _len)
  (delete-overlay ol))
(defun diff-add-log-current-defuns ()
  "Return an alist of defun names for the current diff.
The elements of the alist are of the form (FILE . (DEFUN...)),
where DEFUN... is a list of function names found in FILE."
  (save-excursion
    (goto-char (point-min))
    (let* ((defuns nil)
           (hunk-end nil)
           (hunk-mismatch-files nil)
           (make-defun-context-follower
            (lambda (goline)
              (let ((eodefun nil)
                    (defname nil))
                (list
                 (lambda () ;; Check for end of current defun.
                   (when (and eodefun
                              (funcall goline)
                              (>= (point) eodefun))
                     (setq defname nil)
                     (setq eodefun nil)))
                 (lambda (&optional get-current) ;; Check for new defun.
                   (if get-current
                       defname
                     (when-let* ((def (and (not eodefun)
                                           (funcall goline)
                                           (add-log-current-defun)))
                                 (eof (save-excursion
                                        (condition-case ()
                                            (progn (end-of-defun) (point))
                                          (scan-error hunk-end)))))
                       (setq eodefun eof)
                       (setq defname def)))))))))
      (while
          ;; Might need to skip over file headers between diff
          ;; hunks (e.g., "diff --git ..." etc).
          (re-search-forward diff-hunk-header-re nil t)
        (setq hunk-end (save-excursion (diff-end-of-hunk)))
        (pcase-let* ((filename (substring-no-properties (diff-find-file-name)))
                     (=lines 0)
                     (+lines 0)
                     (-lines 0)
                     (`(,buf ,line-offset (,beg . ,_end)
                             (,old-text . ,_old-offset)
                             (,new-text . ,_new-offset)
                             ,applied)
                      ;; Try to use the vc integration of
                      ;; `diff-find-source-location', unless it
                      ;; would look for non-existent files like
                      ;; /dev/null.
                      (diff-find-source-location
                       (not (equal "/dev/null"
                                   (car (diff-hunk-file-names t))))))
                     (other-buf nil)
                     (goto-otherbuf
                      ;; If APPLIED, we have NEW-TEXT in BUF, so we
                      ;; need to a buffer with OLD-TEXT to follow
                      ;; -lines.
                      (lambda ()
                        (if other-buf (set-buffer other-buf)
                          (set-buffer (generate-new-buffer " *diff-other-text*"))
                          (insert (if applied old-text new-text))
                          (funcall (buffer-local-value 'major-mode buf))
                          (setq other-buf (current-buffer)))
                        (goto-char (point-min))
                        (forward-line (+ =lines -1
                                         (if applied -lines +lines)))))
                     (gotobuf (lambda ()
                                (set-buffer buf)
                                (goto-char beg)
                                (forward-line (+ =lines -1
                                                 (if applied +lines -lines)))))
                     (`(,=ck-eodefun ,=ck-defun)
                      (funcall make-defun-context-follower gotobuf))
                     (`(,-ck-eodefun ,-ck-defun)
                      (funcall make-defun-context-follower
                               (if applied goto-otherbuf gotobuf)))
                     (`(,+ck-eodefun ,+ck-defun)
                      (funcall make-defun-context-follower
                               (if applied gotobuf goto-otherbuf))))
          (unless (eql line-offset 0)
            (cl-pushnew filename hunk-mismatch-files :test #'equal))
          ;; Some modes always return nil for `add-log-current-defun',
          ;; make sure at least the filename is included.
          (unless (assoc filename defuns)
            (push (cons filename nil) defuns))
          (unwind-protect
              (while (progn (forward-line)
                            (< (point) hunk-end))
                (let ((patch-char (char-after)))
                  (pcase patch-char
                    (?+ (cl-incf +lines))
                    (?- (cl-incf -lines))
                    (?\s (cl-incf =lines)))
                  (save-current-buffer
                    (funcall =ck-eodefun)
                    (funcall +ck-eodefun)
                    (funcall -ck-eodefun)
                    (when-let* ((def (cond
                                      ((eq patch-char ?\s)
                                       ;; Just updating context defun.
                                       (ignore (funcall =ck-defun)))
                                      ;; + or - in existing defun.
                                      ((funcall =ck-defun t))
                                      ;; Check added or removed defun.
                                      (t (funcall (if (eq ?+ patch-char)
                                                      +ck-defun -ck-defun))))))
                      (cl-pushnew def (alist-get filename defuns
                                                 nil nil #'equal)
                                  :test #'equal)))))
            (when (buffer-live-p other-buf)
              (kill-buffer other-buf)))))
      (when hunk-mismatch-files
        (message "Diff didn't match for %s."
                 (mapconcat #'identity hunk-mismatch-files ", ")))
      (dolist (file-defuns defuns)
        (cl-callf nreverse (cdr file-defuns)))
      (nreverse defuns))))

		  (concat "\n[!+<>-]"
    (let* ((other (xor other-file diff-jump-to-old-file))

;;; Prettifying from font-lock

(define-fringe-bitmap 'diff-fringe-add
  [#b00000000
   #b00000000
   #b00010000
   #b00010000
   #b01111100
   #b00010000
   #b00010000
   #b00000000
   #b00000000]
  nil nil 'center)

(define-fringe-bitmap 'diff-fringe-del
  [#b00000000
   #b00000000
   #b00000000
   #b00000000
   #b01111100
   #b00000000
   #b00000000
   #b00000000
   #b00000000]
  nil nil 'center)

(define-fringe-bitmap 'diff-fringe-rep
  [#b00000000
   #b00010000
   #b00010000
   #b00010000
   #b00010000
   #b00010000
   #b00000000
   #b00010000
   #b00000000]
  nil nil 'center)

(define-fringe-bitmap 'diff-fringe-nul
  ;; Maybe there should be such an "empty" bitmap defined by default?
  [#b00000000
   #b00000000
   #b00000000
   #b00000000
   #b00000000
   #b00000000
   #b00000000
   #b00000000
   #b00000000]
  nil nil 'center)

(defun diff--font-lock-prettify (limit)
  (when diff-font-lock-prettify
    (save-excursion
      ;; FIXME: Include the first space for context-style hunks!
      (while (re-search-forward "^[-+! ]" limit t)
        (let ((spec (alist-get (char-before)
                               '((?+ . (left-fringe diff-fringe-add diff-indicator-added))
                                 (?- . (left-fringe diff-fringe-del diff-indicator-removed))
                                 (?! . (left-fringe diff-fringe-rep diff-indicator-changed))
                                 (?\s . (left-fringe diff-fringe-nul))))))
          (put-text-property (match-beginning 0) (match-end 0) 'display spec))))
    ;; Mimicks the output of Magit's diff.
    ;; FIXME: This has only been tested with Git's diff output.
    (while (re-search-forward "^diff " limit t)
      ;; FIXME: Switching between context<->unified leads to messed up
      ;; file headers by cutting the `display' property in chunks!
      (when (save-excursion
              (forward-line 0)
              (looking-at
               (eval-when-compile
                 (concat "diff.*\n"
                         "\\(?:\\(?:new file\\|deleted\\).*\n\\)?"
                         "\\(?:index.*\n\\)?"
                         "--- \\(?:/dev/null\\|a/\\(.*\\)\\)\n"
                         "\\+\\+\\+ \\(?:/dev/null\\|b/\\(.*\\)\\)\n"))))
        (put-text-property (match-beginning 0)
                           (or (match-beginning 2) (match-beginning 1))
                           'display (propertize
                                     (cond
                                      ((null (match-beginning 1)) "new file  ")
                                      ((null (match-beginning 2)) "deleted   ")
                                      (t                          "modified  "))
                                     'face '(diff-file-header diff-header)))
        (unless (match-beginning 2)
          (put-text-property (match-end 1) (1- (match-end 0))
                             'display "")))))
  nil)

;;; Syntax highlighting from font-lock

(defun diff--font-lock-syntax (max)
  "Apply source language syntax highlighting from font-lock.
Calls `diff-syntax-fontify' on every hunk found between point
and the position in MAX."
  (when diff-font-lock-syntax
    (when (get-char-property (point) 'diff--font-lock-syntax)
      (goto-char (next-single-char-property-change
                  (point) 'diff--font-lock-syntax nil max)))
    (diff--iterate-hunks
     max
     (lambda (beg end)
       (unless (get-char-property beg 'diff--font-lock-syntax)
         (diff-syntax-fontify beg end)
         (let ((ol (make-overlay beg end)))
           (overlay-put ol 'diff--font-lock-syntax t)
           (overlay-put ol 'diff-mode 'syntax)
           (overlay-put ol 'evaporate t)
           (overlay-put ol 'modification-hooks
                        '(diff--overlay-auto-delete))))))))

(defun diff-syntax-fontify (beg end)
  "Highlight source language syntax in diff hunk between BEG and END."
  (remove-overlays beg end 'diff-mode 'syntax)
  (save-excursion
    (diff-syntax-fontify-hunk beg end t)
    (diff-syntax-fontify-hunk beg end nil)))

(eval-when-compile (require 'subr-x)) ; for string-trim-right

(defvar-local diff--syntax-file-attributes nil)
(put 'diff--syntax-file-attributes 'permanent-local t)

(defun diff-syntax-fontify-hunk (beg end old)
  "Highlight source language syntax in diff hunk between BEG and END.
When OLD is non-nil, highlight the hunk from the old source."
  (goto-char beg)
  (let* ((hunk (buffer-substring-no-properties beg end))
         ;; Trim a trailing newline to find hunk in diff-syntax-fontify-props
         ;; in diffs that have no newline at end of diff file.
         (text (string-trim-right
                (or (with-demoted-errors (diff-hunk-text hunk (not old) nil))
                    "")))
	 (line (if (looking-at "\\(?:\\*\\{15\\}.*\n\\)?[-@* ]*\\([0-9,]+\\)\\([ acd+]+\\([0-9,]+\\)\\)?")
		   (if old (match-string 1)
		     (if (match-end 3) (match-string 3) (match-string 1)))))
         (line-nb (when line
                    (if (string-match "\\([0-9]+\\),\\([0-9]+\\)" line)
                        (list (string-to-number (match-string 1 line))
                              (string-to-number (match-string 2 line)))
                      (list (string-to-number line) 1)))) ; One-line diffs
         (props
          (or
           (when (and diff-vc-backend
                      (not (eq diff-font-lock-syntax 'hunk-only)))
             (let* ((file (diff-find-file-name old t))
                    (file (and file (expand-file-name file)))
                    (revision (and file (if (not old) (nth 1 diff-vc-revisions)
                                          (or (nth 0 diff-vc-revisions)
                                              (vc-working-revision file))))))
               (when file
                 (if (not revision)
                     ;; Get properties from the current working revision
                     (when (and (not old) (file-readable-p file)
                                (file-regular-p file))
                       (let ((buf (get-file-buffer file)))
                         ;; Try to reuse an existing buffer
                         (if buf
                             (with-current-buffer buf
                               (diff-syntax-fontify-props nil text line-nb))
                           ;; Get properties from the file.
                           (with-current-buffer (get-buffer-create
                                                 " *diff-syntax-file*")
                             (let ((attrs (file-attributes file)))
                               (if (equal diff--syntax-file-attributes attrs)
                                   ;; Same file as last-time, unmodified.
                                   ;; Reuse buffer as-is.
                                   (setq file nil)
                                 (erase-buffer)
                                 (insert-file-contents file)
                                 (setq diff--syntax-file-attributes attrs)))
                             (diff-syntax-fontify-props file text line-nb)))))
                   ;; Get properties from a cached revision
                   (let* ((buffer-name (format " *diff-syntax:%s.~%s~*"
                                               file revision))
                          (buffer (get-buffer buffer-name)))
                     (if buffer
                         ;; Don't re-initialize the buffer (which would throw
                         ;; away the previous fontification work).
                         (setq file nil)
                       (setq buffer (ignore-errors
                                      (vc-find-revision-no-save
                                       file revision
                                       diff-vc-backend
                                       (get-buffer-create buffer-name)))))
                     (when buffer
                       (with-current-buffer buffer
                         (diff-syntax-fontify-props file text line-nb))))))))
           (let ((file (car (diff-hunk-file-names old))))
             (cond
              ((and file diff-default-directory
                    (not (eq diff-font-lock-syntax 'hunk-only))
                    (not diff-vc-backend)
                    (file-readable-p file) (file-regular-p file))
               ;; Try to get full text from the file.
               (with-temp-buffer
                 (insert-file-contents file)
                 (diff-syntax-fontify-props file text line-nb)))
              ;; Otherwise, get properties from the hunk alone
              ((memq diff-font-lock-syntax '(hunk-also hunk-only))
               (with-temp-buffer
                 (insert text)
                 (diff-syntax-fontify-props file text line-nb t))))))))

    ;; Put properties over the hunk text
    (goto-char beg)
    (when (and props (eq (diff-hunk-style) 'unified))
      (while (< (progn (forward-line 1) (point)) end)
        ;; Skip the "\ No newline at end of file" lines as well as the lines
        ;; corresponding to the "other" version.
        (unless (looking-at-p (if old "[+>\\]" "[-<\\]"))
          (if (and old (not (looking-at-p "[-<]")))
              ;; Fontify context lines only from new source,
              ;; don't refontify context lines from old source.
              (pop props)
            (let ((line-props (pop props))
                  (bol (1+ (point))))
              (dolist (prop line-props)
                ;; Ideally, we'd want to use text-properties as in:
                ;;
                ;;     (add-face-text-property
                ;;      (+ bol (nth 0 prop)) (+ bol (nth 1 prop))
                ;;      (nth 2 prop) 'append)
                ;;
                ;; rather than overlays here, but they'd get removed by later
                ;; font-locking.
                ;; This is because we also apply faces outside of the
                ;; beg...end chunk currently font-locked and when font-lock
                ;; later comes to handle the rest of the hunk that we already
                ;; handled we don't (want to) redo it (we work at
                ;; hunk-granularity rather than font-lock's own chunk
                ;; granularity).
                ;; I see two ways to fix this:
                ;; - don't immediately apply the props that fall outside of
                ;;   font-lock's chunk but stash them somewhere (e.g. in another
                ;;   text property) and only later when font-lock comes back
                ;;   move them to `face'.
                ;; - change the code so work at font-lock's chunk granularity
                ;;   (this seems doable without too much extra overhead,
                ;;   contrary to the refine highlighting, which inherently
                ;;   works at a different granularity).
                (let ((ol (make-overlay (+ bol (nth 0 prop))
                                        (+ bol (nth 1 prop))
                                        nil 'front-advance nil)))
                  (overlay-put ol 'diff-mode 'syntax)
                  (overlay-put ol 'evaporate t)
                  (overlay-put ol 'face (nth 2 prop)))))))))))

(defun diff-syntax-fontify-props (file text line-nb &optional hunk-only)
  "Get font-lock properties from the source code.
FILE is the name of the source file.  If non-nil, it requests initialization
of the mode according to FILE.
TEXT is the literal source text from hunk.
LINE-NB is a pair of numbers: start line number and the number of
lines in the hunk.
When HUNK-ONLY is non-nil, then don't verify the existence of the
hunk text in the source file.  Otherwise, don't highlight the hunk if the
hunk text is not found in the source file."
  (when file
    ;; When initialization is requested, we should be in a brand new
    ;; temp buffer.
    (cl-assert (null buffer-file-name))
    (let ((enable-local-variables :safe) ;; to find `mode:'
          (buffer-file-name file))
      ;; Don't run hooks that might assume buffer-file-name
      ;; really associates buffer with a file (bug#39190).
      (delay-mode-hooks (set-auto-mode))
      ;; FIXME: Is this really worth the trouble?
      (when (and (fboundp 'generic-mode-find-file-hook)
                 (memq #'generic-mode-find-file-hook
                       ;; There's no point checking the buffer-local value,
                       ;; we're in a fresh new buffer.
                       (default-value 'find-file-hook)))
        (generic-mode-find-file-hook))))

  (let ((font-lock-defaults (or font-lock-defaults '(nil t)))
        props beg end)
    (goto-char (point-min))
    (if hunk-only
        (setq beg (point-min) end (point-max))
      (forward-line (1- (nth 0 line-nb)))
      ;; non-regexp looking-at to compare hunk text for verification
      (if (search-forward text (+ (point) (length text)) t)
          (setq beg (- (point) (length text)) end (point))
        (goto-char (point-min))
        (if (search-forward text nil t)
            (setq beg (- (point) (length text)) end (point)))))

    (when (and beg end)
      (goto-char beg)
      (font-lock-ensure beg end)

      (while (< (point) end)
        (let* ((bol (point))
               (eol (line-end-position))
               line-props
               (searching t)
               (from (point)) to
               (val (get-text-property from 'face)))
          (while searching
            (setq to (next-single-property-change from 'face nil eol))
            (when val (push (list (- from bol) (- to bol) val) line-props))
            (setq val (get-text-property to 'face) from to)
            (unless (< to eol) (setq searching nil)))
          (when val (push (list from eol val) line-props))
          (push (nreverse line-props) props))
        (forward-line 1)))
    (nreverse props)))


(defun diff--filter-substring (str)
  (when diff-font-lock-prettify
    ;; Strip the `display' properties added by diff-font-lock-prettify,
    ;; since they look weird when you kill&yank!
    (remove-text-properties 0 (length str) '(display nil) str)
    ;; We could also try to only remove those `display' properties actually
    ;; added by diff-font-lock-prettify rather than removing them all blindly.
    ;; E.g.:
    ;;(let ((len (length str))
    ;;      (i 0))
    ;;  (while (and (< i len)
    ;;              (setq i (text-property-not-all i len 'display nil str)))
    ;;    (let* ((val (get-text-property i 'display str))
    ;;           (end (or (text-property-not-all i len 'display val str) len)))
    ;;      ;; FIXME: Check for display props that prettify the file header!
    ;;      (when (eq 'left-fringe (car-safe val))
    ;;        ;; FIXME: Should we check that it's a diff-fringe-* bitmap?
    ;;        (remove-text-properties i end '(display nil) str))
    ;;      (setq i end))))
    )
  str)

;;; Support for converting a diff to diff3 markers via `wiggle'.

;; Wiggle can be found at http://neil.brown.name/wiggle/ or in your nearest
;; Debian repository.

(defun diff-wiggle ()
  "Use `wiggle' to apply the whole current file diff by hook or by crook.
When a hunk can't cleanly be applied, it gets turned into a diff3-style
conflict."
  (interactive)
  (let* ((bounds (diff-bounds-of-file))
         (file (diff-find-file-name))
         (tmpbuf (current-buffer))
         (filebuf (find-buffer-visiting file))
         (patchfile (make-temp-file
                     (expand-file-name "wiggle" (file-name-directory file))
                     nil ".diff"))
         (errfile (make-temp-file
                     (expand-file-name "wiggle" (file-name-directory file))
                     nil ".error")))
    (unwind-protect
        (with-temp-buffer
          (set-buffer (prog1 tmpbuf (setq tmpbuf (current-buffer))))
          (when (buffer-modified-p filebuf)
            (save-some-buffers nil (lambda () (eq (current-buffer) filebuf)))
            (if (buffer-modified-p filebuf) (user-error "Abort!")))
          (write-region (car bounds) (cadr bounds) patchfile nil 'silent)
          (let ((exitcode
                 (call-process "wiggle" nil (list tmpbuf errfile) nil
                               file patchfile)))
            (if (not (memq exitcode '(0 1)))
                (message "diff-wiggle error: %s"
                         (with-current-buffer tmpbuf
                           (goto-char (point-min))
                           (insert-file-contents errfile)
                           (buffer-string)))
              (with-current-buffer tmpbuf
                (write-region nil nil file nil 'silent)
                (with-current-buffer filebuf
                  (revert-buffer t t t)
                  (save-excursion
                    (goto-char (point-min))
                    (if (re-search-forward "^<<<<<<<" nil t)
                        (smerge-mode 1)))
                  (pop-to-buffer filebuf))))))
      (delete-file patchfile)
      (delete-file errfile))))
