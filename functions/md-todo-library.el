;; To-do library for Markdown
;; Author: Mathieu Basille

;; Function md-todo to insert **TODO** at point.
(defun md-todo ()
  "Insert **TODO** at point in Markdown"
  (interactive)
  (insert "**TODO** "))


(defun md-todo-switch (&optional arg)
  "Change the TODO state of an item.

The state of an item is given by a keyword at the start of a
line, modulo header or list symbols, like
  * **TODO** Write paper
  * **DONE** Call mom

The different keywords are specified in the variable `org-todo-keywords'.
By default the available states are \"TODO\" and \"DONE\".  So, for this
example: when the item starts with TODO, it is changed to DONE.
When it starts with DONE, the DONE is removed.  And when neither TODO nor
DONE are present, add TODO at the beginning of the heading.
You can set up single-character keys to fast-select the new state.  See the
`org-todo-keywords' and `org-use-fast-todo-selection' for details.

With `\\[universal-argument]' prefix ARG, force logging the state change \
and take a
logging note.
With a `\\[universal-argument] \\[universal-argument]' prefix, switch to the \
next set of TODO \
keywords (nextset).
Another way to achieve this is `S-C-<right>'.
With a `\\[universal-argument] \\[universal-argument] \\[universal-argument]' \
prefix, circumvent any state blocking.
With numeric prefix arg, switch to the Nth state.

With a numeric prefix arg of 0, inhibit note taking for the change.
With a numeric prefix arg of -1, cancel repeater to allow marking as DONE.

When called through ELisp, arg is also interpreted in the following way:
`none'        -> empty state
\"\"            -> switch to empty state
`done'        -> switch to DONE
`nextset'     -> switch to the next set of keywords
`previousset' -> switch to the previous set of keywords
\"WAITING\"     -> switch to the specified keyword, but only if it
                 really is a member of `org-todo-keywords'."
  (interactive "P")
  (if (and (org-region-active-p) org-loop-over-headlines-in-active-region)
      (let ((cl (if (eq org-loop-over-headlines-in-active-region 'start-level)
		    'region-start-level 'region))
	    org-loop-over-headlines-in-active-region)
	(org-map-entries
	 (lambda () (org-todo arg))
	 nil cl
	 (when (org-invisible-p) (org-end-of-subtree nil t))))
    (when (equal arg '(16)) (setq arg 'nextset))
    (when (equal arg -1) (org-cancel-repeater) (setq arg nil))
    (let ((org-blocker-hook org-blocker-hook)
	  commentp
	  case-fold-search)
      (when (equal arg '(64))
	(setq arg nil org-blocker-hook nil))
      (when (and org-blocker-hook
		 (or org-inhibit-blocking
		     (org-entry-get nil "NOBLOCKING")))
	(setq org-blocker-hook nil))
      (save-excursion
	(catch 'exit
	  (org-back-to-heading t)
	  (when (org-in-commented-heading-p t)
	    (org-toggle-comment)
	    (setq commentp t))
	  (when (looking-at org-outline-regexp) (goto-char (1- (match-end 0))))
	  (or (looking-at (concat " +" org-todo-regexp "\\( +\\|[ \t]*$\\)"))
	      (looking-at "\\(?: *\\|[ \t]*$\\)"))
	  (let* ((match-data (match-data))
		 (startpos (copy-marker (line-beginning-position)))
		 (force-log (and  (equal arg '(4)) (prog1 t (setq arg nil))))
		 (logging (save-match-data (org-entry-get nil "LOGGING" t t)))
		 (org-log-done org-log-done)
		 (org-log-repeat org-log-repeat)
		 (org-todo-log-states org-todo-log-states)
		 (org-inhibit-logging
		  (if (equal arg 0)
		      (progn (setq arg nil) 'note) org-inhibit-logging))
		 (this (match-string 1))
		 (hl-pos (match-beginning 0))
		 (head (org-get-todo-sequence-head this))
		 (ass (assoc head org-todo-kwd-alist))
		 (interpret (nth 1 ass))
		 (done-word (nth 3 ass))
		 (final-done-word (nth 4 ass))
		 (org-last-state (or this ""))
		 (completion-ignore-case t)
		 (member (member this org-todo-keywords-1))
		 (tail (cdr member))
		 (org-state (cond
			     ((eq arg 'right)
			      ;; Next state
			      (if this
				  (if tail (car tail) nil)
				(car org-todo-keywords-1)))
			     ((eq arg 'left)
			      ;; Previous state
			      (unless (equal member org-todo-keywords-1)
				(if this
				    (nth (- (length org-todo-keywords-1)
					    (length tail) 2)
					 org-todo-keywords-1)
				  (org-last org-todo-keywords-1))))
			     (arg
			      ;; User or caller requests a specific state.
			      (cond
			       ((equal arg "") nil)
			       ((eq arg 'none) nil)
			       ((eq arg 'done) (or done-word (car org-done-keywords)))
			       ((eq arg 'nextset)
				(or (car (cdr (member head org-todo-heads)))
				    (car org-todo-heads)))
			       ((eq arg 'previousset)
				(let ((org-todo-heads (reverse org-todo-heads)))
				  (or (car (cdr (member head org-todo-heads)))
				      (car org-todo-heads))))
			       ((car (member arg org-todo-keywords-1)))
			       ((stringp arg)
				(user-error "State `%s' not valid in this file" arg))
			       ((nth (1- (prefix-numeric-value arg))
				     org-todo-keywords-1))))
			     ((and org-todo-key-trigger org-use-fast-todo-selection)
			      ;; Use fast selection.
			      (org-fast-todo-selection this))
			     ((null member) (or head (car org-todo-keywords-1)))
			     ((equal this final-done-word) nil) ;-> make empty
			     ((null tail) nil) ;-> first entry
			     ((memq interpret '(type priority))
			      (if (eq this-command last-command)
				  (car tail)
				(if (> (length tail) 0)
				    (or done-word (car org-done-keywords))
				  nil)))
			     (t
			      (car tail))))
		 (org-state (or
			     (run-hook-with-args-until-success
			      'org-todo-get-default-hook org-state org-last-state)
			     org-state))
		 (next (if (org-string-nw-p org-state) (concat " " org-state " ") " "))
		 (change-plist (list :type 'todo-state-change :from this :to org-state
				     :position startpos))
		 dolog now-done-p)
	    (when org-blocker-hook
	      (let (org-blocked-by-checkboxes block-reason)
		(setq org-last-todo-state-is-todo
		      (not (member this org-done-keywords)))
		(unless (save-excursion
			  (save-match-data
			    (org-with-wide-buffer
			     (run-hook-with-args-until-failure
			      'org-blocker-hook change-plist))))
		  (setq block-reason (if org-blocked-by-checkboxes
					 "contained checkboxes"
				       (format "\"%s\"" org-block-entry-blocking)))
		  (if (called-interactively-p 'interactive)
		      (user-error "TODO state change from %s to %s blocked (by %s)"
				  this org-state block-reason)
		    ;; Fail silently.
		    (message "TODO state change from %s to %s blocked (by %s)"
			     this org-state block-reason)
		    (throw 'exit nil)))))
	    (store-match-data match-data)
	    (replace-match next t t)
	    (cond ((and org-state (equal this org-state))
		   (message "TODO state was already %s" (org-trim next)))
		  ((not (pos-visible-in-window-p hl-pos))
		   (message "TODO state changed to %s" (org-trim next))))
	    (unless head
	      (setq head (org-get-todo-sequence-head org-state)
		    ass (assoc head org-todo-kwd-alist)
		    interpret (nth 1 ass)
		    done-word (nth 3 ass)
		    final-done-word (nth 4 ass)))
	    (when (memq arg '(nextset previousset))
	      (message "Keyword-Set %d/%d: %s"
		       (- (length org-todo-sets) -1
			  (length (memq (assoc org-state org-todo-sets) org-todo-sets)))
		       (length org-todo-sets)
		       (mapconcat 'identity (assoc org-state org-todo-sets) " ")))
	    (setq org-last-todo-state-is-todo
		  (not (member org-state org-done-keywords)))
	    (setq now-done-p (and (member org-state org-done-keywords)
				  (not (member this org-done-keywords))))
	    (and logging (org-local-logging logging))
	    (when (or (and (or org-todo-log-states org-log-done)
			   (not (eq org-inhibit-logging t))
			   (not (memq arg '(nextset previousset))))
		      force-log)
	      ;; We need to look at recording a time and note.
	      (setq dolog (or (if force-log 'note)
			      (nth 1 (assoc org-state org-todo-log-states))
			      (nth 2 (assoc this org-todo-log-states))))
	      (when (and (eq dolog 'note) (eq org-inhibit-logging 'note))
		(setq dolog 'time))
	      (when (or (and (not org-state) (not org-closed-keep-when-no-todo))
			(and org-state
			     (member org-state org-not-done-keywords)
			     (not (member this org-not-done-keywords))))
		;; This is now a todo state and was not one before
		;; If there was a CLOSED time stamp, get rid of it.
		(org-add-planning-info nil nil 'closed))
	      (when (and now-done-p org-log-done)
		;; It is now done, and it was not done before.
		(org-add-planning-info 'closed (org-current-effective-time))
		(when (and (not dolog) (eq 'note org-log-done))
		  (org-add-log-setup 'done org-state this 'note)))
	      (when (and org-state dolog)
		;; This is a non-nil state, and we need to log it.
		(org-add-log-setup 'state org-state this dolog)))
	    ;; Fixup tag positioning.
	    (org-todo-trigger-tag-changes org-state)
	    (when org-auto-align-tags (org-align-tags))
	    (when org-provide-todo-statistics
	      (org-update-parent-todo-statistics))
	    (when (bound-and-true-p org-clock-out-when-done)
	      (org-clock-out-if-current))
	    (run-hooks 'org-after-todo-state-change-hook)
	    (when (and arg (not (member org-state org-done-keywords)))
	      (setq head (org-get-todo-sequence-head org-state)))
	    (put-text-property (point-at-bol) (point-at-eol) 'org-todo-head head)
	    ;; Do we need to trigger a repeat?
	    (when now-done-p
	      (when (boundp 'org-agenda-headline-snapshot-before-repeat)
		;; This is for the agenda, take a snapshot of the headline.
		(save-match-data
		  (setq org-agenda-headline-snapshot-before-repeat
			(org-get-heading))))
	      (org-auto-repeat-maybe org-state))
	    ;; Fixup cursor location if close to the keyword.
	    (when (and (outline-on-heading-p)
		       (not (bolp))
		       (save-excursion (beginning-of-line 1)
				       (looking-at org-todo-line-regexp))
		       (< (point) (+ 2 (or (match-end 2) (match-end 1)))))
	      (goto-char (or (match-end 2) (match-end 1)))
	      (and (looking-at " ")
		   (not (looking-at " *:"))
		   (just-one-space)))
	    (when org-trigger-hook
	      (save-excursion
		(run-hook-with-args 'org-trigger-hook change-plist)))
	    (when commentp (org-toggle-comment))))))))

