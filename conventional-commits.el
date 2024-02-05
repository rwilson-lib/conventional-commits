(require 'cl-lib)
(require 'vertico)
(require 'cape)
(require 'magit)

;; Scope
(defcustom commit-scope-list
  '((api . "Changes to the application programming interface (API)")
    (build . "Changes to the build system or build-related files")
    (chore . "Routine tasks, maintenance, or tooling changes")
    (config . "Updates to configuration files or settings")
    (core . "Changes related to the core functionality of the system")
    (dep . "Dependency updates or changes")
    (docs . "Updates or additions to documentation")
    (feat . "New features or enhancements")
    (fix . "Bug fixes")
    (perf . "Performance improvements")
    (refactor . "Code refactoring without changing external behavior")
    (security . "Security-related changes")
    (style . "Changes to coding style or formatting")
    (test . "Changes to test files or test-related code")
    (ui . "Changes to the user interface or user experience"))
  "List of commit scopes with descriptions."
  :type '(alist :key-type symbol :value-type string)
  :group 'version-control)

(defcustom commit-types-list
  '((build . "Changes related to the build system or external dependencies")
    (chore . "Routine tasks, maintenance, or tooling changes")
    (ci . "Changes to the Continuous Integration (CI) configuration or scripts")
    (docs . "Documentation changes")
    (feat . "A new feature for the user or a significant change")
    (fix . "A bug fix")
    (perf . "Performance improvements")
    (refactor . "Code refactorings, without changing external behavior")
    (revert . "Reverting a previous commit")
    (style . "Code style changes (e.g., formatting)")
    (test . "Adding or modifying tests"))
  "List of commit types with descriptions."
  :type '(alist :key-type symbol :value-type string)
  :group 'version-control)


(defcustom git-commit-keywords
  '()
  "Git commit list of keywords."
  :type '(list :options keywords-list)
  :group 'version-control)



(defun non-duplicate-commit-keywords (keywords)
  "Return a list of non-duplicate commit KEYWORDS.

   This function is primarily used in `git-commit-keywords'.
   It identifies unique keywords from a list of scope and type keywords.
   It subtracts the keywords that appear in both lists from the union of
   both lists.

   Returns:
     A list of non-duplicate keywords."
  (nconc git-commit-keywords
         (cl-set-difference
	  keywords
          git-commit-keywords :test #'equal)))



(setq git-commit-keywords
      (non-duplicate-commit-keywords
       (mapcar #'symbol-name (mapcar #'car commit-types-list))))

(setq git-commit-keywords
      (non-duplicate-commit-keywords
       (mapcar #'symbol-name (mapcar #'car commit-scope-list))))


(add-hook 'git-commit-mode-hook
	  (lambda ()
	    (if git-commit-mode
		(let* ((major-mode-name (intern (symbol-name major-mode)))
		       (mode-keyword-list (assoc major-mode-name cape-keyword-list)))
		  (if mode-keyword-list
		      (setq-local cape-keyword-list
				  (nconc mode-keyword-list
					 (cl-set-difference cape-keyword-list mode-keyword-list :test #'equal)))
		    (setq-local cape-keyword-list
				(cons `(,major-mode-name . ,git-commit-keywords) cape-keyword-list))))
	      (setq-local cape-keyword-list (default-value 'cape-keyword-list)))
	    (setq-local completion-at-point-functions
			(list (cape-capf-super #'tempel-complete #'cape-keyword
					       #'cape-elisp-symbol)))))


(defun choose-commit-type ()
  "Prompt the user to choose a commit type with descriptions."
  (let* ((options (mapcar (lambda (type) (cons (format "%-10s %s" (car type) (cdr type)) (car type)))
			  commit-types-list))
	 (chosen-type (completing-read "Choose a commit type: " options nil t nil nil '("feat")))
	 (key (car (split-string chosen-type)))
	 (entry (assoc (intern key) commit-types-list)))
    entry
    ))


(defun choose-scope ()
  "Prompt the user to choose a scope for a commit.

This function uses `completing-read` to allow the user to select
a scope from a predefined set.After selecting the scope, the user
is prompted to enter the commit type and description separately.

The final message displays the chosen commit type, scope, and description."
  (let* ((options (mapcar (lambda (scope) (cons (format "%-20s %s" (car scope) (cdr scope)) (car scope)))
			  commit-scope-list))
         (chosen-scope (completing-read "Choose Scope: " options nil t nil nil '("core")))
	 (key (car (split-string chosen-scope)))
	 (entry (assoc (intern key) commit-scope-list)))
    entry
    ))

(defun conventional-commits ()
  "Generate a conventional commit message interactively."
  ;; Replace with your implementation or define `choose-commit-type` and `choose-scope`.
  (let* ((commit-type (choose-commit-type))
         (commit-msg (if (y-or-n-p "Want to add a scope? ")
                         (format "%s(%s)" (car commit-type) (car (choose-scope)))
                       (format "%s" (car commit-type))))

         (final-msg (if (y-or-n-p "Broken change? ")
                        (format "%s!: " commit-msg)
                      (format "%s: " commit-msg))))

    (message "Generated commit message: %s" final-msg)
    final-msg)) ; Return the final commit message


(defun insert-conventional-commits ()
  "Insert a conventional commit message into the current buffer."
  (interactive)
  ;; Call the `conventional-commits` function to generate the commit message
  (let ((commit-msg (conventional-commits)))
    ;; Insert the generated commit message into the buffer
    (insert commit-msg)
    (message "Inserted conventional commit: %s" commit-msg)))

(provide 'conventional-commits)
;;; conventional-commits.el ends here
