;;; achso.el --- AchSo Frontend for GNU Emacs          -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Yuto Kisuge

;; Author: Yuto Kisuge <mail@yo.eki.do>
;; URL: https://github.com/kissge/achso-emacs
;; Version: 0.0.1

;; This program is licensed under the MIT License (X11 License).

;;; Commentary:

;; This is a part of AtCoder Helper Suite.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'let-alist)

;; Customizable variables

(defgroup achso nil
  "AchSo configurations")

(defcustom achso-command (executable-find "achso")
  "AchSo backend command"
  :group 'achso)

(defcustom achso-auto-check-after-submit t
  "If set to non-nil, automatic periodic checks follow achso-submit command."
  :group 'achso)

(defcustom achso-auto-check-interval 8
  "Interval durations for automatic checks (in seconds)"
  :group 'achso)

;; Internal variables

(defvar achso-domain nil
  "Domain of the current AtCoder contest")

(defvar achso-languages nil
  "List of languages available in the current contest")

(defvar achso-tasks nil
  "List of tasks in the current contest")

;; Even more internal variables (don't touch)

(defvar achso-lighter " Ach")
(defvar achso-working-directory nil)
(defvar achso-auto-check-timer nil)
(defvar achso-task-history nil)
(defvar achso-language-history nil)

;; Commands

;;;###autoload
(defun achso-start-contest (domain)
  (interactive "sDomain (e.g. abc001): ")
  (set-variable 'achso-domain nil)

  (let ((achso-domain domain))
    (message "Fetching task list...")
    (achso-get-task-list)
    (message "Fetching language list...")
    (achso-get-language-list)
    (message "Extracting sample inputs and outputs...")
    (achso-extract-sample-to-working-directory))

  ;; now everything was OK, achso-domain can be set safely
  (set-variable 'achso-domain domain)
  (set-variable 'achso-lighter (format " Ach[%s]" domain))
  (set-variable 'achso-working-directory default-directory)

  (achso-mode 1)

  (message "Good luck."))

(define-globalized-minor-mode achso-mode achso-minor-mode achso-minor-mode-on
  :group 'achso
  :require 'achso)

(define-minor-mode achso-minor-mode
  "AchSo: AtCoder Helper Suite"
  :lighter (:eval achso-lighter)
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-1") 'achso-test-and-submit)
            (define-key map (kbd "C-c C-2") 'achso-submit)
            (define-key map (kbd "C-c C-3") 'achso-show-current-status)
            map)
  :group 'achso)

(defun achso-minor-mode-on (&optional init)
  (interactive)
  (if (achso-is-working-directory)
      (achso-minor-mode init)))

(defun achso-extract-sample-to-directory (directory)
  (interactive "DPath to directory: ")
  (let-alist (achso-check-contest-started-and-invoke "sample" achso-domain (expand-file-name directory))
    .files))

(defun achso-extract-sample-to-working-directory ()
  (interactive)
  (let ((files (achso-extract-sample-to-directory achso-working-directory)))
    (if (zerop (length files))
        (message "No samples found.")
      (message "%s and other %d files were saved."
               (aref files 0) (1- (length files))))))

(defun achso-submit (task-id filename language)
  (interactive
   (let ((default-task-id (or (achso-guess-task-id-from-file-name buffer-file-name)
                              (if (consp achso-task-history)
                                  (car achso-task-history))))
         (default-language (if (consp achso-language-history)
                               (car achso-language-history))))
     (list (let ((completion-ignore-case t))
             (completing-read (if default-task-id
                                  (format "Task ID (%s): " default-task-id)
                                "Task ID: ")
                              (achso-task-id-list)
                              nil t nil 'achso-task-history
                              default-task-id))
           (read-file-name (format "Source code file (%s): " buffer-file-name) nil nil buffer-file-name)
           (let ((completion-ignore-case t))
             (completing-read (if default-language
                                  (format "Language (%s): " default-language)
                                "Language: ")
                              achso-languages
                              nil t nil 'achso-language-history
                              default-language)))))
  (let-alist (achso-check-contest-started-and-invoke "submit" achso-domain task-id (expand-file-name filename) language)
    (message "Submitted (%s)" .submission_id)
    (if achso-auto-check-after-submit
        (achso-start-auto-check))))

(defun achso-test (task-id command)
  (interactive
   (let ((default-task-id (or (achso-guess-task-id-from-file-name buffer-file-name)
                              (if (consp achso-task-history)
                                  (car achso-task-history)))))
     (list (let ((completion-ignore-case t))
             (completing-read (if default-task-id
                                  (format "Task ID (%s): " default-task-id)
                                "Task ID: ")
                              (achso-task-id-list)
                              nil t nil 'achso-task-history
                              default-task-id))
           (read-file-name (format "Command (%s): " buffer-file-name) nil nil buffer-file-name))))
  (let-alist (achso-check-contest-started-and-invoke "test" task-id (expand-file-name command) (expand-file-name achso-working-directory))
    (if (zerop .wrong)
        (if (zerop .correct)
            (prog2 (message "No tests were run") nil)
          (progn
            (if (get-buffer "*achso-test-results*")
                (kill-buffer "*achso-test-results*"))
            (message "All %d tests successful!" .correct)
            t))
      (message "%d of %d tests failed" .wrong (+ .wrong .correct))
      (with-output-to-temp-buffer "*achso-test-results*"
        (princ "Task ID: ") (princ task-id)
        (princ "\nExecuted command: ") (princ command)
        (princ "\nResults: ") (princ .correct) (princ "/") (princ (+ .wrong .correct))
        (princ "\n=================================\n\n")
        (dotimes (i (length .output))
          (let-alist (aref .output i)
            (let ((pass (string= .system .expected)))
              (princ (format "Case #%d: %s\n" (1+ i) (if pass "OK" "NG")))
              (princ "-----------\n")
              (princ "System output:\n")
              (princ .system)
              (unless pass
                (princ "Expected output:\n")
                (princ .expected))
              (unless (string= .stderr "")
                (princ "Standard error output:\n")
                (princ .stderr))
              (princ "\n"))))
        (princ "=================================\n")
        nil))))

(defun achso-test-and-submit (task-id command)
  (interactive
   (let ((default-task-id (or (achso-guess-task-id-from-file-name buffer-file-name)
                              (if (consp achso-task-history)
                                  (car achso-task-history)))))
     (list (let ((completion-ignore-case t))
             (completing-read (if default-task-id
                                  (format "Task ID (%s): " default-task-id)
                                "Task ID: ")
                              (achso-task-id-list)
                              nil t nil 'achso-task-history
                              default-task-id))
           (read-file-name (format "Command (%s): " buffer-file-name) nil nil buffer-file-name))))
  (if (achso-test task-id command)
    (apply 'achso-submit task-id
           (let ((default-language (if (consp achso-language-history)
                                       (car achso-language-history))))
             (list
              (read-file-name
               (format "Test successful! Source code file to submit (%s): " buffer-file-name) nil nil buffer-file-name)
              (let ((completion-ignore-case t))
                (completing-read
                 (if default-language (format "Language (%s): " default-language) "Language: ")
                 achso-languages nil t nil 'achso-language-history default-language)))))))

(defun achso-start-auto-check ()
  (interactive)
  (if achso-auto-check-timer
      (achso-cancel-auto-check))
  (set-variable 'achso-auto-check-timer
                (run-with-timer achso-auto-check-interval achso-auto-check-interval 'achso-show-current-status)))

(defun achso-cancel-auto-check ()
  (interactive)
  (cancel-timer achso-auto-check-timer))

(defun achso-show-current-status ()
  (interactive)
  (let-alist (achso-check-contest-started-and-invoke "submissions" achso-domain "--only-first" "--preserve-judging-status")
    (let ((submissions .submissions) continue)
      (message
       "%s (%s)"
       (mapconcat
        (lambda (task)
          (let-alist task
            (let* ((slug .slug)
                   (id .id)
                   (submission (cl-find-if
                                (lambda (submission) (let-alist submission (string= .slug slug)))
                                submissions)))
              (if submission
                  (let-alist submission
                    (if (or (string= .status "WJ") .judge_progress)
                        (setq continue t))
                    (format "%s: %s (%s)" id .status
                            (or (let-alist submission .score) "--")))
                (format "%s: -- (--)" id))))) achso-tasks ", ")
       (current-time-string))
      (if (and (not continue) achso-auto-check-timer)
          (achso-cancel-auto-check)))))

;; Internal functions

(defun achso-task-id-list ()
  (mapcar (lambda (x) (let-alist x .id)) achso-tasks))

(defun achso-get-language-list ()
  (let-alist (achso-check-contest-started-and-invoke "languages" achso-domain)
    (set-variable 'achso-languages (append .languages nil))))

(defun achso-get-task-list ()
  (let-alist (achso-check-contest-started-and-invoke "tasks" achso-domain)
    (set-variable 'achso-tasks .tasks)))

(defun achso-guess-task-id-from-file-name (filename)
  (if achso-tasks
      (let* ((task-id-list (achso-task-id-list))
             (base (file-name-sans-extension (file-name-base filename)))
             (base-upper (upcase base))
             (base-lower (downcase base)))
        (if (member base-upper task-id-list) base-upper
          (if (member base-lower task-id-list) base-lower)))))

(defun achso-is-working-directory ()
  (string= default-directory achso-working-directory))

(defun achso-check-contest-started ()
  "Throw an error if achso-domain is nil"
  (unless achso-domain
    (user-error "Start contest at first")))

(defun achso-check-error (response)
  "Throw an error if backend's response is error"
  (let-alist response
    (if (eq .error :json-false)
        response
      (let-alist .error
        (error (concat .body " [" .type "]"))))))

(defun achso-invoke (&rest args)
  "Invoke AchSo backend functions."
  (let* ((command (concat achso-command " " (mapconcat 'shell-quote-argument args " ") " -o json"))
         (response (shell-command-to-string command)))
    (princ (concat command " => " response "\n"))
    (let ((response (json-read-from-string response)))
      (achso-check-error response))))

(defun achso-check-contest-started-and-invoke (&rest args)
  (achso-check-contest-started)
  (apply #'achso-invoke args))

(provide 'achso)
;;; achso.el ends here
