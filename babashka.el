;;; babashka.el --- Babashka Tasks Interface -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2023, Mykhaylo Bilyanskyy <mb@m1k.pw>
;;
;; Author: Mykhaylo Bilyanskyy <mb@m1k.pw>
;; Maintainer: Mykhaylo Bilyanskyy <mb@m1k.pw>
;; Version: 1.0.3
;; Package-Requires: ((emacs "27.1") (parseedn "1.1.0"))
;;
;; Created: 11 Jun 2023
;;
;; URL: https://github.com/licht1stein/babashka.el
;;
;; License: GPLv3
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see
;; <http://www.gnu.org/licenses/>.
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; Provide a simple minibuffer completion interface to running Babashka tasks
;; for a project. Looks for bb.edn in the current directory or any of its
;; parents, and if found and file contains :tasks provides user with menu to
;; choose a task to run.
;;
;;; Code:
(require 'parseedn)

(defmacro babashka--comment (&rest _)
  "Ignore body eval to nil."
  nil)

(defun babashka--read-edn-file (file-path)
  "Read edn file under FILE-PATH and return as hash-table."
  (parseedn-read-str
   (with-temp-buffer
     (insert-file-contents file-path)
     (buffer-string))))

(defun babashka--run-shell-command-in-directory (directory command &optional output-buffer)
  "Run a shell COMMAND in a DIRECTORY and display output in OUTPUT-BUFFER."
  (let ((default-directory directory))
    (async-shell-command command output-buffer)))

(defun babashka--locate-bb-edn (&optional dir)
  "Recursively search upwards from DIR for bb.edn file."
  (if-let ((found (locate-dominating-file (or dir default-directory) "bb.edn")))
      (concat found "bb.edn")))

(defun babashka--get-tasks-hash-table (file-path)
  "List babashka tasks as hash table from edn file unde FILE-PATH."
  (thread-last
    file-path
    babashka--read-edn-file
    (gethash :tasks)))

(defun babashka--escape-args (s)
  "Shell quote parts of the string S that require it."
  (mapconcat #'shell-quote-argument (split-string s) " "))

(defun babashka--run-task (dir &optional do-not-recurse)
  "Select a task to run from bb.edn in DIR or its parents. If
DO-NOT-RECURSE is passed and is not nil, don't search for bb.edn in
DIR's parents."
  (if-let*
      ((bb-edn (if do-not-recurse
		   (let ((f (concat dir "/bb.edn"))) (and (file-exists-p f) f))
		   (babashka--locate-bb-edn dir)
		 )))
      (let* ((bb-edn-dir (file-name-directory bb-edn))
             (tasks (babashka--get-tasks-hash-table bb-edn))
             (task-names (thread-last tasks hash-table-keys (mapcar #'symbol-name)))
             (sorted-task-names (sort task-names #'string<)))
        (if tasks
            (thread-last
              sorted-task-names
              (completing-read "Run tasks: ")
              babashka--escape-args
              (format "bb %s")
              (babashka--run-shell-command-in-directory bb-edn-dir))
          (message "No tasks found in %s" bb-edn)))
    (message (if do-not-recurse
		 "No bb.edn found in directory."
		 "No bb.edn found in directory or any of the parents."))))

;;;###autoload
(defun babashka-tasks (arg)
  "Run Babashka tasks for project or any path.

Find bb.edn in current dir or its parents, and show a menu to select
and run a task. When called with interactive ARG prompts for directory."
  (interactive "P")
  (let* ((dir (if arg
		  (read-file-name "Enter a path to bb.edn: ")
		default-directory)))
    (if dir
        (babashka--run-task dir)
      (message "Not in a file buffer. Run babashka-tasks when visiting one of your project's files."))))

;;;###autoload
(defun babashka-project-tasks ()
  "Run a Babashka task from the bb.edn file in the project's root
directory.

This command is intended to be used as a `project-switch-project'
command by adding it to `project-switch-commands'. For example by
evaling:

(add-to-list \\='project-switch-commands
  \\='(babashka-project-tasks \"Babashka task\" \"t\"))"
  (interactive)
  (thread-first
    (project-current t)
    project-root
    (babashka--run-task t)))


(provide 'babashka)
;;; babashka.el ends here
