;;; babashka.el --- Babashka Tasks Interface -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2023, Mykhaylo Bilyanskyy <mb@m1k.pw>
;;
;; Author: Mykhaylo Bilyanskyy <mb@m1k.pw>
;; Maintainer: Mykhaylo Bilyanskyy <mb@m1k.pw>
;; Version: 1.0.2
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
;; Provide a simple minibuffer completion interface to running babashka tasks
;; for a project. Looks for bb.edn in the current directory or any of it's
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

(defun babashka--run-task (dir)
  "Select a task to run from bb.edn in DIR or it's parents."
  (if-let*
      ((bb-edn (babashka--locate-bb-edn dir)))
      (let* ((bb-edn-dir (file-name-directory bb-edn))
             (tasks (babashka--get-tasks-hash-table bb-edn))
             (task-names (thread-last tasks hash-table-keys (mapcar 'symbol-name)))
             (sorted-task-names (sort task-names 'string<)))
        (if tasks
            (thread-last
              sorted-task-names
              (completing-read "Run tasks: ")
              (format "bb %s")
              (babashka--run-shell-command-in-directory bb-edn-dir))
          (message (format "No tasks found in %s" bb-edn))))
    (message "No bb.edn found in directory or any of the parents.")))

;;;###autoload
(defun babashka-tasks (arg)
  "Run babashka tasks for project or any path.

Find bb.edn in current dir or it's parents, and show a menu to select
and run a task. When called with interactive ARG prompts for directory."
  (interactive "P")
  (let* ((dir (if arg (read-file-name "Enter a path to bb.edn: ") default-directory)))
    (if dir
        (babashka--run-task dir)
      (message "Not in a file buffer. Run babashka-tasks when visiting one of your project's files."))))

(provide 'babashka)
;;; babashka.el ends here
