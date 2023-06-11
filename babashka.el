;;; babashka.el --- Babashka Tasks Interface -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2023, Mikhail Beliansky <mb@blaster.ai>
;;
;; Author: Mikhail Beliansky <mb@blaster.ai>
;; Maintainer: Mikhail Beliansky <mb@blaster.ai>
;; Version: 1.0.0
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
;; Provide a simple minibuffer completion interface to running babashka tasks for
;; a project. Looks for bb.edn in the current directory or any of it's parents, and if
;; found and file contains :tasks provides user with menu to choose a task to run.
;;
;;; Code:
(require 'parseedn)

(defmacro babashka--comment (&rest _)
  "Ignore body eval to nil."
  nil)

(defun babashka--buffer-dir ()
  "Return directory of the current buffer."
  (if-let ((file-name (buffer-file-name)))
      (file-name-directory file-name)))

(defun babashka--run-shell-command-in-directory (directory command &optional output-buffer)
  "Run a shell COMMAND in a DIRECTORY and display output in OUTPUT-BUFFER."
  (let ((default-directory directory))
    (async-shell-command command output-buffer)))

(defun babashka--find-bb-edn-upwards (dir)
  "Recursively search upwards from DIR for bb.edn file."
  (let ((bb-file (concat (file-truename dir) "/bb.edn")))
    (if (file-exists-p bb-file)
        (expand-file-name bb-file)
      (if (not (equal dir "/"))
          (babashka--find-bb-edn-upwards (expand-file-name "../" dir))
        nil))))

(defun babashka--finb-bb-edn-upwards-from-buffer ()
  "Recursively search upwards from current buffer directory."
  (babashka--find-bb-edn-upwards (babashka--buffer-dir)))

(defun babashka--get-tasks-hash-table (file-path)
  "List babashka tasks as hash table from edn file unde FILE-PATH."
  (thread-last
    file-path
    babashka--read-edn-file
    (gethash :tasks)))

;; (defun babashka--get-tasks-hash-table-from-buffer ()
;;   "Find bb.edn recursively upward from buffer path and return tasks hash-table."
;;   (babashka--get-tasks-hash-table (file-name-directory (buffer-file-name))))

(defun babashka--run-task (dir)
  "Select a task to run from bb.edn in DIR or it's parents."
  (if-let*
      ((bb-edn (babashka--find-bb-edn-upwards dir)))
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
    (message "No bb.edn found")))

;;;###autoload
(defun babashka-tasks (arg)
  "Run babashka tasks for project or any path.

Find bb.edn in current DIR or it's parents, and show a menu to select and run a task. When called with C-u prompts for directory."
  (interactive "P")
  (let* ((dir (if arg (read-file-name "Enter a path to bb.edn: ")
                (babashka--buffer-dir))))
    (if dir
        (babashka--run-task dir)
      (message "Not in a file buffer. Run babashka-tasks when visiting one of your project's files."))))

(provide 'babashka)
;;; babashka.el ends here
