;;; elescope --- Clone remote projects in a flash -*- lexical-binding: t -*-

;; Copyright (C) 2020 Stéphane Maniaci

;; Author: Stéphane Maniaci <stephane dot maniaci at gmail.com>
;; URL: https://github.com/freesteph/elescope
;; Package-Version: 20200117.410
;; Version: 0.1

;; This file is NOT part of GNU Emacs.

;; elescope.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; elescope.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with elescope.el.  If not, see
;; <http://www.gnu.org/licenses/>.


;;; Commentary:
;;; Clone remote projects in a flash.

;;; Code:
(require 'request)
(require 'ivy)

(defgroup elescope nil "Variables related to the Elescope package.")

(defcustom elescope-root-folder nil
  "Directory to clone projects into."
  :group 'elescope
  :type 'directory)

(defcustom elescope-clone-depth 1
  "Depth argument to be passed to git clone.

Defaults to 1 which makes all clones shallow clones."
  :group 'eslescope
  :type 'integer)


(defcustom elescope-use-full-path nil
  "Use the full project path for the resulting clone.

If non-nil, use the full project path including
username/organisation to clone: cloning 'john/foo' and 'john/bar'
results in two 'foo' and 'bar' clones inside a parent 'john'
folder, as opposed to the default, flat hierarchy of 'foo' and
'bar'."
  :group 'elescope
  :type 'boolean)

(defcustom elescope-query-delay "0.7 sec"
  "Time to wait before considering the minibuffer input ready for querying.

How long to wait before considering the minibuffer input a valid
query.  This helps avoid firing a query for every single letter
typed.  Defaults to 0.7 sec and can be set to any value understood
by `run-at-time'."
  :group 'elescope
  :type 'string)

(defvar elescope-forges
  '(github gitlab)
  "Forges understood by elescope.")

(defvar elescope--debounce-timer nil)
(defvar elescope--strings '((no-results . "No matching repositories found.")))

(defun elescope--parse-gh (data)
  "Parse the DATA returned by GitHub and maps on the full name attribute."
  (let ((results (alist-get 'items data))
        (no-results-str (alist-get 'no-results elescope--strings)))
    (or (and (seq-empty-p results) (list "" no-results-str))
        (mapcar
         (lambda (i) (alist-get 'full_name i))
         (seq-take results 10)))))

(defun elescope--call-gh (name)
  "Search for GitHub repositories matching NAME and update the minibuffer with the results."
  (request
    "https://api.github.com/search/repositories"
    :params (list (cons "q" name))
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (let ((results (elescope--parse-gh data)))
                  (ivy-update-candidates results))))))

(defun elescope--search (str)
  "Debounce minibuffer input and pass the resulting STR to the lookup function."
  (or
   (ivy-more-chars)
   (progn
     (and (timerp elescope--debounce-timer)
          (cancel-timer elescope--debounce-timer))
     (setf elescope--debounce-timer
           (run-at-time elescope-query-delay nil #'elescope--call-gh str))
     (list "" (format "Looking for repositories matching %s..." str)))
   0))

(defun elescope--clone-gh (path)
  "Clone the GitHub project identified by PATH."
  (unless (or (not path)
              (not (seq-contains path ?/))
              (equal path (alist-get 'no-results elescope--strings)))
    (let* ((url (format "https://github.com/%s" path))
	   (name (if elescope-use-full-path path (cadr (split-string path "/"))))
           (destination (expand-file-name name elescope-root-folder))
           (command (format
                     "git clone%s %s %s"
		     (if elescope-clone-depth
			 (format
			  " --depth=%s"
			  elescope-clone-depth)
		       "")
                     url
                     destination)))
      (if (file-directory-p destination)
	  (find-file destination)
	(if (eql 0 (shell-command command))
            (find-file destination)
          (user-error "Something went wrong whilst cloning the project"))))))

(defun elescope--ensure-root ()
  "Stop execution if no root directory is set to clone into."
  (unless (and
           elescope-root-folder
           (file-directory-p elescope-root-folder))
    (user-error "You need to set the 'elescope-root-folder' variable before
    checking out any project")))

(defun elescope-checkout (select-forges)
  "Prompt a repository name to search for.

If the function is called with the prefix SELECT-FORGES argument,
prompt a forge to search from (defaults to GitHub)."
  (interactive "P")
  (elescope--ensure-root)
  (if select-forges
      (completing-read "Forge: " elescope-forges)
    (let ((forge 'github))
      (ivy-read "Project: " #'elescope--search
                :dynamic-collection t
                :action #'elescope--clone-gh
                :caller 'elescope-checkout))))


(provide 'elescope)
;;; elescope.el ends here
