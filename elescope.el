;;; elescope --- Summary

;; -*- lexical-binding: t -*-

;;; Commentary:
;;; Clone remote projects in a flash.

;;; Code:
(require 'request)

(defcustom elescope-root-folder nil
  "Directory to clone projects into."
  :group 'elescope-variable
  :type 'directory)

(defvar elescope-forges
  '(github gitlab)
  "Forges understood by elescope.")

(defvar elescope--debounce-timer nil)

(defun elescope--parse-gh (data)
  "Parse the DATA returned by GitHub and maps on the full name attribute."
  (mapcar
   (lambda (i) (alist-get 'full_name i))
   (seq-take (alist-get 'items data) 10)))

(defun elescope--call-gh (name)
  "Search for GitHub repositories matching NAME."
  (request
    "https://api.github.com/search/repositories"
    :params (list (cons "q" name))
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (let ((results (elescope--parse-gh data)))
                  (ivy-update-candidates results))))))

(defun elescope--search (str)
  "Handle the minibuffer STR query and search the relevant forge."
  (or
   (ivy-more-chars)
   (progn
     (and (timerp elescope--debounce-timer)
          (cancel-timer elescope--debounce-timer))
     (setf elescope--debounce-timer
           (run-at-time "0.7 sec" nil #'elescope--call-gh str))
     (list "" (format "Looking for repositories matching %s..." str)))
   0))

(defun elescope--ensure-root ()
  "Make sure there is a root to checkout into."
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
                :action (lambda (res) (message res))
                :caller 'elescope-checkout))))


(provide 'elescope)
;;; elescope.el ends here
