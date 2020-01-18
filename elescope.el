;;; elescope --- Summary

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

(defun elescope--parse-gh (data)
  "Parse the DATA returned by GitHub and maps a list of names."
  (mapcar
   (lambda (i) (alist-get 'name i))
   (seq-take (alist-get 'items data) 10)))

(defun elescope--call-gh (name)
  "Search for GitHub repositories matching NAME."
  (request
       "https://api.github.com/search/repositories"
       :params '(("q" . name))
       :parser 'json-read
       :success (cl-function
                 (lambda (&key data &allow-other-keys)
                   (let ((results (elescope--parse-gh data)))
                     (ivy-update-candidates results))))))

(defun elescope--cb (str)
  "Handle the minibuffer STR query and search the relevant forge."
  (or
   (ivy-more-chars)
   (progn
     (message "firing request for %s" str)
     (elescope--call-gh str)
     (list "" (format "Looking for repositories matching %s..." str)))
   0))

(defun elescope-checkout (select-forges)
  "Prompt a repository name to search for.

If the function is called with the prefix SELECT-FORGES argument,
prompt a forge to search from (defaults to GitHub)."
  (interactive "P")
  (if select-forges
      (completing-read "Forge: " elescope-forges)
    (let ((forge 'github))
      (ivy-read "Project: " #'elescope--cb
                :dynamic-collection t
                :action (lambda (res) (message res))
                :caller 'elescope-checkout))))

(provide 'elescope)
;;; elescope.el ends here
