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
  "Forges understood by elescope")

(defun elescope--parse-gh (data)
  "Parses the DATA returned by GitHub and maps a list of names."
  (mapcar
   (lambda (i) (alist-get 'name i))
   (seq-take (alist-get 'items data) 10)))

(elescope--parse-gh peach-data)

(defun elescope--cb (str)
  (or
   (ivy-more-chars)
   (progn
     (message "firing request for %s" str)
     (request
       "https://api.github.com/search/repositories"
       :params '(("q" . str))
       :parser 'json-read
       :success (cl-function
                 (lambda (&key data &allow-other-keys)
                   (ivy-update-candidates (elescope--parse-gh data)))))
     0)))

(defun elescope-checkout (select-forges)
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
