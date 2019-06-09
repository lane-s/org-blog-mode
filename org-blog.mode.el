(use-package dash)
(use-package dash-functional :after dash)
(use-package s)
(use-package request)

(provide 'org-blog-mode)

;;; Org Blog Mode
;;;; Config
 (defvar org-blog-mode-server-url "http://localhost:3000/api")

;;;; Helper
(defvar org-blog-mode--handle-error
  (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                 (message "An error occurred: %S" error-thrown))))

(defun org-blog-mode--get-current-filename ()
  (->> (current-buffer) buffer-file-name (s-split "/") -last-item))

(defun org-blog-mode--post (preview)
  (-let* ((post (buffer-substring (point-min)
                                  (point-max)))
          (filename (org-blog-mode--get-current-filename))
          (body (json-encode `(("filename" . ,filename)
                               ("post" . ,post)
                               ("preview" . ,preview)))))
    (request (s-concat org-blog-mode-server-url "/post")
             :type "POST"
             :data body
             :headers '(("Content-Type" . "application/json"))
             :parser 'json-read
             :success (cl-function (lambda (&key data &allow-other-keys)
                                     (message "Post successful")))
             :error org-blog-mode--handle-error)))

(defun org-blog-mode--delete (filename)
  (request (s-concat org-blog-mode-server-url "/post/" filename)
           :type "DELETE"
           :parser 'json-read
           :success (cl-function (lambda (&key data &allow-other-keys)
                                   (message "Post removed")))
           :error org-blog-mode--handle-error))

(defun org-blog-mode--list ()
  (request (s-concat org-blog-mode-server-url "/posts")
           :type "GET"
           :parser 'json-read
           :success (cl-function (lambda (&key data &allow-other-keys)
                                   (prin1 data)))
           :error org-blog-mode--handle-error))


;;;; Org Blog Mode
(defun org-blog-mode-post-buffer ()
  "Post the contents of the current buffer
  to the server at `org-blog-mode-server-url`"
  (interactive)
  (org-blog-mode--post "false"))
(defun org-blog-mode-preview-buffer ()
  "Sends the contents of the current buffer
   to the org-blog server which will then
   emit an event which the frontend should
   listen for to render a preview"
  (interactive)
  (org-blog-mode--post "true"))

(defun org-blog-mode-delete-buffer-post ()
  (interactive)
  (org-blog-mode--delete (org-blog-mode--get-current-filename)))

(defun org-blog-mode-list-posts ()
  (interactive)
  (org-blog-mode--list))

