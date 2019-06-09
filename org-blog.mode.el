(use-package dash)
(use-package dash-functional :after dash)
(use-package s)
(use-package request)

(provide 'org-blog-mode)

;;; Org Blog Mode
;;;; Config
(defvar org-blog-mode-server-url "http://localhost:3000/api"
  "The full url of the org-blog server to use")
(defvar org-blog-mode-local-data-file (substitute-in-file-name "$HOME/.org-blog-mode")
  "The full file path specifying where to store local data. Default is ~/.org-blog-mode")

;;;; Helper
(defvar org-blog-mode--cached-file-hash nil)

(defvar org-blog-mode--handle-error
  (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                 (message "An error occurred: %S" error-thrown))))

(defun org-blog-mode--get-current-filename ()
  "Get the filename (not the full path) of the current file"
  (->> (current-buffer) buffer-file-name (s-split "/") -last-item))

(defun org-blog-mode--get-or-create-file-hash ()
  "Get the hash table that maps from filenames to
  fully qualified file paths. If the table is not
  cached, then cache it"
  (unless org-blog-mode--cached-file-hash
    (setq org-blog-mode--cached-file-hash
          (with-current-buffer (find-file org-blog-mode-local-data-file)
            (-let [hash-from-file (ignore-errors
                                    (read (buffer-substring (point-min) (point-max))))]
              (kill-buffer (current-buffer))
              (or hash-from-file (make-hash-table :test 'equal))))))
  org-blog-mode--cached-file-hash)

(defun org-blog-mode--update-file-hash (filename fullpath)
  "Update the hash table so that `filename' maps to `fullpath'"
  (-let [file-map (org-blog-mode--get-or-create-file-hash)]
    (puthash filename fullpath file-map)
    (setq org-blog-mode--cached-file-hash file-map)
    (with-current-buffer (find-file org-blog-mode-local-data-file)
      (erase-buffer)
      (goto-char (point-min))
      (insert (prin1-to-string file-map))
      (save-buffer)
      (kill-buffer (current-buffer)))))

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
             :success (if (string= preview "false")
                        (cl-function (lambda (&key data &allow-other-keys)
                                       (message "Post successful")))
                        (cl-function (lambda (&key data &allow-other-keys)
                                       (message "Post sent for preview"))))
             :error org-blog-mode--handle-error)

    (when (string= preview "false")
      (org-blog-mode--update-file-hash
       filename (buffer-file-name (current-buffer))))))

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

