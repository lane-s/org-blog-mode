(use-package dash)
(use-package dash-functional :after dash)
(use-package s)
(use-package request)

;;; Org Blog Mode
;;;; Config
(defvar org-blog-server-url "http://localhost:3000/api"
  "The full url of the org-blog server to use")
(defvar org-blog-local-data-file (substitute-in-file-name "$HOME/.org-blog")
  "The full file path specifying where to store local data. Default is ~/.org-blog")

;;;; Helper
(setq org-blog--cached-file-hash nil)

(setq org-blog--handle-error
  (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                 (message "An error occurred: %S" error-thrown))))

(setq org-blog--sort-comparator
      (lambda (a b)
        (string>
         (alist-get 'updated_at a)
         (alist-get 'updated_at b))))

(defun org-blog--get-current-filename ()
  "Get the filename (not the full path) of the current file"
  (->> (current-buffer) buffer-file-name (s-split "/") -last-item))

(defun org-blog--get-current-path-relative-to-home ()
  (-let ((home (substitute-in-file-name "$HOME"))
         (full-path (buffer-file-name (current-buffer))))
    (s-chop-prefix home full-path)))

;;;; Faces

(defun org-blog--post-data->display-text (post-data)
  (-let* ((filename (alist-get 'filename post-data))
          (timestamps (s-format " ${created_at} ${updated_at}" 'aget post-data))
          (display-text (s-concat filename timestamps "\n")))
    (put-text-property 0 (length filename)
                       'font-lock-face 'font-lock-function-name-face display-text)
    (put-text-property (length filename) (length display-text)
                       'font-lock-face 'font-lock-constant-face display-text)
    display-text))


(defun org-blog--get-header-line ()
  (-let* ((header-begin "Viewing posts from ")
          (server-url org-blog-server-url)
          (header (s-concat header-begin server-url)))
    (put-text-property 0 (length header-begin)
                       'font-lock-face 'header-line header)
    (put-text-property (length header-begin) (length header)
                       'font-lock-face 'font-lock-constant-face header)
    header))

(defun org-blog--get-label-line ()
  (-let [labels "filename posted_at updated_at\n"]
    (put-text-property 0 (length labels)
                       'font-lock-face 'font-lock-keyword-face labels)
    labels))

;;;; Rest Client

(defun org-blog--get-or-create-file-hash ()
  "Get the hash table that maps from filenames to
  fully qualified file paths. If the table is not
  cached, then cache it"
  (unless org-blog--cached-file-hash
    (setq org-blog--cached-file-hash
          (with-current-buffer (find-file org-blog-local-data-file)
            (-let [hash-from-file (ignore-errors
                                    (read (buffer-substring (point-min) (point-max))))]
              (kill-buffer (current-buffer))
              (or hash-from-file (make-hash-table :test 'equal))))))
  org-blog--cached-file-hash)

(defun org-blog--update-file-hash (filename fullpath)
  "Update the hash table so that `filename' maps to `fullpath'"
  (-let [file-map (org-blog--get-or-create-file-hash)]
    (puthash filename fullpath file-map)
    (setq org-blog--cached-file-hash file-map)
    (with-current-buffer (find-file org-blog-local-data-file)
      (erase-buffer)
      (goto-char (point-min))
      (insert (prin1-to-string file-map))
      (save-buffer)
      (kill-buffer (current-buffer)))))

(defun org-blog--post (preview)
  (-let* ((post (buffer-substring (point-min)
                                  (point-max)))
          (filename (org-blog--get-current-filename))
          (path-relative-to-home (org-blog--get-current-path-relative-to-home))
          (body (json-encode `(("filename" . ,filename)
                               ("path_relative_to_home" . ,path-relative-to-home)
                               ("post" . ,post)
                               ("preview" . ,preview)))))

    (request (s-concat org-blog-server-url "/post")
             :type "POST"
             :data body
             :headers '(("Content-Type" . "application/json"))
             :parser 'json-read
             :success (if (string= preview "false")
                        (cl-function (lambda (&key data &allow-other-keys)
                                       (message "Post successful")))
                        (cl-function (lambda (&key data &allow-other-keys)
                                       (message "Post sent for preview"))))
             :error org-blog--handle-error)

    (when (string= preview "false")
      (org-blog--update-file-hash
       filename (buffer-file-name (current-buffer))))))

(defun org-blog--delete (filename)
  (request (s-concat org-blog-server-url "/post/" filename)
           :type "DELETE"
           :parser 'json-read
           :success (cl-function (lambda (&key data &allow-other-keys)
                                   (message "Post removed")))
           :error org-blog--handle-error))

(defun org-blog--list (success-callback)
  (request (s-concat org-blog-server-url "/posts")
           :type "GET"
           :parser 'json-read
           :success success-callback
           :error org-blog--handle-error))

;;;; Org Blog Mode
(defun org-blog--hide-cursor ()
  (setq-local cursor-type nil)
  (hl-line-mode t))
(define-derived-mode org-blog-mode special-mode "Org Blog"
  "Major mode for managing an org-blog server"
  (org-blog--hide-cursor))

(defun org-blog ()
  "Open an org-blog buffer for managing posts on an org-blog server"
  (interactive)
  (switch-to-buffer (get-buffer-create "*org-blog*"))
  (setq-local buffer-read-only t)
  (org-blog-mode)
  (org-blog-refresh)
  (with-eval-after-load "evil"
    (org-blog--hide-cursor)))

(defun org-blog--redraw-buffer ()
 (with-current-buffer (get-buffer-create "*org-blog*")
   (-let [buffer-read-only nil]
      (save-excursion
        (setq header-line-format (org-blog--get-header-line))
        (erase-buffer)
        (goto-char (point-min))
        (insert (org-blog--get-label-line))
        (-let [list-text (->> posts-cache
                              (-sort org-blog--sort-comparator)
                              (-map 'org-blog--post-data->display-text)
                              (s-join ""))]
          (insert list-text))
        (goto-char (point-min))
        (align-regexp (point) (point-max)
                      "\\(\\s-*\\)\\s-" 1 5 t)))
   (org-blog--hide-cursor)))

(defun org-blog-refresh ()
  "Request posts from the server and refresh the org-blog buffer"
  (interactive)
  (org-blog--list
   (cl-function (lambda (&key data &allow-other-keys)
                  (setq-local posts-cache data)
                  (org-blog--redraw-buffer)))))

(string> "2019-06-09T20:45:10Z" "2019-06-09T23:42:17Z")

(defun org-blog-post-buffer ()
  "Post the contents of the current buffer
  to the server at `org-blog-server-url`"
  (interactive)
  (org-blog--post "false"))
(defun org-blog-preview-buffer ()
  "Sends the contents of the current buffer
   to the org-blog server which will then
   emit an event which the frontend should
   listen for to render a preview"
  (interactive)
  (org-blog--post "true"))

(defun org-blog-delete-buffer-post ()
  (interactive)
  (org-blog--delete (org-blog--get-current-filename)))

(defun org-blog-list-posts ()
  (interactive)
  (org-blog--list))

(provide 'org-blog)
