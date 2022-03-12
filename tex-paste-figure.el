;; -*- lexical-binding: t -*-
;;; tex-paste.el --- paste images into a LaTeX document using xclip

;;; Commentary:
;; You can insert a new figure at point using tex-paste-figure
;; it is partly motivated by ink.el (https://github.com/foxfriday/ink)
;; TODO: Edit an existing figure with tex-edit-figure.

;;; Code:


(defvar tex-paste-fig-dir "drawings"
  "Default image directory.")

(defun tex-paste-insert-fig (file)
  "Insert tex string associated with FILE."
  (let* ((fdir_full (expand-file-name tex-paste-fig-dir default-directory))
	 (fdir (file-relative-name fdir_full default-directory))
         (dname (file-name-directory file))
         (fname (file-name-nondirectory file))
         (name (file-name-sans-extension fname))
         (caption (downcase name))
         (ltex (format tex-paste-figure-string fdir name caption))
	 (ltex_graph (format tex-paste-graphics-string fdir name)))
    (message dname)
    (message (format "current-prefix-arg: %s" current-prefix-arg))
    (if current-prefix-arg
	(insert ltex_graph)
      (insert ltex))
    )
  )

;; check
;; (eval (format tex-paste-graphics-string "1" "2"))
;; (eval (format tex-paste-figure-string "1" "2" "3"))

(makunbound 'tex-paste-graphics-string)
(makunbound 'tex-paste-figure-string)

(defvar tex-paste-graphics-string "\n    \\includegraphics[width=.8\\textwidth]{%s/%s.png}"
  "Default latex insert for the graphics. Used inside a figure environment")

(defvar tex-paste-figure-string "\n\\begin{figure}[ht]
    \\centering
    \\includegraphics[width=.8\\textwidth]{%s/%s.png}
    \\label{fig:%s}
    \\caption{}
\\end{figure}\n"
  "Default latex insert for the full figure.")

;; simple test of elisp
;; (let ((test (file-name-sans-extension "abc.test")))
;;   (message test))

(defun tex-paste-figure (fig)
  "Make a new figure named FIG and insert it at point."
  (interactive "sFigure name: ")
  (let* ((log-buffer (get-buffer-create "*tex-paste-log*"))
         (tdir (expand-file-name tex-paste-fig-dir default-directory))
         (file (concat (file-name-as-directory tdir) fig ".png"))
	 )
    (print file)

    (make-directory tdir t)
    (make-process :name "xclip"
                  ;; :buffer output-buffer		  
    		  :coding 'raw-text		  
                  :command (list "xclip" "-select" "clipboard" "-t" "image/png" "-o")
    		  :filter (lambda (_proc chunk)
    			    (let* ((coding-system-for-write 'raw-text))
    			      (write-region chunk nil file 'append 'silent)))		  
                  :stderr log-buffer)
                  ;; :sentinel 'ink-sentinel)		  
		  ;; :connection-type 'pipe
		  ;; :stderr (make-pipe-process :name "dd test null" :filter #'ignore))		  
    (tex-paste-insert-fig file)
    )
  )



;; alternatively, one could output everything to a buffer and save the buffer afterward. 
