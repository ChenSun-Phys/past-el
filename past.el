;; -*- lexical-binding: t -*-
;;; past.el --- paste images into a LaTeX document using xclip

;;; Commentary:
;; You can insert a new figure at point using pastel-figure
;; it is partly motivated by ink.el (https://github.com/foxfriday/ink)
;; TODO: Edit an existing figure with tex-edit-figure.

;;; Code:


;;
;; the tex strings
;;
(defvar pastel-fig-dir "drawings"
  "Default image directory.")

(makunbound 'pastel-graphics-string-tex)
(makunbound 'pastel-figure-string-tex)

(defvar pastel-graphics-string-tex "\n    \\includegraphics[width=.8\\textwidth]{%s/%s.png}"
  "Default latex insert for the graphics. Used inside a figure environment")

(defvar pastel-figure-string-tex "\n\\begin{figure}[ht]
    \\centering
    \\includegraphics[width=.8\\textwidth]{%s/%s.png}
    \\label{fig:%s}
    \\caption{}
\\end{figure}\n"
  "Default latex insert for the full figure.")

(defvar pastel-figure-string-org "[[%s/%s][%s]]"
  "Default org insert for the figure.")

;; debug checks
;; (eval (format pastel-graphics-string-tex "1" "2"))
;; (eval (format pastel-figure-string-tex "1" "2" "3"))
;; (let ((test (file-name-sans-extension "abc.test")))
;;   (message test))


;;
;; the main routine
;;
(defun pastel-insert-fig-tex (file)
  "Insert file into tex with includegraphics. If prefix is nil, it is wrapped by the figure environment."
  (let* ((fdir_full (expand-file-name pastel-fig-dir default-directory))
	 (fdir (file-relative-name fdir_full default-directory))
         (dname (file-name-directory file))
         (fname (file-name-nondirectory file))
         (name (file-name-sans-extension fname))
         (caption (downcase name))
         (ltex (format pastel-figure-string-tex fdir name caption))
	 (ltex_graph (format pastel-graphics-string-tex fdir name)))
    (message dname)
    (message (format "current-prefix-arg: %s" current-prefix-arg))
    ;; (insert ltex)
    (if current-prefix-arg
    	(insert ltex_graph)
      (insert ltex))
    )
  )


(defun pastel-insert-fig-org (file)
  "Insert file into org"
  ;; (let* (())
  ;;   (message dname)
  ;;   (message (format "current-prefix-arg: %s" current-prefix-arg))
  ;;   ;; (insert ltex)
  ;;   (if current-prefix-arg
  ;;   	(insert ltex_graph)
  ;;     (insert ltex))
  ;;   )
  (let* ((fdir_full (expand-file-name pastel-fig-dir default-directory))
	 (fdir (file-relative-name fdir_full default-directory))
         (dname (file-name-directory file))
         (fname (file-name-nondirectory file))
         (name (file-name-sans-extension fname))
         (caption (downcase name))
         (orgstr (format pastel-figure-string-org
			 (concat "./" fdir) fname caption)))
    (message dname)
    (insert orgstr)
    )
  )


;;
;; sentinels for pastel-figure
;;
(defun pastel-sentinel (process event)
  (when (memq (princ major-mode) '(org-mode))
      (org-display-inline-images))
  )

;;
;; the user interface
;;
(defun pastel-figure (fig)
  "Make a new figure named FIG and insert it at point."
  (interactive "sFigure name: ")
  (let* ((log-buffer (get-buffer-create "*pastel-log*"))
         (tdir (expand-file-name pastel-fig-dir default-directory))
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
                  :stderr log-buffer
                  :sentinel 'pastel-sentinel)		  
		  ;; :connection-type 'pipe
		  ;; :stderr (make-pipe-process :name "dd test null" :filter #'ignore))		  
    (when (memq (princ major-mode) '(org-mode))
      (pastel-insert-fig-org file))
    (when (memq (princ major-mode) '(latex-mode))
      (pastel-insert-fig-tex file))    
    )
  )




(defun pastel-screenshot-part ()
  "Take a partial screenshot in the current working directory.

The command asks the user to interactively select a portion of
the screen."
  (interactive)
  ;; (interactive "sFigure name: ")
  (let* ((log-buffer (get-buffer-create "*pastel-log*"))
	 (fdir_full (expand-file-name pastel-fig-dir default-directory))
	 (default-directory fdir_full)
	 ;; (figlocal fig)
	 )
    (message "Please select the part of your screen to shoot.")
    ;; (start-process-shell-command "pastel-screenshot" log-buffer pastel-screenshot-partial-command)
    (make-process :name "pastel-scrot"
    		  :coding 'raw-text		  
                  :command (list "scrot" "/tmp/%F_%T_$wx$h.png" "-s" "-e" "xclip -selection clipboard -target image/png -i $f")
		  :sentinel (lambda (process event)
		  	      (when (memq (process-status process) '(exit signal))
		  		(call-interactively 'pastel-figure)))
                  :buffer log-buffer
                  :stderr log-buffer		  
		  )
    
    )
  )




;;
;; keybindings
;;

(makunbound 'pastel-mode-map)
(defvar pastel-mode-map
  (let ((pastel--keybindings
         `(;; Brightness
           (,(kbd "H-y") . ,(function pastel-figure))
	   (,(kbd "H-o") . ,(function pastel-screenshot-part))))
        (map (make-sparse-keymap)))
    (dolist (keybinding pastel--keybindings)
      (define-key map (car keybinding) (cdr keybinding)))
    map)
  "Keymap for `pastel-mode'.")

(define-minor-mode pastel-mode
  "Activate keybindings to control your desktop environment.

\\{desktop-environment-mode-map}"
  :global nil
  :require 'pastel
  :lighter " PA"
  :keymap (pastel-mode-map))

(provide 'pastel)


;; DONE: deine a minor mode and keybinding

;; input
;; TODO take file absolute path, (later paste copies it to the drawing folder)
;; DONE take screen region (saves to drawing folder)
;; DONE take xournalpp handwriting copy

;; output
;; DONE: org-mode insert
;; DONE latex insert 

;; DONE: change module name to pastel

;; DONE: keybinding H-o H-y 

;; TODO: if supplied with a file name use file name, otherwise time stamp

;; TODO: acknowledge pjb
