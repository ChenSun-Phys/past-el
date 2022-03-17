;; -*- lexical-binding: t -*-
;;; past.el --- paste images into a LaTeX document using xclip

;;; Commentary:
;; You can insert a new figure at point using pastel-figure
;; it is partly motivated by ink.el (https://github.com/foxfriday/ink)
;; TODO: Edit an existing figure with tex-edit-figure.

;;; Code:


;;
;; the strings to be inserted
;;
(defvar pastel-fig-dir "drawings"
  "Default image directory.")

(makunbound 'pastel-graphics-string-tex)
(makunbound 'pastel-figure-string-tex)
(makunbound 'pastel-figure-string-org)

(defvar pastel-graphics-string-tex "\n    \\includegraphics[width=.8\\textwidth]{%s/%s.png}"
  "Default latex insert for the graphics. Used inside a figure environment")

(defvar pastel-figure-string-tex "\n\\begin{figure}[ht]
    \\centering
    \\includegraphics[width=.8\\textwidth]{%s/%s.png}
    \\label{fig:%s}
    \\caption{}
\\end{figure}\n"
  "Default latex insert for the full figure.")

(defvar pastel-figure-string-org "\n:IMAGE_INFO:
#+NAME: %s
#+CAPTION: 
#+ATTR_HTML: :width 500px
#+ATTR_LATEX: :width .8\\linewidth
:END:
#+ATTR_ORG: :width 500
[[%s/%s]]"
  "Default org insert for the figure.")

;; debug checks
;; (eval (format pastel-graphics-string-tex "1" "2"))
;; (eval (format pastel-figure-string-tex "1" "2" "3"))
;; (let ((test (file-name-sans-extension "abc.test")))
;;   (message test))








;;
;; related to org mode
;;
;; reverse needed b/c this method is based on buffer position
(defun pastel-org-preprocess-drawers (_)
  (let ((img-drws (reverse (org-element-map (org-element-parse-buffer)
                   'drawer (lambda (drw)
                     (when
                         (string= "IMAGE_INFO" (org-element-property :drawer-name drw)) drw))))))
    (cl-loop for drw in img-drws do
         (setf (buffer-substring (org-element-property :begin drw) (org-element-property :end drw))
	       (buffer-substring (org-element-property :contents-begin drw) (org-element-property :contents-end drw))))))


;; some enhancement of org mode
;; function that automatically update equation \tags
;; taken from https://stackoverflow.com/questions/26090651/emacs-org-mode-increment-equation-numbers-with-latex-preview
(defun pastel-update-tag ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((count 1))
      (while (re-search-forward "\\\\tag{\\([0-9]+\\)}" nil t)
        (replace-match (format "%d" count) nil nil nil 1)
        (setq count (1+ count)))))
  )


;; function that automatically remvoe equation \tags
;; used before exporting to latex
(defun pastel-remove-tag (_)
  (save-excursion
;;     ;; those \tags after \begin{align}
;;     (goto-char (point-min))
;;     (while (re-search-forward "\\(\\\\tag{[0-9]+}
;; \\\\end{\\)" nil t)
;;       (princ (match-string 1))
;;       (replace-match "
;; \\\\end{" nil nil nil 1))

    ;; those \tags residing a single line
    (goto-char (point-min))
    (while (re-search-forward "\\(^\\\\tag{[0-9]+}
\\)" nil t)
      (princ (match-string 1))
      (replace-match "" nil nil nil 1))

    ;; the \tags mixed with other stuff
    (goto-char (point-min))
    (while (re-search-forward "\\(\\\\tag{[0-9]+}\\)" nil t)
      (princ (match-string 1))
      (replace-match "" nil nil nil 1))
    
;;     ;; those \tags before \end{align}
;;     (goto-char (point-min))    
;;     (while (re-search-forward "\\(\\\\tag{[0-9]+}
;; \\)" nil t)
;;       (princ (match-string 1))
;;       (replace-match "" nil nil nil 1))
    
    ))
(add-hook 'org-export-before-processing-hook 'pastel-remove-tag)
;; this remove the tag even when being exported to html
;; this is not intended. It is only intended for latex export.
;; However, with org-ref installed, I do not see any problem in the
;; exported html file

;; org hooks
(add-hook 'org-export-before-processing-hook 'pastel-org-preprocess-drawers)

;; recommend adding the following to init.el
;; (add-hook 'org-mode-hook 'latex-math-mode)
;; (setq org-startup-with-inline-images t)
;; (setq org-startup-with-latex-preview t)
;; (setq org-pretty-entities t)
;; (setq TeX-insert-braces nil)








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
  (let* ((fdir_full (expand-file-name pastel-fig-dir default-directory))
	 (fdir (file-relative-name fdir_full default-directory))
         (dname (file-name-directory file))
         (fname (file-name-nondirectory file))
         (name (file-name-sans-extension fname))
         (caption (downcase name))
         (orgstr (format pastel-figure-string-org fname
			 (concat "./" fdir) fname caption)))
    (message dname)
    (insert orgstr)
    )
  )


;;
;; sentinels for pastel-figure
;;
(defun pastel-figure-sentinel (process event)
  ;; to be safe, it's run only after receiving the exit signal
  (when (memq (process-status process) '(exit signal))
    (when (memq (princ major-mode) '(org-mode))
      (org-display-inline-images)))
  )

;; the following needs to be wrapped by let,
;; since it needs filename retrieved from filter

;; (defun pastel-file-sentinel (process event)
;;   (when (memq (process-status process) '(exit signal))
;;     (when (memq (princ major-mode) '(org-mode))
;;       (pastel-insert-fig-org filename))
;;     (when (memq (princ major-mode) '(latex-mode))
;;       (pastel-insert-fig-tex filename)))  
;;   ;; (when (memq (princ major-mode) '(org-mode))
;;   ;;     (org-display-inline-images))
;;   )








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
                  :sentinel 'pastel-figure-sentinel)		  
		  ;; :connection-type 'pipe
		  ;; :stderr (make-pipe-process :name "dd test null" :filter #'ignore))		  
    (when (memq (princ major-mode) '(org-mode))
      (pastel-insert-fig-org file))
    (when (memq (princ major-mode) '(latex-mode))
      (pastel-insert-fig-tex file))    
    )
  )


(defun pastel-file ()
  "Make a figure using path in clipboard."
  (interactive)  
  (let* ((filename nil)
	 (log-buffer (get-buffer-create "*pastel-log*")))
    
    (defun pastel-file-sentinel (process event)
      (when (memq (process-status process) '(exit signal))
	(when (memq (princ major-mode) '(org-mode))
	  (pastel-insert-fig-org filename)
	  (org-display-inline-images))
	(when (memq (princ major-mode) '(latex-mode))
	  (pastel-insert-fig-tex filename))
	;; actually copy the figure
	(copy-file filename (concat "./drawings/" (file-name-nondirectory filename)))
	;;
	))
    
    (make-process :name "xclip"
                  ;; :buffer output-buffer		  
    		  :coding 'raw-text		  
                  :command (list "xclip" "-select" "clipboard" "-t" "TEXT" "-o")
    		  :filter (lambda (_proc chunk)
    			    (let* ((coding-system-for-write 'raw-text))
			      (setq filename chunk)
			      (message (format "file name is %s" filename))
			      ))		  
                  :stderr log-buffer
                  :sentinel 'pastel-file-sentinel)		  
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
                  :command (list "scrot" "/tmp/%F_%T_$wx$h.png" "-sf" "-e" "xclip -selection clipboard -target image/png -i $f")
		  :sentinel (lambda (process event)
		  	      (when (memq (process-status process) '(exit signal))
		  		(call-interactively 'pastel-figure)))
                  :buffer log-buffer
                  :stderr log-buffer		  
		  )
    
    )
  )


;; function that can be used inside org-mode
;; to automatically update \tag{#} and toggle inline display of eqns.
(defun pastel-display-eqs ()
  (interactive)
  (if current-prefix-arg
      (progn (pastel-update-tag)
	     (let ((current-prefix-arg '(16)))
	       (call-interactively 'org-toggle-latex-fragment)))
    (call-interactively 'org-toggle-latex-fragment)))


;;
;; keybindings
;;

(makunbound 'pastel-mode-map)
(defvar pastel-mode-map
  (let ((pastel--keybindings
         `(;; Brightness
           (,(kbd "H-y") . ,(function pastel-figure))
           (,(kbd "H-Y") . ,(function pastel-file))	   
	   (,(kbd "H-o") . ,(function pastel-screenshot-part))
	   (,(kbd "H-l") . ,(function pastel-display-eqs))))
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

;;
;; to use inside LaTeX and org mode, simply add to the corresponding hooks
;;
;; (add-hook 'LaTeX-mode-hook 'pastel-mode)
;; (add-hook 'org-mode-hook 'pastel-mode)

;; Milestones

;; input
;; DONE take file absolute path, (later paste copies it to the drawing folder)
;; DONE take screen region (saves to drawing folder)
;; DONE take xournalpp handwriting copy

;; output
;; DONE: org-mode insert
;; DONE latex insert 

;; misc
;; DONE: deine a minor mode and keybinding
;; DONE: change module name to pastel
;; DONE: keybinding H-o H-y 
;; TODO: if supplied with a file name use file name, otherwise time stamp
;; DONE: acknowledge pjb

;; org mode enhancement
;; TODO: add those snippets into org mode folder automatically
;; DONE: add latex snippets into org mode
;; DONE: auto update tag
;; DONE: add key binding to update tag then toggle inline
;; DONE: remove \tag{} before export to latex
;; TODO: auto toggle equations (ref: https://ivanaf.com/automatic_latex_fragment_toggling_in_org-mode.html)
;; TODO: add docstring
;; TODO: add cellphone support: remotely turn on cellphone camera, capture part of the notebook, then crop on computer screen and insert to org-mode
