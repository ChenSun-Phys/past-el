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


;; select region of screen
;; this part is motivated by desktop-environment.el

;; (makunbound 'pastel-screenshot-partial-command)
;; (defcustom pastel-screenshot-partial-command "scrot -s -e 'printf $f'"
;;   "Shell command taking a partial screenshot in the current working directory.

;; The shell command should let the user interactively select the
;; portion of the screen."
;;   :type 'string)

;; (defun pastel-sentinel (process event)
;;   "Wait for inkscape PROCESS to close but has no use for EVENT."
;;   (when (memq (process-status process) '(exit signal))
;;     ;; TODO: how to pass fig to sentinel?    
;;     (tex-paste-figure figlocal)
;;     )
;;   )

(defun pastel-screenshot-part (fig)
  "Take a partial screenshot in the current working directory.

The command asks the user to interactively select a portion of
the screen."
  ;; (interactive)
  (interactive "sFigure name: ")
  (let* ((log-buffer (get-buffer-create "*tex-paste-log*"))
	 (fdir_full (expand-file-name tex-paste-fig-dir default-directory))
	 (default-directory fdir_full)
	 ;; (figlocal fig)
	 )
    (message "Please select the part of your screen to shoot.")
    ;; (start-process-shell-command "pastel-screenshot" log-buffer pastel-screenshot-partial-command)
    (make-process :name "pastel-scrot"
    		  :coding 'raw-text		  
                  :command (list "scrot" "/tmp/%F_%T_$wx$h.png" "-s" "-e" "xclip -selection clipboard -target image/png -i $f")
                  ;; :command (list "scrot" "/tmp/%F_%T_$wx$h.png" "-e" "trash-put $f")		  
		  :sentinel (lambda (process event)
			      (when (memq (process-status process) '(exit signal))
				(tex-paste-figure fig)))
                  :buffer log-buffer
                  :stderr log-buffer		  
		  )
    
    )
  )

;; TODO: deine a minor mode and keybinding

;; (define-minor-mode desktop-environment-mode
;;   "Activate keybindings to control your desktop environment.

;; \\{desktop-environment-mode-map}"
;;   :global t
;;   :require 'desktop-environment
;;   :lighter " DE"
;;   (desktop-environment-exwm-set-global-keybindings desktop-environment-mode))

;; TODO: input
;; take file absolute path, (later paste copies it to the drawing folder)
;; DONE take screen region (saves to drawing folder)

;; TODO: output
;; org-mode insert
;; latex insert (done)

;; TODO: change module name to pastel

;; TODO: keybinding H-i H-y 

;; TODO: if supplied with a file name use file name, otherwise time stamp

;; TODO: acknowledge pjb
