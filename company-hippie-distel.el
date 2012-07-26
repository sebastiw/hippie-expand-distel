
(require 'hippie-expand-distel)

;;;###autoload
(defun company-hippie-distel (command &optional args &rest ignore)
  (interactive (list 'interactive))
  (case command
    (interactive (company-begin-backend 'company-hippie-distel))
    (prefix ;; nar ska functionen slas pa? returnera ordet man ar pa.
     (let* ((isfunc (eql (char-before) ?:))
	    (word (if isfunc
		      (buffer-substring (point) (save-excursion
						  (backward-char)
						  (skip-syntax-backward "w")
						  (point)))
		    (company-grab-word))))
       (and
	(eq (derived-mode-p 'erlang-mode) 'erlang-mode)
	(or
	 word
	 'stop))))
    (candidates ;; vilka ord returneras
     (erl-company-candidates args))
    (meta
     (let* ((mod (erl-company-get-module-p args))
	    (fun (erl-company-get-function-p args))
	    (met (erl-company-get-metadoc mod fun))
	    doc)
       (erl-company-format-arglists met)))
    (doc-buffer
     (let* ((mod (erl-company-get-module-p args))
	    (fun (erl-company-get-function-p args))
	    (doc (erl-company-local-docs mod fun))
	    (edocs (unless doc (erl-company-get-docs-from-internet-p mod fun)))
	    descr)

       (unless (and doc edocs)
	 (dolist (arg try-erl-args-cache descr)
	   (setq descr (concat descr mod ":" fun (format "%s" arg) "\n"))))
       (with-current-buffer (company-doc-buffer)
	 (when doc (insert (format "%s" doc)))
	 (when edocs (insert edocs))
	 (unless edocs (insert descr))
	 (unless descr (insert (format "Couldn't find any help for %s:%s" mod fun)))
	 (current-buffer))))
    
    (t ;(message "(%s):%s" command args)
       nil)))

(defun erl-company-get-docs-from-internet-p (mod fun) ;; maybe version?
  "Download the documentation from internet."
  (erl-company-html-to-string
   (with-current-buffer 
       (url-retrieve-synchronously (format "http://www.erlang.org/doc/man/%s.html" mod))
     (goto-char (point-min))

     ;; find <p> containing <a name="module"> then
     ;; find <div class="REFBODY">
     (re-search-forward (format "<p>.*?<a name=\"%s.*?\">" fun))

     (let* ((beg (match-beginning 0))
	    (end (progn (re-search-forward "</p>.*?</div>" nil t)
			(match-end 0))))
       (and beg end (buffer-substring beg end))))))

(defun erl-company-html-to-string (string)
  (let ((tagslist '(("^[[:space:]]+" . "")
		    ("</?p>" . "\n")
		    ("<br>" . "\n")
		    ("<[^>]*>" . "")
		    (" $" . "")
		    ("&gt;" . ">")
		    ("&lt;" . "<"))))
    (dolist (tagpair tagslist string)
      (setq string (replace-regexp-in-string (car tagpair) (cdr tagpair) string)))))

;; because cant reach distels??
(defun erl-company-format-arglists (arglists)
  (format "%s"
	  (mapconcat 'identity
		     (mapcar (lambda (arglist)
			       (format "(%s)"
				       (mapconcat 'identity arglist ", ")))
			     arglists)
		     " | ")))

(defun erl-company-candidates (args)
  (let ((ismod (eql (char-before) ?:))
	(ismodfunc (eql (char-before (- (point) (length args))) ?:)))
    (if ismod (let ((comp (try-erl-complete args (- (point) (length args))))
		    (temp-list '()))
		(dolist (x comp temp-list) (add-to-list 'temp-list (concat args x))))
      (if ismodfunc (let* ((mod (erl-company-get-module-p args))
			   (beg (- (point) (+ (length args) (length mod) 1))))
		      (try-erl-complete (concat mod ":" args) beg))
	(try-erl-complete args (- (point) (length args)))))))

(defun erl-company-get-module-p (args)
  "Return modulename or nil looking back from point."
  (let ((ismod (eql (char-before) ?:))
	(ismodfunc (eql (char-before (save-excursion
				       (skip-syntax-backward "w")
				       (point))) ?:))
	(point-before (save-excursion
			(skip-syntax-backward "w")
			(point))))
    (or (and ismod
	     (buffer-substring (- (point) 1) (save-excursion
					       (backward-char)
					       (skip-syntax-backward "w")
					       (point))))
	(and ismodfunc
	     (buffer-substring (- point-before 1) (save-excursion
						    (skip-syntax-backward "w")
						    (backward-char)
						    (skip-syntax-backward "w")
						    (point)))))))
(defun erl-company-get-function-p (args)
  (let ((ismod (eql (char-before) ?:))
	(ismodfunc (eql (char-before (save-excursion
				       (skip-syntax-backward "w")
				       (point))) ?:)))
    (or (and
	 ismod
	 (substring args (+ (string-match ":" args) 1)))
	(and
	 ismodfunc
	 args))))

(defvar try-erl-args-cache nil)
(defvar try-erl-desc-cache nil)

(defun erl-company-get-metadoc (mod fun)
  (let ((node erl-nodename-cache))
    (erl-company-args mod fun))
  (sleep-for 0.1)
  try-erl-args-cache)

(defun erl-company-local-docs (mod fun)
  (erl-company-get-metadoc mod fun)
  (let ((node erl-nodename-cache))
    (setq try-erl-desc-cache '())
    (dolist (args try-erl-args-cache)
      (erl-company-describe mod fun args))
    (sleep-for 0.1)
   try-erl-desc-cache))

(defun erl-company-describe (mod fun args)
  (erl-spawn
    (erl-send-rpc node 'distel 'describe (list (intern mod) (intern fun) (length args)))
    (&erl-company-receive-describe args))
  try-erl-desc-cache)

(defun &erl-company-receive-describe (args)
  (erl-receive (args)
      ((['rex ['ok desc]]
	(setq try-erl-desc-cache (nconc desc try-erl-desc-cache)))
       (else
	(message "fail: %s" else)))))

(defun erl-company-args (mod fun)
  (erl-spawn
    (erl-send-rpc node 'distel 'get_arglists (list mod fun))
    (&erl-company-receive-args))
  try-erl-args-cache)

(defun &erl-company-receive-args ()
  (erl-receive ()
      ((['rex docs]
	(setq try-erl-args-cache docs))
       (else
	(message "fail: %s" else)
	(setq try-erl-args-cache nil)))))

(provide 'company-hippie-distel)
