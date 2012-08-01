(require 'hippie-expand-distel)
(require 'distel)

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; docs funs         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; if no internetconnection
(defun erl-company-get-docs-from-internet-p (mod fun) ;; maybe version?
  "Download the documentation from internet."
  (let ((str
	 (with-current-buffer 
	     (url-retrieve-synchronously (format "http://www.erlang.org/doc/man/%s.html" mod))
	   (goto-char (point-min))
	   
	   ;; find <p> containing <a name="module"> then
	   ;; find <div class="REFBODY">
	   
	   (let* ((m (re-search-forward (format "<p>.*?<a name=\"%s.*?\">" fun) nil t))
		  (beg (and m (match-beginning 0)))
		  (end (and m (progn (re-search-forward "</p>.*?</div>" nil t)
				     (match-end 0)))))
	     (and beg end (buffer-substring beg end))))))
    (and str
	 (erl-company-html-to-string str))))

(defun erl-company-html-to-string (string)
  (let ((replaces '(("^[[:space:]]+" . "")
		    ("</?p>" . "\n")
		    ("<br>" . "\n")
		    ("<[^>]*>" . "")
		    (" $" . "")
		    ("&gt;" . ">")
		    ("&lt;" . "<"))))
    (dolist (tagpair replaces string)
      (setq string (replace-regexp-in-string (car tagpair) (cdr tagpair) string)))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Distel funs       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun erl-company-candidates (args)
  "Get completion candidates for args."
  (let* ((isfun (string-match ":" args))
	 (mod (and isfun (substring args 0 (+ isfun 1))))
	 (comp (try-erl-complete (downcase args) 0))
	 ret)
    (dolist (x comp ret) (add-to-list 'ret (concat mod x)))))

(defvar try-erl-args-cache nil)
(defvar try-erl-desc-cache "")

(defun erl-company-get-metadoc (mod fun)
  "Get the arguments for a function."
  (let ((node erl-nodename-cache))
    (erl-company-args mod fun))
  (sleep-for 0.1)
  try-erl-args-cache)

(defun erl-company-local-docs (mod fun)
  "Get localdocs for a function."
  (erl-company-get-metadoc mod fun)
  (let ((node erl-nodename-cache))
    (setq try-erl-desc-cache "")
    (dolist (args try-erl-args-cache)
      (erl-company-describe mod fun args)))
  (sleep-for 0.1)
  try-erl-desc-cache)

(defun erl-company-describe (mod fun args)
  (erl-spawn
    (erl-send-rpc node 'distel 'describe (list (intern mod) (intern fun) (length args)))
    (&erl-company-receive-describe args)))

(defun &erl-company-receive-describe (args)
  (erl-receive (args)
      ((['rex ['ok desc]]
	(let ((descr (format "%s:%s/%s\n%s\n\n"
			     (elt (car desc) 0)
			     (elt (car desc) 1)
			     (elt (car desc) 2)
			     (elt (car desc) 3))))
	(when desc (setq try-erl-desc-cache (concat descr try-erl-desc-cache)))))
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

;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Local buffer funs ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defun erl-company-grab-word ()
  "Grab the current Erlang mod/fun/word."
  (interactive)
  (buffer-substring (point) (save-excursion
			      (skip-chars-backward "a-zA-Z:_")
			      (point))))

(defun erl-company-is-comment-or-cite-p ()
  "Returns t if point is inside a comment or a cite."
  (save-excursion
    (let ((po (point)))
      (beginning-of-line)
      (re-search-forward "[%\|\"|\']" po t)
      (or (eql (char-before) ?%)
	  (and (or (eql (char-before) ?\")
		   (eql (char-before) ?\'))
	       (not (re-search-forward "[\"\|\']" po t)))))))


(provide 'company-distel-lib)
