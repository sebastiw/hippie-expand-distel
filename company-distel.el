(require 'company-distel-lib)
(require 'popup)

(defvar use-popup nil)

;;;###autoload
(defun company-distel (command &optional args &rest ignore)
  (interactive (list 'interactive))
  (case command
    (interactive (company-begin-backend 'company-distel))
    (prefix ;; nar ska functionen slas pa? returnera ordet man ar pa.
     (erl-company-find-prefix))
    (candidates ;; vilka ord returneras
     (erl-company-candidates args))
    (meta
     (let* ((isok (string-match ":" args))
	    (mod (and isok (substring args 0 isok)))
	    (fun (and isok (substring args (+ isok 1))))
	    (met (erl-company-get-metadoc mod fun)))
       (when isok (concat "Args: " (erl-format-arglists met)))))
    (doc-buffer
     (erl-company-get-doc-buffer args))

    (t ;(message "(%s):%s" command args)
       nil)))

(defun erl-company-find-prefix ()
  (let ((no-comment (not (erl-company-is-comment-or-cite-p)))
	(word (erl-company-grab-word)))
    (and
     (eq (derived-mode-p 'erlang-mode) 'erlang-mode)
     (or
      (and no-comment word)
      'stop))))

(defun erl-company-get-doc-buffer (args)
  (let* ((isok (string-match ":" args))
	 (mod (and isok (substring args 0 isok)))
	 (fun (and isok (substring args (+ isok 1))))
	 (doc (erl-company-local-docs mod fun))
	 (edocs (when (string= doc "") (erl-company-get-docs-from-internet-p mod fun)))
	 (met (erl-format-arglists (erl-company-get-metadoc mod fun)))
	 to-show popup)
    
    (setq to-show (or (and (not (string= doc "")) doc)
		      (and (not (string= edocs "")) edocs)
		      (concat mod ":" fun met)
		      (format "Couldn't find any help for %s:%s" mod fun)))
    (when use-popup (popup-tip to-show))
    (with-current-buffer (company-doc-buffer)
      (insert to-show)
      (current-buffer))))


(provide 'company-distel)
