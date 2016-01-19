(ql:quickload :yason)
(ql:quickload :drakma)
(ql:quickload :alexandria)
(ql:quickload :cl-ppcre)

(defpackage :glossas
  (:use :cl :drakma :alexandria))

(in-package :glossas)

(defun call-get-method (url &key parameters)
  "Alternative to CALL-REST-METHOD that uses a stream; this is more memory efficient, but it may cause problems if YASON:PARSE takes too long to parse the stream and the stream may be cut due to timeout."
    (let* ((stream (drakma:http-request
                   (format nil "~a" url)
                   :parameters parameters
		   :external-format-out :utf-8
                   :method :get
                   :connection-timeout 120
                   :want-stream t)))
    (setf (flexi-streams:flexi-stream-external-format stream) :utf-8)
    (let ((obj (yason:parse stream
			    :object-as :plist
			    :object-key-fn #'make-keyword)))
      (close stream)
      obj)))

;; from https://gist.github.com/arademaker/96947c7517360337b738
(defun group-by (alist n &optional res)
  (if (null alist)
      (reverse res)
      (group-by (subseq alist n) n (cons (subseq alist 0 n) res))))

;; from https://gist.github.com/arademaker/96947c7517360337b738
(defun split-gloss (gloss)
  (let* ((re "[ ]*;[ ]*\"[^\"]+\"")
	  (pos (cl-ppcre:all-matches re gloss))
	  (bw '(#\; #\" #\Space)))
    (if pos
	;; For each two consecutive numbers in the sequence, group
	;; start-end block. Each block is an example. The beginning,
	;; between position 0 and the first number, is the
	;; definition. Before return the parts with values, trim them
	;; removing leading space and \;
	(append (list (string-trim bw (subseq gloss 0 (car pos))))
		(loop for pair in (group-by pos 2)
		           collect (string-trim bw (subseq gloss (car pair) (cadr pair)))))
	(list gloss))))

(defun call-watson (text)
  (drakma:http-request
		  "https://gateway.watsonplatform.net/language-translation/api/v2/translate"
		  :basic-authorization '("<user>" "<password>")
		  :parameters (list (cons "text" text)
				    (cons "source" "en")
				    (cons "target" "pt"))
		  :external-format-out :utf-8
		  :method :post
		  :connection-timeout 120
		  :want-stream nil))


(defun fetch-glosses ()
  (getf 
   (getf 
    (call-get-method "http://localhost:8983/solr/wn/select?"
		     :parameters '(("q" . "*:*")
				   ("fl" . "doc_id,gloss_en")
				   ("wt" . "json")
				   ("rows" . "2000000")))
    :|response|) :|docs|))

(defun translate ()
  (let ((glosses (fetch-glosses)))
    (dolist (g glosses)
      (let ((count 0)
	    (text (car (getf g :|gloss_en|)))
	    (id (getf g :|doc_id|)))
	(when (null (split-gloss text))
	  (format t "!! ~a~%" text))
	(dolist (txt (split-gloss text))
	  (format t "~a | ~a | ~a | ~a ~%" id txt (call-watson txt) (if (= 0 count) "g" "e"))
	  (incf count))))))

(translate)

(sb-ext:quit)
