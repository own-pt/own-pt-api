(ql:quickload :yason)
(ql:quickload :drakma)
(ql:quickload :alexandria)

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
      (let ((text (car (getf g :|gloss_en|)))
	    (id (getf g :|doc_id|)))
	(format t "~a | ~a |~a ~%" id text (call-watson text))))))
