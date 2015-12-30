(defun make-delete-field-json (id field)
  (format nil "[ { \"id\" : \"~a\", \"~a\" : {\"set\":null} } ]" id field))

(defun delete-field (id field)
  (drakma:http-request "http://localhost:8983/solr/wn/update"
                       :content-type "application/json"
                       :method :post
                       :content (make-delete-field-json id field)))

 (defun get-version (id)
  (let* ((url (concatenate 'string "http://localhost:8983/solr/wn/get?id=" id))	
  	 (resp (drakma:http-request url)))
    (format t resp)
    (search "version_\":" resp)))

(defun commit-wn ()
  (drakma:http-request "http://localhost:8983/solr/wn/update?commit=true"))


(defun delete-ids ()
    (let* ((stream (drakma:http-request "http://localhost:8983/solr/wn/select?q=*:*&rows=90000000&fl=id&wt=json&indent=true"))
	   (obj (yason:parse stream))
	   (ids (gethash "docs" (gethash "response" obj)))      
	   (no-ids (gethash "numFound" (gethash "response" obj))))
      (dolist (h ids)
	(gethash "id" h))))