(ql:quickload :yason)
(ql:quickload :drakma)
(ql:quickload :alexandria)

(defpackage :fields
  (:use :cl :drakma :alexandria))

(in-package :fields)

(defparameter *predicates*
  '("wn30_adjectivePertainsTo" "wn30_adverbPertainsTo"
    "wn30_antonymOf" "wn30_attribute" "wn30_causes"
    "wn30_classifiedByRegion" "wn30_classifiedByTopic"
    "wn30_classifiedByUsage" "wn30_classifiesByRegion"
    "wn30_classifiesByTopic" "wn30_classifiesByUsage"
    "wn30_derivationallyRelated" "wn30_entails" "wn30_hasInstance"
    "wn30_memberHolonymOf" "wn30_hypernymOf" "wn30_hyponymOf"
    "wn30_instanceOf"  "wn30_memberMeronymOf" "wn30_partHolonymOf"
    "wn30_partMeronymOf" "wn30_sameVerbGroupAs" "wn30_seeAlso"
    "wn30_substanceHolonymOf" "wn30_substanceMeronymOf"))

(defun call-get-method (url &key parameters)
  "Alternative to CALL-REST-METHOD that uses a stream; this is more
   memory efficient, but it may cause problems if YASON:PARSE takes
   too long to parse the stream and the stream may be cut due to
   timeout."
  (with-open-stream (s (drakma:http-request url
					    :parameters parameters
					    :external-format-out :utf-8
					    :method :get
					    :connection-timeout 120
					    :want-stream t))
    (setf (flexi-streams:flexi-stream-external-format s) :utf-8)
    (yason:parse s :object-as :hash-table)))

(defun fetch-docs (limit &optional (q-value "-rdf_type:Nominalization"))
  (let* ((l (format nil "~a" limit))
	 (q (format nil "~a" q-value))
	 (out (call-get-method "http://localhost:8983/solr/wn/select?"
			       :parameters `(("q" . ,q)
					     ("wt" . "json")
					     ("rows" . ,l)))))
    (gethash "docs" (gethash "response" out))))

 (defun get-nom-ids (limit)
	   (let ((docs (fetch-docs limit "rdf_type:Nominalization")))
	     (loop for i in docs
		collect (gethash "id" i))))

(defun gen-delete-json (ids stream)
  (let ((h (alexandria:alist-hash-table `(("delete" . ,ids)))))
    (yason:encode h stream)))

(defun updatable-p (obj)
  (dolist (p *predicates*)
    (if (gethash p obj)
	(return t))))

(defun update-obj (obj)
  (let ((cmd (alist-hash-table '(("set" . nil))))
	(modified (make-hash-table :test #'equal)))
    (loop for k being the hash-keys in obj
	  using (hash-value v)
	  do (cond
	       ((member k *predicates* :test #'equal)
		(setf (gethash k modified) cmd))
	       ((equal "id" k)
		(setf (gethash k modified) v))))
    modified))

(defun delete-by-id (limit)
	   (labels ((send-data (s)
		      (setf (flexi-streams:flexi-stream-external-format s) :utf-8)
		      (gen-delete-json (get-nom-ids limit) s)))
	     (drakma:http-request "http://localhost:8983/solr/wn/update"
				  :content-type "application/json"
				  :method :post
				  :content #'send-data)))

(defun index-update (docs)
  (labels ((send-data (s)
	     (setf (flexi-streams:flexi-stream-external-format s) :utf-8)
	     (yason:encode docs s)))
    (drakma:http-request "http://localhost:8983/solr/wn/update"
				   :content-type "application/json"
				   :method :post
				   :content #'send-data)))

;; (defun modify-db (docs)
;;   (let ((cont (drakma:http-request "http://localhost:8983/solr/wn/update"
;; 				   :content-type "application/json"
;; 				   :method :post
;; 				   :content :continuation)))
;;     (funcall cont (lambda (s)
;; 		    (setf (flexi-streams:flexi-stream-external-format s) :utf-8)
;; 		    (yason:encode (mapcar #'update-obj docs) s)) t)))


(defun index-commit ()
  (drakma:http-request "http://localhost:8983/solr/wn/update?commit=true"))

;; load script and run the following commands:
;; sbcl --dynamic-space-size 5000
;;
;; (index-update (mapcar #'update-obj (remove-if-not #'updatable-p (fetch-docs 122000 "-rdf_type:Nominalization"))))
;; (index-commit)
;; (delete-by-id 200000)
;; (index-commit)
 
