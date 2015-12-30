
(ql:quickload :yason)
(ql:quickload :drakma)
(ql:quickload :alexandria)

(defpackage :fields
  (:use :cl :drakma :alexandria))

(in-package :fields)

(defparameter *predicates*
  '("wn30_adjectivePertainsTo" "wn30_adverbPertainsTo" "wn30_antonymOf"
    "wn30_attribute" "wn30_causes" "wn30_classifiedByRegion"
    "wn30_classifiedByTopic" "wn30_classifiedByUsage" "wn30_classifiesByRegion"
    "wn30_classifiesByTopic" "wn30_classifiesByUsage" "wn30_derivationallyRelated"
    "wn30_entails" "wn30_hasInstance" "wn30_hypernymOf" "wn30_hyponymOf"
    "wn30_instanceOf" "wn30_memberHolonymOf" "wn30_memberMeronymOf"
    "wn30_partHolonymOf" "wn30_partMeronymOf" "wn30_sameVerbGroupAs"
    "wn30_seeAlso" "wn30_substanceHolonymOf" "wn30_substanceMeronymOf"))

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
			    :object-as :hash-table)))
      (close stream)
      obj)))

(defun fetch-docs (limit)
  (let ((l (format nil "~a" limit)))
    (gethash "docs" (gethash "response"
			     (call-get-method "http://localhost:8983/solr/wn/select?"
					      :parameters `(("q" . "*:*")
							    ("wt" . "json")
							    ("rows" . ,l)))))))


(defun update-obj (obj)
  (let ((cmd (alist-hash-table '(("set" . nil))))
	(modified (make-hash-table :test #'equal)))
    (loop for k being the hash-keys in obj
	  using (hash-value v)
	  do
	  (cond
	    ((member k *predicates* :test #'equal)
	     (setf (gethash k modified) cmd))
	    ((equal "id" k)
	     (setf (gethash k modified) v))))
    modified))


(defun delete-field (docs)
  ;; criar stream out para passar
  (yason:encode (mapcar #'update-obj docs) out)
  (drakma:http-request "http://localhost:8983/solr/wn/update"
                       :content-type "application/json"
                       :method :post
                       :content out))

(defun commit-wn ()
  (drakma:http-request "http://localhost:8983/solr/wn/update?commit=true"))







