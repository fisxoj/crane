(defpackage crane.introspection
  (:use :cl :sxql :anaphora)

  (:export :table-exists-p)

  (:import-from :crane.types
		:text)

  (:documentation "Package contains code for inspecting the real, current state of the database."))

(in-package :crane.introspection)

(defparameter +type-mapping+
  '(("character varying" . cl-user::varchar)
    ("integer" . cl-user::integer)
    ("text" . cl-user::text)
    ("timestamp without time zone" . cl-user::timestamp)))

;; http://stackoverflow.com/questions/20582500/how-to-check-if-a-table-exists-in-a-given-schema
(defun table-exists-p (table-name)
  (let* ((conn (crane.connect:get-connection (crane.meta:table-database (find-class table-name))))
	 (response (dbi:fetch-all
		    (dbi:execute
		     (dbi:prepare
		      conn
		      "SELECT EXISTS ( SELECT 1 FROM information_schema.tables WHERE table_name=?)")
		     (string-downcase table-name)))))
    (and (car response) (getf (car response) :|exists|))))

(defun table-definition (table-name)
  "Generate a table definition in the style of the migration files that is instrospected from sql defintions"
  (let* ((table-class (find-class table-name))
	 (conn (crane.connect:get-connection (crane.meta:table-database table-class)))
	 (response (dbi:fetch-all
		    (dbi:execute
		     (dbi:prepare
		      conn
"SELECT
  column_name as name,
  data_type as type,
  is_nullable as nullp
 FROM information_schema.columns
 WHERE table_name=?")
		     (string-downcase table-name))))
	 (constraints (table-constraints table-name)))

    (labels ((intern-name (name)
	     (intern (string-upcase name) (symbol-package table-name)))

	   (convert-typestring (typestring)
	     (let ((type-assoc (cdr (assoc typestring +type-mapping+ :test 'equal))))
	       (or type-assoc (intern-name typestring)))))

      (list :table-options (list :database (crane.meta:table-database table-class))
	    :columns
	    (loop
	       for column in response
	       for name = (intern-name (getf column :|name|))
	       for type = (convert-typestring (getf column :|type|))
	       for nullp = (string= "YES" (getf column :|nullp|))
	       for uniquep = nil
	       for primaryp = nil
	       for indexp = nil

	       ;; Associate constraints with their columns
	       do (loop for constraint in constraints
		     for constraint-name = (intern-name (getf constraint :|name|))
		     for type = (getf constraint :|type|)

		     when (eq name constraint-name)
		     do (cond
			  ((string= type "UNIQUE") (setf uniquep t))
			  ((string= type "PRIMARY KEY") (print 'pk) (setf primaryp t))
			  ((string= type "INDEX") (setf indexp t))))

	       collecting (list
			   :name name
			   :type type
			   :nullp nullp
			   :uniquep uniquep
			   :primaryp primaryp
			   :indexp indexp))))))

(defun table-constraints (table-name)
  (let* ((conn (crane.connect:get-connection (crane.meta:table-database (find-class table-name))))
	 (response (dbi:fetch-all
		    (dbi:execute
		     (dbi:prepare
		      conn
		      "SELECT
ccu.column_name as NAME,
tc.constraint_type as TYPE

FROM information_schema.constraint_column_usage ccu
LEFT JOIN information_schema.table_constraints tc
ON ccu.constraint_name = tc.constraint_name
WHERE tc.table_name = ?")
		     (string-downcase table-name)))))

    (remove-if (lambda (con) (string= (getf con :|type|) "CHECK"))
	       response)))

;; (defun table-constraint-column (table-name)
;;   (let* ((conn (crane.connect:get-connection (crane.meta:table-database (find-class table-name))))
;; 	 (response (dbi:fetch-all
;; 		    (dbi:execute
;; 		     (dbi:prepare
;; 		      conn
;; 		      "SELECT column_name as name, constraint_name FROM information_schema.constraint_column_usage WHERE table_name = ?")
;; 		     (string-downcase table-name)))))
;;     response))
