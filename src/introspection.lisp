(defpackage crane.introspection
  (:use :cl :sxql :anaphora)

  (:export :table-exists-p)

  (:documentation "Package contains code for inspecting the real, current state of the database."))

(in-package :crane.introspection)

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
