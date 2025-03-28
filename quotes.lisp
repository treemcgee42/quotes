;; Copyright (C) 2025 by Varun Malladi

(defpackage :quotes
  (:use :cl)
  (:export
   #:main))

(in-package :quotes)

;; --- begin util -------------------------------------------------------------------

(defun current-date-string ()
  "Returns a string for the current date, formatted as YYYY-MM-DD."
  (multiple-value-bind (sec min hour day month year)
      (get-decoded-time)
    (declare (ignore sec min hour))
    ;; Shamelessly taken from from ChatGPT. WTF is going on here?!
    (format nil "~4,'0D-~2,'0D-~2,'0D" year month day)))

;; --- end util ---------------------------------------------------------------------
;; --- begin Quote-class ------------------------------------------------------------

(defclass Quote-class ()
  ((date-added :initarg :date-added)
   (quote :initarg :quote)
   (mood :initarg :mood)))

(defmethod print-object ((obj Quote-class) stream)
  (format stream "#<Quote-class: date-added: ~A quote: \"~A\" mood: ~A>"
          (slot-value obj 'date-added)
          (slot-value obj 'quote)
          (slot-value obj 'mood)))

(defmethod print-single ((obj Quote-class) stream)
  "Print the quote, assuming we aren't printing a list of quotes."
  (format stream "~A | \"~A\" | ~A~%"
          (slot-value obj 'date-added)
          (slot-value obj 'quote)
          (slot-value obj 'mood)))

;; --- end Quote-class --------------------------------------------------------------
;; --- begin db functions -----------------------------------------------------------

(defun create-quotes-table (db)
  "Create the quotes table in DB if it does not already exist. If it already exists,
this is a no-op."
  (sqlite:execute-non-query db "
CREATE TABLE IF NOT EXISTS quotes (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  date_added TEXT NOT NULL,
  quote TEXT NOT NULL,
  mood TEXT NOT NULL
);"))

(defun dump-quotes (db)
  (let ((rows (sqlite:execute-to-list db "SELECT * FROM quotes")))
    (dolist (row rows)
      (format t "~A | ~A | ~A~%"
              (nth 1 row) (nth 2 row) (nth 3 row)))))

(defun insert-quote-into-db (db date-added quote mood)
  "DATE-ADDED is in YYYY-MM-DD format."
  (sqlite:execute-non-query db "
INSERT INTO quotes (date_added, quote, mood)
VALUES (?, ?, ?);"
                            date-added quote mood))

(defun retrieve-quote (db sql-query)
  "Retrieve a row from the database as a Quote-class object."
  (multiple-value-bind (id date-added quote mood)
      (sqlite:execute-one-row-m-v db sql-query)
    (when (not (eql id nil))
      (make-instance 'Quote-class
                     :date-added date-added
                     :quote quote
                     :mood mood))))

(defun get-quote-matching-quote (db quote)
  "Returns a quote object derived from the entry whose quote matches QUOTE exactly.
Returns NIL if no match is found."
  (retrieve-quote
   db
   (format nil
           "SELECT * FROM quotes WHERE quote = \"~A\""
           quote)))

(defun test-quote-insert-retrieve ()
  (let ((db (sqlite:connect ":memory:")))
    (unwind-protect
         (progn
           (create-quotes-table db)
           (let ((date "YYYY-MM-DD")
                 (quote "quote1")
                 (mood "mood1"))
             (insert-quote-into-db db date quote mood)
             ;; Test case: not matching quote.
             (assert (eql (get-quote-matching-quote db "FOO") nil))
             ;; Test case: match quote.
             (let ((quote-obj (get-quote-matching-quote db quote)))
               (assert (string= (slot-value quote-obj 'date-added) date))
               (assert (string= (slot-value quote-obj 'quote) quote))
               (assert (string= (slot-value quote-obj 'mood) mood)))))
      (sqlite:disconnect db))))

(defun retrieve-quotes (db sql-query parameters)
  "Retrieve multiple rows from the database table as a list of `Quote-class'
objects."
  (loop
    with stmt = (sqlite:prepare-statement db sql-query)
    initially (loop for i from 1
                    for param in parameters
                    do (sqlite:bind-parameter stmt i param))
    while (sqlite:step-statement stmt)
    collect (make-instance 'Quote-class
                           :date-added (sqlite:statement-column-value stmt 1)
                           :quote (sqlite:statement-column-value stmt 2)
                           :mood (sqlite:statement-column-value stmt 3))
    finally (sqlite:finalize-statement stmt)))

(defun select-multiple-quotes (db &optional date-range-start date-range-end mood)
  "Retrieve multiple rows from the database table as a list of `Quote-class' objects,
subject to value restrictions. Date restrictions are enforced when DATE-RANGE-START
is set. DATE-RANGE-END may also be set in this case, or the current date is used if
ommitted (and DATE-RANGE-START is set). Dates are in YYYY-MM-DD format. The date
range is inclusive. MOOD may also be specified."
  (when date-range-end (assert date-range-start))
  (let ((query
          (concatenate
           'string
           "SELECT * FROM quotes WHERE "
           (if date-range-start
               (format nil "date_added BETWEEN \"~A\" and \"~A\" "
                       date-range-start (if date-range-end
                                            date-range-end
                                            (current-date-string)))
               "1=1")
           (when mood (format nil "AND mood = \"~A\"" mood)))))
    (retrieve-quotes db query '())))

(defun get-random-quote (db &optional mood)
  (let ((query
          (concatenate 'string
                       "SELECT * FROM quotes "
                       (when mood (concatenate 'string
                                               "WHERE mood = \""
                                               mood
                                               "\" "))
                       "ORDER BY RANDOM() LIMIT 1;")))
  (multiple-value-bind (id date-added quote mood)
      (sqlite:execute-one-row-m-v db query)
    (when (not (eql id nil))
      (make-instance 'Quote-class
                     :date-added date-added
                     :quote quote
                     :mood mood)))))

(defun test-get-random-quote ()
  (let ((db (sqlite:connect ":memory:")))
    (unwind-protect
         (progn
           (create-quotes-table db)
           ;; Test case: no mood, no rows.
           (assert (eql (get-random-quote db) nil))
           (let ((date "YYYY-MM-DD")
                 (quote "quote1")
                 (mood "mood1"))
             (insert-quote-into-db db date quote mood)
             ;; Test case: no mood.
             (let ((quote-obj (get-random-quote db)))
               (assert (string= (slot-value quote-obj 'date-added) date))
               (assert (string= (slot-value quote-obj 'quote) quote))
               (assert (string= (slot-value quote-obj 'mood) mood)))
             ;; Test case: mood.
             (let ((quote-obj (get-random-quote db mood)))
               (assert (string= (slot-value quote-obj 'date-added) date))
               (assert (string= (slot-value quote-obj 'quote) quote))
               (assert (string= (slot-value quote-obj 'mood) mood))))
           ;; Test case: mood, rows but no matching rows.
           (assert (eql (get-random-quote db "invalid-mood") nil)))
      (sqlite:disconnect db))))

;; --- end db functions -------------------------------------------------------------
;; --- begin CLI --------------------------------------------------------------------

(defvar *db* nil
  "The database handle the CLI uses.")

;; ------ begin insert subcommand ---------------------------------------------------

(defun cli-insert-options ()
  (list
   (clingon:make-option
    :string
    :long-name "quote"
    :description "The quote"
    :key :quote
    :required t)
   (clingon:make-option
    :string
    :long-name "mood"
    :description "The mood"
    :key :mood
    :required t)
   (clingon:make-option
    :string
    :long-name "date"
    :description "Date, in YYYY-MM-DD format"
    :key :date
    :required nil)))

(defun cli-insert-handler (cmd)
  "Handler for the insert subcommand."
  (let ((quote (clingon:getopt cmd :quote))
        (mood (clingon:getopt cmd :mood))
        (date (or (clingon:getopt cmd :date) (current-date-string))))
    (insert-quote-into-db *db* date quote mood)))

(defun cli-insert-command ()
  "Subcommand for inserting quotes."
  (clingon:make-command
   :name "insert"
   :description "Insert a quote."
   :options (cli-insert-options)
   :handler #'cli-insert-handler))

(defun test-cli-insert ()
  (let ((*db* (sqlite:connect ":memory:")))
    (unwind-protect
         (let ((the-quote "foo quote")
               (the-mood "foo mood"))
           (create-quotes-table *db*)
           (let ((cmd (clingon:parse-command-line
                       (cli-insert-command)
                       `("--quote" ,the-quote  "--mood" ,the-mood))))
             (cli-insert-handler cmd)
             (let ((q (get-quote-matching-quote *db* the-quote)))
               (assert (not (eql q nil)))
               (assert (string= (slot-value q 'date-added)
                                (current-date-string)))
               (assert (string= (slot-value q 'quote) the-quote))
               (assert (string= (slot-value q 'mood) the-mood)))))
      (sqlite:disconnect *db*))))

;; ------ end insert subcommand -----------------------------------------------------
;; ------ begin random subcommand ---------------------------------------------------

(defun cli-random-options ()
  (list
   (clingon:make-option
    :string
    :long-name "mood"
    :description "Get a quote with the specified mood."
    :required nil
    :key :mood)))

(defun cli-random-handler (cmd)
  (let ((mood (clingon:getopt cmd :mood)))
    (print-single (get-random-quote *db* mood) t)))

(defun cli-random-command ()
  "Subcommand for retrieving a random quote."
  (clingon:make-command
   :name "random"
   :description "Retrieve a random quote."
   :options (cli-random-options)
   :handler #'cli-random-handler))

;; ------ end random subcommand -----------------------------------------------------
;; ------ begin list subcommand ---------------------------------------------------

(defun cli-list-options ()
  (list
   (clingon:make-option
    :string
    :long-name "date-start"
    :description "Specify the starting date range (inclusive)."
    :required nil
    :key :date-start)
   (clingon:make-option
    :string
    :long-name "date-end"
    :description "Specify the starting date range (inclusive)."
    :required nil
    :key :date-end)
   (clingon:make-option
    :string
    :long-name "mood"
    :description "Get a quote with the specified mood."
    :required nil
    :key :mood)))

(defun cli-list-handler (cmd)
  (let ((date-start (clingon:getopt cmd :date-start))
        (date-end (clingon:getopt cmd :date-end))
        (mood (clingon:getopt cmd :mood)))
    (let ((quotes (select-multiple-quotes *db* date-start date-end mood)))
      (dolist (quote quotes)
        (print-single quote t)))))

(defun cli-list-command ()
  (clingon:make-command
   :name "list"
   :description "List quotes."
   :options (cli-list-options)
   :handler #'cli-list-handler))

;; ------ end list subcommand -----------------------------------------------------

(defun cli-handler (cmd)
  (declare (ignore cmd)))

(defun cli-command ()
  "A command to say hello to someone"
  (clingon:make-command
   :name "quotes"
   :description "Database of my quotes!"
   :version "0.1.0"
   :authors '("Varun Malladi")
   :options '()
   :handler #'cli-handler
   :sub-commands (list (cli-insert-command)
                       (cli-list-command)
                       (cli-random-command))))

(defun main ()
  (setf *db* (sqlite:connect "quotes.db"))
  (unwind-protect
       (progn
         (create-quotes-table *db*)
         (clingon:run (cli-command)))
    (sqlite:disconnect *db*)))
