;;;
;;; TODO sql example
;;;
;;; ASYNC
;;;

;;;(addon-require :sql-feature :release 'pre-0.1)

;;;(error "<font color='red' background='white'>Dont load. Use console copy/past step by step and see result</font>")
(error "<span style='color:red;background-color:white'>Dont load. Use console copy/past step by step and see result</span>")


;;; Data base
(defparameter *db nil)

;;; Open
(setf *db (sql-dbopen "ToDo" "1.01" "Chrome ToDo lists" 200000))


;;;
;;; try select count(*). if error, then table must be created
;;;
(sql-db-transaction
 *db
 (lambda (tx)
     (sql-tx-execute
      tx
      "SELECT COUNT(*) FROM ToDo"
      #()
      (lambda (&rest tail)
          (let* ((result-set (second tail))
                 (ops)
                 (val))
              (setf result-set (oget result-set "rows"))
              (map-js-object (lambda (k v) (setf ops k val v)) (aref result-set 0))
              (format t "~%<font color='gree'> Table has ~a recodrs</font>~%" val) ))
      (lambda (ctx err &rest tail)
          ;; Something went wrong. See error status
          ;; and try create table
          (format t "<font color='red'>~%Table will be create</font>~%")
          (#j:console:log "See error structure->" ctx err tail )
          (sql-tx-execute
           tx
           "CREATE TABLE ToDo (id REAL UNIQUE, label TEXT, timestamp REAL)"
           #()
           (lambda (&rest tail)
               ;; Ok. Table created
               (format t "<font color='green'>~%Table created</font>~%")
               (apply #j:console:log "Ok" (flatten tail)))
           (lambda (&rest tail)
               ;; Something went wrong
               (format t "<font color='red'>~%Table created error</font>~%")
               (apply #j:console:log "Err" (flatten tail)))
           ))) ))

;;;
;;; total count
;;;
(sql-db-transaction
 *db
 (lambda (tx)
     (sql-tx-execute
      tx
      "SELECT COUNT(*) FROM ToDo" #()
      (lambda (&rest tail)
          (let* ((result-set (second tail))
                 (ops)
                 (val))
              (setf result-set (oget result-set "rows"))
              (map-js-object (lambda (k v) (setf ops k val v)) (aref result-set 0))
              (format t "~%<font color='gree'> Table has ~a recodrs</font>~%" val) ))
      (lambda (&rest tail) (apply #j:console:log "Err" (flatten tail))) )))



#|
;;; e
(sql-db-transaction
 *db
 (lambda (tx)
     (sql-tx-execute
      tx
      "SELECT COUNT(*) FROM ToDo"
      #()
      (lambda (&rest tail)
          (apply #j:console:log "Ok" (flatten tail)))
      (lambda (ctx err &rest tail)
          (print (list 'Create ctx err))
          (#j:console:log ctx err)
          (sql-tx-execute
           ctx
           "CREATE TABLE ToDo (id REAL UNIQUE, label TEXT, timestamp REAL)"
           #()
           (lambda (&rest tail) (apply #j:console:log "Ok" (flatten tail)))
           (lambda (&rest tail) (apply #j:console:log "Err" (flatten tail)))
           ))) ))
|#

;;;
;;; insert some todo record
;;;
;;; Repeat several times with different values replaced "What to do"
;;; After, will e return to previousle step "total count", for to see the number of entries
;;; and go to the next step "Look todo list"
;;;
(sql-db-transaction
 *db
 (lambda (tx)
     (sql-tx-execute
      tx
      "INSERT INTO ToDo (label, timestamp) values(?, ?)"
      (funcall (lambda ()
                   (let ((ar (make-new #j:Array))
                         (date (make-new #j:Date)))
                       (funcall ((oget ar "push" "bind") ar "What to do"))
                       (funcall ((oget ar "push" "bind") ar
                                 (funcall ((oget date "getTime" "bind") date))))
                       ar)))
      (lambda (&rest tail) (apply #j:console:log "Ok" (flatten tail)))
      (lambda (&rest tail) (apply #j:console:log "Err" (flatten tail)))
      )))


;;;
;;; Look todo list
;;;
(sql-db-transaction
 *db
 (lambda (tx)
     (sql-tx-execute
      tx
      "SELECT label, timestamp FROM ToDo"
      #()
      (lambda (&rest tail)
          (let* ((sql-result-set (second tail)))
              (apply #j:console:log "Ok" sql-result-set (flatten tail))
              (setf sql-result-set (oget (second tail) "rows"))
              (print sql-result-set)
              (#j:console:log sql-result-set)
              (map 'vector
                   (lambda (row)
                       (let* ((time (make-new #j:Date (oget row "timestamp")))
                              (when (funcall ((oget time "toDateString" "bind") time))))
                           (format t "<font color='red'>~a</font>: ~a~%" when (oget row "label"))
                           (setf time nil when nil) ))
                   sql-result-set)
              ))
      (lambda (&rest tail) (apply #j:console:log "Err" (flatten tail))) )))





;;;;; eof
