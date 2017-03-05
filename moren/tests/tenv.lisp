;;; -*- mode:lisp;  coding:utf-8 -*-

(export '(*total-tests* *passed-tests* *failed-tests* *expected-failures*
          *unexpected-passes* *timestamp* test-fn
          test expected-failure   test-equal) )


(defparameter *total-tests* 0)
(defparameter *passed-tests* 0)
(defparameter *failed-tests* 0)
(defparameter *expected-failures* 0)
(defparameter *unexpected-passes* 0)
(defparameter *test-seqn* 1000)
(defparameter *timestamp* nil)
(defparameter *execute-timer* nil)


(defparameter *passed-msg* #("Passed" "Passed unexpectedly!"))
(defparameter *failed-msg* #("Failed" "Failed expectedly"))


(defun test-fn (expr form expected-flg)
    (#j:setTimeout
     (lambda ()
         (let ((start-time (get-internal-real-time))
               (end-time 0)
               (elapsed 0)
               (secs 0))
             (handler-case
                 (progn
                     (incf *total-tests*)
                     (format t "<font color='yellow'>Test [~d]</font><font color='white'> ~S</font>" *total-tests* form)
                     (cond
                       ((eval form)
                        (setf end-time (get-internal-real-time)
                              elapsed (- (get-internal-real-time) start-time)
                              secs (/ elapsed internal-time-units-per-second 1.0))
                        (format t "~%    <font color='yellow'> ~a</font>~%" (aref *passed-msg* expected-flg ))
                        (format t "<font color='yellow'>     Elapsed ~d sec</font>~%~%" secs)
                        (incf *passed-tests*)
                        (push (- (get-internal-real-time) start-time) *execute-timer*))
                       (t (push (- (get-internal-real-time) start-time) *execute-timer*)
                          (when (= expected-flg 1) (incf *expected-failures*))
                          (warn (aref *failed-msg* expected-flg )))))
               (warning (msg)
                   (format t "~%    <font color='orange'> Warning: ~s.</font>~%" (jscl::!condition-args msg) )
                   (format t "<font color='yellow'>     Elapsed ~d sec</font>~%~%" secs)
                   (incf *failed-tests*))
               (error (msg)
                   (incf *failed-tests*)
                   (setf end-time (get-internal-real-time)
                         elapsed (- (get-internal-real-time) start-time)
                         secs (/ elapsed internal-time-units-per-second 1.0))
                   (format t "~%    <font color='red'> Error: ~s.</font>~%" (jscl::!condition-args msg))
                   (format t "<font color='yellow'>     Elapsed ~d sec</font>~%~%" secs)
                   ))
             ))
     *test-seqn* ) )



(defun set-timestamp ()
    (setq *timestamp* (get-internal-real-time))
    (setq *execute-timer* nil)
    (setq *test-seqn* 1000)
    (setq *total-tests* 0)
    (setq *passed-tests* 0)
    (setq *expected-failures* 0)
    (setq *unexpected-passes* 0))


(defun reports ()
    (#j:setTimeout
     (lambda ()
         (format t "~%<font color='yellow'>Batch finished. Total ~a tests</font>~%" *total-tests*)
         (format t "<font color='yellow'>The execution time ~a seconds</font>~%"
                 (/ (- (get-internal-real-time) *timestamp*) internal-time-units-per-second 1.0))
         (format t "<font color='yellow'>Elapsed time ~a seconds</font>~%"
                 (/ (sum *execute-timer*) internal-time-units-per-second 1.0))

         (if (= *passed-tests* *total-tests*)
             (format t "<font color='yellow'>All the tests (~a) passed successfully.</font>~%" *total-tests*)
             (format t "<font color='yellow'>~a/~a test(s) passed successfully.</font>~%" *passed-tests* *total-tests*))

         (unless (zerop *expected-failures*)
             (format t "<font color='orange'>~a test(s) failed expectedly.</font>~%" *expected-failures*))

         (unless (zerop *unexpected-passes*)
             (format t "<font color='yellow'>~a test(s) passed unexpectedly.</font>~%" *unexpected-passes*))

         (terpri)
         (format t "Done~%" ))
     *test-seqn* ) )



;;;
;;; Test API
;;;


;;;
;;; (test (eql t nil))
;;; =>
;;;    Test [22] (EQL T NIL)
;;;         Warning: ("Failed").
;;;         Elapsed 0 sec
;;;
;;;
;;; (test (if (true) (true) (fail)))
;;; =>
;;;    Test [26] (IF (TRUE) (TRUE) (FAIL))
;;;          Passed
;;;          Elapsed 0.105 sec
;;;
;;; (test (fail))
;;; =>
;;;    Test [25] (FAIL)
;;;         Warning: ("Failed").
;;;         Elapsed 0 sec
;;;
;;;
;;; (test (error "Its error"))
;;; =>
;;;    Test [27] (ERROR "Its error")
;;;          Error: ("Its error").
;;;          Elapsed 0.07 sec
;;;
;;; (test (warn "its warn message"))
;;; =>
;;;     Test [28] (WARN "its warn message")
;;;          Warning: ("its warn message").
;;;          Elapsed 0 sec
;;;
;;; (test (catch 'result
;;;    (setq i 0 j 0)
;;;    (loop (incf j 3) (incf i)
;;;          (if (= i 3) (throw 'result (values i j)))))
;;;
;;; =>
;;;    Test [29] (CATCH (QUOTE RESULT) (SETQ I 0 J 0) (LOOP (INCF J 3) (INCF I) (IF (= I 3) (THROW (QUOTE RESULT) (VALUES I J)))))
;;;         Passed
;;;         Elapsed 0.26 sec

;;;   Know problems
;;;
;;;   (test (warn "Val ~d" 1))
;;;    =>
;;;         Test [36] (WARN "Val ~d" 1)
;;;         Warning: ("Val ~d" 1).
;;;         Elapsed 0 sec
;;;
;;;  Solved as:
;;;
;;;   (test (warn (concat "Val " 1)))
;;;   (test (warn (format nil "Val ~d" 1)))
;;;
;;;  Below 4 tests
;;;
;;;    (let* ((a '(1 2)) (b a) (c a))
;;;         (test (equal (mapcar #'+ a b c) '( 3  6)))
;;;         (test (equal (mapcar #'- a b c) '(-1 -2)))
;;;         (test (equal (mapcar #'* a b c) '( 1  8)))
;;;         (test (equal (mapcar #'/ a b c) '( 1  0.5))))
;;;
;;;  will be failed. There is a problem with the binding local variables inside the closure and their visibility
;;;  in the new eval context.
;;;
;;;  The problem is solved as:
;;;
;;;  (test
;;;     (let* ((a '(1 2)) (b a) (c a))
;;;        (and
;;;          (equal (mapcar #'+ a b c) '( 3  6))
;;;          (equal (mapcar #'- a b c) '(-1 -2))
;;;          (equal (mapcar #'* a b c) '( 1  8))
;;;          (equal (mapcar #'/ a b c) '( 1  0.5)))))
;;;
;;;  or 1) using global variables
;;;
;;;      (defparameter var-1 0)
;;;      (defparameter var-2 1)
;;;
;;;      (test (= var-1 var-2))
;;;      (test (> var-1 var-2))
;;;
;;; or  2) using binding variables from other package
;;;
;;;      (test (equal (mapcar #'+ frob::a frob::b frob::c) '( 3  6)))
;;;
;;; Goddamn knows it is a bug or a feature
;;;

(defmacro test (condition)
    `(test-fn ',condition ',condition 0))


;;;
;;; (expected-failure (fail))
;;;
(defmacro expected-failure (condition)
    `(test-fn ',condition ',condition 1))

;;;
;;; (test-equal (gethash frob::key frob::ht) 1234)
;;;
(defmacro test-equal (form value)
    `(test (equal ,form ,value )))

;;;
;;; Batch-begin
;;;
;;; Recomended begining for any test batch
;;;
(defun batch-begin (&optional header)
    (declare (ignore header))
    (set-timestamp))

;;;
;;; Batch-end
;;;
;;; Print run-time statistic for the test batch
;;;
(defun batch-end ()
    (reports))



;;;
;;; Load-tb
;;;
;;; Load test batch (source lisp file with test forms) from http-server
;;; work only on http protocol
;;;
;;; (load-tb "static/js/integers.lisp")
;;;
;;;

(defun eval-test-form (input)
    (let* ((forms (read-from-string (concat "(" input ")"))))
        (dolist (x forms)
            (eval x))) )


(defun load-tb (host-file-name)
    (xhr-receive  host-file-name
                  (lambda (input)
                      (eval-test-form (substitute #\Space (code-char 13) input) ))
                  (lambda (uri status)
                      (format t "~%Load: Can't load ~s. Status: ~a~%" uri status)) )
    (values))
