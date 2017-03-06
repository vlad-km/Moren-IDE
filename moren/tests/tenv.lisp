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
;;; Load test batch (source lisp file with test forms)
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
