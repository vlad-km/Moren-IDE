(defparameter *list '())
(defparameter *array (make-array 10000))
(defparameter *hash (make-hash-table :test #'equal))
(defparameter *res 0)

(batch-begin "Timing")

;;; list initialize

(test
 (let ()
     (setf *list '())
     (dotimes (i 10000)
         (push i *list))
     (setf *list (reverse *list)) t))


(test
 (progn
     (setf *list '())
     (dotimes (i 10000)
         (push i *list))
     (setf *list (reverse *list))
     t))

;;; array initialize
(test
 (let () 
     (dotimes (i 10000)
         (setf (aref *array i) i)) t) )

(test
 (progn 
     (dotimes (i 10000)
         (setf (aref *array i) i)) t) )

;;; hash-table initialize
(test
 (let ()
     (setf *hash (make-hash-table :test #'equal))
     (dotimes (i 10000)
         (setf (gethash (jscl::ensure-list i) *hash) i )) t))


(test
 (progn
     (setf *hash (make-hash-table :test #'equal))
     (dotimes (i 10000)
         (setf (gethash (jscl::ensure-list i) *hash) i )) t))

;;; list access
#|
(test
 (let ()
     (setf *res 0)
     (dotimes (i 10000)
         (setf *res (+ *res (elt *list i)))) t))


(test
 (let ()
     (setf *res 0)
     (dotimes (i 10000)
         (incf *res  (elt *list i))) t))


(test
 (progn
     (setf *res 0)
     (dotimes (i 10000)
         (setf *res (+ *res (elt *list i)))) t))



(test
 (progn
     (setf *res 0)
     (dotimes (i 10000)
         (incf *res (elt *list i))) t))

|#

;;; array access

(test 
 (let ()
     (setf *res 0)
     (dotimes (i 10000)
         (incf *res (aref *array i)) ) t))


(test 
 (progn
     (setf *res 0)
     (dotimes (i 10000)
         (incf *res (aref *array i)))  t))



;;; hash-table access

(test
 (let ()
     (setf *res 0)
     (dotimes (i 10000)
         (incf *res (gethash (jscl::ensure-list i) *hash))) t)) 

(test
 (progn
     (setf *res 0)
     (dotimes (i 10000)
         (incf  *res (gethash (jscl::ensure-list i) *hash))) t)) 


(batch-end)
