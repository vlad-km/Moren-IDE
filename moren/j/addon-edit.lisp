;;;
;;; JSCL code mirror plugin
;;;

(defparameter *cm-css-set*
  '("./dist/CodeMirror/lib/codemirror.css"
    "./dist/CodeMirror/theme/blackboard.css"
    "./dist/CodeMirror/addon/scroll/simplescrollbars.css"
    "./dist/CodeMirror/addon/dialog/dialog.css"
    "./css/cm-ed.css"
    ))

(defparameter *cm-js-set*
  '("./dist/CodeMirror/lib/codemirror.js"
   "./dist/CodeMirror/addon/scroll/simplescrollbars.js"
   "./dist/CodeMirror/mode/commonlisp/commonlisp.js"
   "./dist/CodeMirror/addon/selection/active-line.js"
   "./dist/CodeMirror/mode/clike/clike.js"
   "./dist/CodeMirror/keymap/emacs.js"
   "./dist/CodeMirror/addon/edit/matchbrackets.js"
   "./dist/CodeMirror/addon/comment/comment.js"
   "./dist/CodeMirror/addon/dialog/dialog.js"
   "./dist/CodeMirror/addon/search/searchcursor.js"
   "./dist/CodeMirror/addon/search/search.js"
   "./addon/cm-edit.js"
   ()))

(defun cm-plugin()
    (map 'nil (lambda (x) (resource-loader :css x)) *cm-css-set*)
    ;;(map 'nil (lambda (x) (resource-loader :script x)) *cm-js-set*)
    ;;(resource-loader :script "plug/cm-edit.js" #'(lambda (x) (cm-edit-init)))
    (resource-loader :script (first *cm-js-set*)
                     (lambda (a0) 
                         (let* ((s0 (get-internal-real-time)))
                             (format t "~a...~%" (first *cm-js-set*))
                             (map 'nil 
                                  (lambda (x) 
                                      (if (null x)
                                          (format t "Edit plugged ~a ms~%" 
                                                  (/ (- (get-internal-real-time) s0) 1000.0) )
                                          (resource-loader :script x
                                                           (lambda (a1) 
                                                               (let* ((e0 (- (get-internal-real-time) s0)))
                                                                   (format t "~a: ~a ms~%" x (/ e0 1000.0)))) ))) 
                                  (rest *cm-js-set*)))))
    (values))

(cm-plugin)
;;(cm-edit-init)
