;;;
;;; viewport sketch
;;;
;;; Copyright, 2017, mvk
;;;


(export '(viewport))
(def!struct viewport
    position
    left top
    width height
    border
    div)

;;; viewport background template
;;;
(export '(wpbackground))
(def!struct wpbackground color image repeat attachment position)

;;;
;;; color - background color
(defun wpbackground-expand (bp)
    (let* ((rs '()))
        (labels ((result (fmt arg)
                     (push (format nil fmt arg) rs)))
            (if (wpbackground-color bp) (result "background-color:~a;" (wpbackground-color bp)))
            (if (wpbackground-image bp) (result "background-image:~a;" (wpbackground-image bp)))
            (if (wpbackground-repeat bp) (result "background-repeat:~a;" (wpbackground-repeat bp)))
            (if (wpbackground-attachment bp) (result "background-attachment:~a;" (wpbackground-attachment bp)))
            (if (wpbackground-position bp) (result "background-position:~a;" (wpbackground-position bp)))
            (print (list 'back (apply #'concat (reverse rs)) ))
            (apply #'concat (reverse rs)))))

;;; set background css
;;; any div any style attributes
;;;
(export '(wp-background-css))
(defun wp-background-css (back-owner &rest conses)
    (map nil (lambda (x)
                 (setf (oget back-owner "style" (concat "background-" (car x))) (cdr x))) conses)
    back-owner)


;;; set viewport css
;;;
(export '(viewport-background-css))
(defun viewport-background-css (vp &rest conses)
    (let ((back-owner (viewport-div vp)))
        (map nil (lambda (x)
                     (setf (oget back-owner "style" (concat "background-" (car x))) (cdr x))) conses)
        (values-list nil)))





;;; viewport border template
;;; :radius "5px" :style "groove" etc. etc. from atribute border spec
;;;
(export '(wpborder))

(def!struct wpborder style width color padding radius padding margin background )


;;;
;;; set border css style
;;;
;;; border's style for any div
;;;
;;; (wp-border-css (dom-create "div")
;;;      (cons "width" "3px") (cons "color" "white") (cons "style" "groove"))
;;; =>
;;;     #<JS-OBJECT [object HTMLDivElement]>
;;;
;;; border style for viewport unit
;;;
;;; (viewport-border-css (*vp)
;;;      (cons "width" "3px") (cons "color" "white") (cons "style" "groove"))
;;;
;;; => none
;;;
(export '(wp-border-css))
(defun wp-border-css (border-owner &rest conses)
    (map nil (lambda (x)
                 (setf (oget border-owner "style" (concat "border-" (car x))) (cdr x))) conses)
    border-owner)

(export '(viewport-border-css))
(defun viewport-border-css (vp &rest conses)
    (let ((border-owner (viewport-div vp)))
        (map nil (lambda (x)
                     (setf (oget border-owner "style" (concat "border-" (car x))) (cdr x))) conses)
        (values-list nil)))


(defun wpborder-expand (bp)
    (let* ((rs '()))
        (labels ((result (fmt arg)
                     (push (format nil fmt arg) rs)))

            (if (wpborder-style bp) (result "border-style:~a;'" (wpborder-style bp) ))
            (if (wpborder-width bp) (result "border-width:~a;" (wpborder-width bp) ))
            (if (wpborder-color bp) (result "border-color:~a;" (wpborder-color bp) ))
            (if (wpborder-padding bp) (result "border-padding:~a;" (wpborder-padding bp) ))
            (if (wpborder-radius bp) (result "border-radius:~a;" (wpborder-radius bp) ))
            (apply #'concat rs))))

;;;
;;; position = absolute | fixed | relative
;;; color = any color for text "red" "orange" "#0666"
;;; hide =t do not display until unhide viewport
;;; drag =t make viewport draggable. just click mousee and pull
;;; scroll = auto | hidden | scroll | visible | inherit
;;;

(export '(viewport-create))
(defun viewport-create (&key (parent nil) (childs nil) (position "absolute")
                          (left 10) (top 10) (width 300) (height 200) color
                          border (padding nil) margin background (hide nil) (drag nil)
                          (scroll nil))
    (let* ((css)
           (div)
           (vp)
           (rs '()))
        (setf css
              (concat "position:" position ";"
                      (format nil " left:~apx; top:~apx; width:~apx; height:~apx;" left top width height)))
        (push css rs)
        (if padding (push (format nil "padding:~a;" padding) rs))
        (if margin (push (format nil "margin:~a;" margin) rs))
        (if color (push (format nil "color:~a;" color) rs) )
        (if background (push (wpbackground-expand background) rs))
        (if border (push (wpborder-expand border) rs))
        (if scroll (push (format nil "overflow:~a;") rs))
        (setf div (dom-create "div"
                              (list (cons "id" (jscl::gen-uid "vp" "div"))
                                    (cons "style" (apply #'concat (reverse rs))))))
        (if hide (setf (oget div "style" "display") "none"))
        (setf vp (make-viewport :position position :left left :top top :width width :height height :border border :div
                                div ))
        (if childs
            (apply #'dom-mount (append (list div) (mapcar (lambda (x) (if (symbolp x) (symbol-value x) x)) childs))))
        (if parent (dom-mount parent div))
        (if drag
            (let* ((pvp (#j:$ div )))
                (funcall ((oget pvp "draggable" "bind") pvp))) )
        vp))



;;;
;;; (setf vp (create-viewport :border " border-style:groove; border-width:1px; border-color:blue; padding:1px"))
;;;

;;; todo: Depricate
(export '(create-viewport))
(defun create-viewport (&key (position "absolute") (left 10) (top 10) (width 300) (height 200) border)
    (let* ((css)
           (div)
           (vp))
        (setf css
              (concat "position:" position ";"
                      (format nil " left:~apx; top:~apx; width:~apx; height:~apx;" left top width height)))

        (if border (setf css (concat css border)))

        (setf div (dom-create "div"
                              (list (cons "id" (jscl::gen-uid "vp" "div"))
                                    (cons "style" css))))
        (setf vp (make-viewport :position position :left left :top top :width width :height height :border border :div div ))
        vp))


;;;
;;;
;;; o1 => (create-viewport :position "relative" :left 1 :top 1 :width w1 :height h1)
;;; o2 => (create-viewport :position "relative" :left 5 :top 5 :width w2 :height h2)
;;; o3 => (create-viewport :width (+ w1 w2 10) :height (+ h1 h2 10))
;;;
;;; (dom-mount (viewport-div 03) (viewport-div o1) (viewport-div o2))
;;; (dom-mount (dom-get-body) (viewport-div o3))
;;;                             or
;;; (viewport-childs-mount o3 o1 o2)
;;; (viewport-mount (dom-get-body) o3)
;;;

(export '(viewport-childs-mount))

(defun viewport-childs-mount (vp &rest childs)
    (apply #'dom-mount (append (list (viewport-div vp)) (mapcar (lambda (x) (viewport-div x)) childs))))


;;;
;;; (viewport-get-position vp) => ("100px" "100px")
;;; (viewport-get-position vp t) => (100 100)
;;; => (left top)
;;;

(export '(viewport-get-position))

(defun viewport-get-position (vp &optional (int nil))
    (let* ((div (viewport-div vp))
           (left (oget div  "style" "left"))
           (top (oget div  "style" "top")))
        (flet ((parser (str) (parse-integer str :junk-allowed t)))
            (list (if int (parser left) left)
                  (if int (parser top) top)))))


;;;
;;; (viewport vp :left 120 :top 100)
;;;

(export '(viewport-move))

(defun viewport-move (vp &key left top)
    (let* ((div (viewport-div vp)))
        (if left
            (setf (oget div  "style" "left") (format nil "~apx" left)))
        (if top
            (setf (oget div "style" "top") (format nil "~apx" top))))
    (values))


;;;
;;; display / none
;;;

(export '(viewport-hide))

(defun viewport-hide (vp &optional (hid t))
    (if hid
        (setf (oget (viewport-div vp) "style" "display") "none")
        (setf (oget (viewport-div vp) "style" "display") ""))
    (values))



(defun viewport-hide (vp &optional (hid t))
    (symbol-macrolet ((viewport-display (oget (viewport-div vp) "style" "display")))
        (if hid
            (setf viewport-display "none")
            (setf viewport-display ""))
        (values)))


;;;
;;; zIndex
;;; todo:

(export '(viewport-front viewport-back viewport-layer))

(defun viewport-front (vp &optional (zIndex 90))
    (setf (oget (viewport-div vp) "style" "zIndex") zIndex)
    (values) )

(defun viewport-back (vp)
    (setf (oget (viewport-div vp)  "style" "zIndex") -1)
    (values) )

(defun viewport-layer (vp layer)
    (setf (oget (viewport-div vp)  "style" "zIndex") layer)
    (values) )


;;;
;;; (viewport-image vp img)
;;; (viewport-image vp (res-refer img-nick))
;;; (viewport-image (res-refer vp-nick) (res-refer img-nicl))
;;;
;;; todo:

(export '(viewport-image))
(defun viewport-image (vp img)
    (dom-mount (viewport-div vp) img))



;;;
;;; (viewport-replace-image vp old new)
;;; old, new - dom img element
;;;

(export '(viewport-replace-image))
(defun viewport-replace-image (vp old new)
    (let* ((div (viewport-div vp)))
        (dom-remove div old)
        (dom-mount div new)))

;;;
;;; (viewport-remove-image vp img)
;;; (viewport-remove-image vp (res-refer img-nick))
;;;
;;; img - dom img element
;;;

(export '(viewport-remove-image))
(defun viewport-remove-image (vp img)
    (dom-remove (viewport vp) img))



;;; jquery css
;;;
;;; (viewport-css vp prop value)
;;;

(export '(viewport-get-css))
(defun viewport-get-css (vp prop)
    (let* ((div (#j:$ (viewport-div vp))))
        (funcall ((oget div "css" "bind") div prop))))


;;;
;;; (viewport-set-css vp "left" "100px")
;;;
;;; (viewport-set-css vp "left" (lambda (idx oldvalue) (declare (ignore idx)) "100px") )
;;; note: format oldvalue in css syntax
;;;       i.e for property "left" => "500px"
;;;

(export '(viewport-set-css))
(defun viewport-set-css (vp prop value)
    (let* ( (div (#j:$ (viewport-div vp))))
        (funcall ((oget div "css" "bind") div prop value))))

;;;
;;; (viewport-mset-css vp "left" "100px" "top" "10px")
;;;

(export '(viewport-mset-css))


(defun viewport-mset-css (vp &rest props)
    (let* ((arg (apply #'make-js-object props ))
           (div (#j:$ (viewport-div vp))))
        (funcall ((oget div "css" "bind") div arg))))




;;; jquery.draggable
;;;
;;; (viewport-draggable vp)
;;;

(export '(viewport-draggable))
(defun viewport-draggable (vp)
    (let* ((pvp (#j:$ (viewport-div vp))))
        (funcall ((oget pvp "draggable" "bind") pvp))))


#|
(defun viewport-tabs (vp)
    (let* ((pvp (#j:$ vp)))
        (funcall ((oget pvp "tabs" "bind") pvp))))
|#


;;;
;;; change  viewport backgroundImage
;;;
;;; (viewport-background dom-elt "images/way.jpg")

(export '(replace-body-background-image))
(defun replace-body-background-image (ref &optional style)
    (let* ((url (concat "url(" ref ")"))
           (dom (dom-get-body)))
        (setf (oget dom "style" "backgroundImage") url)
        (when style
            (setf (oget dom "style" "backgroundSize") style))))


(export '(wp-background-image))
(defun wp-background-image (dom ref &optional style)
    (let* ((url (concat "url(" ref ")")))
        (setf (oget dom "style" "backgroundImage") url)
        (when style
            (setf (oget dom "style" "backgroundSize") style))))


(defun viewport-background-image (vp ref &optional style)
    (let* ((url (concat "url(" ref ")"))
           (dom (viewport-div vp)))
        (setf (oget dom "style" "backgroundImage") url)
        (when style
            (setf (oget dom "style" "backgroundSize") style))))


;;;;; eof
