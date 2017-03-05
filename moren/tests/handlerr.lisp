(defun han (p) 
   (case p 
     (0 (list 'ok)) 
     (1 (warn "Warn")) 
     (2 (error "Error"))))

(defparameter *bastard (dom-create "img"))


(batch-begin "Error handlers")

(test-equal (handler-case  
 (let () (han 0) )  
 (error (err) "HAN err" )  
 (warning (war) "HAN war" )) (list 'ok) )

(test-equal (handler-case  
 (let () (han 1) )  
 (error (err) "HAN err" )  
 (warning (war) "HAN war" )) "HAN war" )

(test-equal (handler-case  
 (let () (han 2) )  
 (error (err) "HAN err" )  
 (warning (war) "HAN war" )) "HAN err" )


(test (handler-case  
 (oget *bastard "parentNode")   
 (error (err) (error (list "HAN err" err)))  
 (warning (war) (list "HAN war" war))))

(test-equal (handler-case  
 (oget *bastard "parentNode")   
 (error (err) "HAN err" )  
 (warning (war) "HAN war")) "HAN err") 


(test (handler-case  
 (let () (oget *bastard "parentNode"))   
 (error (err) (list "HAN err" err))  
 (warning (war) (list "HAN war" war))))



(test (handler-case  
 (let () (oget *bastard "parentNode") nil )  
 (error (err) (list "HAN err" err))  
 (warning (war) (list "HAN war" war))))

(test (handler-case  
 (let () (oget *bastard "parentNode") t )  
 (error (err) (list "HAN err" err))  
 (warning (war) (list "HAN war" war))))

(test (handler-case  
 (let () (print (oget *bastard "parentNode")))  
 (error (err) (list "HAN err" err))  
 (warning (war) (list "HAN war" war))))

(batch-end)


