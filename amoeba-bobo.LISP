;;This program will simulate the Amoeba Bo-Bo problem
;;Each ameoba can either die, live, or reproduce
;;

(defparameter *limit* 500)
(defparameter *max-cycles* 1000)

(defparameter live 0)
(defparameter reproduce 1)
(defparameter die 2)

;;(defun amoeba-bobo-simulation (sample)
;;    
;;    (loop for x from 1 to sample collect 
;;           (if (> (cdr (amoeba-bobo)) 0) 
;;                 (cdr (amoeba-bobo))                 
;;                 )
;;    )
;;)

(defun amoeba-stats (sample report-func)
  ;;(funcall report-func
           (loop for x from 1 to sample collect (amoeba-bobo) 
                 )
)

(defun percentage-survivals ()
  
)

(defun amoeba-bobo () 
(setf *random-state* (make-random-state t))
  (let* ((colony-size 1) (cycles 0))
    (loop while T
      do (setf colony-size (next-step colony-size)) (setf cycles (1+ cycles))
      
      (cond ((>= cycles *max-cycles*) (return-from amoeba-bobo (cons 'max-age colony-size)))
          ((<= colony-size 0) (return-from amoeba-bobo (cons 'died cycles)))
          ((>= colony-size *limit*) (return-from amoeba-bobo (cons 'max-size cycles)))
        )
      )
  )
)

;;This function takes the current size of the amoeba colony, looping through each individual
;;amoeba to decide whether it should live, die (subtract one), or reproduce (add one). It then returns
;;the new size of the colony.
(defun next-step (current-size)
(let ((new-size current-size))
  (loop for x from 1 to current-size
       do (case (random 3)
           (1 (setf new-size (1+ new-size)))
           (2 (setf new-size (1- new-size)))
          )
       ) new-size
))