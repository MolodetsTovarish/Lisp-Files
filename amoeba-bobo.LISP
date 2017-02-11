;;This program will simulate the Amoeba Bo-Bo problem
;;Each ameoba can either die, live, or reproduce
;;

(defparameter *limit* 500)
(defparameter *max-cycles* 1000)

(defparameter live 0)
(defparameter reproduce 1)
(defparameter die 2)

(defun amoeba-bobo-simulation (sample)
  
    (float (/ 
            (count t (loop for x from 1 to sample collect 
           (if (> (amoeba-bobo) 0) 
                 t nil                 
                 )
    )) sample))
    
)

(defun amoeba-bobo () 
(setf *random-state* (make-random-state t))
  (let* ((colony-size 1) (total 1) (deaths 0) (survivors 0) (reproductions 0) (cycles 0))
    (loop while (and (> colony-size 0) (<= colony-size *limit*) (< cycles *max-cycles*))
      do (setf colony-size (next-step colony-size)) (setf cycles (1+ cycles))
      ;;(print total)
      )
    colony-size
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

;;(let ((x 0)) (loop while (< x 10) do (setf x (1+ x))) x)