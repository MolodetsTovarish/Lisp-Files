;;This program will simulate the Amoeba Bo-Bo problem
;;Each ameoba can either die, live, or reproduce
;;

(defparameter limit 500)

(defparameter live 0)
(defparameter reproduce 1)
(defparameter die 2)


(defun amoeba-bo-bo () 
  (let ((total 1) (deaths 0) (survivors 0) (reproductions 0))
    (loop while (and (> total 0) (<= total limit))
      do (setf total (next-step total))
      )
    total
  )
)

;;This function takes the current size of the amoeba colony, looping through each individual
;;amoeba to decide whether it should live, die (subtract one), or reproduce (add one). It then updates
;;the current size of the colony.
(defun next-step (current-size)
  (loop for x from 1 to current-size
       do (case (random 3)
           (0 (setf current-size current-size))
           (1 (setf current-size (1+ current-size)))
           (2 (setf current-size (1- current-size)))
          )
       ) current-size
)