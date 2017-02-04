;;This program will simulate the Amoeba Bo-Bo problem
;;Each ameoba can either die, live, or reproduce
;;

(defparameter live 0)
(defparameter reproduce 1)
(defparameter die 2)

(defun amoeba-bo-bo (cycles) 
  (let ((total 1) (deaths 0) (survivors 0) (reproductions 0))
    (loop for x from 1 to cycles
       do (case (random 3)
           (0 (+ 0 total))
           (1 (setf total (1+ total)))
           (2 (setf total (1- total)))
          )
      )
    total
  )
)