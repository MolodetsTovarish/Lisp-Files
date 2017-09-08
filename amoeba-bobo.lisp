;;This program will simulate the Amoeba Bo-Bo problem
;;Each ameoba can either die, live, or reproduce
;;

(defparameter *limit* 500)
(defparameter *max-cycles* 1000)

(defparameter test-list nil)
(setf test-list (list (cons 'DIED 9)(cons 'DIED 1)(cons 'LIVED 11)))

;;This function applies any report function on a list of amoeba-bobo results.
;;This function is responsible for simulating the amoeba-bobo problem on a given sample for multiple colonies. 
(defun amoeba-stats (sample init-report-value report-func)
  (setf acc init-report-value)
  ;;(funcall report-func
           (loop for x from 1 to sample do
                 (setf acc (funcall report-func (amoeba-bobo) acc))
                 )
;;)
acc
)

;;This report function checks if an amoeba colony has died. 
;;If not, it increments the count for the total number of survivals.
(defun survival-report-func (result acc)
  (setf count acc)
  
    (if (not (equal (car result) 'died))
            (setf count (1+ count))
            count
    )
  
  count
)

;;The survival report uses the amoeba-stats function to apply the survival-report-func on a given sample,
;;returning the percentage of colonies that survive.
(defun survival-report (sample)
  (setf num-of-survivals (amoeba-stats sample 0 #'survival-report-func))
  (float (/ num-of-survivals sample))
)

;;This report function returns the average age of colonies that die naturally.
;;The average is calculated within this function.
;;The avg natural age report uses the amoeba-stats function to apply the avg-natural-age-report-func on a given sample,
;;returning the average age of colonies that die off naturally.
(defun avg-natural-age-report (sample)
(setf naturals 0)
(setf total-age 0)

(setf report-func
   (lambda (result acc)
     (if (equal (car result) 'died)
         (block x (setf total-age (+ (cdr result) total-age)) (setf naturals (+ naturals 1)))
     )
     ))
  
  (amoeba-stats sample nil report-func)

  ;;Calculating average age
   (float (/ total-age naturals))
)

;;This report function returns the average age of colonies that reach the maximum size limit.
;;The avg max-size report uses the amoeba-stats function to apply the avg-max-size-report-func on a given sample,
;;returning the average age of colonies that reach the maximum size.
(defun avg-max-size-report (sample)
  (setf survivals 0)
  (setf total-age 0)
  
  (setf report-func
   (lambda (result acc)
     (if (equal (car result) 'max-size)
         (block x (setf total-age (+ (cdr result) total-age)) (setf survivals (+ survivals 1)))
     )
   ))

  (amoeba-stats sample nil report-func)

  (float (/ total-age survivals))
)

;;The amoeba-bobo function simulates a colony for the amoeba-bobo problem.
;;This one colony's life ends when it dies (reaches size zero), exceeds the max size,
;;or lives past the max age. Death, survival, and reproduction for each amoeba are decided
;;in the next-step function, which iterates on the current colony size until one of the conditions
;;are met.
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
