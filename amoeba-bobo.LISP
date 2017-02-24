;;This program will simulate the Amoeba Bo-Bo problem
;;Each ameoba can either die, live, or reproduce
;;

(defparameter *limit* 500)
(defparameter *max-cycles* 1000)

(defparameter live 0)
(defparameter reproduce 1)
(defparameter die 2)

(defparameter test-list nil)
(setf test-list (list (cons 'DIED 9)(cons 'DIED 1)(cons 'LIVED 11)))

;;(reduce #'+ (mapcar #'cdr '(('DIED . 4)('DIED . 3))))

(defun amoeba-stats (sample report-func)
  (funcall report-func
           (loop for x from 1 to sample collect (amoeba-bobo) 
                 ))
)

(defun survival-report-func (list)
  (reduce
  (lambda (count result) 
    (if (not (equal (car result) 'died))
            (1+ count)
            count
    )
  )
  list :initial-value 0
  )
)

(defun survival-report (sample)
  (setf num-of-survivals (amoeba-stats sample #'survival-report-func))
  (float (/ num-of-survivals sample))
)

(defun avg-natural-age-report-func (result)
   (setf naturals 0)
   (setf total-age 0)
   
   (reduce
   (lambda (acc result)
     (if (equal (car result) 'died)
         (block x (setf total-age (+ (cdr result) total-age)) (setf naturals (+ acc 1)))
         (block y (setf total-age total-age) (setf naturals acc))
     )
   )
   result :initial-value 0
   )
   
   (float (/ total-age naturals))

)

(defun avg-natural-age-report (sample)
  (amoeba-stats sample #'avg-natural-age-report-func)
  ;;(float (/ total-age sample))
  
)

(defun avg-max-size-report-func (list)
  (reduce
   (lambda (total result)
     (if (equal (car result) 'max-size)
         (+ (cdr result) total)
         total
     )
   )
   list :initial-value 0
   )
)

(defun avg-max-size-report (sample)
  (setf total-age (amoeba-stats sample #'avg-max-size-report-func))
  (float (/ total-age sample))
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