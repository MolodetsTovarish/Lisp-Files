(setf *random-state* (make-random-state t))

(defun medical-test (percentage-sick positive-percentage sample tests)
    (let* (
        (sick 0)
        (healthy 0)
        (true-positive 0)
        (false-positive 0)
        (num-of-tests (- tests 1))
        )
  ;;set the amount of sick people
  (setf sick (count t (loop for x from 1 to sample collect (random-percentage percentage-sick))))
  ;;set the amount of healthy people (total population - sick population)
  (setf healthy (- sample sick))
  
  ;;set the amount of sick people who get a true positive result on the test
  (setf true-positive (count t (loop for x from 1 to sick collect (random-percentage positive-percentage))))
  (setf true-positive (additional-tests true-positive num-of-tests positive-percentage))

  ;;set the amount of healthy people who get a false positive result on the test
  (setf false-positive (count t (loop for x from 1 to healthy collect (random-percentage (- 100 positive-percentage)))))
  (setf false-positive (additional-tests false-positive num-of-tests (- 100 positive-percentage)))

  ;;return final percentage of being sick while having a positive result on the test
  (float (/ true-positive (+ true-positive false-positive)))
))

;;this function will make the testing for true and false positives more accurate for however many extra tests the user wants
(defun additional-tests (sample num-of-tests percentage)
(let* ((result sample))
  (loop for y from 1 to num-of-tests 
    do (setf result (count t (loop for x from 1 to result collect (random-percentage percentage))))) result
  )
)

;;this function returns true if the random number is less than the inputted number
(defun random-percentage (percentage)
  (<= (random 100.0) percentage)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;Additional Test Pseudocode
;;              (additional tests (sample num-of-tests percentage))
;;                for <num-of-tests> loops {
;;                    collect true if (< 100 percentage)
;;                }
;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;EXTRA (and previous) NOTES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(defun medical-test-mk2 (percentage-sick positive-percentage sample)
;;    (let* (
;;        (sick 0)
;;        (healthy 0)
;;        (true-positive 0)
;;        (false-positive 0)
;;        )
  ;;set the amount of sick people
;;  (setf sick (count t (loop for x from 1 to sample collect (<= (random 100.0) percentage-sick))))
  ;;set the amount of healthy people (total population - sick population)
;;  (setf healthy (- sample sick))
  
  ;;set the amount of sick people who get a true positive result on the test
;;  (setf true-positive (count t (loop for x from 1 to sick collect (<= (random 100.0) positive-percentage))))

  ;;set the amount of healthy people who get a false positive result on the test
;;  (setf false-positive (count t (loop for x from 1 to healthy collect (<= (random 100.0) (- 100 positive-percentage)))))


  ;;return final percentage of being sick while having a positive result on the test
;;  (/ true-positive (+ true-positive false-positive))

;;)
;;)

;; (let* ((result 0)) (loop for y from 1 to 10
;;    do (setf result (count t (loop for x from 1 to 10000 collect (<= (random 100.0) 95))))
;;  return result))

;;(count t (loop for x from 1 to sample collect (run switch?))))

;; (count t (loop for x from 1 to 10000 collect (<= (random 100.0)(- 100 99))))
