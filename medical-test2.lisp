;; Simulates series of trials in order to estimate probability
;; of the patient having the disease given the test for the disease is positive
;;
(defparameter *true-positive* 1)
(defparameter *false-positive* 2)

(defvar *expected-sick-percentage*)
(defvar *test-accuracy-percentage*)

;; Runs the sample to estimate probability of the patient being sick
;; given being positively tested 'num-of-tests' times
;; 'sick' - statistical percentage of the sick cases relative to the population
;; 'accuracy' - percentage of test being accurate (i.e. true positive/true negative).

(defun medical-test (sick accuracy sample num-of-tests)
  (setf *random-state* (make-random-state t))
  (setf *expected-sick-percentage* sick)
  (setf *test-accuracy-percentage* accuracy)
  (setf true-positives 0)
  (setf false-positives 0)
   (dotimes (n sample)
     (case  (repeating-trial num-of-tests)
               (1 (setf true-positives (1+ true-positives)))
               (2 (setf false-positives (1+ false-positives)))
               )
     )
   (float (/ true-positives (+ true-positives false-positives)))
   )


(defun random-percent (percent)
  (<= (random 1.0) (/ percent 100))
  )

;; The trial returns:
;; 1 if the patient has disease and tests positively,
;; 2 if the patient doesn't have disease and tests positively,
;; nil otherwise (i.e. negative tests)

(defun trial ()
   (let* (
         (has-disease? (random-percent *expected-sick-percentage*)) ;; probability of the patient having disease is 1% 
         (true-positive-test? (random-percent *test-accuracy-percentage*)) ;; probability of the positive test being valid is 95%
         (false-positive-test?  (not true-positive-test?)) 
         )
     (cond
       ((and has-disease? true-positive-test?) *true-positive*)
       ((and (not has-disease?) false-positive-test?) *false-positive*)
       (t nil)
      )
   )
   )

(defun next-trial (prev-result)
  (cond
    ((and (eql prev-result *true-positive*) (random-percent *test-accuracy-percentage*)) *true-positive*)
   
    ((and (eql prev-result *false-positive*) (random-percent (- 100 *test-accuracy-percentage*))) *false-positive*)
   (t nil)
   )
  )

(defun repeating-trial (repeats)
  (let ((current-trial (trial)))
  (case repeats
    (1 first-trial)
    (t
     (dotimes (n (1- repeats))
              (setf current-trial (next-trial current-trial))
              )
     )
    )
    current-trial
  )
)
