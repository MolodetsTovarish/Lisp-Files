(defparameter seconds 0.001)
(defparameter milliseconds 1)
(defparameter microseconds 1000)

;;this function records the runtime of a given function for a given amount of tests, and returns the total runtime (milliseconds by default)
;;Parameters:
;;  func takes in a lambda anonymous function, which is called in the program
;;  load-tests are how many times you want the function to be tested
;;  units are what units of time you want the result to be returned in
(defun load-test (func load-tests &optional units)
  (let ((x 0.0))
    (setf x (get-internal-real-time))
    (loop for y from 1 to load-tests do (time (funcall func)))
    (if (equal units nil)
      (float (- (get-internal-real-time) x))
      (float (* (- (get-internal-real-time) x) units)))
    
))

;;the function to be inputted into the load-test
(defparameter myfunc nil)
(setf myfunc (lambda() (medical-test 1 95 100000 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;OLD VERSION;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;this function returns the time it takes for a function to process (NON-LAMBDA VERSION)
(defun load-test-simple (load-tests)
(let ((x 0.0))
  (setf x (get-internal-real-time))
  (loop for y from 1 to load-tests do (setf x (+ x (time (medical-test 1 95 100000 1)))))
  (- (get-internal-real-time) x)
))