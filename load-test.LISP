;;this function returns the time it takes for a function to process
(defun load-test (func load-tests)
  (let ((x 0.0))
    (setf x (get-internal-real-time))
    (loop for y from 1 to load-tests do (time (funcall func)))
    (float (/ (- (get-internal-real-time) x) 1000))

))

;;the function to be inputted into the load-test
(defparameter myfunc nil)
(setf myfunc (lambda() (medical-test 1 95 100000 1)))

;;this function returns the time it takes for a function to process (NON-LAMBDA VERSION)
(defun load-test-simple (load-tests)
(let ((x 0.0))
  (setf x (get-internal-real-time))
  (loop for y from 1 to load-tests do (setf x (+ x (time (medical-test 1 95 100000 1)))))
  (- (get-internal-real-time) x)
))