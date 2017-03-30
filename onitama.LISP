;;ONITAMA

;;Parameters

;;Need rule cards for:
;; Monkey, Crane, Mantis, Rabbit, Frog, Tiger, Dragon, Crab,
;; Elephant, Eel, Cobra, Horse, Ox, Rooster, Goose, Boar
;;
;;
;;

(defparameter horse '((0 . 1) (0 . -1) (-1 . 0)))
(defparameter ox '((0 . 1) (0 . -1) (1 . 0)))
(defparameter crane '((0 . 1) (-1 . -1) (1 . -1)))
(defparameter mantis '((-1 . 1) (1 . 1) (0 . -1)))
(defparameter eel '((-1 1) (-1 . -1) (1 . 0)))
(defparameter cobra '((-1 . 0) (1 . 1) (1 . -1)))
(defparameter rooster '((-1 . -1) (-1 . 0) (1 . 0) (1 . 1)))
(defparameter goose '((-1 . 1) (-1 . 0) (1 . 0) (1 . -1)))
(defparameter frog '((-2 . 0) (-1 . 1) (1 . -1)))
(defparameter rabbit '((-1 . -1) (1 . 1) (2 . 0)))
(defparameter monkey '((-1 . 1) (1 . 1) (-1 . -1) (1 . -1)))
(defparameter boar '((1 . 0) (0 . -1) (0 . 1)))
(defparameter tiger '((2 . 0) (0 . -1)))
(defparameter dragon '((-1 . -1) (1 . -1) (-2 . 1) (2 . 1)))
(defparameter crab '((0 . 1) (-2 . 0) (2 . 0)))
(defparameter elephant '((-1 . 1) (-1 . 0) (1 . 0) (1 . 1)))


;;Structures
(defstruct game win-state side-card pawns masters )

(defstruct player pawn-list won current-cards)

(defstruct board width height legal-moves thrones )

(defstruct move current-pos final-pos)

;;isMoveLegal function
(defun check-card-rule (original-pos final-pos card)

   (setf h-shift (- (car final-pos) (car original-pos)))
   (setf v-shift (- (cdr final-pos) (cdr original-pos)))

   (member nil card 
           :test (lambda (item card-rule) (and (eq h-shift (car card-rule)) (eq v-shift (cdr card-rule)))))
       
     
)


;;a function to check if your move is the winning move
;;(Win conditions: kill the other Master or put your Master on the opposite Throne)


;;Board Initialization