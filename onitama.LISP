;;ONITAMA

;;Parameters

;;Need rule cards for:
;; Monkey, Crane, Mantis, Rabbit, Frog, Tiger, Dragon, Crab,
;; Elephant, Eel, Cobra, Horse, Ox, Rooster, Goose, Boar
;;
;;
;;

(defparameter horse '((0 1) (0 -1) (-1 0)))
(defparameter ox '((0 1) (0 -1) (1 0)))
(defparameter crane '((0  1) (-1 -1) ( 1 -1)))
(defparameter mantis '((-1  1) (1 1) (0 -1)))
(defparameter eel '((-1  1) (-1 -1) (1 0)))
(defparameter cobra '((-1 0) (1 1) (1 -1)))
(defparameter rooster '((-1 -1) (-1 0) (1 0) (1 1)))
(defparameter goose '((-1 1) (-1 0) (1 0) (1 -1)))
(defparameter frog '((-2 0) (-1 1) (1 -1)))
(defparameter rabbit '((-1 -1) (1 1) (2 0)))
(defparameter monkey '((-1 1) (1 1) (-1 -1) (1 -1)))
(defparameter boar '((1 0) (0 -1) (0 1)))
(defparameter tiger '((2 0) (0 -1)))
(defparameter dragon '((-1 -1) (1 -1) (-2 1) (2 1)))
(defparameter crab '((0 1) (-2 0) (2 0)))
(defparameter elephant '((-1 1) (-1 0) (1 0) (1 1)))


;;Structures
(defstruct game win-state side-card pawns masters )

(defstruct player pawn-list won current-cards)

(defstruct board width height legal-moves thrones )

(defstruct move current-pos final-pos)

;;isMoveLegal function
(defun check-card-rule (original-pos new-move card)
                             ;;x coordinate                         ;;y coordinate
   (setf check-move (list (- (nth 0 new-move) (nth 0 original-pos)) (- (nth 1 new-move) (nth 1 original-pos))))
)


;;a function to check if your move is the winning move
;;(Win conditions: kill the other Master or put your Master on the opposite Throne)


;;Board Initialization